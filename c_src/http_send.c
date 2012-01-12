#include "gen_http.h"



void gen_http_drv_schedule_write(ErlDrvData handle, ErlIOVec *ev)
{
  HTTP* d = (HTTP *)handle;
  driver_enqv(d->port, ev, 0);
  //fprintf(stderr, "Queue %d bytes, %d\r\n", ev->size,  driver_sizeq(d->port));
  activate_write(d);
  if(driver_sizeq(d->port) > d->buffer_limit) {
    set_busy_port(d->port, 1);
  }
}



void gen_http_drv_ready_output(ErlDrvData handle, ErlDrvEvent event)
{
  HTTP* d = (HTTP*) handle;
  
  if(d->mode == REQUEST_MODE && d->state == CONNECTING_STATE) {
    accept_connection(d);
    return;
  }
  
  
  SysIOVec* vec;
  int vlen = 0;
  size_t written;
  
  vec = driver_peekq(d->port, &vlen);
  if(!vec || !vlen) {
    deactivate_write(d);
    return;
  }
  written = writev(d->socket, (const struct iovec *)vec, vlen > IOV_MAX ? IOV_MAX : vlen);
  if(vlen > IOV_MAX) {
    fprintf(stderr, "Buffer overloaded: %d, %d\r\n", vlen, (int)(driver_sizeq(d->port) - written));
  }
  if(written == -1) {
    if((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN)) {
        // fprintf(stderr, "Error in writev: %s, %d bytes left\r\n", strerror(errno), (int)driver_sizeq(d->port));
      tcp_exit(d);
      return;
    }
  } else {
    ErlDrvSizeT rest = driver_deq(d->port, written);
    
    if(rest == 0) {
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, atom_http,
        ERL_DRV_PORT, driver_mk_port(d->port),
        ERL_DRV_ATOM, atom_empty,
        ERL_DRV_TUPLE, 3
      };
      
      pid_list_send(d->exhausted, d->port, reply, sizeof(reply) / sizeof(reply[0]));
      pid_list_free(&d->exhausted);
      
      set_busy_port(d->port, 0);
    }
    // fprintf(stderr, "Network write: %d (%d)\r\n", (int)written, (int)rest);
    
  }
}

