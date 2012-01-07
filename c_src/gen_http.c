#include "gen_http.h"

ErlDrvTermData atom_http;
ErlDrvTermData atom_keepalive;
ErlDrvTermData atom_close;
ErlDrvTermData atom_eof;
ErlDrvTermData atom_empty;
ErlDrvTermData atom_connected;
ErlDrvTermData method_atoms[HTTP_PATCH+1];





static int gen_http_init(void) {
  atom_http = driver_mk_atom("http");
  atom_keepalive = driver_mk_atom("keepalive");
  atom_close = driver_mk_atom("close");
  atom_eof = driver_mk_atom("eof");
  atom_empty = driver_mk_atom("empty");
  atom_connected = driver_mk_atom("connected");
  int i;
  for(i = 0; i <= HTTP_PATCH; i++) {
    method_atoms[i] = driver_mk_atom((char *)http_method_str(i));
  }

  init_http_handling();
  init_cache();
  return 0;
}


static ErlDrvData gen_http_drv_start(ErlDrvPort port, char *buff)
{
    HTTP* d = (HTTP *)driver_alloc(sizeof(HTTP));
    bzero(d, sizeof(HTTP));
    d->port = port;
    // set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    return (ErlDrvData)d;
}


static void gen_http_drv_stop(ErlDrvData handle)
{
  HTTP* d = (HTTP *)handle;
  if(d->parser) {
    driver_free(d->parser);
    d->parser = NULL;
  }
  if(d->buffer) {
    driver_free_binary(d->buffer);
    d->buffer = NULL;
  }
  if(d->body) {
    driver_free_binary(d->body);
    d->body = NULL;
  }
  if(d->mode == LISTENER_MODE) {
    Acceptor *a = d->acceptor;
    Acceptor *next;
    
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("http_closed"),
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_TUPLE, (ErlDrvTermData)2
    };
    
    while(a) {
      next = a->next;
      driver_send_term(d->port, a->pid, reply, sizeof(reply) / sizeof(reply[0]));      
      driver_free(a);
      a = next;
    }
    
  }
  
  deactivate_write(d);
  deactivate_read(d);
  close(d->socket);
  driver_free((char*)handle);
}


static void tcp_exit(HTTP *d)
{
  deactivate_write(d);
  deactivate_read(d);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("http_closed"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, (ErlDrvTermData)2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_exit(d->port, 0);
}

static void gen_http_drv_schedule_write(ErlDrvData handle, ErlIOVec *ev)
{
  HTTP* d = (HTTP *)handle;
  driver_enqv(d->port, ev, 0);
  //fprintf(stderr, "Queue %d bytes, %d\r\n", ev->size,  driver_sizeq(d->port));
  activate_write(d);
}



static void gen_http_drv_ready_output(ErlDrvData handle, ErlDrvEvent event)
{
  HTTP* d = (HTTP*) handle;
  
  if(d->mode == REQUEST_MODE && d->state == CONNECTING_STATE) {
    accept_connection(d);
    return;
  }
  
  
  SysIOVec* vec;
  int vlen = 0;
  size_t written;
  driver_set_timer(d->port, d->timeout);
  vec = driver_peekq(d->port, &vlen);
  if(!vec || !vlen) {
    driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 0);
    return;
  }
  written = writev(d->socket, (const struct iovec *)vec, vlen > IOV_MAX ? IOV_MAX : vlen);
  if(vlen > IOV_MAX) {
    fprintf(stderr, "Buffer overloaded: %d, %d\r\n", vlen, (int)(driver_sizeq(d->port) - written));
  }
  if(written == -1) {
    if((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN)) {
        fprintf(stderr, "Error in writev: %s, %d bytes left\r\n", strerror(errno), (int)driver_sizeq(d->port));
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
      
      driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
    }
    // fprintf(stderr, "Network write: %d (%d)\r\n", (int)written, (int)rest);
    
  }
}


void activate_write(HTTP *d) {
  driver_select(d->port, (ErlDrvEvent)d->socket, ERL_DRV_WRITE, 1);
}

void deactivate_write(HTTP *d) {
  driver_select(d->port, (ErlDrvEvent)d->socket, ERL_DRV_WRITE, 0);
}

void activate_read(HTTP *d) {
  driver_select(d->port, (ErlDrvEvent)d->socket, ERL_DRV_READ, 1);
}

void deactivate_read(HTTP *d) {
  driver_select(d->port, (ErlDrvEvent)d->socket, ERL_DRV_READ, 0);
}

static int error_reply(char **rbuf, char *err) {
  char *s = *rbuf;
  *s = 0;
  int len = strlen(err);
  memcpy(s+1, err, len);
  return len+1;
  
}

static int errno_reply(char **rbuf) {
  return error_reply(rbuf, erl_errno_id(errno));
}

static ErlDrvSSizeT gen_http_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
  HTTP* d = (HTTP*) handle;
  
  switch(command) {
    case CMD_LISTEN: {
      int flags;
      struct sockaddr_in si;
      
      d->socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
      if(d->socket == -1) {
        return errno_reply(rbuf);
      }
      
      if(len != sizeof(Config)) {
        driver_failure_atom(d->port, "invalid_config");
        return 0;
      }
      
      int on = 1;
      if(d->config.reuseaddr || 1) {
        if(setsockopt(d->socket, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) == -1) {
          return errno_reply(rbuf);
        }
      }
      if(d->config.keepalive) {
        if(setsockopt(d->socket, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof(on)) == -1) {
          return errno_reply(rbuf);
        }
      }
      
      
      
      memcpy(&d->config, buf, sizeof(Config));
      
      bzero(&si, sizeof(si));
      si.sin_family = AF_INET;
      si.sin_port = d->config.port; // It comes in network byte order
      si.sin_addr.s_addr = htonl(INADDR_ANY);
      if(bind(d->socket, (struct sockaddr *)&si, sizeof(si)) == -1) {
        return errno_reply(rbuf);
      }
      flags = fcntl(d->socket, F_GETFL);
      if(flags == -1) {
        return errno_reply(rbuf);
      }
      if(fcntl(d->socket, F_SETFL, flags | O_NONBLOCK) == -1) {
        return errno_reply(rbuf);
      }
      
      d->mode = LISTENER_MODE;
      if(listen(d->socket, d->config.backlog) == -1) {
        return errno_reply(rbuf);
      }
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_ACTIVE_ONCE: {
      if(d->mode == LISTENER_MODE) {
        return error_reply(rbuf, "listener_mode");
      }
      
      activate_read(d);
      driver_set_timer(d->port, d->timeout);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_RECEIVE_BODY: {
      if(d->mode == LISTENER_MODE) {
        return error_reply(rbuf, "listener_mode");
      }
      d->settings.on_body = receive_body;
      memcpy(*rbuf, "ok", 2);
      return 2;
    }

    case CMD_SKIP_BODY: {
      if(d->mode == LISTENER_MODE) {
        return error_reply(rbuf, "listener_mode");
      }
      d->settings.on_body = skip_body;
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_SET_CHUNK_SIZE: {
      if(d->mode == LISTENER_MODE) {
        return error_reply(rbuf, "listener_mode");
      }
      d->chunk_size = *(uint32_t *)buf;
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_ACCEPT_ONCE: {
      if(d->mode != LISTENER_MODE) {
        return error_reply(rbuf, "non_listener_mode");
      }

      Acceptor *acceptor = driver_alloc(sizeof(Acceptor));
      acceptor->next = d->acceptor;
      acceptor->pid = driver_caller(d->port);
      
	    if (driver_monitor_process(d->port, acceptor->pid ,&acceptor->monitor) != 0) {
        driver_free(acceptor);
        return error_reply(rbuf, "noproc");
  	  }
      activate_read(d);
      d->acceptor = acceptor;
      
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_CONNECT: {
      d->mode = REQUEST_MODE;
      
      if(len != sizeof(RequestConfig)) {
        return error_reply(rbuf, "invalid_config");
      }
      
      RequestConfig *config = (RequestConfig *)buf;
      d->socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
      if(d->socket == -1) {
        return errno_reply(rbuf);
      }
      struct sockaddr_in si;
      si.sin_family = AF_INET;
      si.sin_addr.s_addr = config->ip;
      si.sin_port = config->port;
      
      int flags;
      flags = fcntl(d->socket, F_GETFL);
      if(flags == -1) {
        return errno_reply(rbuf);
      }
      if(fcntl(d->socket, F_SETFL, flags | O_NONBLOCK) == -1) {
        return errno_reply(rbuf);
      }
      
      if(connect(d->socket, (struct sockaddr *)&si, sizeof(struct sockaddr_in)) == -1) {
        if(errno != EINPROGRESS) {
          return errno_reply(rbuf);
        }
        d->state = CONNECTING_STATE;
      } else {
        accept_connection(d);
      }
      
      d->chunk_size = DEFAULT_CHUNK_SIZE;
      
      d->parser = driver_alloc(sizeof(http_parser));
      bzero(d->parser, sizeof(http_parser));
      d->parser->data = d;
      d->settings.on_message_begin = on_message_begin;
      d->settings.on_url = on_url;
      d->settings.on_header_field = on_header_field;
      d->settings.on_header_value = on_header_value;
      d->settings.on_headers_complete = on_headers_complete;
      d->settings.on_body = receive_body;
      d->settings.on_message_complete = on_message_complete;
      d->normalize_headers = 1;
      http_parser_init(d->parser, HTTP_RESPONSE);
      d->buffer = driver_alloc_binary(d->chunk_size);
      
      activate_write(d);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case INET_REQ_GETFD: {
      char *ret = *rbuf;
      *ret = 1;
      int fd = htonl(d->socket);
      memcpy(ret+1, &fd, 4);
      return 5;
    }
    
    case INET_REQ_IGNOREFD: {
      **rbuf = 1;
      return 1;
    }
    
    
    case CMD_SET_CACHE: {
      char *ptr;
      for(ptr = buf; ptr && (ptr < buf + len) && *ptr; ptr++) {
      }
      if(ptr < buf + len - 1) {
        set_cache(d, buf, (uint8_t *)ptr+1, len - (buf - ptr) - 1);
        memcpy(*rbuf, "ok", 2);
        return 2;
      } else {
        return error_reply(rbuf, "badarg");
      }
    }
    
    case CMD_DELETE_CACHE: {
      char *ptr;
      for(ptr = buf; ptr && ptr < buf + len && *ptr; ptr++) {
      }
      if(ptr == buf + len) {
        return error_reply(rbuf, "badarg");
      }
      delete_cache(d, buf);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_LIST_CACHE: {
      list_cache(d);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    default: {
      return error_reply(rbuf, "unknown_command");
    }
  }
  return 0;
}

static void gen_http_process_exit(ErlDrvData handle, ErlDrvMonitor *monitor) {
  HTTP *d = (HTTP *)handle;
  ErlDrvTermData pid = driver_get_monitored_process(d->port, monitor);
  Acceptor *a, *next;
  Acceptor *root = NULL;
  Acceptor *deleting = NULL;
  
  a = d->acceptor;
  
  while(a) {
    next = a->next;
    if(a->pid == pid) {
      a->next = deleting;
      deleting = a;
    } else {
      a->next = root;
      root = a;
    }
    a = next;
  }
  d->acceptor = root;
  while(deleting) {
    // fprintf(stderr, "Removing dead acceptor\r\n");
    
    
    driver_demonitor_process(d->port, &deleting->monitor);
    next = deleting->next;
    driver_free(deleting);
    deleting = next;
  }
}

static void accept_tcp(HTTP *d)
{
  
  if(!d->acceptor) {
    deactivate_read(d);
    return;
  }
  
  socklen_t sock_len;
  struct sockaddr_in client_addr;
  int fd = accept(d->socket, (struct sockaddr *)&client_addr, &sock_len);
  // deactivate_read(d);
  if(fd == -1) {
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("http_error"),
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_PORT, driver_mk_atom(erl_errno_id(errno)),
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
    return;
  }
  
  ErlDrvTermData owner = d->acceptor->pid;
  driver_demonitor_process(d->port, &d->acceptor->monitor);
  Acceptor *old = d->acceptor;
  d->acceptor = d->acceptor->next;
  if(d->acceptor) activate_read(d);
  driver_free(old);
  
  HTTP* c = driver_alloc(sizeof(HTTP));

  c->owner_pid = owner;
  c->socket = fd;
  c->mode = HANDLER_MODE;
  
  d->chunk_size = DEFAULT_CHUNK_SIZE;

  ErlDrvPort client = driver_create_port(d->port, owner, "gen_http_drv", (ErlDrvData)c);
  c->port = client;
  c->timeout = d->config.timeout;
  c->parser = driver_alloc(sizeof(http_parser));
  bzero(c->parser, sizeof(http_parser));
  c->parser->data = c;
  c->settings.on_message_begin = on_message_begin;
  c->settings.on_url = on_url;
  c->settings.on_header_field = on_header_field;
  c->settings.on_header_value = on_header_value;
  c->settings.on_headers_complete = on_headers_complete;
  c->settings.on_body = receive_body;
  c->settings.on_message_complete = on_message_complete;
  c->normalize_headers = 1;
  http_parser_init(c->parser, HTTP_REQUEST);
  c->buffer = driver_alloc_binary(d->chunk_size);
  driver_set_timer(c->port, c->timeout);
  // set_port_control_flags(c->port, PORT_CONTROL_FLAG_BINARY);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("http_connection"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_PORT, driver_mk_port(client),
    ERL_DRV_TUPLE, 3
  };
  driver_send_term(d->port, owner, reply, sizeof(reply) / sizeof(reply[0]));
}


static void gen_http_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
  HTTP* d = (HTTP*) handle;
  
  
  if(d->mode == LISTENER_MODE) {
    accept_tcp(d);
    return;
  }
  
  if(d->mode == HANDLER_MODE) {
    read_http(d);
    return;
  }
  
  if(d->mode == REQUEST_MODE) {
    if(d->state == CONNECTING_STATE) {
      accept_connection(d);
    } else {
      read_http(d);
    }
  }
}


void accept_connection(HTTP *d) {
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, atom_http,
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_ATOM, atom_connected,
    ERL_DRV_TUPLE, 3
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  d->state = READY_STATE;
  deactivate_read(d);
  deactivate_write(d);
}


void read_http(HTTP *d) {

  ssize_t n = recv(d->socket, d->buffer->orig_bytes, d->buffer->orig_size, 0);
  
  if(n == 0 || (n < 0 && errno == ECONNRESET)) {
    tcp_exit(d);
    return;
  }
  
  if(n < 0) {
    if((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN)) {
      fprintf(stderr, "Error in recv: %s\r\n", strerror(errno));
      tcp_exit(d);
    }  
    return;
  }
  
  assert(n <= d->buffer->orig_size);
  
  
  ssize_t nparsed = http_parser_execute(d->parser, &d->settings, d->buffer->orig_bytes, n);
  
  if(d->parser->upgrade) {
    fprintf(stderr, "Websockets not supported\n");
    tcp_exit(d);
    return;
  }
  
  if(nparsed != n) {
    if(d->parser->pause_on_body) {
      // fprintf(stderr, "Stopped parsing because body met: %d(%d): %.*s\r\n", (int)n, d->parser->state, (int)n, d->buffer->orig_bytes);
    } else {
      fprintf(stderr, "Handle HTTP error: %s(%s)\n", http_errno_name(d->parser->http_errno), http_errno_description(d->parser->http_errno));
      // fprintf(stderr, "Read(%d): '%.*s'\r\n------------\r\n", (int)n, (int)n, d->buffer->orig_bytes);
      fflush(stderr),
      tcp_exit(d);
      return;
    }
  }
  
  // fprintf(stderr, "Parsed all: %d, %d\r\n", (int)nparsed, d->parser->state);
  
  // ErlDrvTermData reply[] = {
  //   ERL_DRV_ATOM, driver_mk_atom("tcp"),
  //   ERL_DRV_PORT, driver_mk_port(d->port),
  //   ERL_DRV_BINARY, (ErlDrvTermData)c->, (ErlDrvTermData)n, 0,
  //   ERL_DRV_TUPLE, 3
  // };
  // driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
}


static void gen_http_inet_timeout(ErlDrvData handle)
{
  HTTP* d = (HTTP *)handle;
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("http_error"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_PORT, driver_mk_atom("timeout"),
    ERL_DRV_TUPLE, 3
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
}

ErlDrvEntry gen_http_driver_entry = {
    gen_http_init,			/* F_PTR init, N/A */
    gen_http_drv_start,		/* L_PTR start, called when port is opened */
    gen_http_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	                /* F_PTR output, called when erlang has sent */
    gen_http_drv_input,		/* F_PTR ready_input, called when input descriptor ready */
    gen_http_drv_ready_output,	/* F_PTR ready_output, called when output descriptor ready */
    "gen_http_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    gen_http_drv_command,			/* F_PTR control, port_command callback */
    gen_http_inet_timeout,			/* F_PTR timeout, reserved */
    gen_http_drv_schedule_write,	/* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    gen_http_process_exit,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(gen_http_drv) /* must match name in driver_entry */
{
    return &gen_http_driver_entry;
}
