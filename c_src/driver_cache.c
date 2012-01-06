#include "gen_http.h"

static ErlDrvBinary *cached_reply_bin = 0;

void init_cache() {
    // char reply[] = "Hello world\n";
    char reply[] = "0123456789A\n";
    int len = 12; // strlen(reply)
    int count = 1000;
    count = 1;
    char headers[] = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nConnection: keep-alive\r\nContent-Length: 12\r\n\r\n";
    ErlDrvBinary *bin = driver_alloc_binary(len*count+sizeof(headers));
    int i;
    char *ptr = bin->orig_bytes;
    memcpy(ptr, headers, sizeof(headers) - 1);
    ptr += sizeof(headers) - 1;
    for(i = 0; i < count; i++) {
      memcpy(ptr, reply, len);
      ptr += len;
    }
    cached_reply_bin = bin;
  
}

int cached_reply(HTTP *d) {
  return 0;
  
  // This is just a temporary stub code to test in-driver cache
  
  if(memcmp(d->url->orig_bytes, "/dvb/2/manifest.f4m", d->url->orig_size)) {
    return 0;
  }
  
  driver_enq_bin(d->port, cached_reply_bin, 0, cached_reply_bin->orig_size);
  activate_write(d);
  
  return 1;
}
