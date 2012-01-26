#include "gen_http.h"

static const char* http_hdr_strings[] = {
    "Cache-Control",
    "Connection",
    "Date",
    "Pragma",
    "Transfer-Encoding",
    "Upgrade",
    "Via",
    "Accept",
    "Accept-Charset",
    "Accept-Encoding",
    "Accept-Language",
    "Authorization",
    "From",
    "Host",
    "If-Modified-Since",
    "If-Match",
    "If-None-Match",
    "If-Range",
    "If-Unmodified-Since",
    "Max-Forwards",
    "Proxy-Authorization",
    "Range",
    "Referer",
    "User-Agent",
    "Age",
    "Location",
    "Proxy-Authenticate",
    "Public",
    "Retry-After",
    "Server",
    "Vary",
    "Warning",
    "Www-Authenticate",
    "Allow",
    "Content-Base",
    "Content-Encoding",
    "Content-Language",
    "Content-Length",
    "Content-Location",
    "Content-Md5",
    "Content-Range",
    "Content-Type",
    "Etag",
    "Expires",
    "Last-Modified",
    "Accept-Ranges",
    "Set-Cookie",
    "Set-Cookie2",
    "X-Forwarded-For",
    "Cookie",
    "Keep-Alive",
    "Proxy-Connection",
    NULL
};

#define HTTP_HDR_HASH_SIZE 53
static http_entry_t* http_hdr_hash[HTTP_HDR_HASH_SIZE];



void init_http_handling() {
  int i;
  for(i = 0; http_hdr_strings[i]; i++) {
    ErlDrvBinary *header = driver_alloc_binary(strlen(http_hdr_strings[i]));
    memcpy(header->orig_bytes, http_hdr_strings[i], strlen(http_hdr_strings[i]));
    gen_http_hash_insert(header, driver_mk_atom((char *)http_hdr_strings[i]), http_hdr_hash, HTTP_HDR_HASH_SIZE);
  }
}


int on_message_begin(http_parser *p) {
  // fprintf(stderr, "S> INCOME REQUEST\r\n");
  HTTP *d = (HTTP *)p->data;
  d->url = NULL;
  d->headers_count = 0;
  d->settings.on_body = receive_body;
  assert(!d->body);
  return 0;
}

int on_url(http_parser *p, const char *url, size_t len) {
  HTTP *d = (HTTP *)p->data;
  d->url = driver_alloc_binary(len);
  activate_read(d);
  memcpy(d->url->orig_bytes, url, len);
  return 0;
}

#define IS_ALPHA(c)         (LOWER(c) >= 'a' && LOWER(c) <= 'z')
#define IS_NUM(c)           ((c) >= '0' && (c) <= '9')
#define LOWER(c)            (unsigned char)(c | 0x20)
#define UPPER(c)            (unsigned char)(c & (0xFF ^ 0x20))
#define IS_ALPHANUM(c)      (IS_ALPHA(c) || IS_NUM(c))

static void normalize_header(ErlDrvBinary *bin) {
  int i;
  char uppering = 1;
  char c;

  for(i = 0; i < bin->orig_size; i++) {
    c = bin->orig_bytes[i];
    if(IS_ALPHA(c)) {
      if(uppering) bin->orig_bytes[i] = UPPER(c);
      else         bin->orig_bytes[i] = LOWER(c);
      uppering = 0;
    } else {
      uppering = 1;
    }
  }
}

int on_header_field(http_parser *p, const char *field, size_t len) {
  HTTP *d = (HTTP *)p->data;
  activate_read(d);
  if(d->headers_count >= HTTP_MAX_HEADERS) {
    p->http_errno = HPE_HEADER_OVERFLOW;
    return 1;
  }
  
  ErlDrvBinary *bin = d->headers[d->headers_count].field = driver_alloc_binary(len);
  memcpy(bin->orig_bytes, field, len);
  
  if(d->normalize_headers) normalize_header(bin);
  
  return 0;
}

int on_header_value(http_parser *p, const char *field, size_t len) {
  HTTP *d = (HTTP *)p->data;
  activate_read(d);

  ErlDrvBinary *bin = d->headers[d->headers_count].value = driver_alloc_binary(len);
  memcpy(bin->orig_bytes, field, len);
  
  d->headers_count++;
  
  return 0;
}

static void free_headers(HTTP *d) {
  int j;
  assert(!d->body);
  if(d->url) {
    driver_free_binary(d->url);
    d->url = NULL;
  }
  for(j = 0; j < d->headers_count; j++) {
    driver_free_binary(d->headers[j].field);
    d->headers[j].field = NULL;

    driver_free_binary(d->headers[j].value);
    d->headers[j].value = NULL;
  }
  d->headers_count = 0;
}

int on_headers_complete(http_parser *p) {
  HTTP *d = (HTTP *)p->data;

  assert(!d->body);
  
  if(p->method == HTTP_GET && d->mode == HANDLER_MODE && cached_reply(d)) {
    free_headers(d);
    return 0;
  }
  
  deactivate_read(d);

  int count = 50 + d->headers_count*10;
  ErlDrvTermData reply[count];
  
  int i = 0;
  
  // fprintf(stderr, "S> %s %.*s HTTP/%d.%d %d\r\n", http_method_str(p->method), (int)d->url->orig_size, d->url->orig_bytes, p->http_major, p->http_minor, http_should_keep_alive(p));
  
  // {http, Socket, Method, URL, Keepalive, Version, Headers} for server side
  // {http, Socket, Status, Keepalive, Version, Headers} for client side
  
  reply[i++] = ERL_DRV_ATOM;
  reply[i++] = p->proto == PROTO_HTTP ? atom_http : p->proto == PROTO_RTSP ? atom_rtsp : driver_mk_atom("error");
  reply[i++] = ERL_DRV_PORT;
  reply[i++] = driver_mk_port(d->port);
  
  if(d->mode == HANDLER_MODE) {
    reply[i++] = ERL_DRV_ATOM;
    reply[i++] = method_atoms[p->method];
    reply[i++] = ERL_DRV_BINARY;
    reply[i++] = (ErlDrvTermData)d->url;
    reply[i++] = (ErlDrvTermData)d->url->orig_size;
    reply[i++] = 0;
  } else if(d->mode == REQUEST_MODE) {
    reply[i++] = ERL_DRV_UINT;
    reply[i++] = (ErlDrvTermData)p->status_code;
  }
  
  reply[i++] = ERL_DRV_ATOM;
  reply[i++] = http_should_keep_alive(p) ? atom_keepalive : atom_close;
  
  reply[i++] = ERL_DRV_UINT;
  reply[i++] = (ErlDrvTermData)p->http_major;
  reply[i++] = ERL_DRV_UINT;
  reply[i++] = (ErlDrvTermData)p->http_minor;
  reply[i++] = ERL_DRV_TUPLE;
  reply[i++] = 2;
  
  
  int j = 0;
  
  for(j = 0; j < d->headers_count; j++) {
    ErlDrvTermData atom = gen_http_hash_lookup(d->headers[j].field->orig_bytes, d->headers[j].field->orig_size, http_hdr_hash, HTTP_HDR_HASH_SIZE);
    
    if(atom != driver_term_nil) {
      reply[i++] = ERL_DRV_ATOM;
      reply[i++] = atom;
    } else {
      reply[i++] = ERL_DRV_BINARY;
      reply[i++] = (ErlDrvTermData)d->headers[j].field;
      reply[i++] = (ErlDrvTermData)d->headers[j].field->orig_size;
      reply[i++] = 0;
    }
  
    reply[i++] = ERL_DRV_BINARY;
    reply[i++] = (ErlDrvTermData)d->headers[j].value;
    reply[i++] = (ErlDrvTermData)d->headers[j].value->orig_size;
    reply[i++] = 0;
    
    reply[i++] = ERL_DRV_TUPLE;
    reply[i++] = 2;
  
    // fprintf(stderr, "S> %.*s: %.*s\r\n", (int)d->headers[j].field->orig_size, d->headers[j].field->orig_bytes, (int)d->headers[j].value->orig_size, d->headers[j].value->orig_bytes);
  }
  
  reply[i++] = ERL_DRV_NIL;
  reply[i++] = ERL_DRV_LIST;
  reply[i++] = j+1;
  
  reply[i++] = ERL_DRV_TUPLE;
  reply[i++] = d->mode == HANDLER_MODE ? 7 : d->mode == REQUEST_MODE ? 6 : -1;
  
  if(driver_output_term(d->port, reply, i) == -1) {
    fprintf(stderr, "Failed to send message: %s\r\n", d->mode == HANDLER_MODE ? "handler" : d->mode == REQUEST_MODE ? "request" : "none!");
  }
  
  free_headers(d);
  // driver_free(reply);
  // reply[10] = ERL_DRV_TUPLE;
  // reply[11] = 4;
  // int o = driver_output_term(d->port, reply, 12);
  
  // fprintf(stderr, "S> -- (%d, %d) \r\n", i, o);
  if(d->mode == HANDLER_MODE) {
    d->has_body = 0;
  } else {
    d->has_body = 1;
  }
  if(d->mode == HANDLER_MODE && (p->method == HTTP_POST || p->method == HTTP_PUT)) {
    d->has_body = 1;
    return 0;
  } 
  if(p->method == HTTP_HEAD) {
    return 1;
  }
  
  return 0;
}

void flush_body(HTTP *d) {
  if(!d->body) return;
  
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, atom_http,
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_BINARY, (ErlDrvTermData)d->body, (ErlDrvTermData)d->body->orig_size, (ErlDrvTermData)0, 
    ERL_DRV_TUPLE, 3
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_free_binary(d->body);
  d->body = NULL;
  d->body_offset = 0;  
}

int receive_body(http_parser *p, const char *body, size_t len) {
  HTTP *d = (HTTP *)p->data;
  if(!d->body) {
    d->body = driver_alloc_binary(len > d->chunk_size ? len : d->chunk_size);
    d->body_offset = 0;
  }
  if(d->body->orig_size - d->body_offset < len) {
    d->body = driver_realloc_binary(d->body, d->body_offset + len);
  }
  
  memcpy(d->body->orig_bytes + d->body_offset, body, len);
  d->body_offset += len;
  
  if(d->body_offset >= d->chunk_size) {
    flush_body(d);
  }
  
  return 0;
}

int skip_body(http_parser *p, const char *body, size_t len) {
  HTTP *d = (HTTP *)p->data;
  if(d->body) {
    driver_free_binary(d->body);
    d->body = NULL;
    d->body_offset = 0;
  }
  // fprintf(stderr, "S> skip_body_chunk(%d): %.*s\r\n", (int)len, (int)len, body);
  activate_read(d);
  return 0;
}

int on_message_complete(http_parser *p) {
  HTTP *d = (HTTP *)p->data;
  
  flush_body(d);
  
  if(d->has_body) {
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, atom_http,
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_ATOM, atom_eof,
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  }
  
  assert(!d->body);
  return 0;
}

