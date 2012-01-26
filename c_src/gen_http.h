#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/uio.h>
#include <http_parser.h>

#ifndef IOV_MAX
#define IOV_MAX 1000
#endif

#include "http_hash.h"

#define DEFAULT_CHUNK_SIZE 65536
#define DEFAULT_BUFFER_LIMIT 2*1024*1024


extern ErlDrvTermData atom_http;
extern ErlDrvTermData atom_http_error;
extern ErlDrvTermData atom_rtsp;
extern ErlDrvTermData atom_keepalive;
extern ErlDrvTermData atom_close;
extern ErlDrvTermData atom_eof;
extern ErlDrvTermData atom_empty;
extern ErlDrvTermData atom_connected;
extern ErlDrvTermData method_atoms[HTTP_MAX_METHOD];


typedef enum {LISTENER_MODE, HANDLER_MODE, REQUEST_MODE} SocketMode;
typedef enum {READY_STATE, CONNECTING_STATE} SocketState;

enum {
    CMD_LISTEN = 1,
    CMD_ACTIVE_ONCE = 2,
    CMD_RECEIVE_BODY = 3,
    CMD_ACCEPT_ONCE = 5,
    CMD_CONNECT = 6,
    CMD_SKIP_BODY = 7,
    CMD_SET_CHUNK_SIZE = 8,
    CMD_SET_CACHE = 9,
    CMD_DELETE_CACHE = 10,
    CMD_LIST_CACHE = 11,
    CMD_GET_CACHE = 12,
    CMD_GET_EXHAUSTED = 13,
    INET_REQ_GETFD = 14,
    INET_REQ_IGNOREFD = 28
    };

#pragma pack(1)
typedef struct {
  uint16_t port;
  uint16_t backlog;
  uint8_t reuseaddr;
  uint8_t keepalive;
  uint16_t timeout;
} Config;
// #pragma options align=reset

#pragma pack(1)
typedef struct {
  uint32_t ip;
  uint16_t port;
} RequestConfig;

typedef struct {
  ErlDrvBinary *field;
  ErlDrvBinary *value;
} Header;

struct PidList;
typedef struct PidList {
  ErlDrvTermData pid;
  ErlDrvMonitor monitor;
  struct PidList *next;
} PidList;

void pid_list_send(PidList *a, ErlDrvPort port, ErlDrvTermData *reply, size_t reply_size);
void pid_list_free(PidList **head);
int pid_list_add_caller(PidList **head, ErlDrvPort port);
void pid_list_delete(PidList **head, ErlDrvPort port, ErlDrvMonitor *monitor);
void pid_list_remove_head(PidList **head, ErlDrvPort port);

#define HTTP_MAX_HEADERS 100

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  int socket;
  unsigned long timeout;
  int raw_mode;
  SocketMode mode;
  SocketState state;
  http_parser_settings settings;
  http_parser *parser;
  ErlDrvBinary* buffer;
  Header headers[HTTP_MAX_HEADERS];
  int headers_count;
  ErlDrvBinary *url;
  ErlDrvBinary *body;
  int normalize_headers;
  int has_body;
  
  int auto_reply;
  
  PidList *acceptor;
  PidList *exhausted;
  uint32_t chunk_size;
  size_t body_offset;
  
  size_t buffer_limit;
  
  Config config; // Only for listener mode
} HTTP;

void init_http_handling();
void read_http(HTTP *d);
int receive_body(http_parser *p, const char *data, size_t len);
int skip_body(http_parser *p, const char *data, size_t len);
int on_message_begin(http_parser *p);
int on_url(http_parser *p, const char *url, size_t len);
int on_header_field(http_parser *p, const char *field, size_t len);
int on_header_value(http_parser *p, const char *field, size_t len);
int on_headers_complete(http_parser *p);
int on_message_complete(http_parser *p);
void accept_connection(HTTP *d);
void tcp_exit(HTTP *d);

void gen_http_drv_schedule_write(ErlDrvData handle, ErlIOVec *ev);
void gen_http_drv_ready_output(ErlDrvData handle, ErlDrvEvent event);

void activate_write(HTTP *d);
void deactivate_write(HTTP *d);
void activate_read(HTTP *d);
void deactivate_read(HTTP *d);


void gh_cache_init();
int cached_reply(HTTP *d);

void gh_cache_set(HTTP *d, char *url, uint8_t *data, size_t len);
void gh_cache_delete(HTTP *d, char *url);
void gh_cache_list(HTTP *d);
void gh_cache_get(HTTP *d, char *url);
