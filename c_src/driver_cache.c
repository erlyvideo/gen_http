#include "gen_http.h"

#include "khash.h"

static ErlDrvRWLock *cache_lock;

KHASH_MAP_INIT_STR(gh, ErlDrvBinary *)

static khash_t(gh) *cache;

void init_cache() {
  cache_lock = erl_drv_rwlock_create("gen_http_cache");
  cache = kh_init(gh);
}

int cached_reply(HTTP *d) {
  if(!d->url) return 0;
  
  char url[1025];
  
  if(d->url->orig_size > sizeof(url) - 1) return 0;
  
  bzero(url, sizeof(url));
  memcpy(url, d->url->orig_bytes, d->url->orig_size);

  erl_drv_rwlock_rlock(cache_lock);
  khiter_t k = kh_get(gh, cache, url);
  if(k == kh_end(cache)) {
    // fprintf(stderr, "Cache miss '%s'(%d) %d\r\n", url, kh_str_hash_func(url), kh_size(cache));
    erl_drv_rwlock_runlock(cache_lock);
    return 0;
  }
  
  ErlDrvBinary *reply = kh_value(cache, k);
  erl_drv_rwlock_runlock(cache_lock);

  // fprintf(stderr, "Cache hit '%s' %d '%.*s... (%d more)'\r\n", url, (int)driver_binary_get_refc(reply), 16, reply->orig_bytes, (int)reply->orig_size);
  
  d->auto_reply = 1;
  
  driver_enq_bin(d->port, reply, 0, reply->orig_size);
  assert(!d->body);
  activate_write(d);
  activate_read(d);
  
  return 1;
}

void set_cache(HTTP *d, char *url0, uint8_t *data, size_t len) {
  char *url = malloc(strlen(url0) + 1);
  strcpy(url, url0);
  ErlDrvBinary *bin = driver_alloc_binary(len);
  memcpy(bin->orig_bytes, data, len);
  // fprintf(stderr, "Set cache: '%s' -> %d:'%.*s'\r\n", url, (int)len, (int)len, data);
  erl_drv_rwlock_rwlock(cache_lock);
  int ret;
  khiter_t k = kh_put(gh, cache, url, &ret);
  
  if(!ret) {
    // fprintf(stderr, "Cannot set cache: '%s'(%d)\r\n", url, (int)len);
    kh_del(gh, cache, k);
  } else {
    // fprintf(stderr, "Set cache to %d\r\n", k);
    kh_value(cache, k) = bin;
  }
  erl_drv_rwlock_rwunlock(cache_lock);
}

void delete_cache(HTTP *d, char *url) {
  erl_drv_rwlock_rwlock(cache_lock);
  
  erl_drv_rwlock_rwunlock(cache_lock);  
}

void list_cache(HTTP *d) {
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("http_cache_list"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_NIL,
    ERL_DRV_LIST, 1,
    ERL_DRV_TUPLE, 3
  };
  
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
}
