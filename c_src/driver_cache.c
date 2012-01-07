#include "gen_http.h"

#include "khash.h"

static ErlDrvRWLock *cache_lock;

KHASH_MAP_INIT_STR(gh, ErlDrvBinary *)

static khash_t(gh) *cache;

void gh_cache_init() {
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

void gh_cache_set(HTTP *d, char *url0, uint8_t *data, size_t len) {
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

void gh_cache_delete(HTTP *d, char *url) {
  erl_drv_rwlock_rwlock(cache_lock);
  
  khiter_t k = kh_get(gh, cache, url);
  if(k == kh_end(cache)) {
    // fprintf(stderr, "Cache miss '%s'(%d) %d\r\n", url, kh_str_hash_func(url), kh_size(cache));
    erl_drv_rwlock_rwunlock(cache_lock);
    return;
  }
  
  char *key = (char *)kh_key(cache, k);
  free(key);
  ErlDrvBinary *bin = (ErlDrvBinary *)kh_value(cache, k);
  driver_free_binary(bin);
  kh_del(gh, cache, k);
  
  erl_drv_rwlock_rwunlock(cache_lock);  
}

void gh_cache_list(HTTP *d) {
  int count = 5 + kh_size(cache)*12;
  int i = 0;
  int j = 0;
  ErlDrvTermData reply[count];
  
  reply[i++] = ERL_DRV_ATOM;
  reply[i++] = driver_mk_atom("http_cache_list");
  reply[i++] = ERL_DRV_PORT;
  reply[i++] = driver_mk_port(d->port);
 
  khiter_t k;
  for(k = kh_begin(cache); k != kh_end(cache); ++k) {
    if(kh_exist(cache, k)) {
      j++;
      reply[i++] = ERL_DRV_BUF2BINARY;
      reply[i++] = (ErlDrvTermData)kh_key(cache, k);
      reply[i++] = (ErlDrvTermData)strlen(kh_key(cache, k));
      // reply[i++] = ERL_DRV_BINARY;
      // ErlDrvBinary *bin = (ErlDrvBinary *)kh_val(cache, k);
      // reply[i++] = (ErlDrvTermData)bin;
      // reply[i++] = (ErlDrvTermData)bin->orig_size;
      // reply[i++] = (ErlDrvTermData)0;
    }
  }
  
  reply[i++] = ERL_DRV_NIL;
  reply[i++] = ERL_DRV_LIST;
  reply[i++] = (ErlDrvTermData)(1 + j);
  reply[i++] = ERL_DRV_TUPLE;
  reply[i++] = (ErlDrvTermData)3;
  
  driver_output_term(d->port, reply, i);
}


void gh_cache_get(HTTP *d, char *url) {
  
  erl_drv_rwlock_rlock(cache_lock);
  
  khiter_t k = kh_get(gh, cache, url);
  if(k == kh_end(cache)) {
    erl_drv_rwlock_runlock(cache_lock);

    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("http_cache"),
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_ATOM, driver_mk_atom("undefined"),
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  } else {
    ErlDrvBinary *bin = kh_value(cache, k);
    erl_drv_rwlock_runlock(cache_lock);
    
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("http_cache"),
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_BINARY, (ErlDrvTermData)bin, (ErlDrvTermData)0, (ErlDrvTermData)bin->orig_size,
      ERL_DRV_TUPLE, 3      
    };
    driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  }
  
  
}

