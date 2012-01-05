// Code from erlang packet_parser.c
#include "http_hash.h"


#define hash_update(h,c) do { \
        unsigned long __g; \
        (h) = ((h) << 4) + (c); \
        if ((__g = (h) & 0xf0000000)) { \
            (h) ^= (__g >> 24); \
            (h) ^= __g; \
        } \
    } while(0)

void gen_http_hash_insert(ErlDrvBinary *key, ErlDrvTermData value,
                             http_entry_t** hash, int hsize)
{
    unsigned long h = 0;
    int ix;
    
    http_entry_t* entry = driver_alloc(sizeof(http_entry_t));
    int i;
    for(i = 0; i < key->orig_size; i++) {
        hash_update(h, key->orig_bytes[i]);
    }
    ix = h % hsize;

    entry->next = hash[ix];
    entry->h    = h;
    entry->key  = key;
    driver_binary_inc_refc(key);
    
    entry->value = value;

    hash[ix] = entry;
}


ErlDrvTermData gen_http_hash_lookup(const char* key, int len, http_entry_t** hash, int hsize) {
  unsigned long h = 0;
  int i;
  for(i = 0; i < len; i++) {
    hash_update(h, key[i]);
  }
    
  int ix = h % hsize;
  http_entry_t* ap = hash[ix];

  while (ap != NULL) {
      if ((ap->h == h) && (ap->key->orig_size == len) && 
          (memcmp(ap->key->orig_bytes, key, len) == 0))
          return ap->value;
      ap = ap->next;
  }
  return NULL;
}
