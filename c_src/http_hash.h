#include <erl_driver.h>


typedef struct http_entry {
    struct http_entry* next;   /* next in bucket */
    unsigned long h;          /* stored hash value */
    ErlDrvBinary* key;
    int index;                /* index in table + bit-pos */
    ErlDrvTermData value;     /* erlang atom rep */
} http_entry_t;


void gen_http_hash_insert(ErlDrvBinary *key, ErlDrvTermData value, http_entry_t** hash, int hsize);
ErlDrvTermData gen_http_hash_lookup(const char* key, int len, http_entry_t** hash, int hsize);
