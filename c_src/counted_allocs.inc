static volatile int64_t allocated_xml = 0;

#ifdef COUNTED_ALLOCS

static void *counted_alloc_xml(size_t size) {
  int32_t *ptr = malloc(4+size);
  if (!ptr)
    return ptr;

  ptr[0] = size >= 100000000 ? 0xffffffff : size;

  __sync_fetch_and_add(&allocated_xml, ptr[0]);

  return ptr+1;
}

static void counted_free_xml(void *ptr) {
  if (!ptr)
    return;

  int32_t *p = ((int32_t*)ptr)-1;

  __sync_fetch_and_sub(&allocated_xml, p[0]);

  free(p);
}

static void *counted_realloc_xml(void *ptr, size_t size) {
  if (!ptr) {
     return counted_alloc_xml(size);
  }
  if (size == 0) {
    counted_free_xml(ptr);
    return NULL;
  }

  int32_t *p = ((int32_t*)ptr)-1;
  int32_t s = p[0];
  int32_t *np = realloc(p, size+4);

  if (!np) {
    __sync_fetch_and_sub(&allocated_xml, s);
    return np;
  }

  np[0] = size >= 100000000 ? 0xffffffff : size;

  __sync_fetch_and_add(&allocated_xml, np[0] - s);

  return np+1;
}

#define enif_alloc counted_alloc_xml
#define enif_free counted_free_xml
#define enif_realloc counted_realloc_xml

#endif
