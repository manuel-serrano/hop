/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/_bglhopscript.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Thu Nov  7 07:27:41 2024 (serrano)                */
/*    Copyright   :  2016-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Optional file, used only for the C backend, that optimizes       */
/*    JsObject and cache implementations.                              */
/*=====================================================================*/
#define _GNU_SOURCE
#include <pthread.h>
#include <bigloo.h>
#include "bglhopscript.h"
#include "bglhopscript_types.h"
#if BGL_HAS_THREAD_SETNAME
#  define _PTHREAD_SETNAME(t, n) pthread_setname_np(t, n)
#else
#  define _PTHREAD_SETNAME(t, n)
#endif
#include <stdio.h>

/*---------------------------------------------------------------------*/
/*    Bmem config                                                      */
/*---------------------------------------------------------------------*/
#define BMEM_CONFIG 0

/*---------------------------------------------------------------------*/
/*    Bigloo backward compatibility                                    */
/*---------------------------------------------------------------------*/
#if (defined(BGL_NAN_TAGGING))
#  define vector_t vector
#  define FILLER_COMMA 
#else
#  define FILLER_COMMA ,
#endif

/*---------------------------------------------------------------------*/
/*    JsObject imports                                                 */
/*---------------------------------------------------------------------*/
extern obj_t BGl_JsObjectz00zz__hopscript_typesz00;
extern obj_t BGl_JsProxyz00zz__hopscript_typesz00;
extern obj_t BGl_JsArrayz00zz__hopscript_typesz00;
extern obj_t BGl_JsFunctionz00zz__hopscript_typesz00;
extern obj_t BGl_JsMethodz00zz__hopscript_typesz00;
extern obj_t BGl_JsProcedurez00zz__hopscript_typesz00;
extern obj_t BGl_JsStringLiteralASCIIz00zz__hopscript_typesz00;
extern obj_t BGl_JsGeneratorz00zz__hopscript_typesz00;
extern obj_t BGl_JsYieldz00zz__hopscript_typesz00;
extern obj_t BGl_JsDatez00zz__hopscript_typesz00;

extern obj_t string_append(obj_t, obj_t);

#define JSOBJECT_SIZE \
   sizeof(struct BgL_jsobjectz00_bgl)
#define JSOBJECT_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsObjectz00zz__hopscript_typesz00)
#define JSYIELD_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsYieldz00zz__hopscript_typesz00)

#define JSPROXY_SIZE \
   sizeof(struct BgL_jsproxyz00_bgl)
#define JSPROXY_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsProxyz00zz__hopscript_typesz00)

#define JSFUNCTION_SIZE \
   sizeof(struct BgL_jsfunctionz00_bgl)
#define JSFUNCTION_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsFunctionz00zz__hopscript_typesz00)

#define JSMETHOD_SIZE \
   sizeof(struct BgL_jsmethodz00_bgl)
#define JSMETHOD_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsMethodz00zz__hopscript_typesz00)

#define JSPROCEDURE_SIZE \
   sizeof(struct BgL_jsprocedurez00_bgl)
#define JSPROCEDURE_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsProcedurez00zz__hopscript_typesz00)

#define JSARRAY_SIZE \
   sizeof(struct BgL_jsarrayz00_bgl)
#define JSARRAY_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsArrayz00zz__hopscript_typesz00)

#define JSSTRINGLITERALASCII_SIZE \
   sizeof(struct BgL_jsstringliteralasciiz00_bgl)
#define JSSTRINGLITERALASCII_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsStringLiteralASCIIz00zz__hopscript_typesz00)

#define JSGENERATOR_SIZE \
   sizeof(struct BgL_jsgeneratorz00_bgl)
#define JSGENERATOR_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsGeneratorz00zz__hopscript_typesz00)

#define JSDATE_SIZE \
   sizeof(struct BgL_jsdatez00_bgl)
#define JSDATE_CLASS_NUM \
   BGL_CLASS_NUM(BGl_JsDatez00zz__hopscript_typesz00)

extern obj_t bgl_js_profile_allocs;
obj_t bgl_profile_pcache_tables = BNIL;
extern int GC_pthread_create();

static uint32_t jsobject_mode;

static obj_t jsproxy_constrmap, jsproxy_elements;
static uint32_t jsproxy_mode;

static obj_t jsfunction_elements, jsfunction_alloc;
static BgL_jsconstructmapz00_bglt jsfunction_constrmap, jsfunction_cmap;
static uint32_t jsfunction_mode;

static obj_t jsmethod_elements, jsmethod_alloc;
static BgL_jsconstructmapz00_bglt jsmethod_constrmap, jsmethod_cmap;
static uint32_t jsmethod_mode;

static uint32_t jsprocedure_mode;
static BgL_jsconstructmapz00_bglt jsprocedure_cmap;

static uint32_t jsdate_mode;
static uint32_t jsyield_mode;

static uint32_t jsstringliteralascii_mode, jsstringliteralascii_normmode;
static obj_t jsstringliteralascii_not_a_string_cache;
static uint32_t jsstringliteralascii_normalize_threshold;

static obj_t empty_vector;

// yield objects have two fields, value and done
#define JSYIELD_OBJECT_CONSTRSIZE 2

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef struct BgL_jspropertycachez00_bgl pcache_t;

/*---------------------------------------------------------------------*/
/*    thread alloc                                                     */
/*---------------------------------------------------------------------*/
#define HOP_ALLOC_CLASSIC 1
#define HOP_ALLOC_TLS 2
#define HOP_ALLOC_SPINLOCK 3

#if (!defined(HOP_ALLOC_POLICY))
#  if BGL_HAS_THREAD_LOCALSTORAGE == 1
#    define HOP_ALLOC_POLICY HOP_ALLOC_TLS
#  elif BGL_HAVE_SPINLOCK
#    define HOP_ALLOC_POLICY HOP_ALLOC_SPINLOCK
#  else
#    define HOP_ALLOC_POLICY HOP_ALLOC_CLASSIC
#  endif
#endif

#if BMEM_CONFIG
#  undef HOP_ALLOC_POLICY
#  define HOP_ALLOC_POLICY HOP_ALLOC_CLASSIC
#endif

#define HOP_ALLOC_JSOBJECT_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSPROXY_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSFUNCTION_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSMETHOD_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSPROCEDURE_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSSTRINGLITERALASCII_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSDATE_POLICY HOP_ALLOC_POLICY

#undef HOP_ALLOC_JSSTRINGLITERALASCII_POLICY
#define HOP_ALLOC_JSSTRINGLITERALASCII_POLICY HOP_ALLOC_POLICY
#undef HOP_ALLOC_JSFUNCTION_POLICY
#define HOP_ALLOC_JSFUNCTION_POLICY HOP_ALLOC_CLASSIC
#undef HOP_ALLOC_JSMETHOD_POLICY
#define HOP_ALLOC_JSMETHOD_POLICY HOP_ALLOC_CLASSIC

extern obj_t bgl_make_jsobject(int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode);

#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsobject_sans(int constrsize, obj_t constrmap,
				     obj_t __proto__, uint32_t mode);
#endif

#if HOP_ALLOC_JSPROXY_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsproxy_sans(obj_t target, obj_t handler,
				    obj_t gcache, obj_t scache, obj_t acache,
				    uint32_t mode);
#endif

#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsfunction_sans(obj_t procedure,
				       long arity,
				       long constrsize,
				       obj_t __proto__, obj_t info);
#endif

#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsmethod_sans(obj_t procedure, obj_t method,
				       long arity,
				       long constrsize,
				       obj_t __proto__, obj_t info);
#endif

#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsprocedure_sans(obj_t procedure, long arity, obj_t __proto__);
#endif

#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsstringliteralascii_sans(uint32_t len, obj_t left, obj_t right);
#endif

#if HOP_ALLOC_JSDATE_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsdate_sans(BgL_jsconstructmapz00_bglt cmap, obj_t __proto__);
#endif

#define POOLSZ(sz)				\
   ((sz == 0) ? 0 :				\
    ((sz == 1) ? 512 :				\
     ((sz == 2) ? 512 :				\
      ((sz == 3) ? 256 :			\
       ((sz == 4) ? 128 :			\
	((sz == 5) ? 64 :			\
	 ((sz == 6) ? 64 :			\
	  ((sz == 7) ? 64 :			\
	   ((sz == 8) ? 64 :			\
	    32)))))))))
#define JSOBJECT_POOLSZ(z) POOLSZ(z)
#define JSPROXY_POOLSZ POOLSZ(3)
#define JSFUNCTION_POOLSZ POOLSZ(4)
#define JSMETHOD_POOLSZ POOLSZ(4)
#define JSPROCEDURE_POOLSZ POOLSZ(4)
#define JSSTRINGLITERALASCII_POOLSZ POOLSZ(3)
#define JSDATE_POOLSZ POOLSZ(2)
#define WORK_NUMBER 1

/*---------------------------------------------------------------------*/
/*    stat                                                             */
/*---------------------------------------------------------------------*/
#define ALLOC_STATS 0

#if ALLOC_STATS
#  define ALLOC_STAT(x) (x)
#else
#  define ALLOC_STAT(x)
#endif

/*---------------------------------------------------------------------*/
/*    spin locks                                                       */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_POLICY == HOP_ALLOC_SPINLOCK
static pthread_spinlock_t lock1, lock2, lock3, lock4, lock5, lock6, lock6, lock8;
static pthread_spinlock_t lockproxy;
static pthread_spinlock_t lockfunction;
static pthread_spinlock_t lockmethod;
static pthread_spinlock_t lockprocedure;
static pthread_spinlock_t lockstringliteralascii;
static pthread_spinlock_t lockdate;

#  define alloc_spin_init(x, attr) pthread_spin_init(x, attr)
#  define alloc_spin_lock(x) pthread_spin_lock(x)
#  define alloc_spin_unlock(x) pthread_spin_unlock(x)
#else
#  define alloc_spin_init(x, attr) 
#  define alloc_spin_lock(x) 
#  define alloc_spin_unlock(x) 
#endif

/*---------------------------------------------------------------------*/
/*    tls                                                              */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_POLICY == HOP_ALLOC_TLS
#  define HOP_ALLOC_THREAD_DECL BGL_THREAD_DECL
#else
#  define HOP_ALLOC_THREAD_DECL
#endif

/*---------------------------------------------------------------------*/
/*    alloc pool types                                                 */
/*---------------------------------------------------------------------*/
typedef union apayload {
   const uint32_t objsize;
   unsigned char dummy;
} apayload_t;

typedef struct apool {
   void (*fill_buffer)(struct apool *pool, void *arg);
   obj_t *buffer;
   const uint32_t size;
   uint32_t idx;
   uint32_t pool_number;
   apayload_t payload;
} apool_t;

/*---------------------------------------------------------------------*/
/*    buffer fillers ...                                               */
/*---------------------------------------------------------------------*/
static void jsobject_fill_buffer(apool_t *pool, void *arg);
static void jsproxy_fill_buffer(apool_t *pool, void *arg);
static void jsfunction_fill_buffer(apool_t *pool, void *arg);
static void jsmethod_fill_buffer(apool_t *pool, void *arg);
static void jsprocedure_fill_buffer(apool_t *pool, void *arg);
static void jsstringliteralascii_fill_buffer(apool_t *pool, void *arg);
static void jsdate_fill_buffer(apool_t *pool, void *arg);

/*---------------------------------------------------------------------*/
/*    alloc pools                                                      */
/*---------------------------------------------------------------------*/
#define MAX_POOL_QUEUE_SIZE 13 
static int pool_queue_idx = 0;
static int pool_queue_len = 0;
static apool_t **pool_queue = 0;;

static pthread_mutex_t alloc_pool_mutex;
static pthread_cond_t alloc_pool_cond;

#define APOOL_JSOBJECT_INIT(pool_num, sz) {\
   .fill_buffer = &jsobject_fill_buffer, \
   .buffer = 0L, \
   .idx = JSOBJECT_POOLSZ(sz), \
   .pool_number = pool_num, \
   .size = JSOBJECT_POOLSZ(sz), \
   .payload = { .objsize = sz } \
};

#define APOOL_JSPROXY_INIT(pool_num) { \
   .fill_buffer = &jsproxy_fill_buffer,	\
   .buffer = 0L, \
   .idx = JSPROXY_POOLSZ, \
   .pool_number = pool_num, \
   .size = JSPROXY_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSFUNCTION_INIT(pool_num) { \
   .fill_buffer = &jsfunction_fill_buffer, \
   .buffer = 0L, \
   .idx = JSFUNCTION_POOLSZ, \
   .pool_number = pool_num, \
   .size = JSFUNCTION_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSMETHOD_INIT(pool_num) { \
   .fill_buffer = &jsmethod_fill_buffer, \
   .buffer = 0L, \
   .idx = JSMETHOD_POOLSZ, \
   .pool_number = pool_num, \
   .size = JSMETHOD_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSPROCEDURE_INIT(pool_num) { \
   .fill_buffer = &jsprocedure_fill_buffer, \
   .buffer = 0L, \
   .idx = JSPROCEDURE_POOLSZ, \
   .pool_number = pool_num, \
   .size = JSPROCEDURE_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSSTRINGLITERALASCII_INIT(pool_num) {	\
   .fill_buffer = &jsstringliteralascii_fill_buffer, \
   .buffer = 0L, \
   .idx = JSSTRINGLITERALASCII_POOLSZ, \
   .pool_number = pool_num, \
   .size = JSSTRINGLITERALASCII_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSDATE_INIT(pool_num) { \
   .fill_buffer = &jsdate_fill_buffer,	\
   .buffer = 0L, \
   .idx = JSDATE_POOLSZ, \
   .pool_number = pool_num, \
   .size = JSDATE_POOLSZ, \
   .payload = { .dummy = 0 } \
};

static HOP_ALLOC_THREAD_DECL apool_t pool1 = APOOL_JSOBJECT_INIT(0, 1);
static HOP_ALLOC_THREAD_DECL apool_t npool1 = APOOL_JSOBJECT_INIT(0, 1);

static HOP_ALLOC_THREAD_DECL apool_t pool2 = APOOL_JSOBJECT_INIT(1, 2);
static HOP_ALLOC_THREAD_DECL apool_t npool2 = APOOL_JSOBJECT_INIT(1, 2);

static HOP_ALLOC_THREAD_DECL apool_t pool3 = APOOL_JSOBJECT_INIT(2, 3);
static HOP_ALLOC_THREAD_DECL apool_t npool3 = APOOL_JSOBJECT_INIT(2, 3);

static HOP_ALLOC_THREAD_DECL apool_t pool4 = APOOL_JSOBJECT_INIT(3, 4);
static HOP_ALLOC_THREAD_DECL apool_t npool4 = APOOL_JSOBJECT_INIT(3, 4);

static HOP_ALLOC_THREAD_DECL apool_t pool5 = APOOL_JSOBJECT_INIT(4, 5);
static HOP_ALLOC_THREAD_DECL apool_t npool5 = APOOL_JSOBJECT_INIT(4, 5);

static HOP_ALLOC_THREAD_DECL apool_t pool6 = APOOL_JSOBJECT_INIT(5, 6);
static HOP_ALLOC_THREAD_DECL apool_t npool6 = APOOL_JSOBJECT_INIT(5, 6);

static HOP_ALLOC_THREAD_DECL apool_t pool7 = APOOL_JSOBJECT_INIT(6, 7);
static HOP_ALLOC_THREAD_DECL apool_t npool7 = APOOL_JSOBJECT_INIT(6, 7);

static HOP_ALLOC_THREAD_DECL apool_t pool8 = APOOL_JSOBJECT_INIT(7, 8);
static HOP_ALLOC_THREAD_DECL apool_t npool8 = APOOL_JSOBJECT_INIT(7, 8);

static HOP_ALLOC_THREAD_DECL apool_t poolproxy = APOOL_JSPROXY_INIT(8);
static HOP_ALLOC_THREAD_DECL apool_t npoolproxy = APOOL_JSPROXY_INIT(8);

static HOP_ALLOC_THREAD_DECL apool_t poolfunction = APOOL_JSFUNCTION_INIT(9);
static HOP_ALLOC_THREAD_DECL apool_t npoolfunction = APOOL_JSFUNCTION_INIT(9);

static HOP_ALLOC_THREAD_DECL apool_t poolmethod = APOOL_JSMETHOD_INIT(10);
static HOP_ALLOC_THREAD_DECL apool_t npoolmethod = APOOL_JSMETHOD_INIT(10);

static HOP_ALLOC_THREAD_DECL apool_t poolprocedure = APOOL_JSPROCEDURE_INIT(11);
static HOP_ALLOC_THREAD_DECL apool_t npoolprocedure = APOOL_JSPROCEDURE_INIT(11);

static HOP_ALLOC_THREAD_DECL apool_t poolstringliteralascii = APOOL_JSSTRINGLITERALASCII_INIT(12);
static HOP_ALLOC_THREAD_DECL apool_t npoolstringliteralascii = APOOL_JSSTRINGLITERALASCII_INIT(12);

static HOP_ALLOC_THREAD_DECL apool_t pooldate = APOOL_JSDATE_INIT(8);
static HOP_ALLOC_THREAD_DECL apool_t npooldate = APOOL_JSDATE_INIT(8);

long inl1 = 0, snd1 = 0, slow1 = 1;
long inl2 = 0, snd2 = 0, slow2 = 1;
long inl3 = 0, snd3 = 0, slow3 = 1;
long inl4 = 0, snd4 = 0, slow4 = 1;
long inl5 = 0, snd5 = 0, slow5 = 1;
long inl6 = 0, snd6 = 0, slow6 = 1;
long inl7 = 0, snd7 = 0, slow7 = 1;
long inl8 = 0, snd8 = 0, slow8 = 1;

long inlproxy = 0, sndproxy = 0, slowproxy = 1;
long inlfunction = 0, sndfunction = 0, slowfunction = 1;
long inlmethod = 0, sndmethod = 0, slowmethod = 1;
long inlprocedure = 0, sndprocedure = 0, slowprocedure = 1;
long inlstringliteralascii = 0, sndstringliteralascii = 0, slowstringliteralascii = 1;
long inldate = 0, snddate = 0, slowdate = 1;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_stat ...                                                   */
/*---------------------------------------------------------------------*/
#if ALLOC_STATS
static void
alloc_stats_dump(int _) {
   fprintf(stderr, "jsobject...\n");
   fprintf(stderr, "  1 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool1.size, slow1, snd1, inl1, (inl1 * 100) / (inl1 + slow1));
   fprintf(stderr, "  2 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool2.size, slow2, snd2, inl2, (inl2 * 100) / (inl2 + slow2));
   fprintf(stderr, "  3 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool3.size, slow3, snd3, inl3, (inl3 * 100) / (inl3 + slow3));
   fprintf(stderr, "  4 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool4.size, slow4, snd4, inl4, (inl4 * 100) / (inl4 + slow4));
   fprintf(stderr, "  5 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool5.size, slow5, snd5, inl5, (inl5 * 100) / (inl5 + slow5));
   fprintf(stderr, "  6 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool6.size, slow6, snd6, inl6, (inl6 * 100) / (inl6 + slow6));
   fprintf(stderr, "  7 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool7.size, slow7, snd7, inl7, (inl7 * 100) / (inl7 + slow7));
   fprintf(stderr, "  8 (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pool8.size, slow8, snd8, inl8, (inl8 * 100) / (inl8 + slow8));
   fprintf(stderr, "jsproxy...\n");
   fprintf(stderr, "    (%4d): slow=%d outbuf=%d inl=%d %d%%\n", poolproxy.size, slowproxy, sndproxy, inlproxy, (inlproxy * 100) / (inlproxy + slowproxy));
   fprintf(stderr, "jsfunction...\n");
   fprintf(stderr, "    (%4d): slow=%d outbuf=%d inl=%d %d%%\n", poolfunction.size, slowfunction, sndfunction, inlfunction, (inlfunction * 100) / (inlfunction + slowfunction));
   fprintf(stderr, "jsprocedure...\n");
   fprintf(stderr, "    (%4d): slow=%d outbuf=%d inl=%d %d%%\n", poolprocedure.size, slowprocedure, sndprocedure, inlprocedure, (inlprocedure * 100) / (inlprocedure + slowprocedure));
   fprintf(stderr, "jsmethod...\n");
   fprintf(stderr, "    (%4d): slow=%d outbuf=%d inl=%d %d%%\n", poolmethod.size, slowmethod, sndmethod, inlmethod, (inlmethod * 100) / (inlmethod + slowmethod));
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
   fprintf(stderr, "jsstring...\n");
   fprintf(stderr, "    (%4d): slow=%d outbuf=%d inl=%d %d%%\n", poolstringliteralascii.size, slowstringliteralascii, sndstringliteralascii, inlstringliteralascii, (inlstringliteralascii * 100) / (inlstringliteralascii + slowstringliteralascii));
#endif   
   fprintf(stderr, "jsdate...\n");
   fprintf(stderr, "    (%4d): slow=%d outbuf=%d inl=%d %d%%\n", pooldate.size, slowdate, snddate, inldate, (inldate * 100) / (inldate + slowdate));
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    pool_queue_add ...                                               */
/*---------------------------------------------------------------------*/
static void pool_queue_add(apool_t *pool) {
   pthread_mutex_lock(&alloc_pool_mutex);
   pool_queue[pool_queue_idx++] = pool;
   pthread_cond_signal(&alloc_pool_cond);
   pthread_mutex_unlock(&alloc_pool_mutex);
}

/*---------------------------------------------------------------------*/
/*    static void *                                                    */
/*    thread_alloc_worker ...                                          */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
static void *
thread_alloc_worker(void *arg) {
   apool_t *pool;

   while (1) {
      pthread_mutex_lock(&alloc_pool_mutex);
      while (pool_queue_idx == 0) {
	 pthread_cond_wait(&alloc_pool_cond, &alloc_pool_mutex);
      }
      
      if (pool_queue_idx < 0) {
	 fprintf(stderr, "BAD INDEX %d\n", pool_queue_idx);
      }
      pool = pool_queue[--pool_queue_idx];
      pthread_mutex_unlock(&alloc_pool_mutex);

      pool->fill_buffer(pool, arg);
      pool->idx = 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsobject_buffer_fill ...                                         */
/*---------------------------------------------------------------------*/
static void
jsobject_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;
   const uint32_t objsize = pool->payload.objsize;

   for (i = 0; i < size; i++) {
      buffer[i] = bgl_make_jsobject_sans(objsize, 0L, 0L, (uint32_t)(long)arg);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsproxy_buffer_fill ...                                          */
/*---------------------------------------------------------------------*/
static void
jsproxy_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSPROXY_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for (i = 0; i < size; i++) {
      buffer[i] =
	 bgl_make_jsproxy_sans(0L, 0L, 0L, 0L, 0L, jsproxy_mode);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsfunction_buffer_fill ...                                       */
/*---------------------------------------------------------------------*/
static void
jsfunction_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for (i = 0; i < size; i++) {
      buffer[i] =
	 bgl_make_jsfunction_sans(0L, 0L, 0L, 0L, 0L);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsmethod_buffer_fill ...                                         */
/*---------------------------------------------------------------------*/
static void
jsmethod_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for (i = 0; i < size; i++) {
      buffer[i] = bgl_make_jsmethod_sans(0L, 0L, 0L, 0L, 0L, 0L);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsprocedure_buffer_fill ...                                      */
/*---------------------------------------------------------------------*/
static void
jsprocedure_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for (i = 0; i < size; i++) {
      buffer[i] = bgl_make_jsprocedure_sans(0L, 0L, 0L);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsstringliteralascii_buffer_fill ...                             */
/*---------------------------------------------------------------------*/
static void
jsstringliteralascii_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for (i = 0; i < size; i++) {
      buffer[i] = bgl_make_jsstringliteralascii_sans(0, 0L, 0L);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsdate_buffer_fill ...                                           */
/*---------------------------------------------------------------------*/
static void
jsdate_fill_buffer(apool_t *pool, void *arg) {
#if HOP_ALLOC_JSDATE_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for (i = 0; i < size; i++) {
      buffer[i] = bgl_make_jsdate_sans(0L, 0L);
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_jsalloc_locks ...                                       */
/*    -------------------------------------------------------------    */
/*    This function is split from BGL_INIT_JSALLOC to enable bmem      */
/*    lock initialization without spawning the worker threads.         */
/*---------------------------------------------------------------------*/
void
bgl_init_jsalloc_locks() {
#if BMEM_CONFIG
   fprintf(stderr, "hop alloc using bmem configuration...\n");
#endif   

   pthread_mutex_init(&alloc_pool_mutex, 0L);
   pthread_cond_init(&alloc_pool_cond, 0L);
   
   alloc_spin_init(&lock1, 0L);
   alloc_spin_init(&lock2, 0L);
   alloc_spin_init(&lock3, 0L);
   alloc_spin_init(&lock4, 0L);
   alloc_spin_init(&lock5, 0L);
   alloc_spin_init(&lock6, 0L);
   alloc_spin_init(&lock7, 0L);
   alloc_spin_init(&lock8, 0L);
   alloc_spin_init(&lockproxy, 0L);
   alloc_spin_init(&lockfunction, 0L);
   alloc_spin_init(&lockmethod, 0L);
   alloc_spin_init(&lockprocedure, 0L);
   alloc_spin_init(&lockstringliteralascii, 0L);
   alloc_spin_init(&lockdate, 0L);

   empty_vector = create_vector_uncollectable(0);

#if ALLOC_STATS
   atexit((void (*)(void))alloc_stats_dump);
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc ...                                             */
/*    -------------------------------------------------------------    */
/*    Initialized the multithreaded background allocator.              */
/*---------------------------------------------------------------------*/
int bgl_init_jsalloc(uint32_t md) {
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsobject_mode = md;

   /* initializes the mutexes and condition variables */
   bgl_init_jsalloc_locks();

   /* initialize the pool queue */
   pool_queue_len = MAX_POOL_QUEUE_SIZE;
   pool_queue = malloc(pool_queue_len * sizeof(apool_t *));
   
   /* start the allocator workers */
   for (i = 0; i < WORK_NUMBER; i++) {
      pthread_t th;
      pthread_attr_t thattr;
      pthread_attr_init(&thattr);
      pthread_attr_setdetachstate(&thattr, PTHREAD_CREATE_DETACHED);
      GC_pthread_create(&th, &thattr, thread_alloc_worker, (void *)(long)md);
      _PTHREAD_SETNAME(th, "hopjs-alloc");
   }

   return 0;
#endif   
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_worker_jsalloc ...                                      */
/*    -------------------------------------------------------------    */
/*    Each time a new worker is created the pool queue must be         */
/*    extended.                                                        */
/*---------------------------------------------------------------------*/
int
bgl_init_worker_jsalloc() {
   pthread_mutex_lock(&alloc_pool_mutex);

   pool_queue_len += MAX_POOL_QUEUE_SIZE;
   pool_queue = realloc(pool_queue, pool_queue_len * sizeof(apool_t *));
   pthread_mutex_unlock(&alloc_pool_mutex);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_proxy ...                                       */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_proxy(obj_t constrmap, obj_t elements, uint32_t mode) {
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsproxy_constrmap = constrmap;
   jsproxy_elements = elements;
   jsproxy_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_function ...                                    */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_function(BgL_jsconstructmapz00_bglt constrmap,
			   BgL_jsconstructmapz00_bglt cmap,
			   obj_t elements, obj_t alloc,
			   uint32_t mode) {
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsfunction_constrmap = constrmap;
   jsfunction_cmap = cmap;
   jsfunction_elements = elements;
   jsfunction_alloc = alloc;
   jsfunction_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_method ...                                      */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_method(BgL_jsconstructmapz00_bglt constrmap,
			   BgL_jsconstructmapz00_bglt cmap,
			   obj_t elements, obj_t alloc,
			   uint32_t mode) {
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsmethod_constrmap = constrmap;
   jsmethod_cmap = cmap;
   jsmethod_elements = elements;
   jsmethod_alloc = alloc;
   jsmethod_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_procedure ...                                   */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_procedure(BgL_jsconstructmapz00_bglt cmap,
			    uint32_t mode) {
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsprocedure_cmap = cmap;
   jsprocedure_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_stringliteralascii ...                          */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_stringliteralascii(uint32_t mode, uint32_t normmode, obj_t not_a_string_cache, uint32_t threshold) {
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsstringliteralascii_mode = mode;
   jsstringliteralascii_normmode = normmode;
   jsstringliteralascii_not_a_string_cache = not_a_string_cache;
   jsstringliteralascii_normalize_threshold = threshold;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_date ...                                        */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_date(uint32_t mode) {
   static int jsinit = 0;
   int i;

   if (jsinit) return 1;

   jsinit = 1;

   jsdate_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_yield ...                                       */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_yield(uint32_t mode) {
   jsyield_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject_sans ...                                       */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsObject                                         */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements (make-vector constrsize (js-undefined))))         */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSOBJECT_SANS static obj_t bgl_make_jsobject_sans
#else
#   define BGL_MAKE_JSOBJECT_SANS obj_t bgl_make_jsobject
#endif

BGL_MAKE_JSOBJECT_SANS(int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode) {
   long bsize = JSOBJECT_SIZE + VECTOR_SIZE + ((constrsize-1) * OBJ_SIZE);
   BgL_jsobjectz00_bglt o = (BgL_jsobjectz00_bglt)GC_MALLOC(bsize);
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSOBJECT_CLASS_NUM);
   
   // fields init
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   o->BgL_elementsz00 = empty_vector;

   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)mode);
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);
   
   // elements initialization
   vector = (obj_t)(&(o->BgL_elementsz00) + 1);

#if (!defined(TAG_VECTOR))
   vector->vector.header = MAKE_HEADER(VECTOR_TYPE, 0);
#endif		
   vector->vector.length = constrsize;
   vector = BVECTOR(vector);
   
   for (i = 0; i < constrsize; i++) {
      VECTOR_SET(vector, i, BUNSPEC);
   }

#if (defined(HOP_PROFILE))
   {
      long i = (constrsize >= VECTOR_LENGTH(bgl_js_profile_allocs) - 2
		 ? VECTOR_LENGTH(bgl_js_profile_allocs) -1
		 : constrsize);
      long cnt = BLLONG_TO_LLONG(VECTOR_REF(bgl_js_profile_allocs, i));
      VECTOR_SET(bgl_js_profile_allocs, i, LLONG_TO_BLLONG(cnt + 1));
   }
#endif

   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    BGL_MAKE_JSOBJECT ...                                            */
/*---------------------------------------------------------------------*/
#define BGL_MAKE_JSOBJECT(sz) \
   static obj_t bgl_make_jsobject##sz(obj_t constrmap, obj_t __proto__) { \
   alloc_spin_lock(&lock##sz); \
   if (pool##sz.idx < JSOBJECT_POOLSZ(sz)) { \
      obj_t o = pool##sz.buffer[pool##sz.idx]; \
      pool##sz.buffer[pool##sz.idx++] = 0; \
      alloc_spin_unlock(&lock##sz); \
      BGL_OBJECT_WIDENING_SET(o, __proto__); \
      ((BgL_jsobjectz00_bglt)(COBJECT(o)))->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap; \
      ALLOC_STAT(inl##sz++); \
      return o; \
   } else if (npool##sz.idx == 0) { \
      /* swap the two pools */ \
      obj_t *buffer = pool##sz.buffer; \
      obj_t *nbuffer = npool##sz.buffer; \
      obj_t o = nbuffer[0]; \
      \
      nbuffer[0] = 0; \
      pool##sz.buffer = nbuffer; \
      pool##sz.idx = 1; \
      \
      npool##sz.buffer = buffer; \
      npool##sz.idx = npool##sz.size; \
      \
      /* add the pool to the pool queue */ \
      pool_queue_add(&npool##sz); \
      \
      BGL_OBJECT_WIDENING_SET(o, __proto__); \
      ((BgL_jsobjectz00_bglt)(COBJECT(o)))->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap; \
      \
      ALLOC_STAT(snd##sz++); \
      alloc_spin_unlock(&lock##sz); \
      return o; \
   } else { \
      /* initialize the two alloc pools */ \
      if (!pool##sz.buffer) { \
	 pool##sz.buffer = \
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSOBJECT_POOLSZ(sz)); \
	 npool##sz.buffer =						\
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSOBJECT_POOLSZ(sz)); \
	 pool_queue_add(&npool##sz);					\
      } \
      \
      /* default slow alloc */ \
      ALLOC_STAT(slow##sz++); \
      alloc_spin_unlock(&lock##sz); \
      return bgl_make_jsobject_sans(sz, constrmap, __proto__, jsobject_mode); \
   } \
}

#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
BGL_MAKE_JSOBJECT(1)
BGL_MAKE_JSOBJECT(2)
BGL_MAKE_JSOBJECT(3)
BGL_MAKE_JSOBJECT(4)
BGL_MAKE_JSOBJECT(5)
BGL_MAKE_JSOBJECT(6)
BGL_MAKE_JSOBJECT(7)
BGL_MAKE_JSOBJECT(8)
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject ...                                            */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsobject(int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode) {
   switch(constrsize) {
      case 1: return bgl_make_jsobject1(constrmap, __proto__);
      case 2: return bgl_make_jsobject2(constrmap, __proto__);
      case 3: return bgl_make_jsobject3(constrmap, __proto__);
      case 4: return bgl_make_jsobject4(constrmap, __proto__);
      case 5: return bgl_make_jsobject5(constrmap, __proto__);
      case 6: return bgl_make_jsobject6(constrmap, __proto__);
      case 7: return bgl_make_jsobject7(constrmap, __proto__);
      case 8: return bgl_make_jsobject8(constrmap, __proto__);
      default: return bgl_make_jsobject_sans((int)constrsize, constrmap, __proto__, mode);
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject_bmem ...                                       */
/*    -------------------------------------------------------------    */
/*    Only for bmem wrapping                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsobject_bmem(int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode) {
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
   return bgl_make_jsobject_sans((int)constrsize, constrmap, __proto__, mode);
#else   
   return bgl_make_jsobject((int)constrsize, constrmap, __proto__, mode);
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsproxy_sans ...                                        */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsProxy                                          */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements proxy-elements))                                  */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSPROXY_POLICY != HOP_ALLOC_CLASSIC
#  define BGL_MAKE_JSPROXY_SANS static obj_t bgl_make_jsproxy_sans
#else   
#  define BGL_MAKE_JSPROXY_SANS obj_t bgl_make_jsproxy
#endif
   
BGL_MAKE_JSPROXY_SANS(obj_t target, obj_t handler,
		       obj_t getcache, obj_t setcache, obj_t applycache,
		       uint32_t mode) {   
   long bsize = JSPROXY_SIZE;
   BgL_jsproxyz00_bglt o = (BgL_jsproxyz00_bglt)GC_MALLOC(bsize);

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSPROXY_CLASS_NUM);
   
   // fields init
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)mode);
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), target);
   
   o->BgL_handlerz00 = (struct BgL_jsobjectz00_bgl *)handler;
   o->BgL_getcachez00 = getcache;
   o->BgL_setcachez00 = setcache;
   o->BgL_applycachez00 = applycache;
   
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)jsproxy_constrmap;
   o->BgL_elementsz00 = jsproxy_elements;
   
   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsproxy ...                                             */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsProxy                                          */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements proxy-elements))                                  */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSPROXY_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsproxy(obj_t target, obj_t handler,
		  obj_t getcache, obj_t setcache, obj_t applycache,
		  uint32_t md) {
   alloc_spin_lock(&lockproxy); 
   if (poolproxy.idx < JSPROXY_POOLSZ) { 
      obj_t o = poolproxy.buffer[poolproxy.idx]; 
      poolproxy.buffer[poolproxy.idx++] = 0; 
      alloc_spin_unlock(&lockproxy);
      
      BGL_OBJECT_WIDENING_SET(o, target);
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_handlerz00 = (BgL_jsobjectz00_bglt)handler; 
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_getcachez00 = getcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_setcachez00 = setcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_applycachez00 = applycache; 
      ALLOC_STAT(inlproxy++);
      return o; 
   } else if (npoolproxy.idx == 0) { 
      /* swap the two pools */ 
      obj_t *buffer = poolproxy.buffer; 
      obj_t o = npoolproxy.buffer[0]; 
      
      poolproxy.buffer = npoolproxy.buffer; 
      poolproxy.buffer[0] = 0; 
      poolproxy.idx = 1; 
      
      npoolproxy.buffer = buffer; 
      npoolproxy.idx = npoolproxy.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add(&npoolproxy); 
      
      BGL_OBJECT_WIDENING_SET(o, target);
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_handlerz00 = (BgL_jsobjectz00_bglt)handler; 
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_getcachez00 = getcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_setcachez00 = setcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT(o)))->BgL_applycachez00 = applycache; 
      
      ALLOC_STAT(sndproxy++); 
      alloc_spin_unlock(&lockproxy); 
      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if (!poolproxy.buffer) { 
	 poolproxy.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSPROXY_POOLSZ); 
	 poolproxy.idx = JSPROXY_POOLSZ; 
	 npoolproxy.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSPROXY_POOLSZ); 
	 npoolproxy.idx = JSPROXY_POOLSZ; 
	 pool_queue_add(&npoolproxy); 
      } 
      
      /* default slow alloc */ 
      ALLOC_STAT(slowproxy++); 
      alloc_spin_unlock(&lockproxy);
      return bgl_make_jsproxy_sans(target, handler,
				    getcache, setcache, applycache, jsproxy_mode); 
   } 
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsfunction_sans ...                                     */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsFunction                                       */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements function-elements))                               */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSFUNCTION_SANS static obj_t bgl_make_jsfunction_sans
#else
#   define BGL_MAKE_JSFUNCTION_SANS obj_t bgl_make_jsfunction
#endif

BGL_MAKE_JSFUNCTION_SANS(obj_t procedure,
			  long arity, long constrsize,
			  obj_t __proto__, obj_t info) {
   BgL_jsfunctionz00_bglt o = (BgL_jsfunctionz00_bglt)GC_MALLOC(JSFUNCTION_SIZE);

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSFUNCTION_CLASS_NUM);

   // immutable fields init
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)jsfunction_mode);
   o->BgL_allocz00 = jsfunction_alloc;
   o->BgL_constrmapz00 = jsfunction_constrmap;
   o->BgL_elementsz00 = jsfunction_elements;
   o->BgL_cmapz00 = jsfunction_cmap;
   o->BgL_prototypez00 = BCHAR('F');

   // mutable fields
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
   if (procedure)
#endif
   {
      BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);
      o->BgL_procedurez00 = procedure;
      o->BgL_arityz00 = arity;
      o->BgL_infoz00 = info;
      o->BgL_constrsiza7eza7 = constrsize;
   }
   
#if (defined(HOP_PROFILE))
   {
      long i = (constrsize >= VECTOR_LENGTH(bgl_js_profile_allocs) - 2
		 ? VECTOR_LENGTH(bgl_js_profile_allocs) -1
		 : constrsize);
      long cnt = BLLONG_TO_LLONG(VECTOR_REF(bgl_js_profile_allocs, i));
      VECTOR_SET(bgl_js_profile_allocs, i, LLONG_TO_BLLONG(cnt + 1));
   }
#endif

   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsfunction ...                                          */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsFunction                                       */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements function-elements))                               */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsfunction(obj_t procedure,
		     long arity, long constrsize,
		     obj_t __proto__, obj_t info) {
   alloc_spin_lock(&lockfunction);

   if (poolfunction.idx < JSFUNCTION_POOLSZ) { 
      obj_t o = poolfunction.buffer[poolfunction.idx]; 
      poolfunction.buffer[poolfunction.idx++] = 0; 
      alloc_spin_unlock(&lockfunction);
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_procedurez00 = procedure;
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_arityz00 = arity;
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_infoz00 = info;
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT(inlfunction++); 
      return o; 
   } else if (npoolfunction.idx == 0) { 
      /* swap the two pools */ 
      obj_t *buffer = poolfunction.buffer; 
      obj_t o = npoolfunction.buffer[0]; 
      
      poolfunction.buffer = npoolfunction.buffer; 
      poolfunction.buffer[0] = 0; 
      poolfunction.idx = 1; 
      
      npoolfunction.buffer = buffer; 
      npoolfunction.idx = npoolfunction.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add(&npoolfunction); 
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_procedurez00 = procedure;
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_arityz00 = arity;
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_infoz00 = info;
      ((BgL_jsfunctionz00_bglt)(COBJECT(o)))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT(sndfunction++); 
      alloc_spin_unlock(&lockfunction);

      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if (!poolfunction.buffer) { 
	 poolfunction.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSFUNCTION_POOLSZ); 
	 poolfunction.idx = JSFUNCTION_POOLSZ; 
	 npoolfunction.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSFUNCTION_POOLSZ); 
	 npoolfunction.idx = JSFUNCTION_POOLSZ; 
	 pool_queue_add(&npoolfunction); 
      } 

      /* default slow alloc */ 
      ALLOC_STAT(slowfunction++); 
      alloc_spin_unlock(&lockfunction); 
      return bgl_make_jsfunction_sans(procedure,
				       arity, constrsize,
				       __proto__, info);
   } 
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsmethod_sans ...                                       */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsMethod                                         */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements method-elements))                                 */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSMETHOD_SANS static obj_t bgl_make_jsmethod_sans
#else
#   define BGL_MAKE_JSMETHOD_SANS obj_t bgl_make_jsmethod
#endif

BGL_MAKE_JSMETHOD_SANS(obj_t procedure, obj_t method,
			  long arity, long constrsize,
			  obj_t __proto__, obj_t info) {
   BgL_jsmethodz00_bglt o = (BgL_jsmethodz00_bglt)GC_MALLOC(JSMETHOD_SIZE);

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSMETHOD_CLASS_NUM);

   // immutable fields init
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)jsmethod_mode);
   o->BgL_allocz00 = jsmethod_alloc;
   o->BgL_constrmapz00 = jsmethod_constrmap;
   o->BgL_elementsz00 = jsmethod_elements;
   o->BgL_cmapz00 = jsmethod_cmap;
   o->BgL_prototypez00 = BCHAR('F');

   // mutable fields
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
   if (procedure)
#endif
   {
      BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);
      o->BgL_procedurez00 = procedure;
      o->BgL_methodz00 = method;
      o->BgL_arityz00 = arity;
      o->BgL_infoz00 = info;
      o->BgL_constrsiza7eza7 = constrsize;
   }
   
#if (defined(HOP_PROFILE))
   {
      long i = (constrsize >= VECTOR_LENGTH(bgl_js_profile_allocs) - 2
		 ? VECTOR_LENGTH(bgl_js_profile_allocs) -1
		 : constrsize);
      long cnt = BLLONG_TO_LLONG(VECTOR_REF(bgl_js_profile_allocs, i));
      VECTOR_SET(bgl_js_profile_allocs, i, LLONG_TO_BLLONG(cnt + 1));
   }
#endif

   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsmethod ...                                            */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsMethod                                         */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements method-elements))                                 */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsmethod(obj_t procedure, obj_t method,
		     long arity, long constrsize,
		     obj_t __proto__, obj_t info) {
   alloc_spin_lock(&lockmethod);

   if (poolmethod.idx < JSMETHOD_POOLSZ) { 
      obj_t o = poolmethod.buffer[poolmethod.idx]; 
      poolmethod.buffer[poolmethod.idx++] = 0; 
      alloc_spin_unlock(&lockmethod);
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_procedurez00 = procedure;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_methodz00 = method;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_arityz00 = arity;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_infoz00 = info;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT(inlmethod++); 
      return o; 
   } else if (npoolmethod.idx == 0) { 
      /* swap the two pools */ 
      obj_t *buffer = poolmethod.buffer; 
      obj_t o = npoolmethod.buffer[0]; 
      
      poolmethod.buffer = npoolmethod.buffer; 
      poolmethod.buffer[0] = 0; 
      poolmethod.idx = 1; 
      
      npoolmethod.buffer = buffer; 
      npoolmethod.idx = npoolmethod.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add(&npoolmethod); 
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_procedurez00 = procedure;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_methodz00 = method;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_arityz00 = arity;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_infoz00 = info;
      ((BgL_jsmethodz00_bglt)(COBJECT(o)))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT(sndmethod++); 
      alloc_spin_unlock(&lockmethod);

      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if (!poolmethod.buffer) { 
	 poolmethod.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSMETHOD_POOLSZ); 
	 poolmethod.idx = JSMETHOD_POOLSZ; 
	 npoolmethod.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSMETHOD_POOLSZ); 
	 npoolmethod.idx = JSMETHOD_POOLSZ; 
	 pool_queue_add(&npoolmethod); 
      } 

      /* default slow alloc */ 
      ALLOC_STAT(slowmethod++); 
      alloc_spin_unlock(&lockmethod); 
      return bgl_make_jsmethod_sans(procedure, method, 
				       arity, constrsize,
				       __proto__, info);
   } 
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsprocedure_sans ...                                    */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSPROCEDURE_SANS static obj_t bgl_make_jsprocedure_sans
#else
#   define BGL_MAKE_JSPROCEDURE_SANS obj_t bgl_make_jsprocedure
#endif

BGL_MAKE_JSPROCEDURE_SANS(obj_t procedure, long arity, obj_t __proto__) {
   BgL_jsprocedurez00_bglt o = (BgL_jsprocedurez00_bglt)GC_MALLOC(JSPROCEDURE_SIZE);

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSPROCEDURE_CLASS_NUM);

   // immutable fields init
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)jsprocedure_mode);
   o->BgL_cmapz00 = jsfunction_cmap;
   o->BgL_elementsz00 = empty_vector;
      
   // mutable fields init
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
   if (procedure)
#endif
   {
      o->BgL_procedurez00 = procedure;
      o->BgL_arityz00 = arity;
      o->BgL_cmapz00 = jsprocedure_cmap;
   }

   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsprocedure ...                                         */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsprocedure(obj_t procedure, long arity, obj_t __proto__) {
   alloc_spin_lock(&lockprocedure);

   if (poolprocedure.idx < JSPROCEDURE_POOLSZ) { 
      obj_t o = poolprocedure.buffer[poolprocedure.idx]; 
      poolprocedure.buffer[poolprocedure.idx++] = 0; 
      alloc_spin_unlock(&lockprocedure);
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsprocedurez00_bglt)(COBJECT(o)))->BgL_procedurez00 = procedure;
      ((BgL_jsprocedurez00_bglt)(COBJECT(o)))->BgL_arityz00 = arity;
      
      ALLOC_STAT(inlprocedure++); 
      return o; 
   } else if (npoolprocedure.idx == 0) { 
      /* swap the two pools */ 
      obj_t *buffer = poolprocedure.buffer; 
      obj_t o = npoolprocedure.buffer[0]; 
      
      poolprocedure.buffer = npoolprocedure.buffer; 
      poolprocedure.buffer[0] = 0; 
      poolprocedure.idx = 1; 
      
      npoolprocedure.buffer = buffer; 
      npoolprocedure.idx = npoolprocedure.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add(&npoolprocedure); 
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsprocedurez00_bglt)(COBJECT(o)))->BgL_procedurez00 = procedure;
      ((BgL_jsprocedurez00_bglt)(COBJECT(o)))->BgL_arityz00 = arity;
      
      ALLOC_STAT(sndprocedure++); 
      alloc_spin_unlock(&lockprocedure);

      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if (!poolprocedure.buffer) { 
	 poolprocedure.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSPROCEDURE_POOLSZ); 
	 poolprocedure.idx = JSPROCEDURE_POOLSZ; 
	 npoolprocedure.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSPROCEDURE_POOLSZ); 
	 npoolprocedure.idx = JSPROCEDURE_POOLSZ; 
	 pool_queue_add(&npoolprocedure); 
      } 

      /* default slow alloc */ 
      ALLOC_STAT(slowprocedure++); 
      alloc_spin_unlock(&lockprocedure); 
      return bgl_make_jsprocedure_sans(procedure, arity, __proto__);
   } 
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsprocedure_bmem ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsprocedure_bmem(obj_t procedure, long arity, obj_t __proto__) {
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
   return bgl_make_jsprocedure_sans(procedure, arity, __proto__);
#else   
   return bgl_make_jsprocedure(procedure, arity, __proto__);
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsstringliteralascii_sans ...                           */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSSTRINGLITERALASCII_SANS static obj_t bgl_make_jsstringliteralascii_sans
#else
#   define BGL_MAKE_JSSTRINGLITERALASCII_SANS obj_t bgl_make_jsstringliteralascii
#endif

BGL_MAKE_JSSTRINGLITERALASCII_SANS(uint32_t len, obj_t left, obj_t right) {
   BgL_jsstringliteralasciiz00_bglt o = (BgL_jsstringliteralasciiz00_bglt)GC_MALLOC(JSSTRINGLITERALASCII_SIZE);

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSSTRINGLITERALASCII_CLASS_NUM);

   // field init
   o->BgL_lengthz00 = len;
   o->BgL_leftz00 = left;
   o->BgL_rightz00 = right;
   
   // immutable fields init
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), BFALSE);
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)jsstringliteralascii_mode);

   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsstringliteralascii ...                                */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsstringliteralascii(uint32_t len, obj_t left, obj_t right) {
   alloc_spin_lock(&lockstringliteralascii);

   if (poolstringliteralascii.idx < JSSTRINGLITERALASCII_POOLSZ) {
      obj_t o = poolstringliteralascii.buffer[poolstringliteralascii.idx];
      poolstringliteralascii.buffer[poolstringliteralascii.idx++] = 0;
      alloc_spin_unlock(&lockstringliteralascii);

      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_lengthz00 = len; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_leftz00 = left; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_rightz00 = right;

      ALLOC_STAT(inlstringliteralascii++);
      return o;
   } else if (npoolstringliteralascii.idx == 0) {
      /* swap the two pools */
      obj_t *buffer = poolstringliteralascii.buffer;
      obj_t *nbuffer = npoolstringliteralascii.buffer;
      obj_t o = nbuffer[0];

      poolstringliteralascii.buffer = nbuffer;
      poolstringliteralascii.buffer[0] = 0;
      poolstringliteralascii.idx = 1;

      npoolstringliteralascii.buffer = buffer;
      npoolstringliteralascii.idx = npoolstringliteralascii.size;

      /* add the pool to the pool queue */
      pool_queue_add(&npoolstringliteralascii);

      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_lengthz00 = len; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_leftz00 = left; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_rightz00 = right;
      
      ALLOC_STAT(sndstringliteralascii++);
      alloc_spin_unlock(&lockstringliteralascii);

      return o;
   } else {
      /* initialize the two alloc pools */
      if (!poolstringliteralascii.buffer) {
	 poolstringliteralascii.buffer =
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSSTRINGLITERALASCII_POOLSZ);
	 poolstringliteralascii.idx = JSSTRINGLITERALASCII_POOLSZ;
	 npoolstringliteralascii.buffer =
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSSTRINGLITERALASCII_POOLSZ);
	 npoolstringliteralascii.idx = JSSTRINGLITERALASCII_POOLSZ;
	 pool_queue_add(&npoolstringliteralascii);
      }

      /* default slow alloc */
      ALLOC_STAT(slowstringliteralascii++);
      alloc_spin_unlock(&lockstringliteralascii);
      return bgl_make_jsstringliteralascii_sans(len, left, right);
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_jsstring_append_ascii ...                                    */
/*    -------------------------------------------------------------    */
/*    This version is not used currently                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_jsstring_append_ascii(obj_t left, obj_t right) {
   uint32_t len = ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(left)))->BgL_lengthz00
      + ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(right)))->BgL_lengthz00;

   alloc_spin_lock(&lockstringliteralascii);
   
   if (len < jsstringliteralascii_normalize_threshold) {
      obj_t nleft = string_append(
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(left)))->BgL_leftz00,
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(right)))->BgL_leftz00);
      obj_t nright = jsstringliteralascii_not_a_string_cache;

      if (poolstringliteralascii.idx < JSSTRINGLITERALASCII_POOLSZ) {
	 obj_t o = poolstringliteralascii.buffer[poolstringliteralascii.idx];
	 poolstringliteralascii.buffer[poolstringliteralascii.idx++] = 0;
	 alloc_spin_unlock(&lockstringliteralascii);

	 HOP_OBJECT_HEADER_SIZE_SET(o, (long)jsstringliteralascii_normmode);
	 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_lengthz00 = len; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_leftz00 = nleft; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_rightz00 = nright;

	 ALLOC_STAT(inlstringliteralascii++);
	 return o;
      } else {
	 alloc_spin_unlock(&lockstringliteralascii);

	 {
	    obj_t o = bgl_make_jsstringliteralascii(len, nleft, nright);
	    HOP_OBJECT_HEADER_SIZE_SET(o, (long)jsstringliteralascii_normmode);
	    return o;
	 }
      }
   } else {
      if (poolstringliteralascii.idx < JSSTRINGLITERALASCII_POOLSZ) {
	 obj_t o = poolstringliteralascii.buffer[poolstringliteralascii.idx];
	 poolstringliteralascii.buffer[poolstringliteralascii.idx++] = 0;
	 alloc_spin_unlock(&lockstringliteralascii);

	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_lengthz00 = len; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_leftz00 = left; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT(o)))->BgL_rightz00 = right;

	 ALLOC_STAT(inlstringliteralascii++);
	 return o;
      } else {
	 alloc_spin_unlock(&lockstringliteralascii);

	 {
	    return bgl_make_jsstringliteralascii(len, left, right);
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsdate_sans ...                                         */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsDate                                           */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap))                                           */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSDATE_POLICY != HOP_ALLOC_CLASSIC
#  define BGL_MAKE_JSDATE_SANS static obj_t bgl_make_jsdate_sans
#else   
#  define BGL_MAKE_JSDATE_SANS obj_t bgl_make_jsdate
#endif
   
BGL_MAKE_JSDATE_SANS(BgL_jsconstructmapz00_bglt cmap, obj_t __proto__) {
   long bsize = JSDATE_SIZE;
   BgL_jsdatez00_bglt o = (BgL_jsdatez00_bglt)GC_MALLOC(bsize);

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSDATE_CLASS_NUM);
   
   // fields init
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)jsdate_mode);
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);
   
   o->BgL_timez00 = BFALSE;
   o->BgL_z52valz52 = BFALSE;
   
   o->BgL_cmapz00 = cmap;
   o->BgL_elementsz00 = empty_vector;
   
   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsdate ...                                              */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsDate                                           */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap))                                           */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSDATE_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsdate(BgL_jsconstructmapz00_bglt cmap, obj_t __proto__) {
   alloc_spin_lock(&lockdate); 
   if (pooldate.idx < JSDATE_POOLSZ) { 
      obj_t o = pooldate.buffer[pooldate.idx]; 
      pooldate.buffer[pooldate.idx++] = 0; 
      alloc_spin_unlock(&lockdate);
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsdatez00_bglt)(COBJECT(o)))->BgL_cmapz00 = cmap; 
      ((BgL_jsdatez00_bglt)(COBJECT(o)))->BgL_timez00 = BFALSE; 
      ((BgL_jsdatez00_bglt)(COBJECT(o)))->BgL_z52valz52 = BFALSE;
      ALLOC_STAT(inldate++);
      return o; 
   } else if (npooldate.idx == 0) { 
      /* swap the two pools */ 
      obj_t *buffer = pooldate.buffer; 
      obj_t o = npooldate.buffer[0]; 
      
      pooldate.buffer = npooldate.buffer; 
      pooldate.buffer[0] = 0; 
      pooldate.idx = 1; 
      
      npooldate.buffer = buffer; 
      npooldate.idx = npooldate.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add(&npooldate); 
      
      BGL_OBJECT_WIDENING_SET(o, __proto__);
      ((BgL_jsdatez00_bglt)(COBJECT(o)))->BgL_cmapz00 = cmap; 
      ((BgL_jsdatez00_bglt)(COBJECT(o)))->BgL_timez00 = BFALSE;
      ((BgL_jsdatez00_bglt)(COBJECT(o)))->BgL_z52valz52 = BFALSE;
      
      ALLOC_STAT(snddate++); 
      alloc_spin_unlock(&lockdate); 
      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if (!pooldate.buffer) { 
	 pooldate.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSDATE_POOLSZ); 
	 pooldate.idx = JSDATE_POOLSZ; 
	 npooldate.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE(sizeof(obj_t)*JSDATE_POOLSZ); 
	 npooldate.idx = JSDATE_POOLSZ; 
	 pool_queue_add(&npooldate); 
      } 
      
      /* default slow alloc */ 
      ALLOC_STAT(slowdate++); 
      alloc_spin_unlock(&lockdate);
      return bgl_make_jsdate_sans(cmap, __proto__); 
   } 
}
#endif

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    empty_vector ...                                                 */
/*---------------------------------------------------------------------*/
#if (!defined(TAG_VECTOR))
static struct {
   __CNST_ALIGN header_t header;
   long length;
} _empty_vector = { __CNST_FILLER MAKE_HEADER(VECTOR_TYPE, 0), 0 };
static obj_t empty_vector = BVECTOR(&(_empty_vector.header));
#else   
static struct {
   __CNST_ALIGN long length;
} _empty_vector = { __CNST_FILLER 0 };
static obj_t empty_vector = BVECTOR(&(_empty_vector.length));
#endif   

/*---------------------------------------------------------------------*/
/*    BGL_MAKE_JSARRAY_SANS_INIT                                       */
/*---------------------------------------------------------------------*/
#define BGL_MAKE_JSARRAY_SANS_INIT(o, size, len, ilen, constrmap, __proto___, mode) \
   long bsize = JSARRAY_SIZE + VECTOR_SIZE + ((size-1) * OBJ_SIZE); \
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)GC_MALLOC(bsize);  \
   obj_t vector;						      \
								      \
   /* class initialization */					      \
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSARRAY_CLASS_NUM);  \
								      \
   /* fields init */						      \
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;	      \
   o->BgL_elementsz00 = empty_vector;				      \
   o->BgL_lengthz00 = len;					      \
   o->BgL_ilenz00 = ilen;					      \
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)mode);	      \
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);	      \
								      \
   /* vector initialization */					      \
   if (size > 0) {						      \
      vector = (obj_t)(&(o->BgL_vecz00) + 1);			      \
      BGL_TAG_VECTOR(vector);					      \
      vector->vector.length = size;				      \
      vector = BVECTOR(vector);				      \
   } else {							      \
      vector = empty_vector;					      \
   }								      \
								      \
   o->BgL_vecz00 = vector;					      \
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsarray_sans_init ...                                   */
/*    -------------------------------------------------------------    */
/*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
/*    -------------------------------------------------------------    */
/*    This C version of the array allocation creates inner pointer     */
/*    from the object to its vector component. This is safe as long    */
/*    as the array lives in the JavaScript world but not if the        */
/*    array is passed to Scheme because there will not necessary be a  */
/*    pointer to the beginning of the object. For this to be safe the  */
/*    GC has to be configured with INNER_POINTER activated.            */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsarray_sans_init(long size, uint32_t len, uint32_t ilen, obj_t constrmap, obj_t __proto__, uint32_t mode) {
   BGL_MAKE_JSARRAY_SANS_INIT(o, size, len, ilen, constrmap, __proto__, mode);
   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsarray ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsarray(long size, uint32_t len, obj_t constrmap, obj_t __proto__, obj_t absent, uint32_t mode) {
   BGL_MAKE_JSARRAY_SANS_INIT(array, size, len, 0, constrmap, __proto__, mode);
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)(array);

   if (size > 0) {
      obj_t ivector = o->BgL_vecz00;
      int i;
      
      for (i = 0; i < size; i++) {
	 VECTOR_SET(ivector, i, absent);
      }
   }

   return BHOPOBJECT(array);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_jsarray_shift_builtin ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_jsarray_shift_builtin(obj_t array) {
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)COBJECT(array);
   obj_t vec = CVECTOR(o->BgL_vecz00);
   obj_t res = VECTOR_REF(BVECTOR(vec), 0);
   long size = VECTOR_LENGTH(BVECTOR(vec));
   obj_t nvec = (obj_t)(((obj_t *)vec) + 1);

   // avoid memory leaks
   VECTOR_SET(BVECTOR(vec), 0, BUNSPEC);
   
   BGL_TAG_VECTOR(nvec);
   nvec->vector.length = size - 1;
   nvec = BVECTOR(nvec);

   // assign to remember that this array is fully inlined
   *(&(o->BgL_vecz00) + 1) = nvec;
   o->BgL_vecz00 = nvec;

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_init_vector ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_init_vector(obj_t vector, long len, obj_t init) {
#if ( VECTOR_SIZE_TAG_NB_BIT != 0)
   if (len & ~(VECTOR_LENGTH_MASK)) {
      C_FAILURE("create_vector", "vector too large", BINT(len));
      return BUNSPEC;
   } else
#endif
   {
      BGL_TAG_VECTOR(vector);
      vector->vector.length = len;

      bgl_fill_vector(BVECTOR(vector), 0, len, init);
      return BVECTOR(vector);
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_init_vector_sans_fill ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_init_vector_sans_fill(obj_t vector, long len) {
   // see bglhopscript.h
   return BGL_INIT_VECTOR_SANS_FILL(vector, len);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_vector_bytesize ...                                          */
/*---------------------------------------------------------------------*/
long
bgl_vector_bytesize(long len) {
   return VECTOR_SIZE + ((len-1) * OBJ_SIZE);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_pcache_table ...                                        */
/*    -------------------------------------------------------------    */
/*    Create a fake Bigloo vector whose elements are inlined pcache    */
/*    entries.                                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_pcache_table(obj_t obj, int len, obj_t src, obj_t thread, obj_t template) {
   pcache_t *pcache = (pcache_t *)obj;
   int i;

   for (i = 0; i < len; i++) {
      memcpy(&(pcache[i]), COBJECT(template), sizeof(pcache_t));
   }

   bgl_profile_pcache_tables = MAKE_PAIR(MAKE_PAIR((obj_t)pcache, BINT(len)), bgl_profile_pcache_tables);

   return obj;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_profile_get_pcaches ...                                      */
/*    -------------------------------------------------------------    */
/*    This function is called by the cache profiler, after the         */
/*    execution, in order to get the list of all used                  */
/*    property caches.                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_profile_get_pcaches() {
   obj_t res = BNIL;
   obj_t tables = bgl_profile_pcache_tables;

   while (PAIRP(tables)) {
      obj_t table = CAR(tables);
      long i = CINT(CDR(table));
      pcache_t *pcache = (pcache_t *)CAR(table);

      while (--i >= 0) {
	 res = MAKE_PAIR(BHOPOBJECT(&(pcache[i])), res);
      }

      tables = CDR(tables);
   }
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsgenerator ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsgenerator(obj_t constrmap, obj_t __proto__, long sz, obj_t next, uint32_t mode) {
   long bsize = JSGENERATOR_SIZE + VECTOR_SIZE + ((sz-1) * OBJ_SIZE);
   BgL_jsgeneratorz00_bglt o = (BgL_jsgeneratorz00_bglt)GC_MALLOC(bsize);
   obj_t env;

   /* class initialization */
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSGENERATOR_CLASS_NUM);

   /* fields init */
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   o->BgL_elementsz00 = empty_vector;
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), (long)mode);
   BGL_OBJECT_WIDENING_SET(BHOPOBJECT(o), __proto__);

   /* vector initialization */
   env = (obj_t)(&(o->BgL_z52envz52) + 1);
   BGL_TAG_VECTOR(env);
   env->vector.length = sz;
   env = BVECTOR(env);

   /* generator field initialization */
   o->BgL_z52nextz52 = next;
   o->BgL_z52envz52 = env;

   return BHOPOBJECT(o);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_init_jsyield_object ...                                      */
/*---------------------------------------------------------------------*/
obj_t
bgl_init_jsyield_object(obj_t p) {
   BgL_jsobjectz00_bglt o = (BgL_jsobjectz00_bglt)p;
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET(BHOPOBJECT(o), JSYIELD_CLASS_NUM);

   // fields init
   o->BgL_elementsz00 = empty_vector;
   HOP_OBJECT_HEADER_SIZE_SET(BHOPOBJECT(o), jsyield_mode);
   
   // elements initialization
   vector = (obj_t)(&(o->BgL_elementsz00) + 1);

#if (!defined(TAG_VECTOR))
   vector->vector.header = MAKE_HEADER(VECTOR_TYPE, 0);
#endif		
   vector->vector.length = JSYIELD_OBJECT_CONSTRSIZE;

#if (defined(DEBUG))
   vector = BVECTOR(vector);

   for (i = 0; i < 2; i++) {
      VECTOR_SET(vector, i, BUNSPEC);
   }
#endif
   
   return BHOPOBJECT(o);
}
