/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/_bglhopscript.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Wed Apr 29 17:41:08 2020 (serrano)                */
/*    Copyright   :  2016-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Optional file, used only for the C backend, that optimizes       */
/*    JsObject and cache implementations.                              */
/*=====================================================================*/
#include <stdio.h>
#include <bigloo.h>
#include "bglhopscript.h"
#include "bglhopscript_malloc.h"
#include <pthread.h>

/*---------------------------------------------------------------------*/
/*    Bigloo backward compatibility                                    */
/*---------------------------------------------------------------------*/
#if( defined( BGL_NAN_TAGGING ) )
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

extern obj_t string_append( obj_t, obj_t );

#define JSOBJECT_SIZE \
   sizeof( struct BgL_jsobjectz00_bgl )
#define JSOBJECT_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsObjectz00zz__hopscript_typesz00 )

#define JSPROXY_SIZE \
   sizeof( struct BgL_jsproxyz00_bgl )
#define JSPROXY_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsProxyz00zz__hopscript_typesz00 )

#define JSFUNCTION_SIZE \
   sizeof( struct BgL_jsfunctionz00_bgl )
#define JSFUNCTION_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsFunctionz00zz__hopscript_typesz00 )

#define JSMETHOD_SIZE \
   sizeof( struct BgL_jsmethodz00_bgl )
#define JSMETHOD_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsMethodz00zz__hopscript_typesz00 )

#define JSPROCEDURE_SIZE \
   sizeof( struct BgL_jsprocedurez00_bgl )
#define JSPROCEDURE_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsProcedurez00zz__hopscript_typesz00 )

#define JSARRAY_SIZE \
   sizeof( struct BgL_jsarrayz00_bgl )
#define JSARRAY_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsArrayz00zz__hopscript_typesz00 )

#define JSSTRINGLITERALASCII_SIZE \
   sizeof( struct BgL_jsstringliteralasciiz00_bgl )
#define JSSTRINGLITERALASCII_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsStringLiteralASCIIz00zz__hopscript_typesz00 )

extern obj_t bgl_js_profile_allocs;
obj_t bgl_profile_pcache_tables = BNIL;
extern int GC_pthread_create();

static obj_t jsproxy_constrmap, jsproxy_elements;

static obj_t jsfunction_elements, jsfunction_alloc;
static BgL_jsconstructmapz00_bglt jsfunction_constrmap, jsfunction_cmap;
static uint32_t jsfunction_mode;

static obj_t jsmethod_elements, jsmethod_alloc;
static BgL_jsconstructmapz00_bglt jsmethod_constrmap, jsmethod_cmap;
static uint32_t jsmethod_mode;

static uint32_t jsprocedure_mode;
static BgL_jsconstructmapz00_bglt jsprocedure_cmap;

static uint32_t jsstringliteralascii_mode, jsstringliteralascii_normmode;
static obj_t jsstringliteralascii_not_a_string_cache;
static uint32_t jsstringliteralascii_normalize_threshold;

static obj_t empty_vector;

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

#if( !defined( HOP_ALLOC_POLICY ) )
#  if BGL_HAS_THREAD_LOCALSTORAGE == 1
#    define HOP_ALLOC_POLICY HOP_ALLOC_TLS
#  elif BGL_HAVE_SPINLOCK
#    define HOP_ALLOC_POLICY HOP_ALLOC_SPINLOCK
#  else
#    define HOP_ALLOC_POLICY HOP_ALLOC_CLASSIC
#  endif
#endif

/* #undef HOP_ALLOC_POLICY                                             */
/* #define HOP_ALLOC_POLICY HOP_ALLOC_CLASSIC                          */

#define HOP_ALLOC_JSOBJECT_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSPROXY_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSFUNCTION_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSMETHOD_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSPROCEDURE_POLICY HOP_ALLOC_POLICY
#define HOP_ALLOC_JSSTRINGLITERALASCII_POLICY HOP_ALLOC_POLICY

/* #undef HOP_ALLOC_JSFUNCTION_POLICY                                  */
/* #define HOP_ALLOC_JSFUNCTION_POLICY HOP_ALLOC_CLASSIC               */
/* #undef HOP_ALLOC_JSMETHOD_POLICY                                  */
/* #define HOP_ALLOC_JSMETHOD_POLICY HOP_ALLOC_CLASSIC               */

extern obj_t bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode );

#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsobject_sans( int constrsize, obj_t constrmap,
				     obj_t __proto__, uint32_t mode );
#endif

#if HOP_ALLOC_JSPROXY_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsproxy_sans( obj_t target, obj_t handler,
				    obj_t gcache, obj_t scache, obj_t acache,
				    uint32_t mode );
#endif

#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsfunction_sans( obj_t procedure,
				       long arity,
				       long constrsize,
				       obj_t __proto__, obj_t info );
#endif

#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsmethod_sans( obj_t procedure, obj_t method,
				       long arity,
				       long constrsize,
				       obj_t __proto__, obj_t info );
#endif

#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsprocedure_sans( obj_t procedure, long arity, obj_t __proto__ );
#endif

#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsstringliteralascii_sans( uint32_t len, obj_t left, obj_t right );
#endif

#define POOLSZ( sz ) ((12800 >> sz) + 400)
#define JSPROXY_POOLSZ POOLSZ( 3 )
#define JSFUNCTION_POOLSZ POOLSZ( 4 )
#define JSMETHOD_POOLSZ POOLSZ( 4 )
#define JSPROCEDURE_POOLSZ POOLSZ( 4 )
#define JSSTRINGLITERALASCII_POOLSZ POOLSZ( 4 )
#define WORK_NUMBER 1

#define ALLOC_STATS 0
#define ALLOC_STAT_FREQ 100000

/*---------------------------------------------------------------------*/
/*    stat                                                             */
/*---------------------------------------------------------------------*/
#if ALLOC_STATS
#  define ALLOC_STAT( x ) (x)
#else
#  define ALLOC_STAT( x )
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

#  define alloc_spin_init( x, attr ) pthread_spin_init( x, attr )
#  define alloc_spin_lock( x ) pthread_spin_lock( x )
#  define alloc_spin_unlock( x ) pthread_spin_unlock( x )
#else
#  define alloc_spin_init( x, attr ) 
#  define alloc_spin_lock( x ) 
#  define alloc_spin_unlock( x ) 
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
   void (*fill_buffer)( struct apool *pool, void *arg );
   obj_t *buffer;
   const uint32_t size;
   uint32_t idx;
   apayload_t payload;
} apool_t;

/*---------------------------------------------------------------------*/
/*    buffer fillers ...                                               */
/*---------------------------------------------------------------------*/
static void jsobject_fill_buffer( apool_t *pool, void *arg );
static void jsproxy_fill_buffer( apool_t *pool, void *arg );
static void jsfunction_fill_buffer( apool_t *pool, void *arg );
static void jsmethod_fill_buffer( apool_t *pool, void *arg );
static void jsprocedure_fill_buffer( apool_t *pool, void *arg );
static void jsstringliteralascii_fill_buffer( apool_t *pool, void *arg );

/*---------------------------------------------------------------------*/
/*    alloc pools                                                      */
/*---------------------------------------------------------------------*/
static int pool_queue_idx = 0;
static int pool_queue_len = 0;
static apool_t **pool_queue = 0;;

static pthread_mutex_t alloc_pool_mutex;
static pthread_cond_t alloc_pool_cond;

#define APOOL_JSOBJECT_INIT( sz ) { \
   .fill_buffer = &jsobject_fill_buffer, \
   .idx = POOLSZ( sz ), \
   .size = POOLSZ( sz ), \
   .payload = { .objsize = sz } \
};

#define APOOL_JSPROXY_INIT() { \
   .fill_buffer = &jsproxy_fill_buffer,	\
   .idx = JSPROXY_POOLSZ, \
   .size = JSPROXY_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSFUNCTION_INIT() { \
   .fill_buffer = &jsfunction_fill_buffer, \
   .idx = JSFUNCTION_POOLSZ, \
   .size = JSFUNCTION_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSMETHOD_INIT() { \
   .fill_buffer = &jsmethod_fill_buffer, \
   .idx = JSMETHOD_POOLSZ, \
   .size = JSMETHOD_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSPROCEDURE_INIT() { \
   .fill_buffer = &jsprocedure_fill_buffer, \
   .idx = JSPROCEDURE_POOLSZ, \
   .size = JSPROCEDURE_POOLSZ, \
   .payload = { .dummy = 0 } \
};

#define APOOL_JSSTRINGLITERALASCII_INIT() { \
   .fill_buffer = &jsstringliteralascii_fill_buffer, \
   .idx = JSSTRINGLITERALASCII_POOLSZ, \
   .size = JSSTRINGLITERALASCII_POOLSZ, \
   .payload = { .dummy = 0 } \
};

static HOP_ALLOC_THREAD_DECL apool_t pool1 = APOOL_JSOBJECT_INIT( 1 );
static HOP_ALLOC_THREAD_DECL apool_t npool1 = APOOL_JSOBJECT_INIT( 1 );

static HOP_ALLOC_THREAD_DECL apool_t pool2 = APOOL_JSOBJECT_INIT( 2 );
static HOP_ALLOC_THREAD_DECL apool_t npool2 = APOOL_JSOBJECT_INIT( 2 );

static HOP_ALLOC_THREAD_DECL apool_t pool3 = APOOL_JSOBJECT_INIT( 3 );
static HOP_ALLOC_THREAD_DECL apool_t npool3 = APOOL_JSOBJECT_INIT( 3 );

static HOP_ALLOC_THREAD_DECL apool_t pool4 = APOOL_JSOBJECT_INIT( 4 );
static HOP_ALLOC_THREAD_DECL apool_t npool4 = APOOL_JSOBJECT_INIT( 4 );

static HOP_ALLOC_THREAD_DECL apool_t pool5 = APOOL_JSOBJECT_INIT( 5 );
static HOP_ALLOC_THREAD_DECL apool_t npool5 = APOOL_JSOBJECT_INIT( 5 );

static HOP_ALLOC_THREAD_DECL apool_t pool6 = APOOL_JSOBJECT_INIT( 6 );
static HOP_ALLOC_THREAD_DECL apool_t npool6 = APOOL_JSOBJECT_INIT( 6 );

static HOP_ALLOC_THREAD_DECL apool_t pool7 = APOOL_JSOBJECT_INIT( 7 );
static HOP_ALLOC_THREAD_DECL apool_t npool7 = APOOL_JSOBJECT_INIT( 7 );

static HOP_ALLOC_THREAD_DECL apool_t pool8 = APOOL_JSOBJECT_INIT( 8 );
static HOP_ALLOC_THREAD_DECL apool_t npool8 = APOOL_JSOBJECT_INIT( 8 );

static HOP_ALLOC_THREAD_DECL apool_t poolproxy = APOOL_JSPROXY_INIT();
static HOP_ALLOC_THREAD_DECL apool_t npoolproxy = APOOL_JSPROXY_INIT();

static HOP_ALLOC_THREAD_DECL apool_t poolfunction = APOOL_JSFUNCTION_INIT();
static HOP_ALLOC_THREAD_DECL apool_t npoolfunction = APOOL_JSFUNCTION_INIT();

static HOP_ALLOC_THREAD_DECL apool_t poolmethod = APOOL_JSMETHOD_INIT();
static HOP_ALLOC_THREAD_DECL apool_t npoolmethod = APOOL_JSMETHOD_INIT();

static HOP_ALLOC_THREAD_DECL apool_t poolprocedure = APOOL_JSPROCEDURE_INIT();
static HOP_ALLOC_THREAD_DECL apool_t npoolprocedure = APOOL_JSPROCEDURE_INIT();

static HOP_ALLOC_THREAD_DECL apool_t poolstringliteralascii = APOOL_JSSTRINGLITERALASCII_INIT();
static HOP_ALLOC_THREAD_DECL apool_t npoolstringliteralascii = APOOL_JSSTRINGLITERALASCII_INIT();

int inl1 = 0, snd1 = 0, slow1 = 0, qsz1 = 0;
int inl2 = 0, snd2 = 0, slow2 = 0, qsz2 = 0;
int inl3 = 0, snd3 = 0, slow3 = 0, qsz3 = 0;
int inl4 = 0, snd4 = 0, slow4 = 0, qsz4 = 0;
int inl5 = 0, snd5 = 0, slow5 = 0, qsz5 = 0;
int inl6 = 0, snd6 = 0, slow6 = 0, qsz6 = 0;
int inl7 = 0, snd7 = 0, slow7 = 0, qsz7 = 0;
int inl8 = 0, snd8 = 0, slow8 = 0, qsz8 = 0;

int inlproxy = 0, sndproxy = 0, slowproxy = 0, qszproxy = 0;
int inlfunction = 0, sndfunction = 0, slowfunction = 0, qszfunction = 0;
int inlmethod = 0, sndmethod = 0, slowmethod = 0, qszmethod = 0;
int inlprocedure = 0, sndprocedure = 0, slowprocedure = 0, qszprocedure = 0;
int inlstringliteralascii = 0, sndstringliteralascii = 0, slowstringliteralascii = 0, qszstringliteralascii = 0;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    pool_queue_add ...                                               */
/*---------------------------------------------------------------------*/
static void
pool_queue_add( apool_t *pool ) {
   pthread_mutex_lock( &alloc_pool_mutex );
   
   if( pool_queue_idx == pool_queue_len ) {
      if( pool_queue_len == 0 ) {
	 pool_queue_len = 10;
	 pool_queue = malloc( pool_queue_len * sizeof( apool_t * ) );
      } else {
	 pool_queue_len *= 2;
	 pool_queue = realloc( pool_queue, pool_queue_len * sizeof( apool_t * ) );
      }
   }

   pool_queue[ pool_queue_idx++ ] = pool;
   pthread_cond_signal( &alloc_pool_cond );
   pthread_mutex_unlock( &alloc_pool_mutex );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsobject_buffer_fill ...                                         */
/*---------------------------------------------------------------------*/
static void
jsobject_fill_buffer( apool_t *pool, void *arg ) {
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;
   const uint32_t objsize = pool->payload.objsize;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] = bgl_make_jsobject_sans( objsize, 0L, 0L, (uint32_t)(long)arg );
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsproxy_buffer_fill ...                                          */
/*---------------------------------------------------------------------*/
static void
jsproxy_fill_buffer( apool_t *pool, void *arg ) {
#if HOP_ALLOC_JSPROXY_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] =
	 bgl_make_jsproxy_sans( 0L, 0L, 0L, 0L, 0L, (uint32_t)(long)arg );
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsfunction_buffer_fill ...                                       */
/*---------------------------------------------------------------------*/
static void
jsfunction_fill_buffer( apool_t *pool, void *arg ) {
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] =
	 bgl_make_jsfunction_sans( 0L, 0L, 0L, 0L, 0L );
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsmethod_buffer_fill ...                                         */
/*---------------------------------------------------------------------*/
static void
jsmethod_fill_buffer( apool_t *pool, void *arg ) {
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] =
	 bgl_make_jsmethod_sans( 0L, 0L, 0L, 0L, 0L, 0L );
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsprocedure_buffer_fill ...                                      */
/*---------------------------------------------------------------------*/
static void
jsprocedure_fill_buffer( apool_t *pool, void *arg ) {
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] =
	 bgl_make_jsprocedure_sans( 0L, 0L, 0L );
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsstringliteralascii_buffer_fill ...                             */
/*---------------------------------------------------------------------*/
static void
jsstringliteralascii_fill_buffer( apool_t *pool, void *arg ) {
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] =
	 bgl_make_jsstringliteralascii_sans( 0, 0L, 0L );
   }
#endif   
}
     
/*---------------------------------------------------------------------*/
/*    static void *                                                    */
/*    thread_alloc_worker ...                                          */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
static void *
thread_alloc_worker( void *arg ) {
   apool_t *pool;

   while( 1 ) {
      pthread_mutex_lock( &alloc_pool_mutex );
      while( pool_queue_idx == 0 ) {
	 pthread_cond_wait( &alloc_pool_cond, &alloc_pool_mutex );
      }
      
      pool = pool_queue[ --pool_queue_idx ];
      pthread_mutex_unlock( &alloc_pool_mutex );

      pool->fill_buffer( pool, arg );

      pool->idx = 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_jsalloc_locks ...                                       */
/*    -------------------------------------------------------------    */
/*    This function is split from BGL_INIT_JSALLOC to enable bmem      */
/*    lock initialization without spawning the worker threads.         */
/*---------------------------------------------------------------------*/
void
bgl_init_jsalloc_locks() {
   pthread_mutex_init( &alloc_pool_mutex, 0L );
   pthread_cond_init( &alloc_pool_cond, 0L );
   
   alloc_spin_init( &lock1, 0L );
   alloc_spin_init( &lock2, 0L );
   alloc_spin_init( &lock3, 0L );
   alloc_spin_init( &lock4, 0L );
   alloc_spin_init( &lock5, 0L );
   alloc_spin_init( &lock6, 0L );
   alloc_spin_init( &lock7, 0L );
   alloc_spin_init( &lock8, 0L );
   alloc_spin_init( &lockproxy, 0L );
   alloc_spin_init( &lockfunction, 0L );
   alloc_spin_init( &lockmethod, 0L );
   alloc_spin_init( &lockprocedure, 0L );
   alloc_spin_init( &lockstringliteralascii, 0L );

   empty_vector = create_vector_uncollectable( 0 );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc ...                                             */
/*    -------------------------------------------------------------    */
/*    Initialized the multithreaded background allocator.              */
/*---------------------------------------------------------------------*/
int bgl_init_jsalloc( uint32_t md ) {
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

   jsinit = 1;

   /* initializes the mutexes and condition variables */
   bgl_init_jsalloc_locks();
   
   /* start the allocator workers */
   for( i = 0; i < WORK_NUMBER; i++ ) {
      pthread_t th;
      pthread_attr_t thattr;
      pthread_attr_init( &thattr );
      pthread_attr_setdetachstate( &thattr, PTHREAD_CREATE_DETACHED );
      GC_pthread_create( &th, &thattr, thread_alloc_worker, (void *)(long)md );
   }

   return 0;
#endif   
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_proxy ...                                       */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_proxy( obj_t constrmap, obj_t elements ) {
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

   jsinit = 1;

   jsproxy_constrmap = constrmap;
   jsproxy_elements = elements;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_function ...                                    */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_function( BgL_jsconstructmapz00_bglt constrmap,
			   BgL_jsconstructmapz00_bglt cmap,
			   obj_t elements, obj_t alloc,
			   uint32_t mode ) {
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

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
bgl_init_jsalloc_method( BgL_jsconstructmapz00_bglt constrmap,
			   BgL_jsconstructmapz00_bglt cmap,
			   obj_t elements, obj_t alloc,
			   uint32_t mode ) {
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

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
bgl_init_jsalloc_procedure( BgL_jsconstructmapz00_bglt cmap,
			    uint32_t mode ) {
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

   jsinit = 1;

   jsprocedure_cmap = cmap;
   jsprocedure_mode = mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc_stringliteralascii ...                          */
/*---------------------------------------------------------------------*/
int
bgl_init_jsalloc_stringliteralascii( uint32_t mode, uint32_t normmode, obj_t not_a_string_cache, uint32_t threshold ) {
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

   jsinit = 1;

   jsstringliteralascii_mode = mode;
   jsstringliteralascii_normmode = normmode;
   jsstringliteralascii_not_a_string_cache = not_a_string_cache;
   jsstringliteralascii_normalize_threshold = threshold;
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

BGL_MAKE_JSOBJECT_SANS( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
   long bsize = JSOBJECT_SIZE + VECTOR_SIZE + ( (constrsize-1) * OBJ_SIZE );
   BgL_jsobjectz00_bglt o = (BgL_jsobjectz00_bglt)HOP_MALLOC( bsize );
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSOBJECT_CLASS_INDEX );
   
   // fields init
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)mode );
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), __proto__ );
   
   // elements initialization
   vector = (obj_t)(&(o->BgL_elementsz00) + 1);

#if( !defined( TAG_VECTOR ) )
   vector->vector.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
   vector->vector.length = constrsize;
   vector = BVECTOR( vector );
   
   o->BgL_elementsz00 = vector;

   for( i = 0; i < constrsize; i++ ) {
      VECTOR_SET( vector, i, BUNSPEC );
   }

#if( defined( HOP_PROFILE ) )
   {
      long i = ( constrsize >= VECTOR_LENGTH( bgl_js_profile_allocs ) - 2
		 ? VECTOR_LENGTH( bgl_js_profile_allocs ) -1
		 : constrsize );
      long cnt = BLLONG_TO_LLONG( VECTOR_REF( bgl_js_profile_allocs, i ) );
      VECTOR_SET( bgl_js_profile_allocs, i, LLONG_TO_BLLONG( cnt + 1 ) );
   }
#endif

   return BNANOBJECT( o );
}

/*---------------------------------------------------------------------*/
/*    BGL_MAKE_JSOBJECT ...                                            */
/*---------------------------------------------------------------------*/
#define BGL_MAKE_JSOBJECT( sz ) \
   static obj_t bgl_make_jsobject##sz( obj_t constrmap, obj_t __proto__, uint32_t md ) { \
   alloc_spin_lock( &lock##sz ); \
   if( pool##sz.idx < POOLSZ( sz ) ) { \
      obj_t o = pool##sz.buffer[ pool##sz.idx ]; \
      pool##sz.buffer[ pool##sz.idx++ ] = 0; \
      alloc_spin_unlock( &lock##sz ); \
      BGL_OBJECT_WIDENING_SET( o, __proto__ ); \
      ((BgL_jsobjectz00_bglt)(COBJECT( o )))->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap; \
      ALLOC_STAT( inl##sz++ ); \
      return o; \
   } else if( npool##sz.idx == 0 ) { \
      /* swap the two pools */ \
      obj_t *buffer = pool##sz.buffer; \
      obj_t o = npool##sz.buffer[ 0 ]; \
      \
      pool##sz.buffer = npool##sz.buffer; \
      pool##sz.buffer[ 0 ] = 0; \
      pool##sz.idx = 1; \
      \
      npool##sz.buffer = buffer; \
      npool##sz.idx = npool##sz.size; \
      \
      /* add the pool to the pool queue */ \
      pool_queue_add( &npool##sz ); \
      \
      BGL_OBJECT_WIDENING_SET( o, __proto__ ); \
      ((BgL_jsobjectz00_bglt)(COBJECT( o )))->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap; \
      \
      ALLOC_STAT( snd##sz++ ); \
      alloc_spin_unlock( &lock##sz ); \
      return o; \
   } else { \
      /* initialize the two alloc pools */ \
      if( !pool##sz.buffer ) { \
	 pool##sz.buffer = \
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*POOLSZ( sz ) ); \
	 pool##sz.idx = POOLSZ( sz ); \
      } \
      if( !npool##sz.buffer ) { \
	 npool##sz.buffer = \
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*POOLSZ( sz ) ); \
	 npool##sz.idx = POOLSZ( sz ); \
	 pool_queue_add( &npool##sz ); \
      } \
      \
      /* default slow alloc */ \
      ALLOC_STAT( slow##sz++ ); \
      ALLOC_STAT( pool_queue_idx > qsz##sz ? qsz##sz = pool_queue_idx : 0 ); \
      ALLOC_STAT( (slow##sz % ALLOC_STAT_FREQ == 0) ? fprintf( stderr, "sz=%d inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", sz, inl##sz, snd##sz, slow##sz, (long)(100*(double)slow##sz/(double)(inl##sz+snd##sz)), inl##sz + snd##sz + slow##sz, qsz##sz) : 0 ); \
      alloc_spin_unlock( &lock##sz ); \
      return bgl_make_jsobject_sans( sz, constrmap, __proto__, md ); \
   } \
}

#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
BGL_MAKE_JSOBJECT( 1 )
BGL_MAKE_JSOBJECT( 2 )
BGL_MAKE_JSOBJECT( 3 )
BGL_MAKE_JSOBJECT( 4 )
BGL_MAKE_JSOBJECT( 5 )
BGL_MAKE_JSOBJECT( 6 )
BGL_MAKE_JSOBJECT( 7 )
BGL_MAKE_JSOBJECT( 8 )
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject ...                                            */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSOBJECT_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
   obj_t o;

   switch( constrsize ) {
      case 1: return bgl_make_jsobject1( constrmap, __proto__, mode );
      case 2: return bgl_make_jsobject2( constrmap, __proto__, mode );
      case 3: return bgl_make_jsobject3( constrmap, __proto__, mode );
      case 4: return bgl_make_jsobject4( constrmap, __proto__, mode );
      case 5: return bgl_make_jsobject5( constrmap, __proto__, mode );
      case 6: return bgl_make_jsobject6( constrmap, __proto__, mode );
      case 7: return bgl_make_jsobject7( constrmap, __proto__, mode );
      case 8: return bgl_make_jsobject8( constrmap, __proto__, mode );
      default: return bgl_make_jsobject_sans( (int)constrsize, constrmap, __proto__, mode );
   }
}
#endif

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
   
BGL_MAKE_JSPROXY_SANS( obj_t target, obj_t handler,
		       obj_t getcache, obj_t setcache, obj_t applycache,
		       uint32_t mode ) {   
   long bsize = JSPROXY_SIZE;
   BgL_jsproxyz00_bglt o = (BgL_jsproxyz00_bglt)HOP_MALLOC( bsize );

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSPROXY_CLASS_INDEX );
   
   // fields init
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)mode );
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), target );
   
   o->BgL_handlerz00 = (struct BgL_jsobjectz00_bgl *)handler;
   o->BgL_getcachez00 = getcache;
   o->BgL_setcachez00 = setcache;
   o->BgL_applycachez00 = applycache;
   
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)jsproxy_constrmap;
   o->BgL_elementsz00 = jsproxy_elements;
   
   return BNANOBJECT( o );
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
bgl_make_jsproxy( obj_t target, obj_t handler,
		  obj_t getcache, obj_t setcache, obj_t applycache,
		  uint32_t md ) {
   alloc_spin_lock( &lockproxy ); 
   if( poolproxy.idx < JSPROXY_POOLSZ ) { 
      obj_t o = poolproxy.buffer[ poolproxy.idx ]; 
      poolproxy.buffer[ poolproxy.idx++ ] = 0; 
      alloc_spin_unlock( &lockproxy );
      
      BGL_OBJECT_WIDENING_SET( o, target );
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_handlerz00 = (BgL_jsobjectz00_bglt)handler; 
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_getcachez00 = getcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_setcachez00 = setcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_applycachez00 = applycache; 
      ALLOC_STAT( inlproxy++ );
      return o; 
   } else if( npoolproxy.idx == 0 ) { 
      /* swap the two pools */ 
      obj_t *buffer = poolproxy.buffer; 
      obj_t o = npoolproxy.buffer[ 0 ]; 
      
      poolproxy.buffer = npoolproxy.buffer; 
      poolproxy.buffer[ 0 ] = 0; 
      poolproxy.idx = 1; 
      
      npoolproxy.buffer = buffer; 
      npoolproxy.idx = npoolproxy.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add( &npoolproxy ); 
      
      BGL_OBJECT_WIDENING_SET( o, target );
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_handlerz00 = (BgL_jsobjectz00_bglt)handler; 
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_getcachez00 = getcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_setcachez00 = setcache; 
      ((BgL_jsproxyz00_bglt)(COBJECT( o )))->BgL_applycachez00 = applycache; 
      
      ALLOC_STAT( sndproxy++ ); 
      alloc_spin_unlock( &lockproxy ); 
      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if( !poolproxy.buffer ) { 
	 poolproxy.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSPROXY_POOLSZ ); 
	 poolproxy.idx = JSPROXY_POOLSZ; 
      } 
      if( !npoolproxy.buffer ) { 
	 npoolproxy.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSPROXY_POOLSZ ); 
	 npoolproxy.idx = JSPROXY_POOLSZ; 
	 pool_queue_add( &npoolproxy ); 
      } 
      
      /* default slow alloc */ 
      ALLOC_STAT( slowproxy++ ); 
      ALLOC_STAT( pool_queue_idx > qszproxy ? qszproxy = pool_queue_idx : 0 ); 
      ALLOC_STAT( (slowproxy % ALLOC_STAT_FREQ == 0) ? fprintf( stderr, "inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", inlproxy, sndproxy, slowproxy, (long)(100*(double)slowproxy/(double)(inlproxy+sndproxy)), inlproxy + sndproxy + slowproxy, qszproxy) : 0 ); 
      alloc_spin_unlock( &lockproxy ); 
      return bgl_make_jsproxy_sans( target, handler,
				    getcache, setcache, applycache, md ); 
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
/*         (elements proxy-elements))                                  */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSFUNCTION_SANS static obj_t bgl_make_jsfunction_sans
#else
#   define BGL_MAKE_JSFUNCTION_SANS obj_t bgl_make_jsfunction
#endif

BGL_MAKE_JSFUNCTION_SANS( obj_t procedure,
			  long arity, long constrsize,
			  obj_t __proto__, obj_t info ) {
   BgL_jsfunctionz00_bglt o = (BgL_jsfunctionz00_bglt)HOP_MALLOC( JSFUNCTION_SIZE );

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSFUNCTION_CLASS_INDEX );

   // immutable fields init
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)jsfunction_mode );
   o->BgL_allocz00 = jsfunction_alloc;
   o->BgL_constrmapz00 = jsfunction_constrmap;
   o->BgL_elementsz00 = jsfunction_elements;
   o->BgL_cmapz00 = jsfunction_cmap;
   o->BgL_prototypez00 = BCHAR( 'F' );

   // mutable fields
#if HOP_ALLOC_JSFUNCTION_POLICY != HOP_ALLOC_CLASSIC
   if( procedure )
#endif
   {
      BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), __proto__ );
      o->BgL_procedurez00 = procedure;
      o->BgL_arityz00 = arity;
      o->BgL_infoz00 = info;
      o->BgL_constrsiza7eza7 = constrsize;
   }
   
#if( defined( HOP_PROFILE ) )
   {
      long i = ( constrsize >= VECTOR_LENGTH( bgl_js_profile_allocs ) - 2
		 ? VECTOR_LENGTH( bgl_js_profile_allocs ) -1
		 : constrsize );
      long cnt = BLLONG_TO_LLONG( VECTOR_REF( bgl_js_profile_allocs, i ) );
      VECTOR_SET( bgl_js_profile_allocs, i, LLONG_TO_BLLONG( cnt + 1 ) );
   }
#endif

   return BNANOBJECT( o );
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
bgl_make_jsfunction( obj_t procedure,
		     long arity, long constrsize,
		     obj_t __proto__, obj_t info ) {
   alloc_spin_lock( &lockfunction );

   if( poolfunction.idx < JSFUNCTION_POOLSZ ) { 
      obj_t o = poolfunction.buffer[ poolfunction.idx ]; 
      poolfunction.buffer[ poolfunction.idx++ ] = 0; 
      alloc_spin_unlock( &lockfunction );
      
      BGL_OBJECT_WIDENING_SET( o, __proto__ );
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_procedurez00 = procedure;
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_arityz00 = arity;
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_infoz00 = info;
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT( inlfunction++ ); 
      return o; 
   } else if( npoolfunction.idx == 0 ) { 
      /* swap the two pools */ 
      obj_t *buffer = poolfunction.buffer; 
      obj_t o = npoolfunction.buffer[ 0 ]; 
      
      poolfunction.buffer = npoolfunction.buffer; 
      poolfunction.buffer[ 0 ] = 0; 
      poolfunction.idx = 1; 
      
      npoolfunction.buffer = buffer; 
      npoolfunction.idx = npoolfunction.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add( &npoolfunction ); 
      
      BGL_OBJECT_WIDENING_SET( o, __proto__ );
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_procedurez00 = procedure;
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_arityz00 = arity;
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_infoz00 = info;
      ((BgL_jsfunctionz00_bglt)(COBJECT( o )))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT( sndfunction++ ); 
      alloc_spin_unlock( &lockfunction );

      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if( !poolfunction.buffer ) { 
	 poolfunction.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSFUNCTION_POOLSZ ); 
	 poolfunction.idx = JSFUNCTION_POOLSZ; 
      } 
      if( !npoolfunction.buffer ) { 
	 npoolfunction.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSFUNCTION_POOLSZ ); 
	 npoolfunction.idx = JSFUNCTION_POOLSZ; 
	 pool_queue_add( &npoolfunction ); 
      } 

      /* default slow alloc */ 
      ALLOC_STAT( slowfunction++ ); 
      ALLOC_STAT( pool_queue_idx > qszfunction ? qszfunction = pool_queue_idx : 0 ); 
      ALLOC_STAT( (slowfunction % ALLOC_STAT_FREQ == 0) ? fprintf( stderr, "inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", inlfunction, sndfunction, slowfunction, (long)(100*(double)slowfunction/(double)(inlfunction+sndfunction)), inlfunction + sndfunction + slowfunction, qszfunction) : 0 ); 
      alloc_spin_unlock( &lockfunction ); 
      return bgl_make_jsfunction_sans( procedure,
				       arity, constrsize,
				       __proto__, info );
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
/*         (elements proxy-elements))                                  */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSMETHOD_SANS static obj_t bgl_make_jsmethod_sans
#else
#   define BGL_MAKE_JSMETHOD_SANS obj_t bgl_make_jsmethod
#endif

BGL_MAKE_JSMETHOD_SANS( obj_t procedure, obj_t method,
			  long arity, long constrsize,
			  obj_t __proto__, obj_t info ) {
   BgL_jsmethodz00_bglt o = (BgL_jsmethodz00_bglt)HOP_MALLOC( JSMETHOD_SIZE );

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSMETHOD_CLASS_INDEX );

   // immutable fields init
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)jsmethod_mode );
   o->BgL_allocz00 = jsmethod_alloc;
   o->BgL_constrmapz00 = jsmethod_constrmap;
   o->BgL_elementsz00 = jsmethod_elements;
   o->BgL_cmapz00 = jsmethod_cmap;
   o->BgL_prototypez00 = BCHAR( 'F' );

   // mutable fields
#if HOP_ALLOC_JSMETHOD_POLICY != HOP_ALLOC_CLASSIC
   if( procedure )
#endif
   {
      BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), __proto__ );
      o->BgL_procedurez00 = procedure;
      o->BgL_methodz00 = method;
      o->BgL_arityz00 = arity;
      o->BgL_infoz00 = info;
      o->BgL_constrsiza7eza7 = constrsize;
   }
   
#if( defined( HOP_PROFILE ) )
   {
      long i = ( constrsize >= VECTOR_LENGTH( bgl_js_profile_allocs ) - 2
		 ? VECTOR_LENGTH( bgl_js_profile_allocs ) -1
		 : constrsize );
      long cnt = BLLONG_TO_LLONG( VECTOR_REF( bgl_js_profile_allocs, i ) );
      VECTOR_SET( bgl_js_profile_allocs, i, LLONG_TO_BLLONG( cnt + 1 ) );
   }
#endif

   return BNANOBJECT( o );
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
bgl_make_jsmethod( obj_t procedure, obj_t method,
		     long arity, long constrsize,
		     obj_t __proto__, obj_t info ) {
   alloc_spin_lock( &lockmethod );

   if( poolmethod.idx < JSMETHOD_POOLSZ ) { 
      obj_t o = poolmethod.buffer[ poolmethod.idx ]; 
      poolmethod.buffer[ poolmethod.idx++ ] = 0; 
      alloc_spin_unlock( &lockmethod );
      
      BGL_OBJECT_WIDENING_SET( o, __proto__ );
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_procedurez00 = procedure;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_methodz00 = method;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_arityz00 = arity;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_infoz00 = info;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT( inlmethod++ ); 
      return o; 
   } else if( npoolmethod.idx == 0 ) { 
      /* swap the two pools */ 
      obj_t *buffer = poolmethod.buffer; 
      obj_t o = npoolmethod.buffer[ 0 ]; 
      
      poolmethod.buffer = npoolmethod.buffer; 
      poolmethod.buffer[ 0 ] = 0; 
      poolmethod.idx = 1; 
      
      npoolmethod.buffer = buffer; 
      npoolmethod.idx = npoolmethod.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add( &npoolmethod ); 
      
      BGL_OBJECT_WIDENING_SET( o, __proto__ );
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_procedurez00 = procedure;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_methodz00 = method;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_arityz00 = arity;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_infoz00 = info;
      ((BgL_jsmethodz00_bglt)(COBJECT( o )))->BgL_constrsiza7eza7 = constrsize;
      
      ALLOC_STAT( sndmethod++ ); 
      alloc_spin_unlock( &lockmethod );

      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if( !poolmethod.buffer ) { 
	 poolmethod.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSMETHOD_POOLSZ ); 
	 poolmethod.idx = JSMETHOD_POOLSZ; 
      } 
      if( !npoolmethod.buffer ) { 
	 npoolmethod.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSMETHOD_POOLSZ ); 
	 npoolmethod.idx = JSMETHOD_POOLSZ; 
	 pool_queue_add( &npoolmethod ); 
      } 

      /* default slow alloc */ 
      ALLOC_STAT( slowmethod++ ); 
      ALLOC_STAT( pool_queue_idx > qszmethod ? qszmethod = pool_queue_idx : 0 ); 
      ALLOC_STAT( (slowmethod % ALLOC_STAT_FREQ == 0) ? fprintf( stderr, "inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", inlmethod, sndmethod, slowmethod, (long)(100*(double)slowmethod/(double)(inlmethod+sndmethod)), inlmethod + sndmethod + slowmethod, qszmethod) : 0 ); 
      alloc_spin_unlock( &lockmethod ); 
      return bgl_make_jsmethod_sans( procedure, method, 
				       arity, constrsize,
				       __proto__, info );
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

BGL_MAKE_JSPROCEDURE_SANS( obj_t procedure, long arity, obj_t __proto__ ) {
   BgL_jsprocedurez00_bglt o = (BgL_jsprocedurez00_bglt)HOP_MALLOC( JSPROCEDURE_SIZE );

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSPROCEDURE_CLASS_INDEX );

   // immutable fields init
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), __proto__ );
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)jsprocedure_mode );
   o->BgL_cmapz00 = jsfunction_cmap;
   o->BgL_elementsz00 = empty_vector;
      
   // mutable fields init
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
   if( procedure )
#endif
   {
      o->BgL_procedurez00 = procedure;
      o->BgL_arityz00 = arity;
      o->BgL_cmapz00 = jsprocedure_cmap;
   }

   return BNANOBJECT( o );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsprocedure ...                                         */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSPROCEDURE_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsprocedure( obj_t procedure, long arity, obj_t __proto__ ) {
   alloc_spin_lock( &lockprocedure );

   if( poolprocedure.idx < JSPROCEDURE_POOLSZ ) { 
      obj_t o = poolprocedure.buffer[ poolprocedure.idx ]; 
      poolprocedure.buffer[ poolprocedure.idx++ ] = 0; 
      alloc_spin_unlock( &lockprocedure );
      
      BGL_OBJECT_WIDENING_SET( o, __proto__ );
      ((BgL_jsprocedurez00_bglt)(COBJECT( o )))->BgL_procedurez00 = procedure;
      ((BgL_jsprocedurez00_bglt)(COBJECT( o )))->BgL_arityz00 = arity;
      
      ALLOC_STAT( inlprocedure++ ); 
      return o; 
   } else if( npoolprocedure.idx == 0 ) { 
      /* swap the two pools */ 
      obj_t *buffer = poolprocedure.buffer; 
      obj_t o = npoolprocedure.buffer[ 0 ]; 
      
      poolprocedure.buffer = npoolprocedure.buffer; 
      poolprocedure.buffer[ 0 ] = 0; 
      poolprocedure.idx = 1; 
      
      npoolprocedure.buffer = buffer; 
      npoolprocedure.idx = npoolprocedure.size; 
      
      /* add the pool to the pool queue */ 
      pool_queue_add( &npoolprocedure ); 
      
      BGL_OBJECT_WIDENING_SET( o, __proto__ );
      ((BgL_jsprocedurez00_bglt)(COBJECT( o )))->BgL_procedurez00 = procedure;
      ((BgL_jsprocedurez00_bglt)(COBJECT( o )))->BgL_arityz00 = arity;
      
      ALLOC_STAT( sndprocedure++ ); 
      alloc_spin_unlock( &lockprocedure );

      return o; 
   } else { 
      /* initialize the two alloc pools */ 
      if( !poolprocedure.buffer ) { 
	 poolprocedure.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSPROCEDURE_POOLSZ ); 
	 poolprocedure.idx = JSPROCEDURE_POOLSZ; 
      } 
      if( !npoolprocedure.buffer ) { 
	 npoolprocedure.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSPROCEDURE_POOLSZ ); 
	 npoolprocedure.idx = JSPROCEDURE_POOLSZ; 
	 pool_queue_add( &npoolprocedure ); 
      } 

      /* default slow alloc */ 
      ALLOC_STAT( slowprocedure++ ); 
      ALLOC_STAT( pool_queue_idx > qszprocedure ? qszprocedure = pool_queue_idx : 0 ); 
      ALLOC_STAT( (slowprocedure % ALLOC_STAT_FREQ == 0) ? fprintf( stderr, "inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", inlprocedure, sndprocedure, slowprocedure, (long)(100*(double)slowprocedure/(double)(inlprocedure+sndprocedure)), inlprocedure + sndprocedure + slowprocedure, qszprocedure) : 0 ); 
      alloc_spin_unlock( &lockprocedure ); 
      return bgl_make_jsprocedure_sans( procedure, arity, __proto__ );
   } 
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsstringliteralascii_sans ...                           */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
#   define BGL_MAKE_JSSTRINGLITERALASCII_SANS static obj_t bgl_make_jsstringliteralascii_sans
#else
#   define BGL_MAKE_JSSTRINGLITERALASCII_SANS obj_t bgl_make_jsstringliteralascii
#endif

BGL_MAKE_JSSTRINGLITERALASCII_SANS( uint32_t len, obj_t left, obj_t right ) {
   BgL_jsstringliteralasciiz00_bglt o = (BgL_jsstringliteralasciiz00_bglt)HOP_MALLOC( JSSTRINGLITERALASCII_SIZE );

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSSTRINGLITERALASCII_CLASS_INDEX );

   // field init
   o->BgL_lengthz00 = len;
   o->BgL_leftz00 = left;
   o->BgL_rightz00 = right;
   
   // immutable fields init
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), BFALSE );
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)jsstringliteralascii_mode );

   return BNANOBJECT( o );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsstringliteralascii ...                                */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_JSSTRINGLITERALASCII_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsstringliteralascii( uint32_t len, obj_t left, obj_t right ) {
   alloc_spin_lock( &lockstringliteralascii );

   if( poolstringliteralascii.idx < JSSTRINGLITERALASCII_POOLSZ ) {
      obj_t o = poolstringliteralascii.buffer[ poolstringliteralascii.idx ];
      poolstringliteralascii.buffer[ poolstringliteralascii.idx++ ] = 0;
      alloc_spin_unlock( &lockstringliteralascii );

      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_lengthz00 = len; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_leftz00 = left; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_rightz00 = right;

      ALLOC_STAT( inlstringliteralascii++ );
      return o;
   } else if( npoolstringliteralascii.idx == 0 ) {
      /* swap the two pools */
      obj_t *buffer = poolstringliteralascii.buffer;
      obj_t o = npoolstringliteralascii.buffer[ 0 ];

      poolstringliteralascii.buffer = npoolstringliteralascii.buffer;
      poolstringliteralascii.buffer[ 0 ] = 0;
      poolstringliteralascii.idx = 1;

      npoolstringliteralascii.buffer = buffer;
      npoolstringliteralascii.idx = npoolstringliteralascii.size;

      /* add the pool to the pool queue */
      pool_queue_add( &npoolstringliteralascii );

      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_lengthz00 = len; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_leftz00 = left; 
      ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_rightz00 = right;
      
      ALLOC_STAT( sndstringliteralascii++ );
      alloc_spin_unlock( &lockstringliteralascii );

      return o;
   } else {
      /* initialize the two alloc pools */
      if( !poolstringliteralascii.buffer ) {
	 poolstringliteralascii.buffer =
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSSTRINGLITERALASCII_POOLSZ );
	 poolstringliteralascii.idx = JSSTRINGLITERALASCII_POOLSZ;
      }
      if( !npoolstringliteralascii.buffer ) {
	 npoolstringliteralascii.buffer =
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*JSSTRINGLITERALASCII_POOLSZ );
	 npoolstringliteralascii.idx = JSSTRINGLITERALASCII_POOLSZ;
	 pool_queue_add( &npoolstringliteralascii );
      }

      /* default slow alloc */
      ALLOC_STAT( slowstringliteralascii++ );
      ALLOC_STAT( pool_queue_idx > qszstringliteralascii ? qszstringliteralascii = pool_queue_idx : 0 );
      ALLOC_STAT( (slowstringliteralascii % ALLOC_STAT_FREQ == 0) ? fprintf( stderr, "inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", inlstringliteralascii, sndstringliteralascii, slowstringliteralascii, (long)(100*(double)slowstringliteralascii/(double)(inlstringliteralascii+sndstringliteralascii)), inlstringliteralascii + sndstringliteralascii + slowstringliteralascii, qszstringliteralascii) : 0 );
      alloc_spin_unlock( &lockstringliteralascii );
      return bgl_make_jsstringliteralascii_sans( len, left, right );
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
bgl_jsstring_append_ascii( obj_t left, obj_t right ) {
   uint32_t len = ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( left )))->BgL_lengthz00
      + ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( right )))->BgL_lengthz00;

   alloc_spin_lock( &lockstringliteralascii );
   
   if( len < jsstringliteralascii_normalize_threshold ) {
      obj_t nleft = string_append(
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( left )))->BgL_leftz00,
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( right )))->BgL_leftz00 );
      obj_t nright = jsstringliteralascii_not_a_string_cache;

      if( poolstringliteralascii.idx < JSSTRINGLITERALASCII_POOLSZ ) {
	 obj_t o = poolstringliteralascii.buffer[ poolstringliteralascii.idx ];
	 poolstringliteralascii.buffer[ poolstringliteralascii.idx++ ] = 0;
	 alloc_spin_unlock( &lockstringliteralascii );

	 BGL_OBJECT_HEADER_SIZE_SET( o, (long)jsstringliteralascii_normmode );
	 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_lengthz00 = len; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_leftz00 = nleft; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_rightz00 = nright;

	 ALLOC_STAT( inlstringliteralascii++ );
	 return o;
      } else {
	 alloc_spin_unlock( &lockstringliteralascii );

	 {
	    obj_t o = bgl_make_jsstringliteralascii( len, nleft, nright );
	    BGL_OBJECT_HEADER_SIZE_SET( o, (long)jsstringliteralascii_normmode );
	    return o;
	 }
      }
   } else {
      if( poolstringliteralascii.idx < JSSTRINGLITERALASCII_POOLSZ ) {
	 obj_t o = poolstringliteralascii.buffer[ poolstringliteralascii.idx ];
	 poolstringliteralascii.buffer[ poolstringliteralascii.idx++ ] = 0;
	 alloc_spin_unlock( &lockstringliteralascii );

	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_lengthz00 = len; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_leftz00 = left; 
	 ((BgL_jsstringliteralasciiz00_bglt)(COBJECT( o )))->BgL_rightz00 = right;

	 ALLOC_STAT( inlstringliteralascii++ );
	 return o;
      } else {
	 alloc_spin_unlock( &lockstringliteralascii );

	 {
	    return bgl_make_jsstringliteralascii( len, left, right );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    empty_vector ...                                                 */
/*---------------------------------------------------------------------*/
#if( !defined( TAG_VECTOR ) )
static struct {
   __CNST_ALIGN header_t header;
   long length;
} _empty_vector = { __CNST_FILLER MAKE_HEADER( VECTOR_TYPE, 0 ), 0 };
static obj_t empty_vector = BVECTOR( &(_empty_vector.header ) );
#else   
static struct {
   __CNST_ALIGN long length;
} _empty_vector = { __CNST_FILLER 0 };
static obj_t empty_vector = BVECTOR( &(_empty_vector.length ) );
#endif   
   
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
bgl_make_jsarray_sans_init( long size, uint32_t len, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
   long bsize = JSARRAY_SIZE + VECTOR_SIZE + ( (size-1) * OBJ_SIZE );
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)HOP_MALLOC( bsize );
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSARRAY_CLASS_INDEX );
   
   // fields init
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   o->BgL_elementsz00 = empty_vector;
   o->BgL_lengthz00 = len;
   o->BgL_ilenz00 = 0;
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)mode );
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), __proto__ );
  
   // vector initialization
   vector = (obj_t)(&(o->BgL_vecz00) + 1);

#if( !defined( TAG_VECTOR ) )
   vector->vector.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
   vector->vector.length = size;
   vector = BVECTOR( vector );
   
   o->BgL_vecz00 = vector;

   return BNANOBJECT( o );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsarray ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsarray( long size, uint32_t len, obj_t constrmap, obj_t __proto__, obj_t absent, uint32_t mode ) {
   obj_t array = bgl_make_jsarray_sans_init( size, len, constrmap, __proto__, mode );
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)COBJECT( array );
   obj_t vector = o->BgL_vecz00;
   int i;

   for( i = 0; i < size; i++ ) {
      VECTOR_SET( vector, i, absent );
   }

   return array;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_jsarray_shift_builtin ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_jsarray_shift_builtin( obj_t array ) {
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)COBJECT( array );
   obj_t vec = CVECTOR( o->BgL_vecz00 );
   obj_t res = VECTOR_REF( BVECTOR( vec ), 0 );
   long size = VECTOR_LENGTH( BVECTOR( vec ) );
   obj_t nvec = (obj_t)(((obj_t *)vec) + 1);

#if( !defined( TAG_VECTOR ) )
   nvec->vector.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
   nvec->vector.length = size - 1;
   nvec = BVECTOR( nvec );
   
   *(&(o->BgL_vecz00) + 1) = nvec;
   o->BgL_vecz00 = nvec;

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_init_vector ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_init_vector( obj_t vector, long len, obj_t init ) {
#if(  VECTOR_SIZE_TAG_NB_BIT != 0 )
   if( len & ~(VECTOR_LENGTH_MASK) ) {
      C_FAILURE( "create_vector", "vector too large", BINT( len ) );
      return BUNSPEC;
   } else
#endif
   {
#if( !defined( TAG_VECTOR ) )
      vector->vector.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif
      vector->vector.length = len;

      bgl_fill_vector( BVECTOR( vector ), 0, len, init );
      return BVECTOR( vector );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_init_vector_sans_fill ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_init_vector_sans_fill( obj_t vector, long len ) {
#if(  VECTOR_SIZE_TAG_NB_BIT != 0 )
   if( len & ~(VECTOR_LENGTH_MASK) ) {
      C_FAILURE( "create_vector", "vector too large", BINT( len ) );
      return BUNSPEC;
   } else
#endif
   {
#if( !defined( TAG_VECTOR ) )
      vector->vector.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif
      vector->vector.length = len;

      return BVECTOR( vector );
   }
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_vector_bytesize ...                                          */
/*---------------------------------------------------------------------*/
long
bgl_vector_bytesize( long len ) {
   return VECTOR_SIZE + ( (len-1) * OBJ_SIZE );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_pcache_table ...                                        */
/*    -------------------------------------------------------------    */
/*    Create a fake Bigloo vector whose elements are inlined pcache    */
/*    entries.                                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_pcache_table( obj_t obj, int len, obj_t src, obj_t thread, obj_t template ) {
   pcache_t *pcache = (pcache_t *)obj;
   int i;

   for( i = 0; i < len; i++ ) {
      memcpy( &(pcache[ i ]), COBJECT( template ), sizeof( pcache_t ) );
   }

   bgl_profile_pcache_tables = MAKE_PAIR( MAKE_PAIR( (obj_t)pcache, BINT( len ) ), bgl_profile_pcache_tables );

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

   while( PAIRP( tables ) ) {
      obj_t table = CAR( tables );
      long i = CINT( CDR( table ) );
      pcache_t *pcache = (pcache_t *)CAR( table );

      while( --i >= 0 ) {
	 res = MAKE_PAIR( BNANOBJECT( &(pcache[ i ]) ), res );
      }

      tables = CDR( tables );
   }
   return res;
}

