/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/_bglhopscript.new.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Wed Feb 19 12:21:10 2020 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
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

#define JSOBJECT_SIZE \
   sizeof( struct BgL_jsobjectz00_bgl )
#define JSOBJECT_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsObjectz00zz__hopscript_typesz00 )

#define JSPROXY_SIZE \
   sizeof( struct BgL_jsproxyz00_bgl )
#define JSPROXY_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsProxyz00zz__hopscript_typesz00 )

#define JSARRAY_SIZE \
   sizeof( struct BgL_jsarrayz00_bgl )
#define JSARRAY_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsArrayz00zz__hopscript_typesz00 )

extern obj_t bgl_js_profile_allocs;
obj_t bgl_profile_pcache_tables = BNIL;
extern int GC_pthread_create();

static obj_t jsproxy_constrmap, jsproxy_elements;

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

#if BGL_HAS_THREAD_LOCALSTORAGE == 1
#  define HOP_ALLOC_POLICY HOP_ALLOC_TLS
#elif BGL_HAVE_SPINLOCK
#  define HOP_ALLOC_POLICY HOP_ALLOC_SPINLOCK
#else
#  define HOP_ALLOC_POLICY HOP_ALLOC_CLASSIC
#endif

/* #undef HOP_ALLOC_POLICY                                             */
/* #define HOP_ALLOC_POLICY HOP_ALLOC_CLASSIC                          */

extern obj_t bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode );

#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
static obj_t bgl_make_jsobject_sans( int constrsize, obj_t constrmap,
				     obj_t __proto__, uint32_t mode );
static obj_t bgl_make_jsproxy_sans( // obj_t constrmap, obj_t elements,
				    obj_t target, obj_t handler,
				    obj_t gcache, obj_t scache, obj_t acache,
				    uint32_t mode );
#endif

#define POOL_SIZE( sz ) (12800 >> sz)
#define PROXY_POOL_SIZE POOL_SIZE( 3 )
#define WORK_NUMBER 1
#define ALLOC_STATS 0  

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
static pthread_spinlock_t lock1, lock2, lock3, lock4, lock5, lock6;
static pthread_spinlock_t lockproxy;

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
   .idx = POOL_SIZE( sz ), \
   .size = POOL_SIZE( sz ), \
   .payload = { .objsize = sz } \
};

#define APOOL_JSPROXY_INIT() { \
   .fill_buffer = &jsproxy_fill_buffer,	\
   .idx = PROXY_POOL_SIZE, \
   .size = PROXY_POOL_SIZE, \
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

static HOP_ALLOC_THREAD_DECL apool_t poolproxy = APOOL_JSPROXY_INIT();
static HOP_ALLOC_THREAD_DECL apool_t npoolproxy = APOOL_JSPROXY_INIT();

int inl1 = 0, snd1 = 0, slow1 = 0, qsz1 = 0;
int inl2 = 0, snd2 = 0, slow2 = 0, qsz2 = 0;
int inl3 = 0, snd3 = 0, slow3 = 0, qsz3 = 0;
int inl4 = 0, snd4 = 0, slow4 = 0, qsz4 = 0;
int inl5 = 0, snd5 = 0, slow5 = 0, qsz5 = 0;
int inl6 = 0, snd6 = 0, slow6 = 0, qsz6 = 0;

int inlprox = 0, sndproxy = 0, slowproxy = 0, qszproxy = 0;

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
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;
   const uint32_t objsize = pool->payload.objsize;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] = bgl_make_jsobject_sans( objsize, 0L, 0L, (uint32_t)(long)arg );
   }
}
     
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    jsproxy_buffer_fill ...                                          */
/*---------------------------------------------------------------------*/
static void
jsproxy_fill_buffer( apool_t *pool, void *arg ) {
   int i;
   obj_t *buffer = pool->buffer;
   const uint32_t size = pool->size;

   for( i = 0; i < size; i++ ) {
      buffer[ i ] =
	 bgl_make_jsproxy_sans( //jsproxy_constrmap, jsproxy_elements,
				0L, 0L, 0L, 0L, 0L, (uint32_t)(long)arg );
   }
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
static void
bgl_init_jsalloc_locks() {
   pthread_mutex_init( &alloc_pool_mutex, 0L );
   pthread_cond_init( &alloc_pool_cond, 0L );
   
   alloc_spin_init( &lock1, 0L );
   alloc_spin_init( &lock2, 0L );
   alloc_spin_init( &lock3, 0L );
   alloc_spin_init( &lock4, 0L );
   alloc_spin_init( &lock5, 0L );
   alloc_spin_init( &lock6, 0L );
   alloc_spin_init( &lockproxy, 0L );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc ...                                             */
/*    -------------------------------------------------------------    */
/*    Initialized the multithreaded background allocator.              */
/*---------------------------------------------------------------------*/
int bgl_init_jsalloc( uint32_t md ) {
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
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
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
   static int jsinit = 0;
   int i;

   if( jsinit ) return 1;

   jsinit = 1;

   jsproxy_constrmap = constrmap;
   jsproxy_elements = elements;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_MAKE_JSOBJECT ...                                            */
/*---------------------------------------------------------------------*/
#define BGL_MAKE_JSOBJECT( sz ) \
   static obj_t bgl_make_jsobject##sz( obj_t constrmap, obj_t __proto__, uint32_t md ) { \
   alloc_spin_lock( &lock##sz ); \
   if( pool##sz.idx < POOL_SIZE( sz ) ) { \
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
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*POOL_SIZE( sz ) ); \
	 pool##sz.idx = POOL_SIZE( sz ); \
      } \
      if( !npool##sz.buffer ) { \
	 npool##sz.buffer = \
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*POOL_SIZE( sz ) ); \
	 npool##sz.idx = POOL_SIZE( sz ); \
	 pool_queue_add( &npool##sz ); \
      } \
      \
      /* default slow alloc */ \
      ALLOC_STAT( slow##sz++ ); \
      ALLOC_STAT( pool_queue_idx > qsz##sz ? qsz##sz = pool_queue_idx : 0 ); \
      ALLOC_STAT( (slow##sz % 1000000 == 0) ? fprintf( stderr, "sz=%d inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", sz, inl##sz, snd##sz, slow##sz, (long)(100*(double)slow##sz/(double)(inl##sz+snd##sz)), inl##sz + snd##sz + slow##sz, qsz##sz) : 0 ); \
      alloc_spin_unlock( &lock##sz ); \
      return bgl_make_jsobject_sans( sz, constrmap, __proto__, md ); \
   } \
}

#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
BGL_MAKE_JSOBJECT( 1 )
BGL_MAKE_JSOBJECT( 2 )
BGL_MAKE_JSOBJECT( 3 )
BGL_MAKE_JSOBJECT( 4 )
BGL_MAKE_JSOBJECT( 5 )
BGL_MAKE_JSOBJECT( 6 )
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject ...                                            */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
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
      default: return bgl_make_jsobject_sans( constrsize, constrmap, __proto__, mode );
   }
}
#endif

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
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
static obj_t
bgl_make_jsobject_sans( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
#else   
obj_t
bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
#endif
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
#if HOP_ALLOC_POLICY == HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsproxy( // obj_t constrmap, obj_t elements,
		  obj_t target, obj_t handler,
		  obj_t getcache, obj_t setcache, obj_t applycache,
		  uint32_t mode ) {
#else   
static obj_t
   bgl_make_jsproxy_sans( // obj_t constrmap, obj_t elements,
		       obj_t target, obj_t handler,
		       obj_t getcache, obj_t setcache, obj_t applycache,
		       uint32_t mode ) {
#endif
   long bsize = JSPROXY_SIZE;
   BgL_jsproxyz00_bglt o = (BgL_jsproxyz00_bglt)HOP_MALLOC( bsize );
   int i;
   obj_t constrmap = jsproxy_constrmap;
   obj_t elements = jsproxy_elements;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BNANOBJECT( o ), JSPROXY_CLASS_INDEX );
   
   // fields init
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)mode );
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), target );
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   o->BgL_elementsz00 = elements;
   o->BgL_handlerz00 = (struct BgL_jsobjectz00_bgl *)handler;
   o->BgL_getcachez00 = getcache;
   o->BgL_setcachez00 = setcache;;
   o->BgL_applycachez00 = applycache;
   
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
/*    bgl_make_jsproxy ...                                             */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsProxy                                          */
/*         (__proto__ __proto__)                                       */
/*         (cmap constrmap)                                            */
/*         (elements proxy-elements))                                  */
/*---------------------------------------------------------------------*/
#if HOP_ALLOC_POLICY != HOP_ALLOC_CLASSIC
obj_t
bgl_make_jsproxy( // obj_t constrmap, obj_t elements,
		  obj_t target, obj_t handler,
		  obj_t getcache, obj_t setcache, obj_t applycache,
		  uint32_t md ) {
      alloc_spin_lock( &lockproxy ); 
   if( poolproxy.idx < PROXY_POOL_SIZE ) { 
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
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*PROXY_POOL_SIZE ); 
	 poolproxy.idx = PROXY_POOL_SIZE; 
      } 
      if( !npoolproxy.buffer ) { 
	 npoolproxy.buffer = 
	    (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof(obj_t)*PROXY_POOL_SIZE ); 
	 npoolproxy.idx = PROXY_POOL_SIZE; 
	 pool_queue_add( &npoolproxy ); 
      } 
      
      /* default slow alloc */ 
      ALLOC_STAT( slowproxy++ ); 
      ALLOC_STAT( pool_queue_idx > qszproxy ? qszproxy = pool_queue_idx : 0 ); 
      ALLOC_STAT( (slowproxy % 1000000 == 0) ? fprintf( stderr, "sz=%d inl=%d snd=%d slow=%d %d%% sum=%ld qsz=%d\n", sz, inlproxy, sndproxy, slowproxy, (long)(100*(double)slowproxy/(double)(inlproxy+sndproxy)), inlproxy + sndproxy + slowproxy, qszproxy) : 0 ); 
      alloc_spin_unlock( &lockproxy ); 
      return bgl_make_jsproxy_sans( // constrmap, elements,
				    target, handler,
				    getcache, setcache, applycache, md ); 
   } 
}
#endif

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

   //fprintf( stderr, "bgl_make_jsarray_sans size=%d\n", size );
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

   //fprintf( stderr, "bgl_make_jsarray size=%d\n", size );
   for( i = 0; i < size; i++ ) {
      VECTOR_SET( vector, i, absent );
   }

   return array;
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

