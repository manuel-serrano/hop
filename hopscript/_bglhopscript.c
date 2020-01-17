/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/_bglhopscript.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Fri Jan 17 13:26:40 2020 (serrano)                */
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
extern obj_t BGl_JsArrayz00zz__hopscript_typesz00;

#define JSOBJECT_SIZE \
   sizeof( struct BgL_jsobjectz00_bgl )
#define JSOBJECT_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsObjectz00zz__hopscript_typesz00 )

#define JSARRAY_SIZE \
   sizeof( struct BgL_jsarrayz00_bgl )
#define JSARRAY_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsArrayz00zz__hopscript_typesz00 )

extern obj_t bgl_js_profile_allocs;
obj_t bgl_profile_pcache_tables = BNIL;
extern int GC_pthread_create();

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef struct BgL_jspropertycachez00_bgl pcache_t;

/*---------------------------------------------------------------------*/
/*    thread alloc                                                     */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_SPINLOCK
#  define HOP_THREAD_ALLOC 1
#else
#  define HOP_THREAD_ALLOC 0
#endif

extern obj_t bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode );

#if HOP_THREAD_ALLOC
static obj_t bgl_make_jsobject_sans( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode );
#endif

#define BUCKET_SIZE( sz ) (12800 >> sz)
#define ALLOC_STATS 0  

#define PREALLOC_STATE_IDLE 0
#define PREALLOC_STATE_ALLOCATING 1
#define PREALLOC_STATE_DONE 2

#define declare( sz ) \
   pthread_mutex_t mutex##sz; \
   pthread_spinlock_t lock##sz; \
   pthread_cond_t cond##sz; \
   static obj_t *preallocs##sz, *nextallocs##sz; \
   static uint32_t allocidx##sz = 0; \
   static int alloc_state##sz = PREALLOC_STATE_IDLE; \
   static long nbprealloc##sz = 0, nballoc##sz = 0

#if HOP_THREAD_ALLOC
declare( 1 );
declare( 2 );
declare( 3 );
declare( 4 );
declare( 5 );
declare( 6 );
#endif

/*---------------------------------------------------------------------*/
/*    thread_alloc ...                                                 */
/*---------------------------------------------------------------------*/
#define thread_alloc( sz ) \
   void *thread_alloc##sz( void *arg ) { \
   pthread_mutex_lock( &mutex##sz ); \
   while( 1 ) { \
      uint32_t i; \
      pthread_cond_wait( &cond##sz, &mutex##sz ); \
      nbprealloc##sz++; \
      for( i = 0; i < BUCKET_SIZE( sz ); i++ ) { \
	 nextallocs##sz[ i ] = bgl_make_jsobject_sans( sz, 0L, 0L, (uint32_t)(long)arg ); \
      } \
      alloc_state##sz = PREALLOC_STATE_DONE; \
   } \
}

#if HOP_THREAD_ALLOC
thread_alloc( 1 )
thread_alloc( 2 )
thread_alloc( 3 )
thread_alloc( 4 )
thread_alloc( 5 )
thread_alloc( 6 )
#endif

/*---------------------------------------------------------------------*/
/*    prealloc ...                                                     */
/*---------------------------------------------------------------------*/
#define prealloc( sz ) \
   obj_t prealloc##sz( obj_t constrmap, obj_t proto, uint32_t md ) { \
      switch( alloc_state##sz ) { \
         case PREALLOC_STATE_IDLE: { \
      	    pthread_mutex_lock( &mutex##sz ); \
	    alloc_state##sz = PREALLOC_STATE_ALLOCATING; \
	    pthread_cond_signal( &cond##sz ); \
   	    pthread_mutex_unlock( &mutex##sz ); \
   	    return bgl_make_jsobject_sans( sz, constrmap, proto, md ); \
            } \
         case PREALLOC_STATE_ALLOCATING: \
   	    return bgl_make_jsobject_sans( sz, constrmap, proto, md ); \
         default: { \
	    obj_t tmp; \
      	    pthread_mutex_lock( &mutex##sz ); \
   	    obj_t *nallocs = preallocs##sz; \
   	    preallocs##sz = nextallocs##sz; \
   	    nextallocs##sz = nallocs; \
	    alloc_state##sz = PREALLOC_STATE_ALLOCATING; \
	    pthread_cond_signal( &cond##sz ); \
      	    pthread_mutex_unlock( &mutex##sz ); \
   	    allocidx##sz = 0; \
   	    return bgl_make_jsobject_sans( sz, constrmap, proto, md ); \
         } \
      } \
   }

#if HOP_THREAD_ALLOC
prealloc( 1 )
prealloc( 2 )
prealloc( 3 )
prealloc( 4 )
prealloc( 5 )
prealloc( 6 )
#endif
   
/*---------------------------------------------------------------------*/
/*    make ...                                                         */
/*---------------------------------------------------------------------*/
#if ALLOC_STATS
#  define make_stats( sz ) \
      nballoc##sz++; \
      if( nballoc##sz % 1000000 == 0 ) \
         fprintf( stderr, "alloc(%d) =%d prealloc=%d in=%d (%d%%) out=%d\n", \
	          sz, \
	          nballoc##sz,	  \
	          nbprealloc##sz, \
		  nbprealloc##sz * BUCKET_SIZE( sz ), \
	          (long)(100. * (double)nbprealloc##sz * BUCKET_SIZE( sz ) / ((double)nballoc##sz)), \
	          nballoc##sz - (nbprealloc##sz * BUCKET_SIZE( sz )) )
#else
#  define make_stats( sz )
#endif

#define make( sz ) \
   static obj_t \
   bgl_make_jsobject##sz( obj_t constrmap, obj_t __proto__, uint32_t md ) { \
      pthread_spin_lock( &lock##sz ); \
      if( allocidx##sz < BUCKET_SIZE( sz ) ) {	\
         obj_t o = preallocs##sz[ allocidx##sz ]; \
         preallocs##sz[ allocidx##sz++ ] = 0; \
	 pthread_spin_unlock( &lock##sz ); \
	 make_stats( sz ); \
	 ((BgL_jsobjectz00_bglt)(COBJECT( o )))->BgL___proto__z00 = __proto__; \
	 ((BgL_jsobjectz00_bglt)(COBJECT( o )))->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap; \
	 return o; \
      } else { \
	 pthread_spin_unlock( &lock##sz ); \
         return prealloc##sz( constrmap, __proto__, md ); \
      } \
   }

#if HOP_THREAD_ALLOC
make( 1 )   
make( 2 )   
make( 3 )   
make( 4 )   
make( 5 )   
make( 6 )   
#endif
   
/*---------------------------------------------------------------------*/
/*    init_thread ...                                                  */
/*---------------------------------------------------------------------*/
#define init_thread( sz, md ) { \
   pthread_t th##sz; \
   pthread_attr_t thattr##sz; \
   pthread_mutex_init( &mutex##sz, 0L ); \
   \
   pthread_cond_init( &cond##sz, 0L ); \
   pthread_spin_init( &lock##sz, 0L ); \
   pthread_attr_init( &thattr##sz ); \
   \
   pthread_attr_setdetachstate( &thattr##sz, PTHREAD_CREATE_DETACHED ); \
   nextallocs##sz = \
      (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof( obj_t ) * BUCKET_SIZE( sz ) ); \
   preallocs##sz = \
      (obj_t *)GC_MALLOC_UNCOLLECTABLE( sizeof( obj_t ) * BUCKET_SIZE( sz ) ); \
   allocidx##sz = BUCKET_SIZE( sz ); \
   alloc_state##sz = PREALLOC_STATE_IDLE; \
   GC_pthread_create( &th##sz, &thattr##sz, thread_alloc##sz, (void *)(long)md ); \
} 0


/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_init_jsalloc ...                                             */
/*    -------------------------------------------------------------    */
/*    Initialized the multithreaded background allocator.              */
/*---------------------------------------------------------------------*/
int bgl_init_jsalloc( uint32_t md ) {
   static int jsinit = 0;

   if( jsinit ) return 1;

#if HOP_THREAD_ALLOC
   jsinit = 1;

   init_thread( 1, md );
   init_thread( 2, md );
   init_thread( 3, md );
   init_thread( 4, md );
   init_thread( 5, md );
   init_thread( 6, md );
#endif
   
   return 0;
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

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject ...                                            */
/*---------------------------------------------------------------------*/
#if HOP_THREAD_ALLOC
obj_t
bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
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
/*         (mode mode)                                                 */
/*         (cmap constrmap)                                            */
/*         (elements (make-vector constrsize (js-undefined)))          */
/*         (__proto__ __proto__))                                      */
/*---------------------------------------------------------------------*/
#if HOP_THREAD_ALLOC
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
   o->BgL___proto__z00 = __proto__;
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), BNIL );
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)mode );
   
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
   o->BgL___proto__z00 = __proto__;
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   o->BgL_elementsz00 = empty_vector;
   o->BgL_lengthz00 = len;
   o->BgL_ilenz00 = 0;
   BGL_OBJECT_HEADER_SIZE_SET( BNANOBJECT( o ), (long)mode );
   BGL_OBJECT_WIDENING_SET( BNANOBJECT( o ), BNIL );
  
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

