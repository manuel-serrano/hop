/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/hopscript/_bglhopscript.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Mon Feb 12 14:53:47 2018 (serrano)                */
/*    Copyright   :  2016-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Optional file, used only for the C backend, that optimizes       */
/*    JsObject and cache implementations.                              */
/*=====================================================================*/
#include <stdio.h>
#include <bigloo.h>
#include "bglhopscript.h"
#include "bglhopscript_malloc.h"

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

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef struct BgL_jspropertycachez00_bgl pcache_t;

/*---------------------------------------------------------------------*/
/*    pcache_entry ...                                                 */
/*---------------------------------------------------------------------*/
struct pcache_entry {
   pcache_t *pcache;
   int length;
   obj_t src;
} *pcaches;
   
static int pcaches_len = 0;
static int pcaches_index = 0;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_register_pcache ...                                          */
/*---------------------------------------------------------------------*/
void
bgl_register_pcache( pcache_t *pcache, int len, obj_t src ) {
   if( pcaches_index == pcaches_len ) {
      if( !pcaches_len ) {
	 pcaches =
	    (void *)GC_MALLOC_UNCOLLECTABLE( sizeof( struct pcache_entry ) * 10 );
	 pcaches_len = 10;
      } else { 
	 struct pcache_entry *new =
	    (void *)GC_MALLOC_UNCOLLECTABLE( sizeof( struct pcache_entry ) * pcaches_len * 2 );
	 memcpy( new, pcaches, sizeof( struct pcache_entry ) * pcaches_len );
	 GC_free( (void *)pcaches );
	 pcaches = new;
	 pcaches_len *= 2;
      }
   }
   
   pcaches[ pcaches_index ].pcache = pcache;
   pcaches[ pcaches_index ].length = len;
   pcaches[ pcaches_index++ ].src = src;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_get_pcaches ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_get_pcaches() {
   obj_t res = BNIL;
   int i;

   for( i = 0; i < pcaches_index; i++ ) {
      int j;
      obj_t vec = create_vector( pcaches[ i ].length );
      obj_t o;
      
      for( j = 0; j < pcaches[ i ].length; j++ ) {
	 VECTOR_SET( vec, j, BOBJECT( &(pcaches[ i ].pcache[ j ]) ) );
      }

      o = MAKE_PAIR( pcaches[ i ].src, vec );
      res = MAKE_PAIR( o, res );
   }

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_invalidate_pcaches_pmap ...                                  */
/*---------------------------------------------------------------------*/
void
bgl_invalidate_pcaches_pmap( obj_t proc ) {
   int i;
   for( i = 0; i < pcaches_index; i++ ) {
      int j;
      for( j = 0; j < pcaches[ i ].length; j++ ) {
	 PROCEDURE_ENTRY( proc )( proc, BOBJECT( &(pcaches[ i ].pcache[ j ]) ), BEOA );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_pcache ...                                              */
/*    -------------------------------------------------------------    */
/*    Create a fake Bigloo vector whose elements are inlined pcache    */
/*    entries.                                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_pcache( obj_t obj, int len, obj_t src, obj_t template ) {
   pcache_t *pcache = (pcache_t *)obj;
   int i;

   for( i = 0; i < len; i++ ) {
      memcpy( &(pcache[ i ]), COBJECT( template ), sizeof( pcache_t ) );
   }

   bgl_register_pcache( pcache, len, src );

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsobject ...                                            */
/*    -------------------------------------------------------------    */
/*    Fast C allocation, equivalent to                                 */
/*                                                                     */
/*      (instantiate::JsObject                                         */
/*         (mode mode)                                                 */
/*         (cmap constrmap)                                            */
/*         (elements (make-vector constrsize (js-undefined)))          */
/*         (__proto__ __proto__))                                      */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
   long bsize = JSOBJECT_SIZE + VECTOR_SIZE + ( (constrsize-1) * OBJ_SIZE );
   BgL_jsobjectz00_bglt o = (BgL_jsobjectz00_bglt)HOP_MALLOC( bsize );
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BOBJECT( o ), JSOBJECT_CLASS_INDEX );
   
   // fields init
   o->BgL___proto__z00 = __proto__;
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   BGL_OBJECT_WIDENING_SET( BOBJECT( o ), BNIL );
   BGL_OBJECT_HEADER_SIZE_SET( BOBJECT( o ), (long)mode );
   
   // elements initialization
   vector = (obj_t)(&(o->BgL_elementsz00) + 1);

#if( !defined( TAG_VECTOR ) )
   vector->vector_t.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
   vector->vector_t.length = constrsize;
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
   
   return BOBJECT( o );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    empty_vector ...                                                 */
/*---------------------------------------------------------------------*/
#if( !defined( TAG_VECTOR ) )
static struct {
   __CNST_ALIGN header_t header;
   long length;
} _empty_vector = { __CNST_FILLER, MAKE_HEADER( VECTOR_TYPE, 0 ), 0 };
static obj_t empty_vector = BVECTOR( &(_empty_vector.header ) );
#else   
static struct {
   __CNST_ALIGN long length;
} _empty_vector = { __CNST_FILLER, 0 };
static obj_t empty_vector = BVECTOR( &(_empty_vector.length ) );
#endif   
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_jsarray ...                                             */
/*    -------------------------------------------------------------    */
/*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
/*    -------------------------------------------------------------    */
/*    This C version of the array allocation, creates inner pointer    */
/*    from the object to its vector component. This is safe as long    */
/*    as the array lives in the JavaScript world but not if the        */
/*    array is passed to Scheme as in this situation, there will       */
/*    not necessary be any pointer to the beginning of the object.     */
/*    For this to be safe the GC has to be configured with             */
/*    INNER_POINTER activated.                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_jsarray( long size, uint32_t len, obj_t constrmap, obj_t __proto__, uint32_t mode ) {
   long bsize = JSARRAY_SIZE + VECTOR_SIZE + ( (size-1) * OBJ_SIZE );
   BgL_jsarrayz00_bglt o = (BgL_jsarrayz00_bglt)HOP_MALLOC( bsize );
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BOBJECT( o ), JSARRAY_CLASS_INDEX );
   
   // fields init
   o->BgL___proto__z00 = __proto__;
   o->BgL_cmapz00 = (BgL_jsconstructmapz00_bglt)constrmap;
   o->BgL_elementsz00 = empty_vector;
   o->BgL_lengthz00 = len;
   o->BgL_ilenz00 = 0;
   BGL_OBJECT_HEADER_SIZE_SET( BOBJECT( o ), (long)mode );
   BGL_OBJECT_WIDENING_SET( BOBJECT( o ), BNIL );
  
   // vector initialization
   vector = (obj_t)(&(o->BgL_vecz00) + 1);

#if( !defined( TAG_VECTOR ) )
   vector->vector_t.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
   vector->vector_t.length = size;
   vector = BVECTOR( vector );
   
   o->BgL_vecz00 = vector;

   for( i = 0; i < size; i++ ) {
      VECTOR_SET( vector, i, BUNSPEC );
   }

   return BOBJECT( o );
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
      vector->vector_t.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
      vector->vector_t.length = len;

      bgl_fill_vector( BVECTOR( vector ), 0, len, init );
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

