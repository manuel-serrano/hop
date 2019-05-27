/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/_bglhopscript.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Sun May 26 09:05:39 2019 (serrano)                */
/*    Copyright   :  2016-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Optional file, used only for the C backend, that optimizes       */
/*    JsObject and cache implementations.                              */
/*=====================================================================*/
#include <stdio.h>
#include <bigloo.h>
#include "bglhopscript.h"
#include "bglhopscript_malloc.h"

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

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef struct BgL_jspropertycachez00_bgl pcache_t;

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
/*    long                                                             */
/*    bgl_vector_bytesize ...                                          */
/*---------------------------------------------------------------------*/
long
bgl_vector_bytesize( long len ) {
   return VECTOR_SIZE + ( (len-1) * OBJ_SIZE );
}

