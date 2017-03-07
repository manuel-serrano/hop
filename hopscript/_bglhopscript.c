/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/hopscript/_bglhopscript.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Tue Mar  7 08:27:37 2017 (serrano)                */
/*    Copyright   :  2016-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Optional file, used only for the C backend, that optimizes       */
/*    JsObject and cache implementations.                              */
/*=====================================================================*/
#include <bigloo.h>
#include "bglhopscript.h"

#include <stdio.h>

/*---------------------------------------------------------------------*/
/*    JsObject imports                                                 */
/*---------------------------------------------------------------------*/
extern obj_t BGl_JsObjectz00zz__hopscript_typesz00;

#define JSOBJECT_SIZE \
   sizeof( struct BgL_jsobjectz00_bgl )
#define JSOBJECT_CLASS_INDEX \
   BGL_CLASS_INDEX( BGl_JsObjectz00zz__hopscript_typesz00 )

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
} *pcaches;
   
static int pcaches_len = 0;
static int pcaches_index = 0;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_register_pcache ...                                          */
/*---------------------------------------------------------------------*/
void
bgl_register_pcache( pcache_t *pcache, int len ) {
   if( !pcaches_len ) {
      pcaches = malloc( sizeof( struct pcache_entry ) * 10 );
      pcaches[ 0 ].pcache = pcache;
      pcaches[ 0 ].length = len;
      pcaches_len = 10;
      pcaches_index = 1;
   } else if( pcaches_index < pcaches_len ) {
      pcaches[ pcaches_index ].pcache = pcache;
      pcaches[ pcaches_index++ ].length = len;
   } else {
      struct pcache_entry *new =
	 malloc( sizeof( struct pcache_entry ) * pcaches_len * 2 );
      memcpy( new, pcaches, sizeof( struct pcache_entry ) * pcaches_len );
      free( pcaches );
      
      new[ pcaches_index ].pcache = pcache;
      new[ pcaches_index++ ].length = len;

      pcaches = new;
      pcaches_len *= 2;
   }
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_invalidate_pcaches_pmap ...                                  */
/*---------------------------------------------------------------------*/
void
bgl_invalidate_pcaches_pmap() {
   int i;
   for( i = 0; i < pcaches_index; i++ ) {
      int j;
      for( j = 0; j < pcaches[ i ].length; j++ ) {
	 pcaches[ i ].pcache[ j ].BgL_pmapz00 = BTRUE;
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
bgl_make_pcache( obj_t obj, int len, obj_t template ) {
   pcache_t *pcache = (pcache_t *)obj;
   int i;

   for( i = 0; i < len; i++ ) {
      memcpy( &(pcache[ i ]), template, sizeof( pcache_t ) );
   }

   bgl_register_pcache( pcache, len );

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
bgl_make_jsobject( int constrsize, obj_t constrmap, obj_t __proto__, char mode ) {
   long bsize = JSOBJECT_SIZE + VECTOR_SIZE + ( (constrsize-1) * OBJ_SIZE );
   BgL_jsobjectz00_bglt o = (BgL_jsobjectz00_bglt)GC_MALLOC( bsize );
   obj_t vector;
   int i;

   // class initialization
   BGL_OBJECT_CLASS_NUM_SET( BREF( o ), JSOBJECT_CLASS_INDEX );
   
   // fields init
   o->BgL___proto__z00 = __proto__;
   o->BgL_modez00 = mode;
   o->BgL_propertiesz00 = BNIL; 
   o->BgL_cmapz00 = constrmap;
   
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

   return BREF( o );
}

/* {*---------------------------------------------------------------------*} */
/* {*    pcache_t *                                                       *} */
/* {*    bgl_pcache_ref ...                                               *} */
/* {*    -------------------------------------------------------------    *} */
/* {*    The accessor PCACHE-REF is inlined in property.scm. This         *} */
/* {*    version is documentation only.                                   *} */
/* {*---------------------------------------------------------------------*} */
/* pcache_t *                                                          */
/* bgl_pcache_ref( obj_t pcache, int len ) {                           */
/*    return &(((struct BgL_jspropertycachez00_bgl *)&(VECTOR_REF( pcache, 0 )))[ len ]); */
/* }                                                                   */
