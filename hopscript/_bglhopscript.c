/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/hopscript/_bglhopscript.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 17 07:55:08 2016                          */
/*    Last change :  Thu Feb 18 09:25:05 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Optional file, used only for the C backend, that optimizes       */
/*    JsObject and cache implementations.                              */
/*=====================================================================*/
#include <bigloo.h>
#include "bglhopscript.h"

#include <stdio.h>

/*---------------------------------------------------------------------*/
/*    extern                                                           */
/*---------------------------------------------------------------------*/
extern obj_t create_vector_uncollectable( int );

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef struct BgL_jspropertycachez00_bgl pcache_t;

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
      
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    pcache_t *                                                       */
/*    bgl_pcache_ref ...                                               */
/*    -------------------------------------------------------------    */
/*    The accessor PCACHE-REF is inlined in property.scm. This         */
/*    version is documentation only.                                   */
/*---------------------------------------------------------------------*/
pcache_t *
bgl_pcache_ref( obj_t pcache, int len ) {
   return &(((struct BgL_jspropertycachez00_bgl *)&(VECTOR_REF( pcache, 0 )))[ len ]);
}
