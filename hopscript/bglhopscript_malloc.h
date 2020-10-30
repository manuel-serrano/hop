/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/hopscript/bglhopscript_malloc.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  1 13:46:07 2017                          */
/*    Last change :  Thu Nov  2 07:30:05 2017 (serrano)                */
/*    Copyright   :  2017-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    bglhopscript allocation                                          */
/*=====================================================================*/
#if( BGL_GC_BUMP_ALLOC )

/*---------------------------------------------------------------------*/
/*    macro and config                                                 */
/*---------------------------------------------------------------------*/
#define BGL_BUMP_CARD_SIZE 512

#define BGL_GC_BUMP_MALLOC( sz ) \
  (bgl_gc_ptr_head -= sz, BGL_LIKELY( bgl_gc_ptr_head >= bgl_gc_ptr_base ))

#if( !defined( BGL_GC_THREADS ) )
static char *bgl_gc_ptr_head = (char *)BGL_BUMP_CARD_SIZE;
static char *bgl_gc_ptr_base = (char *)BGL_BUMP_CARD_SIZE;
#else
static BGL_THREAD_DECL char *bgl_gc_ptr_head = (char *)BGL_BUMP_CARD_SIZE;
static BGL_THREAD_DECL char *bgl_gc_ptr_base = (char *)BGL_BUMP_CARD_SIZE;
#endif

/*---------------------------------------------------------------------*/
/*    static void *                                                    */
/*    hop_bump_malloc ...                                              */
/*---------------------------------------------------------------------*/
static void *
bgl_gc_bump_malloc( long sz ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - sz;

   return bgl_gc_ptr_head;
}

/*---------------------------------------------------------------------*/
/*    HOP_MALLOC ...                                                   */
/*---------------------------------------------------------------------*/
#define HOP_MALLOC( sz ) \
   ((sz < (BGL_BUMP_CARD_SIZE >> 2)) \
   ? ((BGL_GC_BUMP_MALLOC( sz ) ? bgl_gc_ptr_head : bgl_gc_bump_malloc( sz )))\
    : GC_MALLOC( sz ))
#else
#define HOP_MALLOC( sz ) GC_MALLOC( sz )
#endif

/*---------------------------------------------------------------------*/
/*    HOP_JSOBJECT_ELEMENTS_INLINEP                                    */
/*    -------------------------------------------------------------    */
/*    Used to detect when to reset object inline elements before an    */
/*    expansion. This helps the collector not to retain dead objects   */
/*    still pointed to by these inlined elements.                      */
/*---------------------------------------------------------------------*/
#define HOP_JSOBJECT_ELEMENTS_INLINEP( _o ) \
  CVECTOR( ((BgL_jsobjectz00_bglt)COBJECT(_o))->BgL_elementsz00 ) == \
  (obj_t)(&(((BgL_jsobjectz00_bglt)COBJECT(_o))->BgL_elementsz00) + 1)

/*---------------------------------------------------------------------*/
/*    HOP_JSARRAY_VECTOR_INLINEP                                       */
/*    -------------------------------------------------------------    */
/*    Used to detect when to reset array inline elements before an     */
/*    expansion. This helps the collector not to retain dead arrays    */
/*    still pointed to by these inlined elements.                      */
/*    -------------------------------------------------------------    */
/*    Used also to implement fast vector shift.                        */
/*---------------------------------------------------------------------*/
#define HOP_JSARRAY_VECTOR_INLINEP( _o ) \
   ((CVECTOR( ((BgL_jsarrayz00_bglt)COBJECT(_o))->BgL_vecz00 ) == \
     (obj_t)(&(((BgL_jsarrayz00_bglt)COBJECT(_o))->BgL_vecz00) + 1)) || \
    (CVECTOR( ((BgL_jsarrayz00_bglt)COBJECT(_o))->BgL_vecz00 ) == \
     (CVECTOR(*((obj_t *)(&(((BgL_jsarrayz00_bglt)COBJECT(_o))->BgL_vecz00) + 1))))))
    
