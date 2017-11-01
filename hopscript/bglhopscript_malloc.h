/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/hopscript/_bglhopscript_malloc.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  1 13:46:07 2017                          */
/*    Last change :  Wed Nov  1 13:54:02 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    bglhopscript allocation                                          */
/*=====================================================================*/
#if( BGL_GC_BUMP_ALLOC )

/*---------------------------------------------------------------------*/
/*    macro and config                                                 */
/*---------------------------------------------------------------------*/
#define BGL_BUMP_CARD_SIZE 256

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
/*    bgl_gc_bump_malloc ...                                           */
/*---------------------------------------------------------------------*/
static void *
bgl_gc_bump_malloc( long sz ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - sz;
   
   return bgl_gc_ptr_head;
}

/*---------------------------------------------------------------------*/
/*    BGL_GC_MALLOC ...                                                */
/*---------------------------------------------------------------------*/
#define BGL_GC_MALLOC( sz ) \
   ((sz < (BGL_BUMP_CARD_SIZE >> 2)) \
    ? (BGL_GC_BUMP_MALLOC( sz ) || bgl_gc_bump_malloc)	\
    : GC_MALLOC( sz ))
#else
#define BGL_GC_MALLOC( sz ) GC_MALLOC( sz )
#endif
