/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/RewriteLib.h              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Nov 17 17:27:07 2023                          */
/*    Last change :  Fri Nov 17 17:44:49 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    RewriteLib.h sample. Compile as follows:                         */
/*      hopc -Ox foo.js -- -copt -DMODE_REWRITE_OPCODE                 */
/*                                                                     */
/*    If only imap should be used then, add the option                 */
/*      --js-cspecs-get "(imap)"                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Prevent Hop generated class definitions                          */
/*---------------------------------------------------------------------*/
#define BGL_MODULE_TYPE_DEFINITIONS
#include "bglhopscript_types.h"

/*---------------------------------------------------------------------*/
/*    Required definition                                              */
/*---------------------------------------------------------------------*/
typedef struct BgL_threadz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
} *BgL_threadz00_bglt;

/*---------------------------------------------------------------------*/
/*    Macro definitions (checking imap hit)                            */
/*---------------------------------------------------------------------*/
#  define HOP_REWRITE_LOCATIONS(n) \
   static char *hop_rewrite_locations[n] = { NULL }
#  define HOP_REWRITE_INIT(n) \
     init_rewrite_lib(n);
#  define HOP_REWRITE_CACHE_HIT(n) \
     BINREWRITELIB_EXPAND_LABEL(n):
#  define BINREWRITELIB_CACHE_MISS_32(n, cache, obj, loc) \
   ((((struct BgL_jsobjectz00_bgl *)obj)->BgL_cmapz00 == (cache)->BgL_imapz00) \
   ? binrewritelib_cache_miss_32(n, cache, obj, loc) \
   : 0)

#define BINREWRITELIB_EXPAND_LABEL(n) \
  LBL_HOPC ## n ## ___

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    init_rewite_lib ...                                              */
/*---------------------------------------------------------------------*/
static int init_rewrite_lib(long n) {
   fprintf(stderr, "init_rewrite_lib, n=%d\n", n);
}

/*---------------------------------------------------------------------*/
/*    init                                                             */
/*    BINREWRITE_LIB_CACHE_MISS_32 ...                                 */
/*---------------------------------------------------------------------*/
static int binrewritelib_cache_miss_32(long n, void *cache, void *obj, void *loc) {
   static long cnt = 0;
   struct BgL_jsobjectz00_bgl *o = (struct BgL_jsobjectz00_bgl *)obj;
   struct BgL_jspropertycachez00_bgl *c = (struct BgL_jspropertycachez00_bgl *)cache;
   
   fprintf(stderr, "%d rewrite n=%d cache=%p obj=%p MATCH=%d\n", cnt++, n, cache, obj, o->BgL_cmapz00 == c->BgL_imapz00);
}
