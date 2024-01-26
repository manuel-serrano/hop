/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/RewriteLib.h              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Nov 17 17:27:07 2023                          */
/*    Last change :  Thu Jan 25 10:51:11 2024 (serrano)                */
/*    Copyright   :  2023-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    RewriteLib.h sample. Compile as follows:                         */
/*      hopc -Ox foo.js -- -copt -DHOP_REWRITE_OPCODE                  */
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

struct hop_rewriteinfo {
   char *loc;
   void *labeladdr;
};
   
/*---------------------------------------------------------------------*/
/*    Macro definitions (checking imap hit)                            */
/*---------------------------------------------------------------------*/
#define BINREWRITELIB_CACHE_MISS_32(obj, i, cache) \
   binrewritelib_cache_miss_32(obj, i, cache)

#define BINREWRITELIB_EXPAND_LABEL(n) \
  LBL_HOPC ## n ## ___

#define BINREWRITELIB_REWRITEINFO(n) \
   (hop_rewriteinfo[n].labeladdr = &&BINREWRITELIB_EXPAND_LABEL(n), (obj_t)(&(hop_rewriteinfo[n])))
   
/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    init_rewite_lib ...                                              */
/*---------------------------------------------------------------------*/
static int init_rewrite_lib(long n) {
   fprintf(stderr, "============ init_rewrite_lib, n=%d\n", n);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    BINREWRITE_LIB_CACHE_MISS_32 ...                                 */
/*---------------------------------------------------------------------*/
static int binrewritelib_cache_miss_32(void *obj, long i, void *cache) {
   static long cnt = 0;
   struct BgL_jsobjectz00_bgl *o = (struct BgL_jsobjectz00_bgl *)COBJECT(obj);
   struct BgL_jspropertycachez00_bgl *c = (struct BgL_jspropertycachez00_bgl *)COBJECT(cache);
   
   if (c->BgL_rewriteinfoz00 && c->BgL_rewriteinfoz00 != BUNSPEC) {
      struct hop_rewriteinfo *info = (struct hop_rewriteinfo *)c->BgL_rewriteinfoz00;
      fprintf(stderr, "INFO=%p\n", info);
      fprintf(stderr, "------------ rewrite #%d loc=%d cache=%p obj=%p MATCH=%d iindex=%d lbl=%p\n",
	      cnt++, c->BgL_pointz00, cache, obj, o->BgL_cmapz00 == c->BgL_imapz00, c->BgL_iindexz00, info->labeladdr);
   }
}

