/*=====================================================================*/
/*    .../prgm/project/hop/hop/hopscript/bglhopscript_rewrite.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 11 09:35:38 2022                          */
/*    Last change :  Thu Jan 25 09:20:42 2024 (serrano)                */
/*    Copyright   :  2022-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Code rewrite (patching) macros.                                  */
/*=====================================================================*/
#ifndef BGLHOPSCRIPT_REWRITE_H 
#define BGLHOPSCRIPT_REWRITE_H

/*---------------------------------------------------------------------*/
/*    Dynamic code patching                                            */
/*---------------------------------------------------------------------*/
#if !defined(HOP_REWRITE_OPCODE)
#  define HOP_REWRITE_LOCATIONS(n)
#  define HOP_REWRITE_INIT(n)
#  define HOP_REWRITE_IMAP_CACHE_HIT(n) 
#  define HOP_REWRITE_CACHE_MISS(obj, index, cache)
#  define HOP_REWRITE_INFO_SET(cache, idx)
#else
#  include "RewriteLib.h"

#  define HOP_REWRITE_LOCATIONS(n) \
   static struct hop_rewriteinfo hop_rewriteinfo[n] = { NULL }
#  define HOP_REWRITE_INIT(n) \
     init_rewrite_lib(n);
#  define HOP_REWRITE_IMAP_CACHE_HIT(n) \
     BINREWRITELIB_EXPAND_LABEL(n):
#  define HOP_REWRITE_CACHE_MISS(obj, index, cache) \
     BINREWRITELIB_CACHE_MISS_32(obj, index, cache)
#  define HOP_REWRITE_INFO_SET(cache, n) \
     cache.BgL_rewriteinfoz00 = BINREWRITELIB_REWRITEINFO(n)
#endif

#endif

