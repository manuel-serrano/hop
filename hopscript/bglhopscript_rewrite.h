/*=====================================================================*/
/*    .../prgm/project/hop/hop/hopscript/bglhopscript_rewrite.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 11 09:35:38 2022                          */
/*    Last change :  Wed May 10 10:29:16 2023 (serrano)                */
/*    Copyright   :  2022-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Code rewrite (patching) macros.                                  */
/*=====================================================================*/
#ifndef BGLHOPSCRIPT_REWRITE_H 
#define BGLHOPSCRIPT_REWRITE_H

/*---------------------------------------------------------------------*/
/*    Dynamic code patching                                            */
/*---------------------------------------------------------------------*/
#if !defined(MODE_REWRITE_OPCODE)
#  define HOP_REWRITE_LOCATIONS(n)
#  define HOP_REWRITE_INIT(n)
#  define HOP_REWRITE_CACHE_HIT(n) 
#  define HOP_REWRITE_CACHE_MISS(n, entry)
#else
#  include "RewriteLib.h"

#  define HOP_REWRITE_LOCATIONS(n) \
   static char *hop_rewrite_locations[n] = { NULL }
#  define HOP_REWRITE_INIT(n) \
     init_rewrite_lib(n);
#  define HOP_REWRITE_CACHE_HIT(n) \
     BINREWRITELIB_EXPAND_LABEL(n):
#  define HOP_REWRITE_CACHE_MISS(n, entry) \
     BINREWRITELIB_CACHE_MISS_32(n, &(__bgl_pcache[((long) n)].entry), &hop_rewrite_locations[n])
#endif

#endif

