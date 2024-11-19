/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/bglhopscript.h            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 11 09:35:38 2022                          */
/*    Last change :  Tue Nov 19 12:03:16 2024 (serrano)                */
/*    Copyright   :  2022-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Macros for accelerating C compilation.                           */
/*=====================================================================*/
#ifndef BGLHOPSCRIPT_H 
#define BGLHOPSCRIPT_H

#include <bigloo.h>
#include "bglhopscript_rewrite.h"

/*---------------------------------------------------------------------*/
/*    import                                                           */
/*---------------------------------------------------------------------*/
extern bool_t hop_js_toboolean_no_boolean(obj_t);

/*---------------------------------------------------------------------*/
/*    BHOPOBJECT                                                       */
/*---------------------------------------------------------------------*/
#define BHOPOBJECT(o) BNANOBJECT(o)

/*---------------------------------------------------------------------*/
/*    Type predicates                                                  */
/*---------------------------------------------------------------------*/
#define HOP_OBJECTP(o) BGL_OBJECTP(o)
#define HOP_JSOBJECTP(o, tag) \
   (HOP_OBJECTP(o) && ((HOP_OBJECT_HEADER_SIZE(o) & tag) == tag))

#define HOP_OBJECT_HEADER_SIZE(o) BGL_OBJECT_HEADER_SIZE(o)
#define HOP_OBJECT_HEADER_SIZE_SET(o, s) BGL_OBJECT_HEADER_SIZE_SET(o, s)

#define HOP_JSARRAYP(o, tag) \
   (HOP_OBJECTP(o) && HOP_OBJECT_HEADER_SIZE(o) >= tag)

#define HOP_OBJECT_JSSTRINGP(o, tag) \
   ((HOP_OBJECT_HEADER_SIZE(o) & tag) == tag)

#define HOP_JSSTRINGP(o, tag) \
   (HOP_OBJECTP(o) && HOP_OBJECT_JSSTRINGP(o, tag))

#define HOP_OBJECT_JSPROCEDUREP(o, tag) \
   ((HOP_OBJECT_HEADER_SIZE(o) & tag) == tag)

#define HOP_JSPROCEDUREP(o, tag) \
   (HOP_OBJECTP(o) && HOP_OBJECT_JSPROCEDUREP(o, tag))

/*---------------------------------------------------------------------*/
/*    Objects & properties predicates                                  */
/*---------------------------------------------------------------------*/
#define HOP_JSOBJECT_MODE_INLINEP(o, tag) \
   ((BGL_OBJECT_HEADER_SIZE(o) & tag) == tag)

#define HOP_JSOBJECT_ELEMENTS_INLINEP(_o) \
   (CVECTOR( ((BgL_jsobjectz00_bglt)COBJECT(_o))->BgL_elementsz00 ) ==	\
    (obj_t)(&(((BgL_jsobjectz00_bglt)COBJECT(_o))->BgL_elementsz00) + 1))

/*---------------------------------------------------------------------*/
/*    HOP_JSOBJECT_INLINE_ELEMENTS ...                                 */
/*---------------------------------------------------------------------*/
#define HOP_JSOBJECT_INLINE_ELEMENTS(_o) \
   BVECTOR((obj_t)(&(((BgL_jsobjectz00_bglt)COBJECT(_o))->BgL_elementsz00) + 1))
#define HOP_JSOBJECT_INLINE_ELEMENTS_LENGTH(_o) \
   VECTOR_LENGTH(HOP_JSOBJECT_INLINE_ELEMENTS(_o))

//#define HOP_DEBUG_INLINE_ELEMENTS

#if !defined(HOP_DEBUG_INLINE_ELEMENTS)
#  define HOP_JSOBJECT_INLINE_ELEMENTS_REF(_o, _i) \
     VECTOR_REF(HOP_JSOBJECT_INLINE_ELEMENTS(_o), _i)
#  define HOP_JSOBJECT_INLINE_ELEMENTS_SET(_o, _i, _v) \
     VECTOR_SET(HOP_JSOBJECT_INLINE_ELEMENTS(_o), _i, _v)
#else
#  define HOP_JSOBJECT_INLINE_ELEMENTS_REF(_o, _i) \
     (_i < HOP_JSOBJECT_INLINE_ELEMENTS_LENGTH(_o) \
      ? VECTOR_REF(HOP_JSOBJECT_INLINE_ELEMENTS(_o), _i) \
      : fprintf(stderr, "WRONG index %d\n", 1/0))
#  define HOP_JSOBJECT_INLINE_ELEMENTS_SET(_o, _i, _v) \
     (_i < HOP_JSOBJECT_INLINE_ELEMENTS_LENGTH(_o) \
      ? VECTOR_SET(HOP_JSOBJECT_INLINE_ELEMENTS(_o), _i, _v) \
      : fprintf(stderr, "WRONG index %d\n", 1/0))
#endif

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
   
/*---------------------------------------------------------------------*/
/*    Conversions                                                      */
/*---------------------------------------------------------------------*/
#define HOP_JSTOTEST(o) \
   (BOOLEANP(o)		\
    ? CBOOL(o)          \
    : (BGL_NULL_OR_UNSPECIFIEDP(o) ? 0 : hop_js_toboolean_no_boolean(o)))

/*---------------------------------------------------------------------*/
/*    Arithmetic ops                                                   */
/*---------------------------------------------------------------------*/
#define HOP_JSEQIL(x, y) \
   (BINT(x) == y || (REALP(y) && (((double) x) == REAL_TO_DOUBLE(y))))

/*---------------------------------------------------------------------*/
/*    Overflow ...                                                     */
/*---------------------------------------------------------------------*/
/* #if BGL_HAVE_OVERFLOW && TAG_INT == 0                               */
/* #  define BGL_ADDFX_OV53(x, y, res) \                               */
/*    !__builtin_saddl_overflow(CINT(x) << (63-53), CINT(y) << (63-53), (long *)(&res)) \ */
/*    ? BINT((long)res >> 63-53) : DOUBLE_TO_REAL((double)((long)res)) */
/* #  define BGL_SUBFX_OV53(x, y, res) \                               */
/*    !__builtin_ssubl_overflow(CINT(x) << (63-53), CINT(y) << (63-53), (long *)(&res)) \ */
/*    ? BINT((long)res >> 63-53) : DOUBLE_TO_REAL((double)((long)res)) */
/* #elif TAG_INT == 0                                                  */

#if TAG_INT == 0
#define BGL_OV53(res) \
   ((uint64_t)((long)res - (long)BINT(-9007199254740992)) <= (uint64_t)BINT(18014398509481983))
#  define BGL_ADDFX_OV53(x, y, res) \
   (res = (obj_t)((long)x + (long)y), \
    BGL_OV53(res) ? res : DOUBLE_TO_REAL((double)((long)res)))
#  define BGL_SUBFX_OV53(x, y, res) \
   (res = (obj_t)((long)x - (long)y), \
    BGL_OV53(res) ? res : DOUBLE_TO_REAL((double)((long)res)))
#else
#define BGL_OV53(res) \
   ((uint64_t)((long)res - (long)(-9007199254740992)) <= (uint64_t)(18014398509481983))
#  define BGL_ADDFX_OV53(x, y, res) \
   (res = (obj_t)(CINT(x) + CINT(y)), \
    BGL_OV53(res) ? BINT(res) : DOUBLE_TO_REAL((double)((long)res)))
#  define BGL_SUBFX_OV53(x, y, res) \
   (res = (obj_t)(CINT(x) - CINT(y)), \
    BGL_OV53(res) ? BINT(res) : DOUBLE_TO_REAL((double)((long)res)))
#endif 

/*---------------------------------------------------------------------*/
/*    Vector manipulations                                             */
/*---------------------------------------------------------------------*/
#if (!defined(TAG_VECTOR))
#  define BGL_INIT_VECTOR(_vec, _len) \
     (((obj_t)(_vec))->vector.header = BGL_MAKE_VECTOR_HEADER(_vec, VECTOR_TYPE, _len))
#else
#  define BGL_INIT_VECTOR(_vec, _len) \
     (((obj_t)(_vec))->vector.length = _len)
#endif

#define BGL_INIT_VECTOR_SANS_FILL_SANS_CHECK(_vector, _len) \
   ((BGL_INIT_VECTOR(_vector, _len), BVECTOR(_vector)))

#if (VECTOR_SIZE_TAG_NB_BIT != 0)
#  define BGL_INIT_VECTOR_SANS_FILL(_vector, _len) \
   ((_len & ~(VECTOR_LENGTH_MASK))				   \
       ? C_FAILURE("create_vector", "vector too large", BINT(_len)) \
   : BGL_INIT_VECTOR_SANS_FILL_SANS_CHECK(_vector, _len))
#else
#  define BGL_INIT_VECTOR_SANS_FILL(vector, len) \
     BGL_INIT_VECTOR_SANS_FILL_SANS_CHECK(vector, len)
#endif

/*---------------------------------------------------------------------*/
/*    endif                                                            */
/*---------------------------------------------------------------------*/
#endif
