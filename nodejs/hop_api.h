/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/hop_api.h                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 24 15:38:53 2023                          */
/*    Last change :  Fri Feb 24 18:56:10 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop Specific macro redefinitions                                 */
/*=====================================================================*/
#ifndef HOP_NODE_API_H_
#define HOP_NODE_API_H_

#include <stdarg.h>

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t bgl_napi_create_string_utf8(obj_t, obj_t);
extern obj_t bgl_napi_get_element(obj_t, obj_t, int);
extern obj_t bgl_napi_get_named_property(obj_t, obj_t, obj_t);
extern obj_t bgl_napi_define_property(obj_t, obj_t, obj_t, obj_t);
extern napi_status _napi_define_properties(napi_env _this, napi_value this, size_t count, const napi_property_descriptor *properties);
extern napi_status _napi_get_cb_info(napi_env _this, napi_callback_info info, size_t *argc, napi_value *argv, napi_value *this_arg, void **data);

/*---------------------------------------------------------------------*/
/*    Module init                                                      */
/*---------------------------------------------------------------------*/
#undef NAPI_C_CTOR
#undef NAPI_MODULE_X
#undef NAPI_MODULE

#define NAPI_C_CTOR(modname)						\
   static obj_t BGl_z62hopscriptz62zz ## modname ## z00(obj_t env, obj_t _this, obj_t this, obj_t scope, obj_t module) { \
     napi_status status; \
     napi_value result; \
     status = napi_get_named_property(_this, module, "exports", &result); \
     _module.nm_register_func(_this, result); \
   } \
   DEFINE_EXPORT_BGL_PROCEDURE(BGl_hopscriptzd2envzd2zz ## modname ## z00, __BGl_z62hopscriptz62zz ## modname ## z00, BGl_z62hopscriptz62zz ## modname ## z00, 0L, BUNSPEC, 4); \
   obj_t bigloo_dlopen_init() { \
     obj_t res = BTRUE; \
     BGL_MVALUES_NUMBER_SET(2); \
     BGL_MVALUES_VAL_SET(1,string_to_bstring(# modname)); \
     return res; \
  } \
  static void _register_ ## modname(void)

#define NAPI_MODULE_X(modname, regfunc, priv, flags)                  \
  EXTERN_C_START                                                      \
    static napi_module _module =                                      \
    {                                                                 \
      NAPI_MODULE_VERSION,                                            \
      flags,                                                          \
      __FILE__,                                                       \
      regfunc,                                                        \
      #modname,                                                       \
      priv,                                                           \
      {0},                                                            \
    };                                                                \
  NAPI_C_CTOR(modname) {				              \
      napi_module_register(&_module);                                 \
    }                                                                 \
  EXTERN_C_END

#define NAPI_MODULE(modname, regfunc)                                 \
  NAPI_MODULE_X(modname, regfunc, NULL, 0)  // NOLINT (readability/null_usage)

/*---------------------------------------------------------------------*/
/*    Wrappers                                                         */
/*---------------------------------------------------------------------*/
#define napi_create_string_utf8(_this, string, sz, res) \
  (*res = bgl_napi_create_string_utf8(_this, string_to_bstring(string)), napi_ok)

#define napi_get_named_property(_this, this, prop, res) \
  (*res = bgl_napi_get_named_property(_this, this, string_to_bstring(prop)), napi_ok)

#define napi_get_element(_this, this, index, res) \
  (*res = bgl_napi_get_element(_this, this, index), napi_ok)

#define napi_define_properties(_this, this, count, properties) \
   _napi_define_properties(_this, this, count, properties)

#define napi_get_cb_info( _this, info, argc, argv, this_arg, data) \
   _napi_get_cb_info(_this, info, argc, argv, this_arg, data)
#endif  
