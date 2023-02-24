/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/_napi.c                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 24 14:34:24 2023                          */
/*    Last change :  Fri Feb 24 18:55:51 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop node_api implementation.                                     */
/*=====================================================================*/
#include <bigloo.h>
#include "node_api.h"

extern char *bgl_typeof();

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    napi_module_register ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
napi_module_register(napi_module *mod) {
   fprintf(stderr, "napi_module_register mod=%s %s\n", mod->nm_filename, mod->nm_modname);
}

/*---------------------------------------------------------------------*/
/*    Local functions                                                  */
/*---------------------------------------------------------------------*/
static obj_t napi_method_stub(obj_t proc, ...) {
   va_list argl;
   obj_t optional;
   obj_t runner;
   long cnt = 0;
   obj_t *args;

   va_start(argl, proc);
   while ((runner = va_arg(argl, obj_t)) != BEOA) cnt++;
   va_end(argl);

   args = alloca((1 + cnt) * sizeof(obj_t));
   args[cnt] = 0;
   cnt = 0;

   va_start(argl, proc);

   while ((runner = va_arg(argl, obj_t)) != BEOA) {
      args[cnt++] = runner;
   }

   va_end(argl);

   return PROCEDURE_VA_ENTRY(proc)(PROCEDURE_ATTR(proc), args);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_napi_method_to_procedure ...                                 */
/*---------------------------------------------------------------------*/
static obj_t
bgl_napi_method_to_procedure(napi_env _this, napi_value this, napi_callback met) {
   obj_t proc = make_va_procedure((function_t)met, -1, 0);
   PROCEDURE(proc).entry = (function_t)napi_method_stub;
   PROCEDURE_ATTR_SET(proc, _this);
   return proc;
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    _napi_define_properties ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
_napi_define_properties(napi_env _this, napi_value this, size_t count, const napi_property_descriptor *properties) {
   while (count-- > 0) {
      obj_t name = properties->name ? properties->name : string_to_bstring((char *)properties->utf8name);
      bgl_napi_define_property(_this, this, name, bgl_napi_method_to_procedure(_this, this, properties->method));
   }
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    _napi_get_cb_info ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
_napi_get_cb_info(napi_env _this, napi_callback_info info, size_t *argc, napi_value *argv, napi_value *this_arg, void **data) {
   int i = 0;

   if (argv) {
      if (argc) {
	 int max = *argc;
	 while(info[i + 1] != 0 && i < max) {
	    argv[i] = info[i + 1];
	    i++;
	 }
      } else {
	 while(info[i + 1] != 0) {
	    argv[i] = info[i + 1];
	    i++;
	 }
      }
      *argc = i;
   }

   if (this_arg) {
      *this_arg = info[0];
   }
}
