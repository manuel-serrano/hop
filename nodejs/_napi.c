/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/_napi.c                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 24 14:34:24 2023                          */
/*    Last change :  Wed Nov  8 20:48:18 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop node_api implementation.                                     */
/*=====================================================================*/
#include <stdlib.h>
#include <pthread.h>
#include <bigloo.h>
#include <uv.h>
#include <math.h>
#include "node_api.h"
#include "../hopscript/bglhopscript_types.h"

/*---------------------------------------------------------------------*/
/*    imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t hop_js_call0(obj_t, obj_t, obj_t);
extern obj_t hop_js_call1(obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call2(obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call3(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call4(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call5(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call6(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call7(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call8(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call9(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_call10(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t);
extern obj_t hop_js_calln(obj_t, obj_t, obj_t, obj_t);

extern obj_t bgl_typeof();

extern uv_loop_t *bgl_napi_uvloop(obj_t);

static obj_t bgl_napi_method_to_procedure(napi_env _this, napi_value this, napi_callback met, void *data);

extern obj_t bgl_napi_coerce_to_bool(obj_t _this, obj_t value);
extern obj_t bgl_napi_coerce_to_number(obj_t _this, obj_t value);
extern obj_t bgl_napi_coerce_to_object(obj_t _this, obj_t value);
extern obj_t bgl_napi_coerce_to_string(obj_t _this, obj_t value);

/*---------------------------------------------------------------------*/
/*    Limits                                                           */
/*---------------------------------------------------------------------*/
#define INT32_MAX_D ((double)INT32_MAX)
#define INT32_MIN_D ((double)INT32_MIN)
#define INT64_MAX_D ((double)INT64_MAX)
#define INT64_MIN_D ((double)INT64_MIN)

/*---------------------------------------------------------------------*/
/*    static napi_status                                               */
/*    napi_last_error_code ...                                         */
/*---------------------------------------------------------------------*/
static napi_status napi_last_error_code;
static char *napi_last_error_message;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    napi_module_register ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
napi_module_register(napi_module *mod) {
   fprintf(stderr, "napi_module_register mod=%s %s\n", mod->nm_filename, mod->nm_modname);
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    napi_method_stub ...                                             */
/*---------------------------------------------------------------------*/
static obj_t napi_method_stub(obj_t proc, ...) {
   va_list argl;
   obj_t runner;
   int cnt = 1, i;
   obj_t args[17], *aargs;

   // count the number of arguments
   va_start(argl, proc);
   while ((runner = va_arg(argl, obj_t)) != BEOA) {
      args[cnt] = runner;
      cnt++;
      if (cnt > 16) {
	 va_end(argl);
	 goto _slow;
      }
   }
   va_end(argl);
   
   args[0] = PROCEDURE_LENGTH(proc) == 1 ? PROCEDURE_REF(proc, 0) : 0L;
   args[cnt] = BEOA;

   // call the C function
   return PROCEDURE_VA_ENTRY(proc)(PROCEDURE_ATTR(proc), args);

_slow:
   // count the number of arguments
   va_start(argl, proc);
   while ((runner = va_arg(argl, obj_t)) != BEOA) cnt++;
   va_end(argl);

   // stack allocate the napi arguments array
   aargs = alloca((1 + cnt) * sizeof(obj_t));
   aargs[0] = PROCEDURE_LENGTH(proc) == 1 ? PROCEDURE_REF(proc, 0) : 0L;

   // collect the arguments
   va_start(argl, proc);
   for (i = 1; i < cnt; i++) {
      aargs[i] = va_arg(argl, obj_t);
   }
   va_end(argl);
   aargs[cnt] = BEOA;

   // call the C function
   return PROCEDURE_VA_ENTRY(proc)(PROCEDURE_ATTR(proc), aargs);
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    napi_callback_to_jsproc ...                                      */
/*---------------------------------------------------------------------*/
static obj_t
napi_callback_to_jsproc(obj_t _this, obj_t this, napi_callback cb, void *data, obj_t name) {
   obj_t proc = bgl_napi_method_to_procedure(_this, this, cb, data);
   return bgl_napi_create_function(_this, proc, name);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    napi_make_prop_desc ...                                          */
/*---------------------------------------------------------------------*/
static obj_t
napi_make_prop_desc(obj_t _this, obj_t this, const napi_property_descriptor *prop) {
   if (((prop->attributes & napi_default) == napi_default) && !prop->getter && !prop->setter) {
      return BFALSE;
   } else {
      return bgl_napi_make_property_descriptor(_this, this, prop->name, prop->value,
					       prop->attributes & napi_writable,
					       prop->attributes & napi_enumerable,
					       prop->attributes & napi_configurable,
					       (obj_t)prop->getter, (obj_t)prop->setter);
   }
}
	 
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    napi_ctor_stub ...                                               */
/*---------------------------------------------------------------------*/
static obj_t napi_ctor_stub(obj_t proc, ...) {
   va_list argl;
   obj_t optional;
   obj_t runner;
   long cnt = 0;
   obj_t *args;
   obj_t this;
   obj_t _this = PROCEDURE_ATTR(proc);

   va_start(argl, proc);
   while ((runner = va_arg(argl, obj_t)) != BEOA) cnt++;
   va_end(argl);

   args = alloca((2 + 1 + cnt) * sizeof(obj_t));
   args[cnt] = 0;
   args[0] = PROCEDURE_REF(proc, 0);
   cnt = 1;

   va_start(argl, proc);

   while ((runner = va_arg(argl, obj_t)) != BEOA) {
      args[cnt++] = runner;
   }
   args[cnt] = BEOA;

   va_end(argl);

   this = PROCEDURE_VA_ENTRY(proc)(_this, args);

   // bind the instance properties
   for (long i = 1; i < PROCEDURE_LENGTH(proc); i++) {
      obj_t p = PROCEDURE_REF(proc, i);

      if (p != BUNSPEC) {
	 napi_property_descriptor *prop = (napi_property_descriptor *)CAR(p);
      
	 if (PAIRP(p) && CDR(p) == BFALSE) {
	    bgl_napi_put_named_property(_this, this, prop->name, prop->value);
	 } else {
	    bgl_napi_define_named_property(_this, this, prop->name, CDR(p));
	 }
      }
   }
   
   return this;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_napi_method_to_procedure ...                                 */
/*---------------------------------------------------------------------*/
static obj_t
bgl_napi_method_to_procedure(napi_env _this, napi_value this, napi_callback met, void *data) {
   obj_t proc = make_va_procedure((function_t)met, -1, data ? 1 : 0);
   PROCEDURE(proc).entry = (function_t)napi_method_stub;
   PROCEDURE_ATTR_SET(proc, _this);

   if (data) {
      PROCEDURE_SET(proc, 0, data);
   }
   return proc;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_napi_method_to_ctor ...                                      */
/*---------------------------------------------------------------------*/
static obj_t
bgl_napi_method_to_ctor(napi_env _this, napi_value this, napi_callback met, void *data, size_t property_count, const napi_property_descriptor *properties) {
   obj_t proc = make_va_procedure((function_t)met, -1, 1 + property_count);

   PROCEDURE(proc).entry = (function_t)napi_ctor_stub;
   PROCEDURE_ATTR_SET(proc, _this);
   
   PROCEDURE_SET(proc, 0, data);

   return proc;
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_create_function ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_function(napi_env _this,
		     const char *utf8name,
		     size_t length,
		     napi_callback cb,
		     void* data,
		     napi_value *result) {
   if (!_this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      *result = napi_callback_to_jsproc(_this, *result, cb, data, string_to_bstring((char *)utf8name));
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    napi_prepare_property ...                                        */
/*---------------------------------------------------------------------*/
static void
napi_prepare_property(napi_env _this, napi_value this, napi_property_descriptor *prop) {
   prop->name = string_to_bstring((char *)prop->utf8name);
	 
   if (prop->method) {
      prop->value = napi_callback_to_jsproc(_this, this, prop->method, 0L, prop->name);
   }
   if (prop->getter) {
      prop->getter = (napi_callback)napi_callback_to_jsproc(_this, this, prop->getter, 0L, prop->name);
   } else {
      prop->getter = (napi_callback)BFALSE;
   }
   if (prop->setter) {
      prop->setter = (napi_callback)napi_callback_to_jsproc(_this, this, prop->setter, 0L, prop->name);
   } else {
      prop->setter = (napi_callback)BFALSE;
   }
}


/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_define_class ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_define_class(napi_env _this, 
		  const char* utf8name,
		  size_t length,
		  napi_callback ctor,
		  void *data,
		  size_t property_count,
		  const napi_property_descriptor *properties,
		  napi_value *result) {
   if (!_this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!utf8name) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!ctor) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (property_count && !properties) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      obj_t proc = bgl_napi_method_to_ctor(_this, BNIL, ctor, data, property_count, properties);
      obj_t this = bgl_napi_create_function(_this, proc, string_to_bstring((char *)utf8name));

      // prepare the class and instance properties
      for (long i = 0; i < property_count; i++) {
	 napi_property_descriptor *prop = (napi_property_descriptor *)GC_MALLOC(sizeof(napi_property_descriptor));
	 
	 *prop = properties[i];
	 napi_prepare_property(_this, this, prop);

	 if (prop->attributes & napi_static) {
	    // class property
	    obj_t desc = napi_make_prop_desc(_this, this, prop);

	    if (desc) {
	       bgl_napi_define_named_property(_this, this, prop->name, desc);
	    } else {
	       bgl_napi_put_named_property(_this, this, prop->name, prop->value);
	    }
	    PROCEDURE_SET(proc, i + 1, BUNSPEC);
	 } else {
	    // instance property
	    PROCEDURE_SET(proc, i + 1, MAKE_PAIR((obj_t)prop, napi_make_prop_desc(_this, this, prop)));
	 }
      }
      
      *result = this;
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_create_double ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_double(napi_env _this, double val, napi_value *result) {
   const unsigned long uv = (unsigned long)val;
   if (trunc(val) == val) {
      long lval = (long)val;

#if (BGL_LONG_MAX > (1L << 53))
#  define MAX_JS_INT (1L << 53)
#else      
#  define MAX_JS_INT BGL_LONG_MAX
#endif
#  define MIN_JS_INT -MAX_JS_INT
      
      if (lval <= MAX_JS_INT && lval >= (MIN_JS_INT)) {
	 *result = BINT((long)val);
      } else {
	 *result = DOUBLE_TO_REAL(val);
      }
   } else {
      *result = DOUBLE_TO_REAL(val);
   }
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_define_properties ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_define_properties(napi_env _this, napi_value this, size_t count, const napi_property_descriptor *properties) {
   if (!_this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!properties) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      while (count-- > 0) {
	 napi_property_descriptor *prop = (napi_property_descriptor *)properties;

	 napi_prepare_property(_this, this, prop);

	 obj_t desc = napi_make_prop_desc(_this, this, prop);

	 if (desc) {
	    bgl_napi_define_named_property(_this, this, prop->name, desc);
	 } else {
	    bgl_napi_put_named_property(_this, this, prop->name, prop->value);
	 }
      
/*       obj_t name = properties->name ? properties->name : string_to_bstring((char *)properties->utf8name); */
/*                                                                     */
/*       bgl_napi_define_property(_this, this, name, bgl_napi_method_to_procedure(_this, this, properties->method || properties->value, properties->data)); */
	 properties++;
      }
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_get_cb_info ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_cb_info(napi_env _this, napi_callback_info info, size_t *argc, napi_value *argv, napi_value *this_arg, void **data) {
   int i = 0;

   if (argv) {
      if (argc) {
	 int max = *argc;
      loop:
	 if (i < max) {
	    if (info[i + 2] != BEOA) {
	       argv[i] = info[i + 2];
	       i++;
	       goto loop;
	    } else {
	       *argc = i;
	       while (i < max) {
		  argv[i++] = BUNSPEC;
	       }
	    }
	 }
      } else {
	 while(info[i + 2] != BEOA) {
	    argv[i] = info[i + 2];
	    i++;
	 }
	 *argc = i;
      }
   }

   if (this_arg) {
      *this_arg = info[1];
   }

   if (data) {
      *data = info[0];
   }
      
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_get_prototype ...                                           */
/*---------------------------------------------------------------------*/
napi_status
napi_get_prototype(napi_env env, napi_value this, napi_value* result) {
   if (!env) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      *result = BGL_OBJECT_WIDENING(this);
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    argv_to_list ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
argv_to_list(size_t argc, napi_value *argv) {
   obj_t res = BNIL;

   while (argc-- > 0) {
      res = MAKE_PAIR(argv[argc], res);
   }
   return res;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_napi_call_function ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_napi_call_function(napi_env _this, obj_t this, obj_t fun, size_t argc, napi_value *argv) {
   switch (argc) {
      case 0: return hop_js_call0(_this, fun, this);
      case 1: return hop_js_call1(_this, fun, this, argv[0]);
      case 2: return hop_js_call2(_this, fun, this, argv[0], argv[1]);
      case 3: return hop_js_call3(_this, fun, this, argv[0], argv[1], argv[2]);
      case 4: return hop_js_call4(_this, fun, this, argv[0], argv[1], argv[2], argv[3]);
      case 5: return hop_js_call5(_this, fun, this, argv[0], argv[1], argv[2], argv[3], argv[4]);
      case 6: return hop_js_call6(_this, fun, this, argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
      case 7: return hop_js_call7(_this, fun, this, argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
      case 8: return hop_js_call8(_this, fun, this, argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
      case 9: return hop_js_call9(_this, fun, this, argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
      case 10: return hop_js_call10(_this, fun, this, argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9]);
      default: return hop_js_calln(_this, fun, this, argv_to_list(argc, argv));
   }
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    bgl_napi_call_function ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
bgl_napi_call_function_res(napi_env _this, obj_t this, obj_t fun, size_t argc, napi_value *argv, napi_value *res) {
   napi_value r = bgl_napi_call_function(_this, this, fun, argc, argv);
   if (res) *res = r;
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    napi_work_complete ...                                           */
/*---------------------------------------------------------------------*/
static void
napi_work_complete(obj_t env) {
   PROCEDURE_VA_ENTRY(env)(PROCEDURE_REF(env, 0), CINT(PROCEDURE_REF(env, 1)), PROCEDURE_REF(env, 2));
}


/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_create_async_work ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_async_work(napi_env _this,
		       napi_value async_resource,
		       napi_value async_resource_name,
		       napi_async_execute_callback execute,
		       napi_async_complete_callback complete,
		       void* data,
		       napi_async_work* result) {
   napi_async_work work = (napi_async_work)malloc(sizeof(struct napi_async_work__));
   
   work->env = _this;
   work->execute = execute;
   work->complete = complete;
   work->data = data;
   work->started = 0;
   
   *result = work;

   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    napi_work_cb ...                                                 */
/*---------------------------------------------------------------------*/
void
napi_work_cb(uv_work_t *req) {
   napi_async_work work = req->data;
	   
   work->started = 1;
   work->execute(work->env, work->data);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    napi_after_work_cb ...                                           */
/*---------------------------------------------------------------------*/
void
napi_after_work_cb(uv_work_t *req, int status) {
   napi_async_work work = req->data;

   work->complete(work->env, napi_ok, work->data);
   free(req);
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_queue_async_work ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_queue_async_work(napi_env _this, napi_async_work work) {
   uv_work_t *req = malloc(sizeof(uv_work_t));
   uv_loop_t *loop = bgl_napi_uvloop(_this);

   req->data = work;
   
   uv_queue_work(loop, req, napi_work_cb, napi_after_work_cb);
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_cancel_async_work ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_cancel_async_work(napi_env env, napi_async_work work) {
   if (work->started) {
      return napi_generic_failure;
   } else {
      work->complete(work->env, napi_cancelled, work->data);
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_bool ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_bool(napi_env _this, napi_value value, bool *res) {
   if (!_this || !value || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (BOOLEANP(value)) {
      *res = CBOOL(value);
      return napi_ok;
   } else {
      napi_last_error_message = "A boolean was expected";
      return napi_boolean_expected;
   }
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_date_value ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_date_value(napi_env _this, napi_value value, double *res) {
   if (!_this || !value || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (bgl_napi_is_date(value)) {
      *res = bgl_napi_get_date_value(value);
      return napi_ok;
   } else {
      napi_last_error_message = "A date was expected";
      return napi_boolean_expected;
   }
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_int32 ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_int32(napi_env _this, napi_value value, int32_t *res) {
   if (!_this || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (INTEGERP(value)) {
      long d = CINT(value);
      
      long l = (((long) d) & ((1L << 32) - 1));
      *res = (int32_t)CINT(value);
      return napi_ok;
   } else if (REALP(value)) {
      double d = REAL_TO_DOUBLE(value);

      if (!isnormal(d)) {
	 *res = 0;
      } else {
	 long l = (((long) d) & ((1L << 32) - 1));
	 *res = (int32_t)l;
      }
      return napi_ok;
   } else {
      napi_last_error_message = "A number was expected";
      return napi_number_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_uint32 ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_uint32(napi_env _this, napi_value value, uint32_t *res) {
   if (!_this || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (INTEGERP(value)) {
      long d = CINT(value);
      
      long l = (((long) d) & ((1L << 32) - 1));
      *res = (uint32_t)CINT(value);
      return napi_ok;
   } else if (REALP(value)) {
      double d = REAL_TO_DOUBLE(value);
      
      if (!isnormal(d)) {
	 *res = 0;
      } else {
	 long l = (((long) d) & ((1L << 32) - 1));
	 *res = (uint32_t)l;
      }
      return napi_ok;
   } else {
      napi_last_error_message = "A number was expected";
      return napi_number_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_int64 ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_int64(napi_env _this, napi_value value, int64_t *res) {
   if (!_this || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (INTEGERP(value)) {
      *res = CINT(value);
      return napi_ok;
   } else if (REALP(value)) {
      double d = REAL_TO_DOUBLE(value);

      if (!isnormal(d)) {
	 *res = 0;
      } else if (d >= INT64_MAX_D) {
	 *res = INT64_MAX;
      } else if (d <= INT64_MIN_D) {
	 *res = INT64_MIN;
      } else {
	 *res = (int64_t)d;
      }
      
      return napi_ok;
   } else {
      napi_last_error_message = "A number was expected";
      return napi_number_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_uint64 ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_uint64(napi_env _this, napi_value value, uint64_t *res) {
   if (!_this || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (INTEGERP(value)) {
      *res = CINT(value);
      return napi_ok;
   } else if (REALP(value)) {
      *res = REAL_TO_DOUBLE(value);
      return napi_ok;
   } else {
      napi_last_error_message = "A number was expected";
      return napi_number_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_double ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_double(napi_env _this, napi_value value, double *res) {
   if (!_this || !res) {      
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (INTEGERP(value)) {
      *res = (double)CINT(value);
      return napi_ok;
   } else if (REALP(value)) {		   
      *res = REAL_TO_DOUBLE(value);
      return napi_ok;
   } else {
      napi_last_error_message = "A number was expected";
      return napi_number_expected;
   }
}
   
/*---------------------------------------------------------------------*/
/*    bool                                                             */
/*    BXALLOC ...                                                      */
/*---------------------------------------------------------------------*/
#if (BGL_HAVE_GMP)
#  define BXALLOC(x) (BIGNUM(x).mpz._mp_alloc)
#endif

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_bigint_int64 ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_bigint_int64(napi_env _this, napi_value value, int64_t *res, bool *lossless) {
#if (BGL_HAVE_GMP)
   if (BIGNUMP(value)) {
      *res = bgl_bignum_to_int64(value);
      *lossless = BXALLOC(value) < 4;
      return napi_ok;
   } else {
      return napi_bigint_expected;
   }
#else
   return napi_generic_failure;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_bigint_int64 ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_bigint_uint64(napi_env _this, napi_value value, uint64_t *res, bool *lossless) {
#if (BGL_HAVE_GMP)
   if (BIGNUMP(value)) {
      *res = bgl_bignum_to_uint64(value);
      *lossless = !BXNEGATIVE(value) && BXALLOC(value) < 4;
      return napi_ok;;
   } else {
      return napi_bigint_expected;
   }
#else
   return napi_generic_failure;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_bigint_words ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_bigint_words(napi_env env,
			    napi_value value,
			    int *sign_bit,
			    size_t *word_count,
			    uint64_t *words) {
#if (BGL_HAVE_GMP)
   if (BIGNUMP(value)) {
      if (sign_bit) *sign_bit = BXNEGATIVE(value);
      mpz_export((void *)words, word_count, 1, sizeof(int64_t), 0, 0, &(BIGNUM(value).mpz));
      return napi_ok;
   } else {
      return napi_bigint_expected;
   }
#else
   return napi_generic_failure;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_create_string_utf8 ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_string_utf8(napi_env env, const char *value, size_t size, napi_value *result) {
   if (!env || !value || !result) {
      if (!size && result && env) {
	 *result = bgl_napi_create_string_utf8(env, string_to_bstring(""));
	 napi_last_error_message = 0L;
	 return napi_last_error_code = napi_ok;
      } else {
	 napi_last_error_message = "Invalid argument";
	 return napi_last_error_code = napi_invalid_arg;
      }
   } else {
      obj_t str = NAPI_AUTO_LENGTH == size
	 ? string_to_bstring((char *)value)
	 : string_to_bstring_len((char *)value, size);
      *result = bgl_napi_create_string_utf8(env, str);
      
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_create_string_utf16 ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_string_utf16(napi_env env, const char16_t *value, size_t size, napi_value *result) {
   if (!env || !value || !result) {
      if (!size && result && env) {
	 *result = bgl_napi_create_string_utf8(env, string_to_bstring(""));
	 napi_last_error_message = 0L;
	 return napi_last_error_code = napi_ok;
      } else {
	 napi_last_error_message = "Invalid argument";
	 return napi_last_error_code = napi_invalid_arg;
      }
   } else {
      obj_t str = ucs2_to_utf8_string((ucs2_t *)value, size);
      *result = bgl_napi_create_string_utf8(env, str);
      
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_create_string_latin1 ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_string_latin1(napi_env env, const char *value, size_t size, napi_value *result) {
   if (!env || !value || !result) {
      if (!size && result && env) {
	 *result = bgl_napi_create_string_utf8(env, string_to_bstring(""));
	 napi_last_error_message = 0L;
	 return napi_last_error_code = napi_ok;
      } else {
	 napi_last_error_message = "Invalid argument";
	 return napi_last_error_code = napi_invalid_arg;
      }
   } else {
      obj_t str = NAPI_AUTO_LENGTH == size
	 ? string_to_bstring((char *)value)
	 : string_to_bstring_len((char *)value, size);
      *result = bgl_napi_create_string_latin1(env, str);

      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    utf8_char_size ...                                               */
/*---------------------------------------------------------------------*/
static int
utf8_char_size(unsigned char c) {
   static int utf8_sizes[] = {1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 4};
   return utf8_sizes[c >> 4];
}

/*---------------------------------------------------------------------*/
/*    BG:_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_string_utf8 ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_string_utf8(napi_env env, napi_value value, char *buf, size_t bufsize, size_t *result) {
   if (!env || !value) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!bgl_napi_jsstringp(value)) {
      napi_last_error_message = "A string was expected";
      return napi_last_error_code = napi_string_expected;
   } else if (!buf && bufsize > 0) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      obj_t str = bgl_napi_jsstring_to_string(value);

      if (STRING_LENGTH(str) < ((long)bufsize - 1)) {
	 if (result) *result = STRING_LENGTH(str);
	 if (buf) {
	    strcpy(buf, BSTRING_TO_STRING(str));
	 }
      } else {
	 // compute the size to be copie
	 long i = 0, size = 0;
	 unsigned char *cstr = (unsigned char *)BSTRING_TO_STRING(str);
	 
	 while (1) {
	    int s = utf8_char_size(cstr[i]);
	    if (size + s < bufsize) {
	       size += s;
	       i += s;
	    } else {
	       break;
	    }
	 }
	 
	 if (buf) {
	    if (result) *result = size;
	    strncpy(buf, (const char *)cstr, size);
	    buf[size] - 0;
	 } else {
	    if (result) *result = STRING_LENGTH(str);
	 }
      }
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_get_value_string_utf6_from_ascii ...                        */
/*---------------------------------------------------------------------*/
napi_status
napi_get_value_string_utf6_from_ascii(napi_env env, obj_t str, char16_t *buf, size_t bufsize, size_t *result) {
   char *ptr = BSTRING_TO_STRING(str);
   long len = STRING_LENGTH(str);

   if (!buf) {
      if (result) *result = len;
   } else {
      long sz = len < bufsize - 1 ? len : bufsize - 1;
      long read;
	    
      if (result) *result = sz;

      for (read = 0; read < sz; read++) {
	 buf[read] = ptr[read];
      }
      buf[read] = 0;
   }
      
   napi_last_error_message = 0L;
   return napi_ok;
}
   
/*---------------------------------------------------------------------*/
/*    napi_status                                                      */
/*    napi_get_value_string_utf16_from_ascii ...                       */
/*---------------------------------------------------------------------*/
napi_status
napi_get_value_string_utf16_from_utf8(napi_env env, BgL_jsstringliteralutf8z00_bglt value, obj_t str, char16_t *buf, size_t bufsize, size_t *result) {
   extern long BGl_utf8zd2stringzd2lengthz00zz__unicodez00(obj_t);
   char *ptr = BSTRING_TO_STRING(str);
   
   if (!buf) {
      if (result) {
	 uint32_t len = value->BgL_z52culenz52;
	 *result = len > 0 ? len : BGl_utf8zd2stringzd2lengthz00zz__unicodez00(str);
      }
   } else {
      long read, write;
      long len = STRING_LENGTH(str);
      
      for (read = 0, write = 0; (write < bufsize - 1) && (read < len); write++) {
	 unsigned char byte = ptr[read++];

	 if (byte <= 0x7f) {
	    buf[write] = (char16_t)byte;
	 } else if (byte == 0xf8) {
	    char b1 = ptr[read++];
	    char b2 = ptr[read++];
	    char b3 = ptr[read++];
	    int u4u3 = (b3 & 0x3) << 2;
	    int xx = (b2 >> 4) & 0x3;
	    int wwww = b1 & 0xf;
	    int u2u1 = (b1 >> 4) & 0x3;
	    int uuuu = u4u3 | u2u1;
	    int vvvv = uuuu -1;
	    int hi = 0x36;
	 
	    buf[write] = (char16_t)(xx | (wwww << 2) | (vvvv << 6) | (hi << 10));
	 } else if (byte == 0xfc) {
	    char b1 = ptr[read++];
	    char b2 = ptr[read++];
	    char b3 = ptr[read++];
	    int zzzzzz = b3 & 0x3f;
	    int yyyy = b2 & 0xf;
	    int hi = 0x37;
	 
	    buf[write] = (char16_t)(zzzzzz | (yyyy << 6) | (hi << 10));
	 } else {
	    unsigned long ucs2 = (unsigned long)byte;
	    int bits = 6;

	    while (byte & 0x40) {
	       unsigned char next = ptr[read++];
               
	       ucs2 = (ucs2 << 6) + (next & 0x3f);
	       byte <<= 1;
	       bits += 5;
	    }

	    ucs2 &= (1<<bits) - 1;

	    if (ucs2 >= 0x10000) {
	       ucs2 -= 0x10000;
	       buf[write++] = (char16_t)((ucs2 >> 10) + 0xd800);
	       buf[write] = (char16_t)((ucs2 & 0x3FF) + 0xdc00);
	    } else {
	       buf[write] = (char16_t)ucs2;
	    }
	 }
      }

      buf[write] = 0;
      if (result) *result = write;
   }
   napi_last_error_message = 0L;
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BG:_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_string_utf16 ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_string_utf16(napi_env env, napi_value value, char16_t *buf, size_t bufsize, size_t *result) {
   if (!env || !value) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!bgl_napi_jsstringp(value)) {
      napi_last_error_message = "A string was expected";
      return napi_last_error_code = napi_string_expected;
   } else if (!buf && bufsize > 0) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      obj_t str = bgl_napi_jsstring_to_string(value);
      
      if (bgl_napi_jsstring_asciip(value)) {
	 return napi_get_value_string_utf6_from_ascii(env, str, buf, bufsize, result);
      } else {
	 return napi_get_value_string_utf16_from_utf8(env, (BgL_jsstringliteralutf8z00_bglt)value, str, buf, bufsize, result);
      }
   }
}
	 
/*---------------------------------------------------------------------*/
/*    BG:_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_string_latin1 ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_string_latin1(napi_env env, napi_value value, char *buf, size_t bufsize, size_t *result) {
   if (!env || !value) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!bgl_napi_jsstringp(value)) {
      napi_last_error_message = "A string was expected";
      return napi_last_error_code = napi_string_expected;
   } else if (!buf && bufsize > 0) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      obj_t str = bgl_napi_jsstring_to_string_latin1(value);

      if (STRING_LENGTH(str) < ((long)bufsize - 1)) {
	 if (result) *result = STRING_LENGTH(str);
	 if (buf) {
	    strcpy(buf, BSTRING_TO_STRING(str));
	 }
      } else {
	 if (buf) {
	    if (result) *result = bufsize - 1;
	    strncpy(buf, BSTRING_TO_STRING(str), bufsize - 1);
	    buf[bufsize - 1] = 0;
	 } else {
	    if (result) *result = STRING_LENGTH(str);
	 }
      }
      
      napi_last_error_message = 0L;
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    make_bignum ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
make_bignum(size_t sz) {
#if (BGL_HAVE_GMP)
#define BIGNUM_ALLOC_SIZE(sz) \
   (BIGNUM_SIZE + ((sz) * sizeof(mp_limb_t)))

   obj_t o = GC_MALLOC_ATOMIC(BIGNUM_ALLOC_SIZE(sz));
   
   o->bignum.header = MAKE_HEADER(BIGNUM_TYPE, 0);
   o->bignum.mpz._mp_d = (mp_limb_t *)&(o->bignum.mp_d);
   o->bignum.mpz._mp_alloc = sz;

   return BREF(o);
#else   
   return BFALSE;
#endif
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_create_bigint_words ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_create_bigint_words(napi_env env,
			 int sign_bit,
			 size_t word_count,
			 const uint64_t* words,
			 napi_value* result) {
#if (BGL_HAVE_GMP)
   if (!word_count) {
      *result = bgl_int64_to_bignum((int64_t)0);
   } else if (word_count > 100) {
      bgl_napi_throw_range_error(env, "napi_create_bigint_words", "size too large");
      return napi_invalid_arg;
   } else {
      obj_t x = make_bignum(sizeof(uint64_t) * word_count);
      mpz_import(&(BIGNUM(x).mpz), word_count, 1, sizeof(int64_t), 0, 0, words);

      if (sign_bit) {
	 mpz_neg(&(BIGNUM(x).mpz), &(BIGNUM(x).mpz));
      }
      *result = x;
   }
   return napi_ok;
#else
   return napi_bigint_expected;
#endif
}


/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_last_error_info ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_last_error_info(napi_env env, const napi_extended_error_info **result) {
   napi_extended_error_info *ei = (napi_extended_error_info *)GC_MALLOC(sizeof(napi_extended_error_info));
   *result = ei;
   ei->error_code = napi_last_error_code;
   ei->error_message = napi_last_error_message;
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_is_exception_pending ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_is_exception_pending(napi_env env, bool *result) {
   *result = 0;
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_coerce_to_bool ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_coerce_to_bool(napi_env env, napi_value value, napi_value *result) {
   if (!env || !result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      const obj_t v = (napi_value)bgl_napi_coerce_to_bool(env, value);
      *result = v;
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_coerce_to_number ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_coerce_to_number(napi_env env, napi_value value, napi_value *result) {
   if (!env || !result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      const obj_t v = (napi_value)bgl_napi_coerce_to_number(env, value);
      *result = v;
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_coerce_to_object ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_coerce_to_object(napi_env env, napi_value value, napi_value *result) {
   if (!env || !result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      const obj_t v = (napi_value)bgl_napi_coerce_to_object(env, value);
      *result = v;
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_coerce_to_string ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_coerce_to_string(napi_env env, napi_value value, napi_value *result) {
   if (!env || !result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      const obj_t v = (napi_value)bgl_napi_coerce_to_string(env, value);
      *result = v;
      napi_last_error_message = 0L;
      return napi_last_error_code = napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_wrap ...                                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_wrap(napi_env env,
	  napi_value js_object,
	  void *native_object,
	  napi_finalize finalize_cb,
	  void *finalize_hint,
	  napi_ref *result) {
   if (result) {
      *result = (napi_ref)bgl_napi_wrap(env, js_object, native_object);
   } else {
      bgl_napi_wrap(env, js_object, (obj_t)native_object);
   }
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_unwrap ...                                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_unwrap(napi_env env,
	    napi_value js_object,
	    void **result) {
   *result = (void *)bgl_napi_unwrap(env, js_object);
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_remove_wrap ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_remove_wrap(napi_env env,
		 napi_value js_object,
		 void **result) {
   *result = (void *)bgl_napi_remove_wrap(env, js_object);
   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_element ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_element(napi_env env, napi_value this, uint32_t index, napi_value *result) {
   if (!env) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      *result = bgl_napi_get_element(env, this, index);
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_set_element ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_set_element(napi_env env, napi_value this, uint32_t index, napi_value val) {
   if (!env) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      bgl_napi_set_element(env, this, index, val);
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_delete_element ...                                          */
/*---------------------------------------------------------------------*/
#undef napi_delete_element
BGL_RUNTIME_DEF napi_status
napi_delete_element(napi_env env, napi_value object, uint32_t index, bool *result) {
   bool res = bgl_napi_delete_element(env, object, index);
   
   if (result) *result = res;

   return napi_ok;
}
      
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_has_element ...                                             */
/*---------------------------------------------------------------------*/
#undef napi_has_element
BGL_RUNTIME_DEF napi_status
napi_has_element(napi_env env, napi_value object, uint32_t index, bool *result) {
   bool res = bgl_napi_has_element(env, object, index);
   
   if (result) *result = res;

   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_delete_property ...                                         */
/*---------------------------------------------------------------------*/
#undef napi_delete_property
BGL_RUNTIME_DEF napi_status
napi_delete_property(napi_env env, napi_value object, napi_value key, bool *result) {
   bool res = bgl_napi_delete_property(env, object, key);
   
   if (result) *result = res;

   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_has_property ...                                            */
/*---------------------------------------------------------------------*/
#undef napi_has_property
BGL_RUNTIME_DEF napi_status
napi_has_property(napi_env env, napi_value this, napi_value key, bool *result) {
   if (!env) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this || !key) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      *result = bgl_napi_has_property(env, this, key);

      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_has_named_property ...                                      */
/*---------------------------------------------------------------------*/
#undef napi_has_named_property
BGL_RUNTIME_DEF napi_status
napi_has_named_property(napi_env env, napi_value object, const char *name, bool *result) {
   bool res = bgl_napi_has_property(env, object, string_to_bstring((char *)name));
   
   if (result) *result = res;

   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_has_own_property ...                                        */
/*---------------------------------------------------------------------*/
#undef napi_has_own_property
BGL_RUNTIME_DEF napi_status
napi_has_own_property(napi_env env, napi_value object, napi_value key, bool *result) {
   bool res = bgl_napi_has_own_property(env, object, key);
   
   if (result) *result = res;

   return napi_ok;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_property_names ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_property_names(napi_env env, napi_value this, napi_value *result) {
   if (!env) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      *result = bgl_napi_get_property_names(env, this);
      return napi_ok;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_all_property_names ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_all_property_names(napi_env env, napi_value this, napi_key_collection_mode key_mode, napi_key_filter key_filter, napi_key_conversion key_conversion, napi_value *result) {
   if (!env) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!this) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else if (!result) {
      napi_last_error_message = "Invalid argument";
      return napi_last_error_code = napi_invalid_arg;
   } else {
      *result = bgl_napi_get_all_property_names(env, this, key_mode, key_filter, key_conversion);
      return napi_ok;
   }
}
