/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/_napi.c                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 24 14:34:24 2023                          */
/*    Last change :  Fri Mar 17 08:14:50 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop node_api implementation.                                     */
/*=====================================================================*/
#include <stdlib.h>
#include <pthread.h>
#include <bigloo.h>
#include <uv.h>
#include "node_api.h"

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

extern char *bgl_typeof();

extern uv_loop_t *bgl_napi_uvloop(obj_t);

static obj_t bgl_napi_method_to_procedure(napi_env _this, napi_value this, napi_callback met, void *data);

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
   obj_t optional;
   obj_t runner;
   long cnt = 0;
   obj_t *args;

   va_start(argl, proc);
   while ((runner = va_arg(argl, obj_t)) != BEOA) cnt++;
   va_end(argl);

   args = alloca((2 + 1 + cnt) * sizeof(obj_t));
   args[cnt] = 0;
   args[0] = PROCEDURE_LENGTH(proc) == 1 ? PROCEDURE_REF(proc, 0) : 0L;
   cnt = 1;

   va_start(argl, proc);

   while ((runner = va_arg(argl, obj_t)) != BEOA) {
      args[cnt++] = runner;
   }
   args[cnt] = BEOA;

   va_end(argl);

   return PROCEDURE_VA_ENTRY(proc)(PROCEDURE_ATTR(proc), args);
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
   if (prop->attributes & napi_default == napi_default && !prop->getter && !prop->setter) {
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
		     const char* utf8name,
		     size_t length,
		     napi_callback cb,
		     void* data,
		     napi_value* result) {
   *result = napi_callback_to_jsproc(_this, *result, cb, data, string_to_bstring((char *)utf8name));
   return napi_ok;
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
   if (trunc(val) == val) {
      long lval = (long)val;

#if (BGL_LONG_MAX > (1L << 53))
#  define MAX_JS_INT (1L << 53)
#else      
#  define MAX_JS_INT BGL_LONG_MAX
#endif
      
      if ((unsigned long)val < (unsigned long)(MAX_JS_INT)) {
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

   while (count-- > 0) {
      obj_t name = properties->name ? properties->name : string_to_bstring((char *)properties->utf8name);
      bgl_napi_define_property(_this, this, name, bgl_napi_method_to_procedure(_this, this, properties->method, properties->data));
      properties++;
   }
   return napi_ok;
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
	 while(info[i + 2] != BEOA && i < max) {
	    argv[i] = info[i + 2];
	    i++;
	 }
	 if (i < max) {
	    *argc = i;
	    while (i < max) {
	       argv[i++] = BUNSPEC;
	    }
	 } else {
	    *argc = i;
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
/*    obj_t                                                            */
/*    bgl_napi_call_function ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
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
   if (BOOLEANP(value)) {
      *res = (double)CBOOL(value);
      return napi_ok;
   } else {
      return napi_boolean_expected;
   }
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_int32 ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_int32(napi_env _this, napi_value value, int32_t *res) {
   if (INTEGERP(value)) {
      *res = CINT(value);
      return napi_ok;
   } else if (REALP(value)) {
      *res = REAL_TO_DOUBLE(value);
      return napi_ok;
   } else {
      return napi_number_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_double ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_double(napi_env _this, napi_value value, double *res) {
   if (INTEGERP(value)) {
      *res = (double)CINT(value);
      return napi_ok;
   } else if (REALP(value)) {		   
      *res = REAL_TO_DOUBLE(value);
      return napi_ok;
   } else {
      return napi_number_expected;
   }
}
   
/*---------------------------------------------------------------------*/
/*    bool                                                             */
/*    BXALLOC ...                                                      */
/*---------------------------------------------------------------------*/
#define BXALLOC(x) (BIGNUM(x).mpz._mp_alloc)

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_bigint_int64 ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_bigint_int64(napi_env _this, napi_value value, int64_t *res, bool *lossless) {
   if (BIGNUMP(value)) {
      *res = bgl_bignum_to_int64(value);
      *lossless = BXALLOC(value) < 4;
      return napi_ok;;
   } else {
      return napi_bigint_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF napi_status                                      */
/*    napi_get_value_bigint_int64 ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF napi_status
napi_get_value_bigint_uint64(napi_env _this, napi_value value, uint64_t *res, bool *lossless) {
   if (BIGNUMP(value)) {
      *res = bgl_bignum_to_uint64(value);
      *lossless = !BXNEGATIVE(value) && BXALLOC(value) < 4;
      return napi_ok;;
   } else {
      return napi_bigint_expected;
   }
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
   if (BIGNUMP(value)) {
#if (BGL_HAVE_GMP)
      if (sign_bit) *sign_bit = BXNEGATIVE(value);
      mpz_export((void *)words, word_count, 1, sizeof(int64_t), 0, 0, &(BIGNUM(value).mpz));
      return napi_ok;
#else
      return napi_invalid_arg;
#endif
   } else {
      return napi_bigint_expected;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    make_bignum ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
make_bignum(size_t sz) {
#define BIGNUM_ALLOC_SIZE(sz) \
   (BIGNUM_SIZE + ((sz) * sizeof(mp_limb_t)))

   obj_t o = GC_MALLOC_ATOMIC(BIGNUM_ALLOC_SIZE(sz));
   
   o->bignum.header = MAKE_HEADER(BIGNUM_TYPE, 0);
   o->bignum.mpz._mp_d = (mp_limb_t *)&(o->bignum.mp_d);
   o->bignum.mpz._mp_alloc = sz;

   return BREF(o);
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