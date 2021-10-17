;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/expanders.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 08:18:53 2017                          */
;*    Last change :  Sun Oct 17 09:09:19 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript expanders                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_expanders

   (option  (register-srfi! 'hop-eval))
   
   (library hop)
   
   (include "property_expd.sch"
	    "arithmetic.sch"
	    "array.sch"
	    "number.sch"
	    "call.sch"
	    "function.sch"
	    "arguments.sch"
	    "public_expd.sch"
	    "lib_expd.sch"
	    "stringliteral_expd.sch"
	    "types_expd.sch"
	    "constants_expd.sch"
	    "names_expd.sch"
	    "tls_expd.sch"
	    "expanders.sch")

   (import  __hopscript_types
	    __hopscript_object
	    __hopscript_error
	    __hopscript_private
	    __hopscript_public
	    __hopscript_worker
	    __hopscript_pair
	    __hopscript_obj
	    __hopscript_function
	    __hopscript_lib
	    __hopscript_property
	    __hopscript_stringliteral)

   (export  (hopscript-install-expanders!)))


