;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scheme2js_makelib

   (option (set! *dlopen-init* "bgl_dload_init_e_scheme2js"))
   
   (import scheme2js
	   expand)

   (eval   (export-all)))
