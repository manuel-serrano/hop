;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scheme2js_makelib

   (option (set! *dlopen-init* "scheme2js_e"))
   
   (import scheme2js
	   expand)

   (eval   (export-all)))
