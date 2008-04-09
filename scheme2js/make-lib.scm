;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scheme2js_makelib

   (option (set! *dlopen-init* "scheme2js_e"))
   
   (import scheme2js
	   (default-scheme2js-config config-set!
	      set-optim-level scheme2js-config
	      config)
	   expand)

   (eval   (export-all)))
