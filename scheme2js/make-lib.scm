;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scheme2js_makelib

   (option (set! *dlopen-init* "scheme2js_e"))
   
   (import scheme2js
	   (default-scheme2js-config
	      set-optim-level scheme2js-config
	      extend-config extend-config*
	      config)
	   expand
	   export-desc
	   module-system)

   (eval   (export-all)))
