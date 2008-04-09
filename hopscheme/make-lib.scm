;*=====================================================================*/
;*    The module used to build the hopscheme heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_makelib
   
   (option (set! *dlopen-init* "hopscheme_e"))
   
   (import hopscheme
	   tilde-escape
	   scm-compil)
   
   (eval   (export-all)))
