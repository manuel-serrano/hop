;*=====================================================================*/
;*    The module used to build the hopscheme heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_makelib
   
   (option (set! *dlopen-init* "bgl_dload_init_e_hopscheme"))
   
   (import hopscheme
	   tilde-escape)
   
   (eval   (export-all)))
