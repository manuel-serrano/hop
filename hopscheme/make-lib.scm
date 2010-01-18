;*=====================================================================*/
;*    The module used to build the hopscheme heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_makelib
   
   (import __hopscheme
	   __hopscheme_tilde-escape
	   __hopscheme_scm-compil
	   __hopscheme_config
	   (replace-dollars! __hopscheme_dollar-escape))
   
   (eval   (export-all)))
