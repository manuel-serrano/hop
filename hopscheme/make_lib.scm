;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/hopscheme/make_lib.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :                                                    */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    THe module used to build the hopscheme heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_makelib
   
   (import __hopscheme
	   (init-hopscheme! __hopscheme_config))
   
   (eval   (export-all)))
