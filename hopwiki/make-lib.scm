;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/make-lib.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:02:51 2006                          */
;*    Last change :  Mon Apr  3 15:04:23 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The wiki library heap.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwiki_makelib
   
   (option (set! *dlopen-init* #t))
   
   (import __hopwiki_syntax)
   
   (eval   (class wiki-syntax)

	   (export-all)))
