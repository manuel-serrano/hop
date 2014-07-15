;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 16:59:06 2013                          */
;*    Last change :  Fri Jul 11 06:54:19 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utility functions                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_utils

   (import __js2scheme_ast
	   __js2scheme_dump)

   (export (pass ::bstring)
	   (error/loc proc obj msg loc)
	   (illegal-node ::bstring ::J2SNode)
	   (config-get ::pair-nil ::keyword #!optional def)))

;*---------------------------------------------------------------------*/
;*    pass ...                                                         */
;*---------------------------------------------------------------------*/
(define (pass name)
   (print name))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc proc obj msg loc)
   (match-case loc
      ((at ?fname ?point)
       (error/location proc obj msg fname point))
      (else
       (error proc obj msg))))

;*---------------------------------------------------------------------*/
;*    illegal-node ...                                                 */
;*---------------------------------------------------------------------*/
(define (illegal-node pass this::J2SNode)
   (with-access::J2SNode this (loc)
      (error/loc pass
	 (format "~a should have been eliminated" (typeof this))
	 (j2s->list this)
	 loc)))

;*---------------------------------------------------------------------*/
;*    config-get ...                                                   */
;*---------------------------------------------------------------------*/
(define (config-get conf k #!optional def)
   (let ((l (memq k conf)))
      (if (pair? l)
	  (cadr l)
	  def)))
