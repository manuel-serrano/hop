;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/use.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Sun May 25 18:45:00 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Count the number of occurrences for all variables                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_use

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-use-stage
	   (generic j2s-use! ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-use-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-use-stage
   (instantiate::J2SStageProc
      (name "use")
      (comment "Count the occurrence of all variables")
      (proc j2s-use!)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-use! ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (j2s-use! this)
   (use this))

;*---------------------------------------------------------------------*/
;*    j2s-use! ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-use! this::J2SProgram)
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (o)
		   (reset-decl o)
		   (use o))
	 nodes)))

;*---------------------------------------------------------------------*/
;*    use ::J2SNode ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (use this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    use ::J2SFun ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (use this::J2SFun)
   (with-access::J2SFun this (params body)
      (reset-decl this)
      (use body)))
   
;*---------------------------------------------------------------------*/
;*    use ::J2SRef ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (use this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (use)
	 (set! use (+fx 1 use))))
   this)

;*---------------------------------------------------------------------*/
;*    reset-decl ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-decl this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-decl ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-decl this::J2SDecl)
   (with-access::J2SDecl this (use)
      (set! use 0)))


