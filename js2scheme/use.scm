;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/use.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Sun Jun 28 07:45:25 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
	   (generic j2s-use! ::obj)
	   (generic use-count ::J2SNode)))

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
   (use-count this))

;*---------------------------------------------------------------------*/
;*    j2s-use! ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-use! this::J2SProgram)
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (o)
		   (use-reset o)
		   (use-count o))
	 nodes)))

;*---------------------------------------------------------------------*/
;*    use ::J2SNode ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    use ::J2SFun ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SFun)
   (with-access::J2SFun this (params body)
      (use-reset this)
      (use-count body)))
   
;*---------------------------------------------------------------------*/
;*    use ::J2SRef ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (use)
	 (set! use (+fx 1 use))))
   this)

;*---------------------------------------------------------------------*/
;*    use-reset ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (use-reset this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    use-reset ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (use-reset this::J2SDecl)
   (with-access::J2SDecl this (use)
      (set! use 0)))


