;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/use.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Tue Dec 29 08:39:56 2015 (serrano)                */
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
	   (generic use-count ::J2SNode)
	   (generic usage ::J2SNode ::symbol)))

;*---------------------------------------------------------------------*/
;*    j2s-use-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-use-stage
   (instantiate::J2SStageProc
      (name "use")
      (comment "Usage property for all variables")
      (proc (lambda (n args) (j2s-use! n)))
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-use! ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (j2s-use! this)
   (use-count this))

;*---------------------------------------------------------------------*/
;*    j2s-use! ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-use! this::J2SProgram)
   
   (define (use-nodes nodes)
      (for-each (lambda (o)
		   (use-count o)
		   (usage o 'ref))
	 nodes))
   
   (with-access::J2SProgram this (nodes headers decls)
      (use-nodes headers)
      (use-nodes decls)
      (use-nodes nodes))
   this)

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
      (use-count body))
   this)
   
;*---------------------------------------------------------------------*/
;*    use ::J2SRef ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usecnt)
	 (set! usecnt (+fx 1 usecnt))))
   this)

;*---------------------------------------------------------------------*/
;*    use ::J2SDecl ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SDecl)
   (with-access::J2SDecl this (usecnt)
      (set! usecnt 0))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SNode ctx)
   (call-default-walker))
   
;*---------------------------------------------------------------------*/
;*    usage ::J2SRef ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SRef ctx)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usage)
	 (when ctx
	    (unless (memq ctx usage)
	       (set! usage (cons ctx usage))))))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SCall ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SCall ctx)
   (with-access::J2SCall this (fun args)
      (usage fun 'call)
      (for-each (lambda (a) (usage a 'ref)) args))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SAssign ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SAssig ctx)
   (with-access::J2SAssig this (lhs rhs)
      (usage lhs 'assig)
      (usage rhs 'ref))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SNew ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SNew ctx)
   (with-access::J2SNew this (clazz args)
      (usage clazz 'new)
      (for-each (lambda (a) (usage a 'ref)) args))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SLetBlock ctx)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d) (usage d 'init)) decls)
      (for-each (lambda (n) (usage n 'ref)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SDeclInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SDeclInit ctx)
   (with-access::J2SDeclInit this ((u usage) val)
      (usage val 'ref)
      (set! u (cons 'init u)))
   this)

;* {*---------------------------------------------------------------------*} */
;* {*    usage ::J2SLetInit ...                                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (usage this::J2SLetInit ctx)                    */
;*    (with-access::J2SLetInit this ((u usage) val)                    */
;*       (usage val 'ref)                                              */
;*       (set! u (cons 'init u)))                                      */
;*    this)                                                            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    usage ::J2SLetOpt ...                                            *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (usage this::J2SLetOpt ctx)                     */
;*    (with-access::J2SLetOpt this ((u usage) val)                     */
;*       (usage val 'ref)                                              */
;*       (set! u (cons 'init u)))                                      */
;*    this)                                                            */
;*                                                                     */
