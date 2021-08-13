;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/constrsize.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Fri Aug 13 16:23:02 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Static approximation of constructors size                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_constrsize

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-constrsize-stage))

;*---------------------------------------------------------------------*/
;*    j2s-constrsize-stage ...                                         */
;*---------------------------------------------------------------------*/
(define j2s-constrsize-stage
   (instantiate::J2SStageProc
      (name "constrsize")
      (comment "Constructor static size approximation")
      (proc j2s-constrsize!)))

;*---------------------------------------------------------------------*/
;*    j2s-constrsize! ::obj ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-constrsize! this args)
   (when (isa? this J2SProgram)
      (j2s-constrsize-program! this args)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-constrsize-program! ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-constrsize-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (n) (constrsize! n)) decls)
      (for-each (lambda (n) (constrsize! n)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SFun)
   (with-access::J2SFun this (body constrsize)
      (let ((acc (make-cell '())))
	 (count-this-assig body acc)
	 (set! constrsize (length (cell-ref acc))))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SClass ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SClass)
   
   (define (prop-names props)
      (filter-map (lambda (p)
		     (with-access::J2SDataPropertyInit p (name)
			(when (isa? name J2SString)
			   (with-access::J2SString name (val)
			      val))))
	 props))
   
   (call-default-walker)
   (let ((ctor (j2s-class-get-constructor this)))
      (when ctor
	 (with-access::J2SClassElement ctor (prop)
	    (with-access::J2SMethodPropertyInit prop (val)
	       (let ((props (j2s-class-instance-properties this)))
		  (with-access::J2SFun val (body)
		     (when (pair? props)
			(let ((acc (make-cell (prop-names props))))
			   (count-this-assig body acc)
			   (with-access::J2SClass this (constrsize)
			      (set! constrsize (length (cell-ref acc))))))))))))
   this)
   
;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SNode acc::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SAssig ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SAssig acc::cell)
   (with-access::J2SAssig this (lhs rhs)
      (call-default-walker)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj field)
	    (when (isa? obj J2SThis)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (unless (member val (cell-ref acc))
			(cell-set! acc (cons val (cell-ref acc)))))))))))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SCond ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SCond acc::cell)
   (with-access::J2SCond this (test then else)
      (count-this-assig test acc)
      (count-this-assig then acc)
      (count-this-assig else acc)))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SIf ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SIf acc::cell)
   (with-access::J2SIf this (test then else)
      (count-this-assig test acc)
      (count-this-assig then acc)
      (count-this-assig else acc)))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SCall ...                                   */
;*---------------------------------------------------------------------*/
(define-method (count-this-assig this::J2SCall acc::cell)
   (with-access::J2SCall this (protocol fun)
      (if (eq? protocol 'bounce)
	  (with-access::J2SRef fun (decl)
	     (with-access::J2SDeclFun decl (val)
		(with-access::J2SFun val (body)
		   (count-this-assig body acc))))
	  (call-next-method))))

