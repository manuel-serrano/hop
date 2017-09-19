;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/constrsize.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Mon Sep 18 04:17:22 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Static approximation of constructors sizes                       */
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
      (let ((acc (make-cell 0)))
	 (count-this-assig body acc)
	 (set! constrsize (cell-ref acc)))
      (call-default-walker)))

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
	 (with-access::J2SAccess lhs (obj)
	    (when (isa? obj J2SThis)
	       (cell-set! acc (+fx 1 (cell-ref acc))))))))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SCond ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SCond acc::cell)
   (with-access::J2SCond this (test then else)
      (count-this-assig test acc)
      (let ((athen (make-cell 0))
	    (aelse (make-cell 0)))
	 (count-this-assig then athen)
	 (count-this-assig else aelse)
	 (cell-set! acc
	    (+fx (cell-ref acc) (max (cell-ref athen) (cell-ref aelse)))))))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SIf ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SIf acc::cell)
   (with-access::J2SIf this (test then else)
      (count-this-assig test acc)
      (let ((athen (make-cell 0))
	    (aelse (make-cell 0)))
	 (count-this-assig then athen)
	 (count-this-assig else aelse)
	 (cell-set! acc
	    (+fx (cell-ref acc) (max (cell-ref athen) (cell-ref aelse)))))))

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
