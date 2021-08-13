;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/newtarget.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 27 18:53:16 2018                          */
;*    Last change :  Fri Aug 13 07:38:56 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Handling ECMAScrip 6 "new.target" meta construct.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_newtarget

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_alpha)

   (export j2s-newtarget-stage))

;*---------------------------------------------------------------------*/
;*    j2s-newtarget-stage                                              */
;*---------------------------------------------------------------------*/
(define j2s-newtarget-stage
   (instantiate::J2SStageProc
      (name "newtarget")
      (comment "Scheme new.target handling")
      (proc j2s-newtarget)))

;*---------------------------------------------------------------------*/
;*    j2s-newtarget ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-newtarget this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls)
	 (map! (lambda (n) (newtarget! n #f)) decls)
	 (map! (lambda (n) (newtarget! n #f)) nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SNode fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SPragma ...                                       */
;*    -------------------------------------------------------------    */
;*    See also scheme/scheme-class.scm.                                */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SPragma fun)
   (if (j2s-new-target? this)
       (if fun
	   (with-access::J2SFun fun (new-target)
	      (set! new-target 'global)
	      this)
	   (with-access::J2SPragma this (loc)
	      (J2SUndefined)))
       this))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SFun fun)
   (with-access::J2SFun this (body new-target)
      (when (eq? new-target 'unknown)
	 (set! body (newtarget! body this)))
      this))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SArrow ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SArrow fun)
   (with-access::J2SFun this (%info body new-target)
      (set! new-target 'no)
      (set! body (newtarget! body fun))
      this))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SDeclFun fun)
   (with-access::J2SDeclFun this (val)
      (let ((f (if (isa? val J2SFun)
		     val
		     (with-access::J2SMethod val (function)
			function))))
	 (cond
	    ((isa? f J2SArrow)
	     ;; arrow functions inherit new.target
	     (with-access::J2SFun f (body)
		(set! body (newtarget! body fun))))
	    ((not (decl-usage-has? this '(assig new ref get set)))
	     (with-access::J2SFun f (body)
		(set! body (newtarget! body #f))))
	    (else
	     (newtarget! f f)))))
   this)
   
;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SClass ...                                        */
;*    -------------------------------------------------------------    */
;*    Force a new-target insertion for class constructor as it will    */
;*    be used to check that ctor are only called with new.             */
;*---------------------------------------------------------------------*/
;* (define-walk-method (newtarget! this::J2SClass fun)                 */
;*                                                                     */
;*    (define (constructor? prop::J2SDataPropertyInit)                 */
;*       (with-access::J2SDataPropertyInit prop (name)                 */
;* 	 (let loop ((name name))                                       */
;* 	    (cond                                                      */
;* 	       ((isa? name J2SLiteralCnst)                             */
;* 		(with-access::J2SLiteralCnst name (val)                */
;* 		   (loop val)))                                        */
;* 	       ((isa? name J2SLiteralValue)                            */
;* 		(with-access::J2SLiteralValue name (val)               */
;* 		   (equal? val "constructor")))))))                    */
;*                                                                     */
;*    (define (find-constructor elements)                              */
;*       (find (lambda (m)                                             */
;* 	       (with-access::J2SClassElement m (prop static)           */
;* 		  (unless static                                       */
;* 		     (when (isa? prop J2SDataPropertyInit)             */
;* 			(constructor? prop)))))                        */
;* 	 elements))                                                    */
;*                                                                     */
;*    (call-default-walker)                                            */
;*                                                                     */
;*    (with-access::J2SClass this (elements)                           */
;*       (let ((ctor (find-constructor elements)))                     */
;* 	 (when ctor                                                    */
;* 	    (with-access::J2SClassElement ctor (prop)                  */
;* 	       (with-access::J2SDataPropertyInit prop (val)            */
;* 		  (with-access::J2SFun val (new-target body loc %info) */
;* 		     (set! new-target #t)))))))                        */
;*    this)                                                            */
