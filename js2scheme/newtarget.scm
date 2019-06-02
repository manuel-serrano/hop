;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/newtarget.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 27 18:53:16 2018                          */
;*    Last change :  Sun Jun  2 06:22:13 2019 (serrano)                */
;*    Copyright   :  2018-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Handling ECMAScrip 6 "new.target" meta construct.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_newtarget

   (include "ast.sch")
   
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
;*    make-new-target-decl ...                                         */
;*---------------------------------------------------------------------*/
(define (make-new-target-decl loc)
   (let ((decl (J2SLetOptRo '(ref) 'new-target
		  (J2SPragma
		     '(with-access::JsGlobalObject %this (js-new-target)
		       (let ((nt js-new-target))
			  (set! js-new-target (js-undefined))
			  nt))))))
      (with-access::J2SDecl decl (_scmid)
	 (set! _scmid 'new-target))
      decl))

;*---------------------------------------------------------------------*/
;*    body-bind-new-target ...                                         */
;*---------------------------------------------------------------------*/
(define (body-bind-new-target body decl)
   (with-access::J2SNode body (loc)
      (J2SLetBlock (list decl)
	 body)))
   
;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SPragma ...                                       */
;*    -------------------------------------------------------------    */
;*    See also scheme/scheme-class.scm.                                */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SPragma fun)
   (with-access::J2SPragma this (lang expr loc)
      (if (and (eq? lang 'javascript) (equal? expr "new.target"))
	  (if fun
	      (with-access::J2SFun fun (%info)
		 (unless (isa? %info J2SDecl)
		    (set! %info (make-new-target-decl loc)))
		 (J2SRef %info))
	      (J2SUndefined))
	  this)))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SFun fun)
   (with-access::J2SFun this (%info body)
      (set! %info #f)
      (set! body (newtarget! body this))
      (when %info
	 (with-access::J2SFun this (loc new-target)
	    (set! new-target %info)
	    (set! body (body-bind-new-target body %info))))
      this))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SArrow ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SArrow fun)
   (with-access::J2SFun this (%info body)
      (set! body (newtarget! body fun))
      this))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SDeclFun fun)
   (with-access::J2SDeclFun this (ronly val)
      (let ((fun (if (isa? val J2SFun)
		     val
		     (with-access::J2SMethod val (function)
			function))))
	 (if (and ronly (not (decl-usage? this '(new ref get set))))
	     (with-access::J2SFun fun (body)
		(set! body (newtarget! body #f)))
	     (newtarget! fun fun))))
   this)
   
;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SClass ...                                        */
;*    -------------------------------------------------------------    */
;*    Force a new-target insertion for class constructor as it will    */
;*    be used to check that ctor are only called with new.             */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SClass fun)

   (define (constructor? prop::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit prop (name)
	 (let loop ((name name))
	    (cond
	       ((isa? name J2SLiteralCnst)
		(with-access::J2SLiteralCnst name (val)
		   (loop val)))
	       ((isa? name J2SLiteralValue)
		(with-access::J2SLiteralValue name (val)
		   (equal? val "constructor")))))))
   
   (define (find-constructor elements)
      (find (lambda (m)
	       (with-access::J2SClassElement m (prop static)
		  (unless static
		     (when (isa? prop J2SDataPropertyInit)
			(constructor? prop)))))
	 elements))

   (call-default-walker)

   (with-access::J2SClass this (elements)
      (let ((ctor (find-constructor elements)))
	 (when ctor
	    (with-access::J2SClassElement ctor (prop)
	       (with-access::J2SDataPropertyInit prop (val)
		  (with-access::J2SFun val (new-target body loc)
		     (unless new-target
			(set! new-target (make-new-target-decl loc))
			(set! body (body-bind-new-target body new-target)))))))))
   
   this)
