;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/newtarget.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 27 18:53:16 2018                          */
;*    Last change :  Fri Dec 28 07:20:30 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
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
;*    newtarget! ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SPragma fun)
   (with-access::J2SPragma this (lang expr loc)
      (if (and (eq? lang 'javascript) (equal? expr "new.target"))
	  (if fun
	      (with-access::J2SFun fun (%info)
		 (unless (isa? %info J2SDecl)
		    (set! %info
		       (J2SLetOpt '(ref) 'new-target
			  (J2SPragma
			     '(with-access::JsGlobalObject %this (js-new-target)
			       js-new-target)))))
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
	    (set! new-target #t)
	    (set! body
	       (J2SLetBlock (list %info)
		  (J2SStmtExpr
		     (J2SPragma
			'(with-access::JsGlobalObject %this (js-new-target)
			  (set! js-new-target (js-undefined)))))
		  body))))
      this))

;*---------------------------------------------------------------------*/
;*    newtarget! ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (newtarget! this::J2SDeclFun fun)
   (with-access::J2SDeclFun this (usage ronly val)
      (let ((fun (if (isa? val J2SFun)
		     val
		     (with-access::J2SMethod val (function)
			function))))
	 (if (and ronly (not (usage? '(new ref get set) usage)))
	     (with-access::J2SFun fun (body)
		(set! body (newtarget! body #f)))
	     (newtarget! fun fun))))
   this)
   
