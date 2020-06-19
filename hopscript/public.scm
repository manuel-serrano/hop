;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/public.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Mon Jun  8 07:07:29 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Public (i.e., exported outside the lib) hopscript functions      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_public

   (option (set! *compiler-debug-trace* 0))

   (include "types.sch" "stringliteral.sch" "property.sch" "array.sch")
   
   (library hop js2scheme)
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_error
	   __hopscript_string
	   __hopscript_boolean
	   __hopscript_number
	   __hopscript_property
	   __hopscript_private
	   __hopscript_worker
	   __hopscript_array
	   __hopscript_json
	   __hopscript_proxy)

   (with   __hopscript_stringliteral
           __hopscript_expanders)

   (export (js-init-public! %this::JsGlobalObject)
	   (js-new ::JsGlobalObject f . args)
	   (js-new/debug ::JsGlobalObject loc f . args)
	   (js-new0 ::JsGlobalObject f)
	   (js-new1 ::JsGlobalObject f a0)
	   (js-new2 ::JsGlobalObject f a0 a1)
	   (js-new3 ::JsGlobalObject f a0 a1 a2)
	   (js-new4 ::JsGlobalObject f a0 a1 a2 a3)
	   (js-new5 ::JsGlobalObject f a0 a1 a2 a3 a4)
	   (js-new6 ::JsGlobalObject f a0 a1 a2 a3 a4 a5)
	   (js-new7 ::JsGlobalObject f a0 a1 a2 a3 a4 a5 a6)
	   (js-new8 ::JsGlobalObject f a0 a1 a2 a3 a4 a5 a6 a8)

	   (js-new-return::JsObject ::JsFunction ::obj ::obj)
	   (inline js-new-return-fast::JsObject ::JsFunction ::JsObject)
	   
	   (js-new-sans-construct ::JsGlobalObject f)

	   (inline js-object-alloc ::JsGlobalObject ::JsFunction)
	   (inline js-object-alloc-fast ::JsGlobalObject ::JsFunction)
	   (js-object-alloc-lazy ::JsGlobalObject ::JsFunction)
	   (inline js-object-alloc/new-target ::JsGlobalObject ::JsFunction)
	   (inline js-no-alloc ::JsGlobalObject ::JsFunction)
	   (js-not-a-constructor-alloc ::JsGlobalObject ::JsFunction)
	   
	   (inline js-function-set-constrmap!::JsFunction ::JsFunction)
	   
	   (js-call0% ::JsGlobalObject ::JsProcedure ::procedure this)
	   (js-call1% ::JsGlobalObject ::JsProcedure ::procedure this a0)
	   (js-call2% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1)
	   (js-call3% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2)
	   (js-call4% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3)
	   (js-call5% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3 a4)
	   (js-call6% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3 a4 a5)
	   (js-call7% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-call9% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (js-call10% ::JsGlobalObject ::JsProcedure ::procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   (js-calln% ::JsGlobalObject ::JsProcedure ::procedure this ::pair-nil)
	   
	   (inline js-call0-jsprocedure ::JsGlobalObject ::JsProcedure this)
	   (inline js-call1-jsprocedure ::JsGlobalObject ::JsProcedure this a0)
	   (inline js-call2-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1)
	   (inline js-call3-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2)
	   (inline js-call4-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3)
	   (inline js-call5-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3 a4)
	   (inline js-call6-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3 a4 a5)
	   (inline js-call7-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3 a4 a5 a6)
	   (inline js-call8-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3 a4 a5 a6 a7)
	   (inline js-call9-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (inline js-call10-jsprocedure ::JsGlobalObject ::JsProcedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   (inline js-calln-jsprocedure ::JsGlobalObject ::JsProcedure this args)
	   
	   (js-call0-procedure fun::procedure this)
	   (js-call1-procedure fun::procedure this a0)
	   (js-call2-procedure fun::procedure this a0 a1)
	   (js-call3-procedure fun::procedure this a0 a1 a2)
	   (js-call4-procedure fun::procedure this a0 a1 a2 a3)
	   (js-call5-procedure fun::procedure this a0 a1 a2 a3 a4)
	   (js-call6-procedure fun::procedure this a0 a1 a2 a3 a4 a5)
	   (js-call7-procedure fun::procedure this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8-procedure fun::procedure this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-call9-procedure fun::procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (js-call10-procedure fun::procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   (js-calln-procedure fun::procedure this args)
	   
	   (js-call0-obj ::JsGlobalObject ::obj this)
	   (js-call1-obj ::JsGlobalObject ::obj this a0)
	   (js-call2-obj ::JsGlobalObject ::obj this a0 a1)
	   (js-call3-obj ::JsGlobalObject ::obj this a0 a1 a2)
	   (js-call4-obj ::JsGlobalObject ::obj this a0 a1 a2 a3)
	   (js-call5-obj ::JsGlobalObject ::obj this a0 a1 a2 a3 a4)
	   (js-call6-obj ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5)
	   (js-call7-obj ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8-obj ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-call9-obj ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (js-call10-obj ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   
	   (inline js-call0 ::JsGlobalObject ::obj this)
	   (inline js-call1 ::JsGlobalObject ::obj this a0)
	   (inline js-call2 ::JsGlobalObject ::obj this a0 a1)
	   (inline js-call3 ::JsGlobalObject ::obj this a0 a1 a2)
	   (inline js-call4 ::JsGlobalObject ::obj this a0 a1 a2 a3)
	   (inline js-call5 ::JsGlobalObject ::obj this a0 a1 a2 a3 a4)
	   (inline js-call6 ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5)
	   (inline js-call7 ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6)
	   (inline js-call8 ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (inline js-call9 ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (inline js-call10 ::JsGlobalObject ::obj this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   (js-calln ::JsGlobalObject ::obj this args)
	   
	   (js-call0/debug ::JsGlobalObject loc fun::obj this)
	   (js-call1/debug ::JsGlobalObject loc fun::obj this a0)
	   (js-call2/debug ::JsGlobalObject loc fun::obj this a0 a1)
	   (js-call3/debug ::JsGlobalObject loc fun::obj this a0 a1 a2)
	   (js-call4/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3)
	   (js-call5/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4)
	   (js-call6/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5)
	   (js-call7/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-call9/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	   (js-call10/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   (js-calln/debug ::JsGlobalObject loc fun::obj this args)

	   (js-call-method0 ::JsGlobalObject val prop)
	   (js-call-method1 ::JsGlobalObject val prop ::obj)
	   (js-call-method2 ::JsGlobalObject val prop ::obj ::obj)
	   (js-call-method3 ::JsGlobalObject val prop ::obj ::obj ::obj)
	   (js-call-method4 ::JsGlobalObject val prop ::obj ::obj ::obj ::obj)
	   (js-call-methodn ::JsGlobalObject val prop . args)

	   (js-apply-service% ::procedure obj args::pair-nil ::int)
	   (js-apply-rest% ::JsGlobalObject ::procedure this ::pair-nil ::int ::int)
	   (js-apply% ::JsGlobalObject ::JsProcedure ::procedure obj ::pair-nil)
	   (js-apply ::JsGlobalObject fun::obj this ::pair-nil)

	   (js-service/debug ::obj ::obj ::procedure)

	   (js-ordinary-instanceof?::bool ::JsGlobalObject v f)
	   (js-object-function-instanceof?::bool ::JsGlobalObject ::JsObject ::JsFunction)
	   (js-function-instanceof?::bool ::JsGlobalObject v ::JsFunction)
	   (js-instanceof?::bool ::JsGlobalObject v f)
	   (js-instanceof?/debug::bool ::JsGlobalObject loc v f)
	   
	   (inline js-make-let::bchar)
	   (inline js-let-ref ::obj ::obj ::obj ::JsGlobalObject)
	   
	   (js-raise-reference-error/loc ::JsGlobalObject loc ::bstring obj . args)
	   (inline js-totest::bool ::obj)
	   (inline js-totest-likely-object::bool ::obj)
	   (js-toboolean::bool ::obj)
	   (js-toboolean-no-boolean::bool ::obj)
	   (generic js-tonumber ::obj ::JsGlobalObject)
	   (macro js-tointeger obj %this)
	   (generic js-tointeger ::obj ::JsGlobalObject)
	   (js-touint16::uint16 ::obj ::JsGlobalObject)
	   
	   (generic js-toindex::uint32 ::obj)
	   (inline js-isindex?::bool ::uint32)
	   (inline js-index?::bool ::obj)

	   (generic js-tostring::bstring ::obj ::JsGlobalObject)
	   (js-tojsstring-safe::JsStringLiteral ::obj ::JsGlobalObject)
	   (js-tojsstring::JsStringLiteral ::obj ::JsGlobalObject)
	   
	   (inline js-toobject-fast::JsObject ::obj ::JsGlobalObject)
	   (js-toobject::obj ::JsGlobalObject ::obj)
	   (js-toobject/debug::obj ::JsGlobalObject loc ::obj)
	   
	   (js-toprimitive-for-string::JsStringLiteral ::obj ::JsGlobalObject)
	   (generic js-toprimitive ::obj ::symbol ::JsGlobalObject)
	   
	   (inline js-equal?::bool ::obj ::obj ::JsGlobalObject)
	   (inline js-equal-fixnum?::bool ::obj ::obj ::JsGlobalObject)
	   (inline js-equal-sans-flonum?::bool ::obj ::obj ::JsGlobalObject)
	   (js-equality?::bool ::obj ::obj ::JsGlobalObject)
	   (js-same-value-zero?::bool ::obj ::obj ::JsGlobalObject)
	   (inline js-strict-equal?::bool ::obj ::obj)
	   (inline js-strict-equal-no-string?::bool ::obj ::obj)
	   (js-eq?::bool ::obj ::obj)
	   (js-eq-no-string?::bool ::obj ::obj)
	   (inline js-eqstring?::bool ::obj ::obj)
	   (inline js-eqil?::bool ::long ::obj)
	   (inline js-eqir?::bool ::obj ::long)
	   (inline js-null-or-undefined?::bool ::obj)
	   (inline js-object-or-null?::bool ::obj)

	   (js-super ::obj ::obj ::JsGlobalObject)
	   
	   (%js-eval-hss ::input-port ::JsGlobalObject ::obj ::obj)
	   (%js-direct-eval ::obj ::bool ::JsGlobalObject ::obj ::JsObject)
	   (%js-eval ::input-port ::symbol ::JsGlobalObject ::obj ::JsObject)
	   
	   (js-raise ::JsError)
	   (js-throw ::obj ::obj ::long)
	   (js-throw/debug ::obj ::obj ::long ::WorkerHopThread)

	   (js-raise-type-error ::JsGlobalObject ::bstring ::obj)
	   (js-raise-type-error/loc ::JsGlobalObject ::obj ::bstring ::obj)
	   (js-raise-range-error ::JsGlobalObject ::bstring ::obj)
	   (js-raise-uri-error ::JsGlobalObject ::bstring ::obj)
	   (js-raise-syntax-error ::JsGlobalObject ::bstring ::obj . ::obj)
	   (js-raise-syntax-error/loc ::JsGlobalObject ::obj ::bstring ::obj)
	   (js-raise-reference-error ::JsGlobalObject ::bstring ::obj . ::obj)
	   (js-raise-error ::JsGlobalObject ::bstring ::obj . ::obj)

	   (generic js-cast-object obj ::JsGlobalObject ::bstring)
	   (generic js-inspect ::obj ::int)

	   (js-typeof ::obj ::JsGlobalObject)

	   (js-html-head ::JsGlobalObject)
	   (js-html-script ::JsGlobalObject)

	   (js-parseint ::obj ::obj ::JsGlobalObject)
	   (js-parseint-string ::obj)
	   (js-parseint-any ::obj ::JsGlobalObject)
	   (js-parseint-string-uint32 ::obj ::uint32)
	   (js-parsefloat ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-init-public! ...                                              */
;*---------------------------------------------------------------------*/
(define (js-init-public! %this::JsGlobalObject)
   (unless (vector? __js_strings) (set! __js_strings (&init!))))

;*---------------------------------------------------------------------*/
;*    js-rest-args ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-rest-args %this num)
   (let loop ((num num))
      (if (=fx num 0)
	  (list (js-vector->jsarray '#() %this))
	  (cons (js-undefined) (loop (-fx num 1))))))

;*---------------------------------------------------------------------*/
;*    js-raise-arity-error ...                                         */
;*---------------------------------------------------------------------*/
(define (js-raise-arity-error %this fun::JsProcedure n)
   (with-access::JsProcedure fun (arity procedure)
      (let ((minlen (negfx
		       (cond
			  ((>fx arity -2049) (-fx (negfx arity) 1))
			  ((>fx arity -3049) (+fx arity 2049))
			  ((>fx arity -4049) (+fx arity 3049))
			  ((>fx arity -5049) (+fx arity 4049))
			  (else (+fx arity 5049))))))
	 (if (js-function? fun)
	     (with-access::JsFunction fun (arity len src)
		(let* ((name (js-get fun (& "name") %this))
		       (m (format "~a: wrong number of arguments ~a provided, ~a expected"
			     (if (js-jsstring? name) (js-jsstring->string name) "")
			     n
			     (cond
				((>fx arity 0) arity)
				((>fx arity -2049) (format ">= ~a" minlen))
				(else (format "[~a..~a]" minlen (procedure-arity procedure)))))))
		   (if (pair? src)
		       (js-raise-type-error/loc %this (car src) m fun)
		       (js-raise-type-error %this m fun))))
	     (let ((m (format "~a: wrong number of arguments ~a provided, ~a expected"
			 n
			 (cond
			    ((>fx arity 0) arity)
			    ((>fx arity -2049) (format ">= ~a" minlen))
			    (else (format "[~a..~a]" minlen (procedure-arity procedure)))))))
		(js-raise-type-error %this m fun))))))

;*---------------------------------------------------------------------*/
;*    optionals ...                                                    */
;*---------------------------------------------------------------------*/
(define optionals
   '#(()
      (#unspecified)
      (#unspecified #unspecified)
      (#unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified)
      (#unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified)))

;*---------------------------------------------------------------------*/
;*    evprocedure-arity ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (evprocedure-arity::long proc::procedure)
   (let ((a (procedure-arity proc)))
      (if (=fx a -5)
	  ;; might be an eval procedure
	  (let ((attr (procedure-attr proc)))
	     (if (and (struct? attr) (eq? (struct-key attr) 'user))
		 ;; awful hack that must be changed
		 ;; see the "user" struct int
		 ;; bigloo:runtime/Eval/evaluate_comp.scm
		 (struct-ref attr 0)
		 a))
	  a)))
   
;*---------------------------------------------------------------------*/
;*    gen-calln ...                                                    */
;*    -------------------------------------------------------------    */
;*    JsProcedure and JsFunction have the same calling protocol.       */
;*    How to call the function is a combination of the                 */
;*    JavaScript ARITY and Scheme ARITY.                               */
;*---------------------------------------------------------------------*/
(define-macro (gen-calln fun procedure this . args)
   
   (define n (+fx 1 (length args)))
   (define arity (gensym 'arity))
   (define parity (gensym 'parity))
   (define required (gensym 'required))
   
   (define (call-missing-rest i offset vec)
      ;; missing required rest call
      `((,(negfx (+fx offset i)))
	(let ((,parity (procedure-arity ,procedure)))
	   (if (js-procedure-hopscript-mode? ,fun)
	       (js-raise-arity-error %this ,fun ,(-fx n 1))
	       (,procedure ,this ,@args
		  ,@(make-list (-fx (+fx i 1) n) '(js-undefined))
		  ,vec)))))
   
   (define (call-missing-rest-array i)
      (call-missing-rest i 3049 '(js-empty-vector->jsarray %this)))
   
   (define (call-missing-rest-vector i)
      (call-missing-rest i 2049 ''#()))
   
   (define (call-ok-rest i offset vec)
      ;; ok rest call
      `((,(negfx (+fx offset i)))
	(,procedure ,this ,@(take args i) ,(vec (drop args i)))))
   
   (define (call-ok-rest-array i)
      (call-ok-rest i 3049
	 (lambda (args) `(js-vector->jsarray (vector ,@args) %this))))
   
   (define (call-ok-rest-vector i)
      (call-ok-rest i 2049
	 (lambda (args) `(vector ,@args))))
   
   (define (call-opt-missing i)
      ;; missing required arguments
      `((,(negfx (+fx 1024 i)))
	(let ((,parity (evprocedure-arity ,procedure)))
	   (if (js-procedure-hopscript-mode? ,fun)
	       (js-raise-arity-error %this ,fun ,(-fx n 1))
	       (case ,parity
		  ;; no need to generate [1.. i-1] because parity > -arity
		  ,@(map (lambda (a)
			    `((,a)
			      (,procedure ,this ,@args
				 ,@(make-list (-fx a n) '(js-undefined)))))
		     (iota (-fx 10 i) (+fx i 1)))
		  (else
		   (if (<fx ,parity 0)
		       (apply ,procedure ,this ,@args
			  (make-list (-fx (negfx ,parity) ,n)
			     (js-undefined)))
		       (apply ,procedure ,this ,@args
			  (vector-ref optionals (-fx ,parity ,n))))))))))

   (define (call-opt-ok-or-too-many i)
      `((,(negfx (+fx 1024 i)))
	;; too many required arguments
	(let ((,parity (evprocedure-arity ,procedure)))
	   (case ,parity
	      ;; no need to generate [1.. i-1] because parity > -arity
	      ,@(map (lambda (a)
			`((,a)
			  ,(cond
			      ((<fx a n)
			       `(if (js-procedure-hopscript-mode? ,fun)
				    (js-raise-arity-error %this ,fun ,(-fx n 1))
				    (,procedure ,this ,@(take args (-fx a 1)))))
			      ((=fx a n)
			       `(,procedure ,this ,@args))
			      (else
			       `(if (js-procedure-hopscript-mode? ,fun)
				    (js-raise-arity-error %this ,fun ,(-fx n 1))
				    (,procedure ,this ,@args
				       ,@(make-list (-fx a n)
					    '(js-undefined))))))))
		 (iota (-fx 10 i) (+fx i 1)))
	      (else
	       (if (<fx ,parity 0)
		   (,procedure ,this ,@args)
		   (apply ,procedure ,this ,@args
		      (make-list (-fx ,parity ,n) (js-undefined)))))))))

   (define (call-scheme-vararg-missing i)
      ;; missing scheme var args
      `((,(negfx i))
	(,procedure ,this ,@args ,@(make-list (-fx i (+fx n 1)) '(js-undefined)))))
   
   (define (call-scheme-vararg-ok i)
      ;; ok scheme var args
      `((,(negfx i))
	(,procedure ,this ,@args)))
   
   (define (call-fix-too-many i)
      ;; too many fix arguments
      `((,i)
	(if (js-procedure-hopscript-mode? ,fun)
	    (js-raise-arity-error %this ,fun ,(-fx n 1))
	    (,procedure ,this ,@(take args (-fx i 1))))))
   
   (define (call-fix-missing i)
      ;; missing fix arguments
      `((,i)
	(if (js-procedure-hopscript-mode? ,fun)
	    (js-raise-arity-error %this ,fun ,(-fx n 1))
	    (,procedure ,this ,@args ,@(make-list (-fx i n) '(js-undefined))))))
   
   (define (call-many-arguments-opt-norest)
      ;; many argument + optional arguments
      `(let ((,required ,arity))
	  (cond
	     ((=fx ,n ,parity)
	      (,procedure ,this ,@args))
	     ((<fx ,required ,n)
	      ;; required arguments missing
	      (if (js-procedure-hopscript-mode? ,fun)
		  (js-raise-arity-error %this ,fun ,(-fx n 1))
		  (apply ,procedure ,this
		     ,@args (make-list (-fx ,parity ,n)
			       (js-undefined)))))
	     ((<fx ,n ,parity)
	      (apply ,procedure ,this
		 ,@args (make-list (-fx ,parity ,n)
			   (js-undefined))))
	     (else
	      (if (js-procedure-hopscript-mode? ,fun)
		  (js-raise-arity-error %this ,fun ,(-fx n 1))
		  (apply ,procedure ,this
		     (take (list ,@args) ,parity)))))))
   
   (define (rest-argument-empty arity)
      `(cond
	  ((>fx ,arity -3049) '#())
	  ((>fx ,arity -4049) (js-empty-vector->jsarray %this))
	  ((>fx ,arity -5049) '#())
	  (else (js-empty-vector->jsarray %this))))
   
   (define (rest-argument arity args)
      `(cond
	  ((>fx ,arity -3049) (list->vector ,args))
	  ((>fx ,arity -4049) (js-vector->jsarray (list->vector ,args) %this))
	  ((>fx ,arity -5049) (list->vector ,args))
	  (else (js-vector->jsarray (list->vector ,args) %this))))
   
   (define (call-many-arguments-opt-rest)
      ;; many argument + optional arguments + rest
      `(if (=fx ,(+fx n 1) ,parity)
	   (,procedure ,this ,@args ,(rest-argument-empty arity))
	   (let ((,required (negfx
			       (cond
				  ((>fx ,arity -3049) (+fx ,arity 2049))
				  ((>fx ,arity -4049) (+fx ,arity 3049))
				  ((>fx ,arity -5049) (+fx ,arity 4049))
				  (else (+fx ,arity 5049))))))
	      (cond
		 ((and (>=fx ,required ,n) (js-procedure-hopscript-mode? ,fun))
		  ;; required arguments missing
		  (js-raise-arity-error %this ,fun ,(-fx n 1)))
		 ((<fx ,n ,parity)
		  ;; arguments missing
		  (apply ,procedure ,this
		     ,@args
		     (append
			(make-list (-fx ,parity ,(+fx n 1)) (js-undefined))
			(list ,(rest-argument-empty arity)))))
		 ((<fx ,parity 0)
		  ;; this schema does not support optional and rest
		  ;; arguments for interpreted procedure
		  ,(let ((l (gensym 'args)))
		      `(let ((,l (list ,@args)))
			  (if (<=fx ,n ,required)
			      (apply ,procedure ,this
				 (append ,l
				    (make-list (-fx ,required ,(-fx n 1))
				       (js-undefined))
				    (list ,(rest-argument-empty arity))))
			      (append (take ,l ,required)
				 (list ,(rest-argument arity
					   `(drop ,l ,required))))))))
		 (else
		  ,(let ((l (gensym 'args)))
		      `(let ((,l (list ,@args)))
			  (apply ,procedure ,this
			     (append
				(take ,l (-fx ,parity 2))
				(list
				   ,(rest-argument arity
				       `(drop ,l (-fx ,parity 2)))))))))))))

   (define (call-many-arguments-scheme-varargs)
      `(let ((,required (-fx (negfx ,parity) 1)))
	  (if (>=fx ,n (negfx ,parity))
	      (,procedure ,this ,@args)
	      (apply ,procedure ,this ,@args
		 (make-list (-fx ,n ,required) (js-undefined))))))
	  
   (define (call-many-arguments)
      ;; dyamic call sequence for many arguments
      `(cond
	  ((>fx ,arity 0)
	   (if (js-procedure-hopscript-mode? ,fun)
	       (js-raise-arity-error %this ,fun ,(-fx n 1))
	       ;; fixed number of arguments
	       (if (>fx ,arity ,n)
		   ;; missing arguments
		   (apply ,procedure ,this ,@args 
		      (make-list (-fx ,arity ,n) (js-undefined)))
		   ;; too many arguments
		   (apply ,procedure ,this (take (list ,@args) ,arity)))))
	  (else
	   ;; optional arguments
	   (let ((,parity (procedure-arity ,procedure)))
	      (cond
		 ((>fx ,arity 1024)
		  ;; scheme var args
		  (let ((,parity (evprocedure-arity ,procedure)))
		     ,(call-many-arguments-scheme-varargs)))
		 ((>fx ,arity -2049)
		  ;; no rest argument
		  ,(call-many-arguments-opt-norest))
		 (else
		  ;; rest argument
		  ,(call-many-arguments-opt-rest)))))))
   
   `(with-access::JsProcedure ,fun (arity)
       (let ((,arity arity))
	  (case ,arity
	     ;; missing rest call (rest allocated in an array)
	     ,@(map call-missing-rest-array (reverse (iota (-fx 10 n) n)))
	     ;; ok rest call (rest allocated in an array)
	     ,@(map call-ok-rest-array (reverse (iota n 0)))
	     ;; missing rest call (rest allocated in an vector)
	     ,@(map call-missing-rest-vector (reverse (iota (-fx 10 n) n)))
	     ;; ok rest call (rest allocated in an vector)
	     ,@(map call-ok-rest-vector (reverse (iota n 0)))
	     ((-2048)
	      ;; eager "arguments" call
	      (,procedure ,this (vector ,@args)))
	     ((-2047)
	      ;; lazy "arguments" call
	      (js-call-with-stack-vector
		 (vector ,@args)
		 (lambda (v) (,procedure ,this v))))
	     ;; opt missing required arguments
	     ,@(map call-opt-missing (reverse (iota (-fx 10 n) n)))
	     ;; opt ok or too many arguments
	     ,@(map call-opt-ok-or-too-many (reverse (iota n 0)))
	     ;; scheme missing var args
	     ,@(map call-scheme-vararg-missing (reverse (iota (-fx 10 n) (+fx n 1))))
	     ;; scheme ok var args
	     ,@(map call-scheme-vararg-ok (reverse (iota n 1)))
	     ;; arguments without optimization
	     ((0) (,procedure ,this ,@args))
	     ;; fix too many arguments
	     ,@(map call-fix-too-many (iota (-fx n 1) 1))
	     ;; direct call
	     ((,n) (,procedure ,this ,@args))
	     ;; fix missing arguments
	     ,@(map call-fix-missing (iota (-fx 10 n) (+fx n 1)))
	     ;; dynamic dispatch
	     (else ,(call-many-arguments))))))

(define (js-call0% %this fun::JsProcedure procedure this)
   (gen-calln fun procedure this))

(define (js-call1% %this fun::JsProcedure procedure this a0)
   (gen-calln fun procedure this a0))

(define (js-call2% %this fun::JsProcedure procedure this a0 a1)
   (gen-calln fun procedure this a0 a1))

(define (js-call3% %this fun::JsProcedure procedure this a0 a1 a2)
   (gen-calln fun procedure this a0 a1 a2))

(define (js-call4% %this fun::JsProcedure procedure this a0 a1 a2 a3)
   (gen-calln fun procedure this a0 a1 a2 a3))

(define (js-call5% %this fun::JsProcedure procedure this a0 a1 a2 a3 a4)
   (gen-calln fun procedure this a0 a1 a2 a3 a4))

(define (js-call6% %this fun::JsProcedure procedure this a0 a1 a2 a3 a4 a5)
   (gen-calln fun procedure this a0 a1 a2 a3 a4 a5))

(define (js-call7% %this fun::JsProcedure procedure this a0 a1 a2 a3 a4 a5 a6)
   (gen-calln fun procedure this a0 a1 a2 a3 a4 a5 a6))

(define (js-call8% %this fun::JsProcedure procedure this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-calln fun procedure this a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call9% %this fun::JsProcedure procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (gen-calln fun procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8))

(define (js-call10% %this fun::JsProcedure procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (gen-calln fun procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))

(define (js-calln-many% %this fun procedure this args n)

   (define (rest-argument-empty arity)
      (cond
	  ((>fx arity -3049) '#())
	  ((>fx arity -4049) (js-empty-vector->jsarray %this))
	  ((>fx arity -5049) '#())
	  (else (js-empty-vector->jsarray %this))))
   
   (define (rest-argument arity args)
      (cond
	  ((>fx arity -3049) (list->vector args))
	  ((>fx arity -4049) (js-vector->jsarray (list->vector args) %this))
	  ((>fx arity -5049) (list->vector args))
	  (else (js-vector->jsarray (list->vector args) %this))))
   
   (define (calln-many-opt-norest arity)
      ;; many argument + optional arguments
      (let ((required arity)
	    (parity (procedure-arity procedure)))
	 (cond
	    ((=fx n parity)
	     (apply procedure this args))
	    ((<fx required n)
	     ;; required arguments missing
	     (if (js-procedure-hopscript-mode? fun)
		 (js-raise-arity-error %this fun (-fx n 1))
		 (apply procedure this
		    (append args
		       (make-list (-fx parity n) (js-undefined))))))
	    ((<fx n parity)
	     (apply procedure this
		(append args
		   (make-list (-fx parity n) (js-undefined)))))
	    (else
	     (if (js-procedure-hopscript-mode? fun)
		 (js-raise-arity-error %this fun (-fx n 1))
		 (apply procedure this (take args parity)))))))

   (define (calln-many-opt-rest arity)
      (let ((parity (procedure-arity procedure)))
	 (if (=fx (+fx n 1) parity)
	     (apply procedure this
		(append args (list (rest-argument-empty arity))))
	     (let ((required (negfx
				(cond
				   ((>fx arity -3049) (+fx arity 2049))
				   ((>fx arity -4049) (+fx arity 3049))
				   ((>fx arity -5049) (+fx arity 4049))
				   (else (+fx arity 5049))))))
		(cond
		   ((and (>=fx required n) (js-procedure-hopscript-mode? fun))
		    ;; required arguments missing
		    (js-raise-arity-error %this fun (-fx n 1)))
		   ((<fx n parity)
		    ;; arguments missing
		    (apply procedure this
		       (append args
			  (make-list (-fx parity (+fx n 1)) (js-undefined))
			  (list (rest-argument-empty arity)))))
		   ((<fx parity 0)
		    ;; this schema does not support optional and rest
		    ;; arguments for interpreted procedure
		    (if (<=fx n required)
			(apply procedure this
			   (append args
			      (make-list (-fx required (-fx n 1))
				 (js-undefined))
			      (list (rest-argument-empty arity))))
			(apply procedure this
			   (append (take args required)
			      (list (rest-argument arity
				       (drop args required)))))))
		   (else
		    (apply procedure this
		       (append (take args (-fx parity 2))
			  (list (rest-argument arity
				   (drop args (-fx parity 2))))))))))))
   
   (with-access::JsProcedure fun (arity)
      (let ((arity arity))
	 (cond
	    ((>fx arity 0)
	     (if (js-procedure-hopscript-mode? fun)
		 (js-raise-arity-error %this fun (-fx n 1))
		 (if (>fx arity n)
		     ;; missing arguments
		     (apply procedure this
			(append args
			   (make-list (-fx arity n) (js-undefined))))
		     ;; too many
		     (apply procedure this (take args (-fx arity 1))))))
	    ((>fx arity -2049)
	     (calln-many-opt-norest arity))
	    (else
	     (calln-many-opt-rest arity))))))

(define (js-calln% %this fun procedure this args)
   (with-access::JsProcedure fun (arity)
      (let ((n (+fx 1 (length args))))
	 (if (=fx n arity)
	     (apply procedure this args)
	     (match-case args
		(()
		 (js-call0% %this fun procedure this))
		((?a0)
		 (js-call1% %this fun procedure this a0))
		((?a0 ?a1)
		 (js-call2% %this fun procedure this a0 a1))
		((?a0 ?a1 ?a2)
		 (js-call3% %this fun procedure this a0 a1 a2))
		((?a0 ?a1 ?a2 ?a3)
		 (js-call4% %this fun procedure this a0 a1 a2 a3))
		((?a0 ?a1 ?a2 ?a3 ?a4)
		 (js-call5% %this fun procedure this a0 a1 a2 a3 a4))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5)
		 (js-call6% %this fun procedure this a0 a1 a2 a3 a4 a5))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6)
		 (js-call7% %this fun procedure this a0 a1 a2 a3 a4 a5 a6))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7)
		 (js-call8% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8)
		 (js-call9% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8))
		((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9)
		 (js-call10% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
		(else
		 (js-calln-many% %this fun procedure this args n)))))))

;*---------------------------------------------------------------------*/
;*    gen-call-jsprocedure ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-call0-jsprocedure %this fun this)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 1)
	  (procedure this)
	  (js-call0% %this fun procedure this))))

(define-inline (js-call1-jsprocedure %this fun this a0)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 2)
	  (procedure this a0)
	  (js-call1% %this fun procedure this a0))))

(define-inline (js-call2-jsprocedure %this fun this a0 a1)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 3)
	  (procedure this a0 a1)
	  (js-call2% %this fun procedure this a0 a1))))

(define-inline (js-call3-jsprocedure %this fun this a0 a1 a2)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 4)
	  (procedure this a0 a1 a2)
	  (js-call3% %this fun procedure this a0 a1 a2))))

(define-inline (js-call4-jsprocedure %this fun this a0 a1 a2 a3)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 5)
	  (procedure this a0 a1 a2 a3)
	  (js-call4% %this fun procedure this a0 a1 a2 a3))))

(define-inline (js-call5-jsprocedure %this fun this a0 a1 a2 a3 a4)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 6)
	  (procedure this a0 a1 a2 a3 a4)
	  (js-call5% %this fun procedure this a0 a1 a2 a3 a4))))

(define-inline (js-call6-jsprocedure %this fun this a0 a1 a2 a3 a4 a5)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 7)
	  (procedure this a0 a1 a2 a3 a4 a5)
	  (js-call6% %this fun procedure this a0 a1 a2 a3 a4 a5))))

(define-inline (js-call7-jsprocedure %this fun this a0 a1 a2 a3 a4 a5 a6)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 8)
	  (procedure this a0 a1 a2 a3 a4 a5 a6)
	  (js-call7% %this fun procedure this a0 a1 a2 a3 a4 a5 a6))))

(define-inline (js-call8-jsprocedure %this fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 9)
	  (procedure this a0 a1 a2 a3 a4 a5 a6 a7)
	  (js-call8% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7))))

(define-inline (js-call9-jsprocedure %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 10)
	  (procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8)
	  (js-call9% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8))))

(define-inline (js-call10-jsprocedure %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (with-access::JsProcedure fun (procedure arity)
      (if (=fx arity 11)
	  (procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	  (js-call10% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))))

(define-inline (js-calln-jsprocedure %this fun this args)
   (with-access::JsProcedure fun (procedure)
      (js-calln% %this fun procedure this args)))

;*---------------------------------------------------------------------*/
;*    gen-call-procedure ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (gen-call-procedure proc this . args)
   (let ((n (+fx 1 (length args))))
      `(let ((arity (procedure-arity ,proc)))
	  (cond
	     ((=fx arity ,n)
	      (,proc ,this ,@args))
	     ((>fx arity ,n)
	      (apply ,proc ,this ,@args
		 (make-list (-fx arity ,n) (js-undefined))))
	     ,@(map (lambda (i)
		       `((=fx arity ,i)
			 (,proc ,this ,@(take args (-fx i 1)))))
		(iota (length args) 1))
	     (else
	      (js-undefined))))))
   
(define (js-call0-procedure proc this)
   (gen-call-procedure proc this))

(define (js-call1-procedure proc this a0)
   (gen-call-procedure proc this a0))

(define (js-call2-procedure proc this a0 a1)
   (gen-call-procedure proc this a0 a1))

(define (js-call3-procedure proc this a0 a1 a2)
   (gen-call-procedure proc this a0 a1 a2))

(define (js-call4-procedure proc this a0 a1 a2 a3)
   (gen-call-procedure proc this a0 a1 a2 a3))

(define (js-call5-procedure proc this a0 a1 a2 a3 a4)
   (gen-call-procedure proc this a0 a1 a2 a3 a4))

(define (js-call6-procedure proc this a0 a1 a2 a3 a4 a5)
   (gen-call-procedure proc this a0 a1 a2 a3 a4 a5))

(define (js-call7-procedure proc this a0 a1 a2 a3 a4 a5 a6)
   (gen-call-procedure proc this a0 a1 a2 a3 a4 a5 a6))

(define (js-call8-procedure proc this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-call-procedure proc this a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call9-procedure proc this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (gen-call-procedure proc this a0 a1 a2 a3 a4 a5 a6 a7 a8))

(define (js-call10-procedure proc this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (gen-call-procedure proc this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))

(define (js-calln-procedure proc this args)
   ;; this protocol only support fix arity
   (let ((n (+fx 1 (length args)))
	 (arity (procedure-arity proc)))
      (cond
	 ((=fx arity n)
	  (apply proc this args))
	 ((>fx arity n)
	  (apply proc this
	     (append args
		(make-list (-fx arity n)
		   (js-undefined)))))
	 (else
	  (apply proc this (take args (-fx arity 1)))))))

;*---------------------------------------------------------------------*/
;*    gen-call-obj ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (gen-call-obj %this fun this . args)
   `(cond
       ((js-procedure? ,fun)
	(with-access::JsProcedure ,fun (procedure)
	   (,(string->symbol (format "js-call~a%" (length args)))
	    ,%this ,fun procedure ,this ,@args)))
       ((js-procedure-proxy? ,fun)
	(,(string->symbol (format "js-call-proxy/cache-miss~a" (length args)))
	 ,%this ,fun ,this ,@args))
       ((procedure? fun)
	(,fun ,@args))
       (else
	(js-raise-type-error ,%this
	   ,(format "call~a: not a function ~~s" (length args)) ,fun))))

(define (js-call0-obj %this fun this)
   (gen-call-obj %this fun this))

(define (js-call1-obj %this fun this a0)
   (gen-call-obj %this fun this a0))

(define (js-call2-obj %this fun this a0 a1)
   (gen-call-obj %this fun this a0 a1))

(define (js-call3-obj %this fun this a0 a1 a2)
   (gen-call-obj %this fun this a0 a1 a2))

(define (js-call4-obj %this fun this a0 a1 a2 a3)
   (gen-call-obj %this fun this a0 a1 a2 a3))

(define (js-call5-obj %this fun this a0 a1 a2 a3 a4)
   (gen-call-obj %this fun this a0 a1 a2 a3 a4))

(define (js-call6-obj %this fun this a0 a1 a2 a3 a4 a5)
   (gen-call-obj %this fun this a0 a1 a2 a3 a4 a5))

(define (js-call7-obj %this fun this a0 a1 a2 a3 a4 a5 a6)
   (gen-call-obj %this fun this a0 a1 a2 a3 a4 a5 a6))

(define (js-call8-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-call-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call9-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (gen-call-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8))

(define (js-call10-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (gen-call-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))

;*---------------------------------------------------------------------*/
;*    js-callXXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-call0 %this fun this)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 1))
       ((js-procedure-procedure fun) this)
       (js-call0-obj %this fun this)))

(define-inline (js-call1 %this fun this a0)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 2))
       ((js-procedure-procedure fun) this a0)
       (js-call1-obj %this fun this a0)))

(define-inline (js-call2 %this fun this a0 a1)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 3))
       ((js-procedure-procedure fun) this a0 a1)
       (js-call2-obj %this fun this a0 a1)))

(define-inline (js-call3 %this fun this a0 a1 a2)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 4))
       ((js-procedure-procedure fun) this a0 a1 a2)
       (js-call3-obj %this fun this a0 a1 a2)))

(define-inline (js-call4 %this fun this a0 a1 a2 a3)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 5))
       ((js-procedure-procedure fun) this a0 a1 a2 a3)
       (js-call4-obj %this fun this a0 a1 a2 a3)))

(define-inline (js-call5 %this fun this a0 a1 a2 a3 a4)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 6))
       ((js-procedure-procedure fun) this a0 a1 a2 a3 a4)
       (js-call5-obj %this fun this a0 a1 a2 a3 a4)))

(define-inline (js-call6 %this fun this a0 a1 a2 a3 a4 a5)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 7))
       ((js-procedure-procedure fun) this a0 a1 a2 a3 a4 a5)
       (js-call6-obj %this fun this a0 a1 a2 a3 a4 a5)))

(define-inline (js-call7 %this fun this a0 a1 a2 a3 a4 a5 a6)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 8))
       ((js-procedure-procedure fun) this a0 a1 a2 a3 a4 a5 a6)
       (js-call7-obj %this fun this a0 a1 a2 a3 a4 a5 a6)))

(define-inline (js-call8 %this fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 9))
       ((js-procedure-procedure fun) this a0 a1 a2 a3 a4 a5 a6 a7)
       (js-call8-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7)))

(define-inline (js-call9 %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 10))
       ((js-procedure-procedure fun) this a0 a1 a2 a3 a4 a5 a6 a7 a8)
       (js-call9-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8)))

(define-inline (js-call10 %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (if (and (js-procedure? fun) (=fx (js-procedure-arity fun) 11))
       ((js-procedure-procedure fun) this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
       (js-call10-obj %this fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)))

(define (js-calln %this fun this args)
   (cond
      ((js-procedure? fun)
       (with-access::JsProcedure fun (procedure)
	  (js-calln% %this fun procedure this args)))
      ((js-procedure-proxy? fun)
       (js-call-proxyn %this fun this args))
      (else
       (js-raise-type-error %this
	  (format "call(~a): not a function ~~s" (length args))
	  fun))))

;*---------------------------------------------------------------------*/
;*    gen-call/debug ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (gen-call/debug %this loc fun this . args)
   `(cond
       ((js-procedure? ,fun)
	(let ((env (current-dynamic-env))
	      (name (js-function-debug-name ,fun %this)))
	   ($env-push-trace env name loc)
	   (with-access::JsProcedure ,fun (procedure arity)
	      (let ((aux (,(string->symbol (format "js-call~a%" (length args)))
			  ,%this ,fun procedure ,this ,@args)))
		 ($env-pop-trace env)
		 aux))))
       ((js-procedure-proxy? ,fun)
	(with-access::JsProxy ,fun ((target __proto__))
	   (let ((env (current-dynamic-env))
		 (name (js-proxy-debug-name ,fun %this)))
	      ($env-push-trace env name loc)
	      (let ((aux (,(string->symbol (format "js-call-proxy/cache-miss~a" (length args)))
			  ,%this ,fun ,this ,@args)))
		 ($env-pop-trace env)
		 aux))))
       (else
	(js-raise-type-error/loc %this loc
	   (format "call~a: not a function ~~s ~a" ,(length args) ,loc) ,fun))))

(define (js-call0/debug %this loc fun this)
   (gen-call/debug %this loc fun this))

(define (js-call1/debug %this loc fun this a0)
   (gen-call/debug %this loc fun this a0))

(define (js-call2/debug %this loc fun this a0 a1)
   (gen-call/debug %this loc fun this a0 a1))

(define (js-call3/debug %this loc fun this a0 a1 a2)
   (gen-call/debug %this loc fun this a0 a1 a2))

(define (js-call4/debug %this loc fun this a0 a1 a2 a3)
   (gen-call/debug %this loc fun this a0 a1 a2 a3))

(define (js-call5/debug %this loc fun this a0 a1 a2 a3 a4)
   (gen-call/debug %this loc fun this a0 a1 a2 a3 a4))

(define (js-call6/debug %this loc fun this a0 a1 a2 a3 a4 a5)
   (gen-call/debug %this loc fun this a0 a1 a2 a3 a4 a5))

(define (js-call7/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6)
   (gen-call/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6))

(define (js-call8/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-call/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call9/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7 a8)
   (gen-call/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7 a8))

(define (js-call10/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
   (gen-call/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))

(define (js-calln/debug %this loc fun this args)
   (cond
      ((js-procedure? fun)
       (with-access::JsProcedure fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun %this)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-calln% %this fun procedure this args)))
		($env-pop-trace env)
		aux))))
      ((js-procedure-proxy? fun)
       (with-access::JsProcedure fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-proxy-debug-name fun %this)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call-proxyn %this fun this args)))
		($env-pop-trace env)
		aux))))
      (else
       (js-raise-type-error/loc %this loc
	  (format "call(~a): not a function ~~s ~a" (length args) loc) fun))))

;*---------------------------------------------------------------------*/
;*    js-call-method ...                                               */
;*    -------------------------------------------------------------    */
;*    These functions are used when a method is invoked on a           */
;*    non-object value.                                                */
;*---------------------------------------------------------------------*/
(define (js-call-method0 %this val prop)
   (let ((o (js-toobject %this val)))
      (js-call0 %this (js-get o prop %this) o)))

(define (js-call-method1 %this val prop a0)
   (let ((o (js-toobject %this val)))
      (js-call1 %this (js-get o prop %this) o a0)))

(define (js-call-method2 %this val prop a0 a1)
   (let ((o (js-toobject %this val)))
      (js-call2 %this (js-get o prop %this) o a0 a1)))
      
(define (js-call-method3 %this val prop a0 a1 a2)
   (let ((o (js-toobject %this val)))
      (js-call3 %this (js-get o prop %this) o a0 a1 a2)))
      
(define (js-call-method4 %this val prop a0 a1 a2 a3)
   (let ((o (js-toobject %this val)))
      (js-call4 %this (js-get o prop %this) o a0 a1 a2 a3)))
      
(define (js-call-methodn %this val prop . args)
   (let ((o (js-toobject %this val)))
      (js-calln %this (js-get o prop %this) o args)))

;*---------------------------------------------------------------------*/
;*    js-apply-rest% ...                                               */
;*---------------------------------------------------------------------*/
(define (js-apply-rest% %this proc::procedure obj args::pair-nil len::int n::int)
   (if (<=fx n (+fx len 1))
       (apply proc obj
	  (append args (js-rest-args %this (-fx (+fx len 1) n))))
       (apply proc obj
	  (append (take args len)
	     (list
		(js-vector->jsarray
		   (apply vector (drop args len)) %this))))))

;*---------------------------------------------------------------------*/
;*    js-apply-service% ...                                            */
;*---------------------------------------------------------------------*/
(define (js-apply-service% proc::procedure obj args::pair-nil arity::int)
   (let ((len (length args)))
      (cond
	 ((=fx arity len)
	  (apply proc obj args))
	 ((<fx arity len)
	  (apply proc obj (take args arity)))
	 (else
	  (let ((rest (make-list (-fx arity len) (js-undefined))))
	     (apply proc obj (append args rest)))))))

;*---------------------------------------------------------------------*/
;*    js-apply% ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-apply% %this fun::JsProcedure proc::procedure obj args::pair-nil)
   (match-case args
      (()
       (js-call0% %this fun proc obj))
      ((?a0)
       (js-call1% %this fun proc obj a0))
      ((?a0 ?a1)
       (js-call2% %this fun proc obj a0 a1))
      ((?a0 ?a1 ?a2)
       (js-call3% %this fun proc obj a0 a1 a2))
      ((?a0 ?a1 ?a2 ?a3)
       (js-call4% %this fun proc obj a0 a1 a2 a3))
      ((?a0 ?a1 ?a2 ?a3 ?a4)
       (js-call5% %this fun proc obj a0 a1 a2 a3 a4))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5)
       (js-call6% %this fun proc obj a0 a1 a2 a3 a4 a5))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6)
       (js-call7% %this fun proc obj a0 a1 a2 a3 a4 a5 a6))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7)
       (js-call8% %this fun proc obj a0 a1 a2 a3 a4 a5 a6 a7))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8)
       (js-call9% %this fun proc obj a0 a1 a2 a3 a4 a5 a6 a7 a8))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9)
       (js-call10% %this fun proc obj a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
      (else
       (js-calln% %this fun proc obj args))))

;*---------------------------------------------------------------------*/
;*    js-apply ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-apply %this fun obj args::pair-nil)
   (cond
      ((js-procedure? fun)
       (with-access::JsProcedure fun (procedure)
	  (js-calln% %this fun procedure obj args)))
      ((js-procedure-proxy? fun)
       (js-apply-proxy %this fun obj args))
      (else
       (js-raise-type-error %this "apply: argument not a function ~s" fun))))

;*---------------------------------------------------------------------*/
;*    js-new/function ...                                              */
;*---------------------------------------------------------------------*/
(define (js-new/function %this::JsGlobalObject f::JsFunction args::pair-nil)
   (case (length args)
      ((0)
       (js-new0 %this f))
      ((1)
       (js-new1 %this f (car args)))
      ((2)
       (js-new2 %this f (car args) (cadr args)))
      ((3)
       (js-new3 %this f (car args) (cadr args) (caddr args)))
      ((4)
       (js-new4 %this f (car args) (cadr args) (caddr args) (cadddr args)))
      (else
       (with-access::JsFunction f (construct alloc)
	  (let* ((o (alloc %this f))
		 (r (js-apply% %this f construct o args)))
	     (if (js-object? r) r o))))))

;*---------------------------------------------------------------------*/
;*    js-new/proxy ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-new/proxy %this::JsGlobalObject p::JsProxy args::pair-nil)
   (with-access::JsProxy p (handler)
      (let ((ctor (js-get handler (& "construct") %this)))
	 (if (js-function? ctor)
	     (let ((obj (js-call2 %this ctor p (js-proxy-target p)
			   (js-vector->jsarray (list->vector args) %this))))
		(cond
		   ((not (js-function? (js-proxy-target p)))
		    (js-raise-type-error %this
		       "Proxy \"construct\" inconsistency"
		       p))
		   ((not (js-object? obj))
		    (js-raise-type-error %this
		       "Proxy \"construct\" result not an object"
		       obj))
		   (else
		    obj)))
	     (js-raise-type-error %this "new: constructor is not a function ~s" p)))))

;*---------------------------------------------------------------------*/
;*    js-new ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.2.2       */
;*---------------------------------------------------------------------*/
(define (js-new %this f . args)
   (cond
      ((js-function? f)
       (js-new/function %this f args))
      ((js-proxy? f)
       (js-new/proxy %this f args))
      (else
       (js-raise-type-error %this "new: constructor is not a function ~s" f))))

;*---------------------------------------------------------------------*/
;*    js-new/debug ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.2.2       */
;*---------------------------------------------------------------------*/
(define (js-new/debug %this loc f . args)
   (cond
      ((js-function? f)
       (js-new/function %this f args))
      ((js-proxy? f)
       (js-new/proxy %this f args))
      (else
       (js-raise-type-error/loc %this loc "new: constructor is not a function ~s" f))))

;*---------------------------------------------------------------------*/
;*    js-object-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-object-alloc %this ctor::JsFunction)
   (with-access::JsFunction ctor (constrsize constrmap %prototype)
      (when (eq? constrmap (js-not-a-cmap))
;*       (with-access::JsConstructMap constrmap (size)                 */
;* 	 (unless (=fx size constrsize)                                 */
	    (js-function-set-constrmap! ctor))
      (js-make-jsobject constrsize constrmap %prototype)))

;*---------------------------------------------------------------------*/
;*    js-object-alloc-fast ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (js-object-alloc-fast %this ctor::JsFunction)
   (with-access::JsFunction ctor (constrsize constrmap %prototype)
      (js-make-jsobject constrsize constrmap %prototype)))

;*---------------------------------------------------------------------*/
;*    js-object-alloc-lazy ...                                         */
;*---------------------------------------------------------------------*/
(define (js-object-alloc-lazy %this ctor::JsFunction)
   (with-access::JsFunction ctor (constrmap %prototype alloc)
      (unless %prototype
	 (js-function-setup-prototype! %this ctor)
	 (set! alloc js-object-alloc))
      (js-object-alloc %this ctor)))

;*---------------------------------------------------------------------*/
;*    js-object-alloc/new-target ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-object-alloc/new-target %this ctor::JsFunction)
   (with-access::JsGlobalObject %this (js-new-target)
      (set! js-new-target ctor)
      (js-object-alloc %this ctor)))

;*---------------------------------------------------------------------*/
;*    js-no-alloc ...                                                  */
;*    -------------------------------------------------------------    */
;*    This is used by functions that allocate ad-hoc constructors.     */
;*---------------------------------------------------------------------*/
(define-inline (js-no-alloc %this ctor::JsFunction)
   (js-undefined))

;*---------------------------------------------------------------------*/
;*    js-not-a-constructor-alloc ...                                   */
;*    -------------------------------------------------------------    */
;*    Used by functions that are not allowed to be used in NEW expr.   */
;*---------------------------------------------------------------------*/
(define (js-not-a-constructor-alloc %this ctor::JsFunction)
   (js-raise-type-error %this "~s not a constructor"
      (js-function-debug-name ctor %this)))

;*---------------------------------------------------------------------*/
;*    js-function-set-constrmap! ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-function-set-constrmap! ctor::JsFunction)
   (with-access::JsFunction ctor (constrmap constrsize)
      (set! constrmap
	 (instantiate::JsConstructMap
	    (ctor ctor)
;* 	    (size constrsize)                                          */
	    (inline #t)))
      ctor))
   
;*---------------------------------------------------------------------*/
;*    js-new-return ...                                                */
;*---------------------------------------------------------------------*/
(define (js-new-return f r o)
   [assert (r o) (or (js-object? r) (js-object? o))]
   (with-access::JsFunction f (constrsize)
      (if (js-object? r)
	  (with-access::JsObject r (elements)
	     (when (vector? elements)
		(set! constrsize (vector-length elements)))
	     r)
	  (with-access::JsObject o (elements)
	     (when (vector? elements)
		(set! constrsize (vector-length elements)))
	     o))))

;*---------------------------------------------------------------------*/
;*    js-new-return-fast ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-new-return-fast ctor o)
   (with-access::JsFunction ctor (constrsize)
      (with-access::JsObject o (elements)
	 (when (vector? elements)
	    (set! constrsize (vector-length elements)))
	 o)))

;*---------------------------------------------------------------------*/
;*    js-new-sans-construct ...                                        */
;*---------------------------------------------------------------------*/
(define (js-new-sans-construct %this ctor)
   ;; used to initialize classes
   (cond
      ((js-function? ctor)
       (with-access::JsFunction ctor (name alloc)
	  (let ((o (alloc %this ctor)))
	     (with-access::JsGlobalObject %this (js-new-target)
		(set! js-new-target (js-undefined)))
	     (js-new-return ctor o o))))
      ((js-proxy? ctor)
       (js-new/proxy %this ctor '()))
      (else
       (js-raise-type-error %this "new: object is not a function ~s" ctor))))

;*---------------------------------------------------------------------*/
;*    js-newXXX ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (gen-new %this ctor . args)
   `(cond
       ((js-function? ,ctor)
	(with-access::JsFunction ,ctor (src construct alloc)
	   (let ((o (alloc %this ,ctor)))
	      (let ((r (gen-calln ,ctor construct o ,@args)))
		 (js-new-return ,ctor r o)))))
       ((js-proxy? ,ctor)
	(js-new/proxy ,%this ,ctor (list ,@args)))
       (else
	(js-raise-type-error ,%this "new: object is not a function ~s" ,ctor))))

(define (js-new0 %this ctor)
   (gen-new %this ctor))
(define (js-new1 %this ctor a0)
   (gen-new %this ctor a0))
(define (js-new2 %this ctor a0 a1)
   (gen-new %this ctor a0 a1))
(define (js-new3 %this ctor a0 a1 a2)
   (gen-new %this ctor a0 a1 a2))
(define (js-new4 %this ctor a0 a1 a2 a3)
   (gen-new %this ctor a0 a1 a2 a3))
(define (js-new5 %this ctor a0 a1 a2 a3 a4)
   (gen-new %this ctor a0 a1 a2 a3 a4))
(define (js-new6 %this ctor a0 a1 a2 a3 a4 a5)
   (gen-new %this ctor a0 a1 a2 a3 a4 a5))
(define (js-new7 %this ctor a0 a1 a2 a3 a4 a5 a6)
   (gen-new %this ctor a0 a1 a2 a3 a4 a5 a6))
(define (js-new8 %this ctor a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-new %this ctor a0 a1 a2 a3 a4 a5 a6 a7))

;*---------------------------------------------------------------------*/
;*    jsarray ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (jsarray %this . args)
   `(let ((a (js-array-construct-alloc-small-sans-init ,%this
		 ,(fixnum->uint32 (length args)))))
       (with-access::JsArray a (vec ilen length)
	  (let ((v vec))
	     ,@(map (lambda (i o)
		       `(vector-set! v ,i ,o))
		  (iota (length args)) args)
	     (set! ilen ,(fixnum->uint32 (length args)))
	     a))))

;*---------------------------------------------------------------------*/
;*    js-call-proxyn ...                                               */
;*---------------------------------------------------------------------*/
(define (js-call-proxyn %this fun this args)
   (with-access::JsProxy fun (handler)
      (let ((target (js-proxy-target fun)))
	 (cond
	    ((js-procedure? target)
	     (let ((xfun (js-get handler (& "apply") %this)))
		(if (js-procedure? xfun)
		    (with-access::JsProcedure xfun (procedure)
		       (js-call3% %this xfun procedure fun target
			  this (js-vector->jsarray (list->vector args) %this)))
		    (with-access::JsProcedure target (procedure)
		       (js-calln% %this target procedure this args)))))
	    ((and (js-proxy? target) (js-proxy-function? target))
	     (js-call-proxyn %this (js-proxy-target fun) this args))
	    (else
	     (js-raise-type-error %this "calln: not a function ~s" fun))))))

;*---------------------------------------------------------------------*/
;*    js-apply-proxy ...                                               */
;*---------------------------------------------------------------------*/
(define (js-apply-proxy %this fun this args)
   (with-access::JsProxy fun (handler)
      (let ((target (js-proxy-target fun)))
	 (cond
	    ((js-procedure? target)
	     (let ((xfun (js-get handler (& "apply") %this)))
		(if (js-procedure? xfun)
		    (with-access::JsProcedure xfun (procedure)
		       (js-call3% %this xfun procedure fun target
			  this (js-vector->jsarray (list->vector args) %this)))
		    (with-access::JsProcedure target (procedure)
		       (js-calln% %this target procedure this args)))))
	    ((js-proxy? target)
	     (js-apply-proxy %this (js-proxy-target fun) this args))
	    (else
	     (js-raise-type-error %this "apply: not a function ~s" fun))))))

;*---------------------------------------------------------------------*/
;*    js-service/debug ...                                             */
;*---------------------------------------------------------------------*/
(define (js-service/debug name loc thunk)
   (let ((env (current-dynamic-env)))
      ($env-push-trace env name loc)
      (let ((aux (thunk)))
	 ($env-pop-trace env)
	 aux)))

;*---------------------------------------------------------------------*/
;*    js-ordinary-instanceof? ...                                      */
;*---------------------------------------------------------------------*/
(define (js-ordinary-instanceof? %this v f)
   (with-access::JsFunction f (%prototype prototype)
      (when %prototype
	 (let ((o prototype))
	    (if (not (js-object? o))
		(js-raise-type-error %this "instanceof: no prototype ~s" v)
		(let loop ((v v))
		   (let ((nv (js-object-proto v)))
		      (cond
			 ((eq? o nv)
			  #t)
			 ((eq? nv (js-null))
			  (when (eq? (object-class v) JsProxy)
			     (loop (js-proxy-target v))))
			 (else
			  (loop nv))))))))))

;*---------------------------------------------------------------------*/
;*    js-function-instanceof? ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.6       */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5.3   */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.5.3     */
;*---------------------------------------------------------------------*/
(define (js-object-function-instanceof? %this v::JsObject f::JsFunction)
   (if (js-object-mode-hasinstance? f)
       (with-access::JsGlobalObject %this (js-symbol-hasinstance)
	  (let ((h (js-get-jsobject f f js-symbol-hasinstance %this)))
	     (if (js-function? h)
		 (js-call1 %this h f v)
		 (js-ordinary-instanceof? %this v f))))
       (with-access::JsFunction f (prototype %prototype)
	  (when %prototype
	     (let ((o prototype))
		(if (not (js-object? o))
		    (js-raise-type-error %this "instanceof: no prototype ~s" v)
		    (let loop ((v v))
		       (let ((nv (js-object-proto v)))
			  (cond
			     ((eq? o nv)
			      #t)
			     ((eq? nv (js-null))
			      (when (eq? (object-class v) JsProxy)
				 (loop (js-proxy-target v))))
			     (else
			      (loop nv)))))))))))

(define (js-function-instanceof? %this v f::JsFunction)
   (when (js-object? v)
      (if (js-object-mode-hasinstance? f)
	  (with-access::JsGlobalObject %this (js-symbol-hasinstance)
	     (let ((h (js-get-jsobject f f js-symbol-hasinstance %this)))
		(if (js-function? h)
		    (js-call1 %this h f v)
		    (js-ordinary-instanceof? %this v f))))
	  (js-ordinary-instanceof? %this v f))))

(define (js-instanceof? %this v f)
   (if (not (js-function? f))
       (with-access::JsGlobalObject %this (js-symbol-hasinstance)
	  (let ((h (js-get f js-symbol-hasinstance %this)))
	     (if (js-function? h)
		 (js-call1 %this h f v)
		 (js-raise-type-error %this
		    "instanceof: not a function ~s" f))))
       (when (js-object? v)
	  (if (js-object-mode-hasinstance? f)
	      (with-access::JsGlobalObject %this (js-symbol-hasinstance)
		 (let ((h (js-get-jsobject f f js-symbol-hasinstance %this)))
		    (if (js-function? h)
			(js-call1 %this h f v)
			(js-ordinary-instanceof? %this v f))))
	      (js-ordinary-instanceof? %this v f)))))

(define (js-instanceof?/debug %this loc v f)
   (if (not (js-function? f))
       (with-access::JsGlobalObject %this (js-symbol-hasinstance)
	  (let ((h (js-get f js-symbol-hasinstance %this)))
	     (if (js-function? h)
		 (js-call1 %this h f v)
		 (js-raise-type-error/loc %this loc
		    "instanceof: not a function ~s" f))))
       (when (js-object? v)
	  (if (js-object-mode-hasinstance? f)
	      (with-access::JsGlobalObject %this (js-symbol-hasinstance)
		 (let ((h (js-get f js-symbol-hasinstance %this)))
		    (if (js-function? h)
			(js-call1 %this h f v)
			(js-ordinary-instanceof? %this v f))))
	      (js-ordinary-instanceof? %this v f)))))

;*---------------------------------------------------------------------*/
;*    js-make-let ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-make-let)
   #\Z)

;*---------------------------------------------------------------------*/
;*    js-let-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (js-let-ref val ident loc %this)
   (if (eq? val #\Z)
       (js-raise-reference-error/loc %this loc "dead-zone access" ident)
       val))

;*---------------------------------------------------------------------*/
;*    js-totest ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-inline (js-totest obj)
   (if (boolean? obj) obj (js-toboolean-no-boolean obj)))
      
;*---------------------------------------------------------------------*/
;*    js-totest-likely-object ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-inline (js-totest-likely-object obj)
   (or (and (object? obj) (eq? (object-class obj) JsObject))
       (js-toboolean obj)))
      
;*---------------------------------------------------------------------*/
;*    js-toboolean ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.2          */
;*---------------------------------------------------------------------*/
(define (js-toboolean obj)
   (cond
      ((js-null-or-undefined? obj) #f)
      ((boolean? obj) obj)
      ((js-jsstring? obj) (js-jsstring-toboolean obj))
      ((object? obj) #t)
      ((fixnum? obj) (not (=fx obj 0)))
      ((flonum? obj) (not (or (=fl obj 0.0) (nanfl? obj))))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    js-toboolean-no-boolean ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.2          */
;*---------------------------------------------------------------------*/
(define (js-toboolean-no-boolean obj)
   (cond
      ((js-null-or-undefined? obj) #f)
      ((js-jsstring? obj) (js-jsstring-toboolean obj))
      ((object? obj) #t)
      ((fixnum? obj) (not (=fx obj 0)))
      ((flonum? obj) (not (or (=fl obj 0.0) (nanfl? obj))))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-generic (js-tonumber obj %this::JsGlobalObject)
   (cond
      ((js-number? obj)
       obj)
      ((eq? obj (js-undefined))
       +nan.0)
      ((eq? obj (js-null))
       0)
      ((eq? obj #t)
       1)
      ((eq? obj #f)
       0)
      ((string? obj)
       (js-string->number obj %this))
      (else
       (bigloo-type-error "toNumber" "JsObject" obj))))

;*---------------------------------------------------------------------*/
;*    js-tofixnum ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (js-tointeger obj %this)
   (if (symbol? obj)
       `(if (fixnum? ,obj)
	    ,obj
	    ((@ js-tointeger __hopscript_public) ,obj ,%this))
       (let ((tmp (gensym)))
	  `(let ((,tmp ,obj))
	      (js-tointeger ,tmp ,%this)))))
	   
;*---------------------------------------------------------------------*/
;*    js-tointeger ::obj ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.4          */
;*---------------------------------------------------------------------*/
(define-generic (js-tointeger obj %this::JsGlobalObject)
   (cond
      ((fixnum? obj)
       obj)
      ((flonum? obj)
       (cond
	  ((nanfl? obj) 0)
	  ((or (=fl obj +inf.0) (=fl obj -inf.0))
	   obj)
	  ((=fx (signbitfl obj) 1)
	   (*fl -1. (floor (abs obj))))
	  (else
	   (floor obj))))
      ((or (js-jsstring? obj) (symbol? obj))
       ((@ js-tointeger __hopscript_public) (js-tonumber obj %this) %this))
      ((eq? obj #t)
       1)
      ((int32? obj)
       (js-int32-tointeger obj))
      ((uint32? obj)
       (js-uint32-tointeger obj))
      (else 0)))

;*---------------------------------------------------------------------*/
;*    js-touint16 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.7          */
;*---------------------------------------------------------------------*/
(define (js-touint16::uint16 obj %this)
   
   (define 2^16 (exptfl 2. 16.))
   
   (define (uint32->uint16 o)
      (fixnum->uint16 (uint32->fixnum o)))
   
   (define (positive-double->uint16::uint16 obj::double)
      (uint32->uint16
	 (if (<fl obj 2^16)
	     (flonum->uint32 obj)
	     (flonum->uint32 (remainderfl obj 2^16)))))
   
   (define (double->uint16::uint16 obj::double)
      (cond
	 ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
	  #u16:0)
	 ((<fl obj 0.)
	  (positive-double->uint16 (+fl 2^16 (*fl -1. (floor (abs obj))))))
	 (else
	  (positive-double->uint16 obj))))
   
   (cond
      ((fixnum? obj) (modulofx obj (bit-lsh 1 16)))
      ((flonum? obj) (double->uint16 obj))
      (else (js-touint16 (js-tointeger obj %this) %this))))

;*---------------------------------------------------------------------*/
;*    js-toindex ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4         */
;*    -------------------------------------------------------------    */
;*    Performance demands this function not to returned a boxed        */
;*    result. So, false is here denoted 1^32-1, as an uint32.          */
;*---------------------------------------------------------------------*/
(define-generic (js-toindex p)
   
   (define false (-u32 #u32:0 #u32:1))

   (cond
      ((fixnum? p)
       (cond-expand
	  (bint30
	   (if (>=fx p 0)
	       (fixnum->uint32 p)
	       false))
	  (bint32
	   (let ((e (fixnum->elong p)))
	      (if (and (>=elong e #e0) (<=elong e (bit-lshelong #e1 31)))
		  (elong->uint32 e)
		  false)))
	  (else
	   (if (and (>=fx p 0) (<fx p (-fx (bit-lsh 1 32) 1)))
	       (fixnum->uint32 p)
	       false))))
      ((flonum? p)
       (if (and (>=fl p 0.) (<fl p (exptfl 2. 31.)) (=fl (roundfl p) p))
	   (cond-expand
	      (bint30
	       (if (<fl p (exptfl 2. 32.))
		   (flonum->uint32 p)
		   (llong->uint32 (flonum->llong p))))
	      (else
	       (flonum->uint32 p)))
	   false))
      ((uint32? p)
       p)
      ((int32? p)
       (int32->uint32 p))
      (else
       false)))

;*---------------------------------------------------------------------*/
;*    js-isindex? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-isindex? u32::uint32)
   (<u32 u32 (-u32 (fixnum->uint32 0) (fixnum->uint32 1))))

;*---------------------------------------------------------------------*/
;*    js-index? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is a number an index?                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-index? num)
   (and (fixnum? num)
	(>=fx num 0)
	(cond-expand
	   ((or bint30 bint32) #t)
	   (else (<fx num (-fx (bit-lsh 1 31) 1))))))

;*---------------------------------------------------------------------*/
;*    js-tostring ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.8          */
;*---------------------------------------------------------------------*/
(define-generic (js-tostring::bstring obj %this::JsGlobalObject)
   (cond
      ((string? obj) obj)
      ((eq? obj (js-undefined)) "undefined")
      ((eq? obj #t) "true")
      ((eq? obj #f) "false")
      ((eq? obj (js-null)) "null")
      ((js-number? obj) (js-number->string obj))
      (else (typeof obj))))

;*---------------------------------------------------------------------*/
;*    js-tojsstring-safe ...                                           */
;*---------------------------------------------------------------------*/
(define (js-tojsstring-safe::JsStringLiteral obj %this::JsGlobalObject)
   (cond
      ((js-jsstring? obj) obj)
      ((eq? obj #t) (& "true"))
      ((eq? obj #f) (& "false"))
      ((fixnum? obj) (js-integer->jsstring obj))
      ((js-number? obj) (js-ascii->jsstring (js-number->string obj)))
      ((isa? obj JsSymbolLiteral) (js-string->jsstring (js-tostring obj %this)))
      (else (js-tojsstring (js-toobject %this obj) %this))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsWrapper ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsWrapper %this::JsGlobalObject)
   (with-access::JsWrapper obj (obj)
      (js-tostring obj %this)))

;*---------------------------------------------------------------------*/
;*    js-toprimitive-for-string ...                                    */
;*---------------------------------------------------------------------*/
(define (js-toprimitive-for-string obj %this::JsGlobalObject)
   (cond
      ((js-jsstring? obj) obj)
      ((fixnum? obj) (js-integer->jsstring obj))
      ((js-number? obj) (js-ascii->jsstring (js-number->string obj)))
      ((eq? obj #t) (& "true"))
      ((eq? obj #f) (& "false"))
      (else (js-tojsstring (js-toprimitive obj 'any %this) %this))))
   
;*---------------------------------------------------------------------*/
;*    js-toprimitive ::obj ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.1          */
;*---------------------------------------------------------------------*/
(define-generic (js-toprimitive obj preferredtype %this::JsGlobalObject)
   obj)

;*---------------------------------------------------------------------*/
;*    js-toprimitive ::JsWrapper ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive obj::JsWrapper preferredtype %this::JsGlobalObject)
   (with-access::JsWrapper obj (obj)
      obj))

;*---------------------------------------------------------------------*/
;*    js-tojsstring ...                                                */
;*---------------------------------------------------------------------*/
(define (js-tojsstring::JsStringLiteral obj %this)

   (define (primitive->jsstring obj)
      (let ((p (js-toprimitive obj 'string %this)))
	 (if (eq? p obj)
	     (js-ascii->jsstring (format "#<~a>" (typeof p)))
	     (js-tojsstring p %this))))
      
   (cond
      ((js-jsstring? obj) obj)
      ((fixnum? obj) (js-integer->jsstring obj))
      ((eq? obj (js-undefined)) (& "undefined"))
      ((eq? obj #t) (& "true"))
      ((eq? obj #f) (& "false"))
      ((eq? obj (js-null)) (& "null"))
      ((js-number? obj) (js-ascii->jsstring (js-number->string obj)))
      ((isa? obj JsSymbolLiteral) (js-string->jsstring (js-tostring obj %this)))
      ((string? obj) (js-string->jsstring obj))
      (else (primitive->jsstring obj))))

;*---------------------------------------------------------------------*/
;*    js-toobject-failsafe ...                                         */
;*---------------------------------------------------------------------*/
(define (js-toobject-failsafe %this::JsGlobalObject o)
   (cond
      ((js-jsstring? o)
       (with-access::JsGlobalObject %this (js-string)
	  (js-new1 %this js-string o)))
      ((js-number? o)
       (with-access::JsGlobalObject %this (js-number)
	  (js-new1 %this js-number o)))
      ((boolean? o)
       (with-access::JsGlobalObject %this (js-boolean)
	  (js-new1 %this js-boolean o)))
      ((isa? o JsSymbolLiteral)
       (with-access::JsGlobalObject %this (js-symbol-ctor)
	  (js-symbol-ctor (js-undefined) o)))
      ((js-object? o)
       o)
      ((isa? o object)
       o)
      ((pair? o)
       o)
      ((string? o)
       (with-access::JsGlobalObject %this (js-string)
	  (js-new1 %this js-string o)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-toobject-fast ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-toobject-fast o %this::JsGlobalObject)
   (if (js-object? o) o (js-toobject %this o)))

;*---------------------------------------------------------------------*/
;*    js-toobject ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.9          */
;*---------------------------------------------------------------------*/
(define (js-toobject %this::JsGlobalObject o)
   (or (js-toobject-failsafe %this o)
       (js-raise-type-error %this "toObject: cannot convert ~s" o)))

;*---------------------------------------------------------------------*/
;*    js-toobject/debug ...                                            */
;*---------------------------------------------------------------------*/
(define (js-toobject/debug %this::JsGlobalObject loc o)
   (or (js-toobject-failsafe %this o)
       (js-raise-type-error/loc %this loc "toObject: cannot convert ~s" o)))

;*---------------------------------------------------------------------*/
;*    js-equal? ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.1       */
;*---------------------------------------------------------------------*/
(define-inline (js-equal? o1 o2 %this::JsGlobalObject)
   (or (and (eq? o1 o2) (not (flonum? o1)))
       (and (not (fixnums? o1 o2)) (js-equality? o1 o2 %this))))

;*---------------------------------------------------------------------*/
;*    js-equal-fixnum? ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.1       */
;*---------------------------------------------------------------------*/
(define-inline (js-equal-fixnum? o1 o2 %this::JsGlobalObject)
   (or (eq? o1 o2) (if (fixnum? o2) #f (js-equality? o1 o2 %this))))

;*---------------------------------------------------------------------*/
;*    js-equal-sans-flonum? ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.1       */
;*---------------------------------------------------------------------*/
(define-inline (js-equal-sans-flonum? o1 o2 %this::JsGlobalObject)
   (or (eq? o1 o2) (js-equality? o1 o2 %this)))

;*---------------------------------------------------------------------*/
;*    js-equality? ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.3       */
;*---------------------------------------------------------------------*/
(define (js-equality? x y %this::JsGlobalObject)
   (let equality? ((x x)
		   (y y))
      (cond
	 ((eq? x y)
	  (not (and (flonum? x) (nanfl? x))))
	 ((eq? x (js-null))
	  (eq? y (js-undefined)))
	 ((eq? x (js-undefined))
	  (eq? y (js-null)))
	 ((js-number? x)
	  (cond
	     ((js-number? y)
	      (= x y))
	     ((js-jsstring? y)
	      (if (= x 0)
		  (or (js-jsstring-null? y) (equality? x (js-tonumber y %this)))
		  (equality? x (js-tonumber y %this))))
	     ((js-object? y)
	      (equality? x ((@ js-toprimitive __hopscript_public) y 'any %this)))
	     ((boolean? y)
	      (equality? x (js-tonumber y %this)))
	     (else #f)))
	 ((js-jsstring? x)
	  (cond
	     ((js-jsstring? y)
	      (js-jsstring=? x y))
	     ((js-number? y)
	      (if (= y 0)
		  (or (js-jsstring-null? x) (equality? (js-tonumber x %this) y))
		  (equality? (js-tonumber x %this) y)))
	     ((js-object? y)
	      (equality? x ((@ js-toprimitive __hopscript_public) y 'any %this)))
	     ((eq? y #f)
	      (js-jsstring-null? x))
	     ((boolean? y)
	      (equality? x (js-tonumber y %this)))
	     (else #f)))
	 ((boolean? x)
	  (cond
	     ((boolean? y)
	      #f)
	     (else
	      (equality? (js-tonumber x %this) y))))
	 ((boolean? y)
	  (equality? x (js-tonumber y %this)))
	 ((js-object? x)
	  (cond
	     ((js-jsstring? y)
	      (equality? ((@ js-toprimitive __hopscript_public) x 'any %this) y))
	     ((js-number? y)
	      (equality? ((@ js-toprimitive __hopscript_public) x 'any %this) y))
	     ((isa? y JsSymbolLiteral)
	      (equality? ((@ js-toprimitive __hopscript_public) x 'any %this) y))
	     (else #f)))
	 ((isa? x JsSymbolLiteral)
	  (if (js-object? y)
	      (equality? x ((@ js-toprimitive __hopscript_public) y 'any %this))
	      #f))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    js-same-value-zero? ...                                          */
;*    -------------------------------------------------------------    */
;*    https://www.ecma-international.org/ecma-262/6.0/                 */
;*       #sec-samevaluezero                                            */
;*---------------------------------------------------------------------*/
(define (js-same-value-zero? x y %this::JsGlobalObject)
   (js-equality? x y %this))
   

;*---------------------------------------------------------------------*/
;*    js-strict-equal?                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.4       */
;*---------------------------------------------------------------------*/
(define-inline (js-strict-equal? o1 o2)
   (or (and (eq? o1 o2) (not (flonum? o1))) (js-eq? o1 o2)))

;*---------------------------------------------------------------------*/
;*    js-strict-equal-no-string?                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.4       */
;*---------------------------------------------------------------------*/
(define-inline (js-strict-equal-no-string? o1 o2)
   (or (and (eq? o1 o2) (not (flonum? o1))) (js-eq-no-string? o1 o2)))

;*---------------------------------------------------------------------*/
;*    js-eq? ...                                                       */
;*---------------------------------------------------------------------*/
(define (js-eq? x y)
   (cond
      ((js-jsstring? x)
       (and (js-jsstring? y)
	    (string=? (js-jsstring->string x) (js-jsstring->string y))))
      ((fixnum? x)
       (if (fixnum? y) (=fx x y) (when (flonum? y) (=fl (fixnum->flonum x) y))))
      ((flonum? x)
       (if (flonum? y) (=fl x y) (when (fixnum? y) (=fl x (fixnum->flonum y)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-eq-no-string? ...                                             */
;*---------------------------------------------------------------------*/
(define (js-eq-no-string? x y)
   (cond
      ((flonum? x)
       (if (flonum? y) (=fl x y) (when (fixnum? y) (=fl x (fixnum->flonum y)))))
      ((fixnum? x)
       (if (fixnum? y) (=fx x y) (when (flonum? y) (=fl (fixnum->flonum x) y))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-eqstring? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (js-eqstring?::bool x y)
   (or (eq? x y)
       (and (js-jsstring? x) (js-jsstring? y)
	    (string=? (js-jsstring->string x) (js-jsstring->string y)))))

;*---------------------------------------------------------------------*/
;*    js-eqil? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-eqil? x y)
   (cond
      ((fixnum? y) (=fx x y))
      ((flonum? y) (=fl (fixnum->flonum x) y))))

;*---------------------------------------------------------------------*/
;*    js-eqir? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (js-eqir? x y)
   (cond
      ((fixnum? x) (=fx x y))
      ((flonum? x) (=fl x (fixnum->flonum y)))))

;*---------------------------------------------------------------------*/
;*    js-null-or-undefined? ...                                        */
;*    -------------------------------------------------------------    */
;*    This inline function is override by a macro that checks the      */
;*    implementation of JS-NULL and JS-UNDEFINED in order to           */
;*    avoid the double test when possible.                             */
;*---------------------------------------------------------------------*/
(define-inline (js-null-or-undefined? obj)
   (cond-expand
      (bigloo4.3c
       (or (eq? obj (js-undefined)) (eq? obj (js-null))))
      (else
       (null-or-unspecified? obj))))

;*---------------------------------------------------------------------*/
;*    js-object-or-null? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-object-or-null? o)
   (or (and (js-object? o) (not (js-procedure? o)))
       (eq? o (js-null))))

;*---------------------------------------------------------------------*/
;*    js-super ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-super obj loc %this)
   (if (js-object? obj)
       (let ((__proto__ (js-object-proto obj)))
	  (if (js-object? __proto__)
	      (js-object-proto __proto__)
	      (js-raise-type-error/loc %this loc
		 "Prototype of prototype not an object" obj)))
       (js-raise-type-error/loc %this loc "Not an object" obj)))
	  
;*---------------------------------------------------------------------*/
;*    %js-hss ...                                                      */
;*---------------------------------------------------------------------*/
(define (%js-eval-hss ip::input-port %this %worker scope)
   (js-worker-exec %worker "eval-hss" #t
      (lambda ()
	 (let ((v (%js-eval ip 'repl %this (js-get scope (& "this") %this) scope)))
	    (if (js-jsstring? v)
		(js-jsstring->string v)
		v)))))

;*---------------------------------------------------------------------*/
;*    lib-hopscript-path ...                                           */
;*---------------------------------------------------------------------*/
(define lib-hopscript-path
   (make-file-path (hop-lib-directory) "hop" (hop-version)))

;*---------------------------------------------------------------------*/
;*    %js-direct-eval ...                                              */
;*    -------------------------------------------------------------    */
;*    tests:                                                           */
;*      ch11/11.13/11.13.2/S11.13.2_A1_T1.js                           */
;*---------------------------------------------------------------------*/
(define (%js-direct-eval s strict %this::JsGlobalObject this scope)
   
   (define (open-string s)
      (with-access::JsGlobalObject %this (js-input-port)
	 (if js-input-port
	     (reopen-input-c-string js-input-port s)
	     (let ((port (open-input-string s)))
		(set! js-input-port port)
		port))))
   
   (define (%eval s)
      (let ((ip (open-string s)))
	 (unwind-protect
	    (%js-eval ip (if strict 'eval-strict 'eval) %this this scope)
	    (close-input-port ip))))

   (define (close-blank ip)
      (let loop ()
	 (let ((c (read-char ip)))
	    (if (eof-object? c)
		(begin
		   (close-input-port ip)
		   #t)
		(if (memq c '(#\space #\tab #\newline #\return))
		    (loop)
		    (begin
		       (close-input-port ip)
		       #f))))))
		 
   (define (%json s)
      (let ((ip (open-string s))
	    (exn #f))
	 (let ((e (with-handler
		     (lambda (_)
			(set! exn #t)
			(close-input-port ip))
		     (js-json-parser ip #f #t #t %this))))
	    (if (and (not exn) (close-blank ip))
		e
		(%eval s)))))
   
   (define (%json-expr s)
      (let ((ip (open-string s))
	    (exn #f))
	 (read-char ip)
	 (let ((e (with-handler
		     (lambda (_)
			(set! exn #t)
			(close-input-port ip))
		     (js-json-parser ip #f #t #t %this))))
	    (cond
	       (exn
		(%eval s))
	       ((char=? (read-char ip) #\))
		(if (close-blank ip)
		    e
		    (%eval s)))
	       (else
		(close-input-port ip)
		(%eval s))))))
   
   (if (not (js-jsstring? s))
       s
       (let ((s (js-jsstring->string s)))
	  (if (=fx (string-length s) 0)
	      (js-undefined)
	      (case (string-ref s 0)
		 ((#\[ #\" #\')
		  (%json s))
		 ((#\{)
		  (if (pregexp-match "{[\t\n ]*}" s)
		      (js-undefined)
		      (%json s)))
		 ((#\()
		  (%json-expr s))
		 (else
		  (%eval s)))))))

;*---------------------------------------------------------------------*/
;*    %js-eval ...                                                     */
;*---------------------------------------------------------------------*/
(define (%js-eval in::input-port parser::symbol %this::JsGlobalObject this scope)
   (library-load 'hopscript lib-hopscript-path)
   ;; bind the global object
   (with-trace 'hopscript-eval '%js-eval
      (trace-item "in=" (input-port-name in))
      (with-handler
	 (lambda (e)
	    (cond
	       ((isa? e &io-parse-error)
		(with-access::&io-parse-error e (proc msg obj fname location)
		   (js-raise-syntax-error %this
		      (format "~a: ~a -- ~a" proc msg obj)
		      obj
		      fname location)))
	       ((isa? e &io-error)
		(with-access::&io-error e (proc msg obj fname location)
		   (js-raise-error %this
		      (format "~a: ~a -- ~a" proc msg obj)
		      obj
		      fname location)))
	       ((isa? e &error)
		(with-access::&error e (proc msg obj fname location)
		   (cond
		      ((and (string? proc) (string=? proc "assignment"))
		       (js-raise-reference-error %this
			  (format "~a -- ~a" msg obj)
			  obj
			  fname location))
		      (else
		       (js-raise-error %this
			  (format "~a: ~a -- ~a" proc msg obj)
			  obj
			  fname location)))))
	       (else
		(raise e))))
	 (let ((e (j2s-compile in
		     :verbose 0
		     :eval #t
		     :driver (j2s-eval-driver)
		     :driver-name "j2s-eval-driver"
		     :es6-arrow-function #t
		     :es6-let #t
		     :es6-defaut-value #t
		     :es6-rest-argument #t
		     :commonjs-export #f
		     :parser parser))
	       (m (js-get scope (& "module") scope)))
	    (with-trace 'hopscript-eval "%js-eval-inner"
	       (trace-item "e=" e)
	       (trace-item "scope=" (typeof scope))
	       (let ((r (eval `(,e ,%this
				  ,this
				  ,scope
				  ,(if (eq? m (js-undefined))
				       (eval-dummy-module %this)
				       m)))))
		  (trace-item "r=" r)
		  r))))))

;*---------------------------------------------------------------------*/
;*    eval-dummy-module ...                                            */
;*---------------------------------------------------------------------*/
(define (eval-dummy-module %this)
   (let ((obj (instantiateJsModule
		 (__proto__ (js-object-proto %this)))))
      (js-put! obj (& "filename") (js-string->jsstring "") #f %this)
      obj))

;*---------------------------------------------------------------------*/
;*    js-raise ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-raise err)
   (with-access::JsError err (stack)
      (set! stack (get-trace-stack))
      (raise err)))

;*---------------------------------------------------------------------*/
;*    js-throw ...                                                     */
;*    -------------------------------------------------------------    */
;*    This function is called by the compiled form of "throw".         */
;*---------------------------------------------------------------------*/
(define (js-throw err f l)
   (when (isa? err JsError)
      (with-access::JsError err (stack fname location)
	 (set! stack (get-trace-stack))
	 (unless (js-jsstring? fname)
	    (set! fname f)
	    (set! location l))))
   (raise err))

;*---------------------------------------------------------------------*/
;*    js-throw/debug ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is called by the compiled form of "throw".         */
;*---------------------------------------------------------------------*/
(define (js-throw/debug err f l %worker)
   (when (isa? err JsError)
      (with-access::JsError err (stack fname location)
	 (set! stack (get-trace-stack))
	 (unless (js-jsstring? fname)
	    (set! fname f)
	    (set! location l))))
   (with-access::WorkerHopThread %worker (%exn)
      (set! %exn
	 (instantiate::&error
	    (proc "throw")
	    (stack (get-trace-stack))
	    (fname f)
	    (location l)
	    (msg "Uncaught exception")
	    (obj err))))
   (raise err))

;*---------------------------------------------------------------------*/
;*    error-obj->string ...                                            */
;*---------------------------------------------------------------------*/
(define (error-obj->string::bstring %this obj)
   (cond
      ((js-object? obj)
       (with-handler
	  (lambda (e)
	     (js-jsstring->string (js-typeof obj %this)))
	  (js-jsstring->string (js-typeof obj %this))))
      ((eq? obj #unspecified)
       "undefined")
      ((eq? obj #f)
       "false")
      ((eq? obj #t)
       "true")
      ((js-jsstring? obj)
       (js-jsstring->string obj))
      ((string? obj)
       obj)
      ((or (js-number? obj) (int32? obj) (uint32? obj))
       (number->string obj))
      (else
       (js-jsstring->string (js-typeof obj %this)))))

;*---------------------------------------------------------------------*/
;*    js-raise-type-error ...                                          */
;*---------------------------------------------------------------------*/
(define (js-raise-type-error %this::JsGlobalObject fmt::bstring obj)
   (with-access::JsGlobalObject %this (js-type-error)
      (js-raise
	 (js-new %this js-type-error
	    (js-string->jsstring
	       (format fmt (error-obj->string %this obj)))))))

;*---------------------------------------------------------------------*/
;*    js-raise-type-error/loc ...                                      */
;*---------------------------------------------------------------------*/
(define (js-raise-type-error/loc %this::JsGlobalObject loc fmt::bstring obj)
   (match-case loc
      ((at ?fname ?loc)
       (with-access::JsGlobalObject %this (js-type-error)
	  (js-raise
	     (js-new %this js-type-error
		(js-string->jsstring (format fmt (error-obj->string %this obj)))
		fname
		loc))))
      (else
       (js-raise-type-error %this fmt obj))))

;*---------------------------------------------------------------------*/
;*    js-raise-range-error ...                                         */
;*---------------------------------------------------------------------*/
(define (js-raise-range-error %this::JsGlobalObject fmt::bstring obj)
   (with-access::JsGlobalObject %this (js-range-error)
      (js-raise
	 (js-new %this js-range-error
	    (js-string->jsstring (format fmt obj))))))

;*---------------------------------------------------------------------*/
;*    js-raise-uri-error ...                                           */
;*---------------------------------------------------------------------*/
(define (js-raise-uri-error %this::JsGlobalObject fmt::bstring obj)
   (with-access::JsGlobalObject %this (js-uri-error)
      (js-raise
	 (js-new %this js-uri-error
	    (js-string->jsstring (format fmt obj))))))

;*---------------------------------------------------------------------*/
;*    js-raise-syntax-error ...                                        */
;*---------------------------------------------------------------------*/
(define (js-raise-syntax-error %this::JsGlobalObject fmt::bstring obj . args)
   (with-access::JsGlobalObject %this (js-syntax-error)
      (js-raise
	 (apply js-new %this js-syntax-error
	    (js-string->jsstring (format fmt obj)) args))))

;*---------------------------------------------------------------------*/
;*    js-raise-syntax-error/loc ...                                    */
;*---------------------------------------------------------------------*/
(define (js-raise-syntax-error/loc %this::JsGlobalObject loc fmt::bstring obj)
   (match-case loc
      ((at ?fname ?loc)
       (with-access::JsGlobalObject %this (js-syntax-error)
	  (js-raise
	     (js-new %this js-syntax-error
		(js-string->jsstring (format fmt obj)) fname loc))))
      (else
       (js-raise-syntax-error %this fmt obj))))

;*---------------------------------------------------------------------*/
;*    js-raise-reference-error ...                                     */
;*---------------------------------------------------------------------*/
(define (js-raise-reference-error %this::JsGlobalObject fmt::bstring obj . args)
   (with-access::JsGlobalObject %this (js-reference-error)
      (js-raise
	 (apply js-new %this js-reference-error
	    (js-string->jsstring (format fmt obj)) args))))

;*---------------------------------------------------------------------*/
;*    js-raise-reference-error/loc ...                                 */
;*---------------------------------------------------------------------*/
(define (js-raise-reference-error/loc %this::JsGlobalObject loc fmt::bstring obj . args)
   (with-access::JsGlobalObject %this (js-reference-error)
      (match-case loc
	 ((at ?fname ?loc)
	  (js-raise
	     (apply js-new %this js-reference-error
		(js-string->jsstring (format fmt obj)) fname loc args)))
	 (else
	  (apply js-raise-reference-error %this fmt obj args)))))

;*---------------------------------------------------------------------*/
;*    js-raise-error ...                                               */
;*---------------------------------------------------------------------*/
(define (js-raise-error %this::JsGlobalObject fmt::bstring obj . args)
   (with-access::JsGlobalObject %this (js-error)
      (js-raise
	 (apply js-new %this js-error
	    (js-string->jsstring (format fmt obj)) args))))

;*---------------------------------------------------------------------*/
;*    js-cast-object ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (js-cast-object obj %this::JsGlobalObject name)
   (cond
      ((js-object? obj)
       obj)
      ((pair? obj)
       obj)
      (else
       (js-raise-type-error %this
	  (format "[[~a]]: not an object \"~~a\"" name)
	  obj))))

;*---------------------------------------------------------------------*/
;*    js-inspect ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (js-inspect o cnt)
   (cond
      ((string? o)
       (js-string->jsstring o))
      ((< cnt 0)
       (js-string->jsstring "..."))
      (else
       (js-string->jsstring
	  (call-with-output-string
	     (lambda (op)
		(write-circle o op)))))))

;*---------------------------------------------------------------------*/
;*    js-typeof ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3       */
;*---------------------------------------------------------------------*/
(define (js-typeof obj %this)
   (cond
      ((or (fixnum? obj) (real? obj))
       (& "number"))
      ((js-jsstring? obj)
       (& "string"))
      ((js-procedure? obj)
       (& "function"))
      ((eq? obj (js-undefined))
       (& "undefined"))
      ((isa? obj JsSymbolLiteral)
       (& "symbol"))
      ((js-proxy? obj)
       (js-proxy-typeof obj %this))
      ((js-object? obj)
       (& "object"))
      ((boolean? obj)
       (& "boolean"))
      ((eq? obj (js-null))
       (& "object"))
      (else
       (js-string->jsstring (typeof obj)))))

;*---------------------------------------------------------------------*/
;*    js-html-head ...                                                 */
;*    -------------------------------------------------------------    */
;*    Normally overriden by nodejs-head@__nodejs_require               */
;*    (see nodejs/require.scm).                                        */
;*---------------------------------------------------------------------*/
(define (js-html-head %this)
   (js-make-function %this
      (lambda (this attrs . nodes)
	 (apply <HEAD> :idiom "javascript" :%context %this
	    (when (js-object? attrs)
	       (js-object->keyword-arguments* attrs %this))
	    (filter (lambda (n)
		       (or (isa? n xml-tilde) (isa? n xml-markup)))
	       nodes)))
      2 (& "HEAD")))

;*---------------------------------------------------------------------*/
;*    js-html-script ...                                               */
;*    -------------------------------------------------------------    */
;*    Normally overriden by nodejs-script@__nodejs_require             */
;*    (see nodejs/require.scm).                                        */
;*---------------------------------------------------------------------*/
(define (js-html-script %this)
   (js-make-function %this
      (lambda (this attrs . nodes)
	 (apply <SCRIPT> :idiom "javascript" :%context %this
	    (when (js-object? attrs)
	       (js-object->keyword-arguments* attrs %this))
	    (filter (lambda (n)
		       (or (isa? n xml-tilde) (isa? n xml-markup)))
	       nodes)))
      2 (& "SCRIPT")))

;*---------------------------------------------------------------------*/
;*    js-parseint ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-parseint string radix %this)
   (js-string-parseint
      (trim-whitespaces+ (js-tostring string %this) :plus #t)
      (js-toint32 radix %this) #f))

;*---------------------------------------------------------------------*/
;*    js-parseint-string ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-parseint-string string)
   (js-string-parseint (trim-whitespaces+ (js-jsstring->string string) :plus #t)
      #s32:0 #f))

;*---------------------------------------------------------------------*/
;*    js-parseint-any ...                                              */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-parseint-any string %this)
   (js-string-parseint (trim-whitespaces+ (js-tostring string %this) :plus #t)
      #s32:0 #f))

;*---------------------------------------------------------------------*/
;*    js-parseint-string-uint32 ...                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.2     */
;*---------------------------------------------------------------------*/
(define (js-parseint-string-uint32 string radix::uint32)
   (js-string-parseint (trim-whitespaces+ (js-jsstring->string string) :plus #t)
      (uint32->int32 radix) #f))

;*---------------------------------------------------------------------*/
;*    js-parsefloat ...                                                */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.3     */
;*---------------------------------------------------------------------*/
(define (js-parsefloat string %this)
   (js-string-parsefloat (trim-whitespaces+ (js-tostring string %this) :plus #t)
      #f))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
