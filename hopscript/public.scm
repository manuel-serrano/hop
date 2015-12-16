;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscript/public.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 08:10:39 2013                          */
;*    Last change :  Wed Dec 16 07:59:52 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Public (i.e., exported outside the lib) hopscript functions      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_public

   (option (set! *compiler-debug-trace* 0))
   
   (library hop js2scheme)
   
   (import __hopscript_types
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_error
	   __hopscript_string
	   __hopscript_stringliteral
	   __hopscript_boolean
	   __hopscript_number
	   __hopscript_property
	   __hopscript_private
	   __hopscript_worker
	   __hopscript_array)
   
   (export (js-new ::JsGlobalObject f . args)
	   (js-new/debug ::JsGlobalObject loc f . args)
	   (js-new0 ::JsGlobalObject f)
	   (js-new1 ::JsGlobalObject f a0)
	   (js-new2 ::JsGlobalObject f a0 a1)
	   (js-new3 ::JsGlobalObject f a0 a1 a2)
	   (js-new4 ::JsGlobalObject f a0 a1 a2 a3)
	   
	   (js-object-alloc ::JsFunction ::JsGlobalObject)
	   
	   (js-apply ::JsGlobalObject fun::obj this ::pair-nil)
	   (js-apply-service% ::procedure obj args::pair-nil ::int)
	   (js-apply-rest% ::JsGlobalObject ::procedure this ::pair-nil ::int ::int)
	   
	   (js-call0 ::JsGlobalObject fun::obj this)
	   (js-call1 ::JsGlobalObject fun::obj this a0)
	   (js-call2 ::JsGlobalObject fun::obj this a0 a1)
	   (js-call3 ::JsGlobalObject fun::obj this a0 a1 a2)
	   (js-call4 ::JsGlobalObject fun::obj this a0 a1 a2 a3)
	   (js-call5 ::JsGlobalObject fun::obj this a0 a1 a2 a3 a4)
	   (js-call6 ::JsGlobalObject fun::obj this a0 a1 a2 a3 a4 a5)
	   (js-call7 ::JsGlobalObject fun::obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8 ::JsGlobalObject fun::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-calln ::JsGlobalObject fun::obj this . args)
	   
	   (js-call0/debug ::JsGlobalObject loc fun::obj this)
	   (js-call1/debug ::JsGlobalObject loc fun::obj this a0)
	   (js-call2/debug ::JsGlobalObject loc fun::obj this a0 a1)
	   (js-call3/debug ::JsGlobalObject loc fun::obj this a0 a1 a2)
	   (js-call4/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3)
	   (js-call5/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4)
	   (js-call6/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5)
	   (js-call7/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5 a6)
	   (js-call8/debug ::JsGlobalObject loc fun::obj this a0 a1 a2 a3 a4 a5 a6 a7)
	   (js-calln/debug ::JsGlobalObject loc fun::obj this . args)

	   (js-service/debug ::obj ::obj ::procedure)

	   (js-instanceof?::bool ::JsGlobalObject v f)
	   (js-instanceof?/debug::bool ::JsGlobalObject loc v f)
	   
	   (js-in?::bool ::JsGlobalObject f obj)
	   (js-in?/debug::bool ::JsGlobalObject loc f obj)

	   (inline js-make-let::cell)
	   (js-let-ref ::cell ::obj ::obj ::JsGlobalObject)
	   (inline js-let-set! ::cell ::obj)
	   
	   (inline js-totest::bool ::obj)
	   (js-toboolean::bool ::obj)
	   (generic js-tonumber ::obj ::JsGlobalObject)
	   (generic js-tointeger ::obj ::JsGlobalObject)
	   (js-touint16::uint16 ::obj ::JsGlobalObject)
	   (js-touint32::uint32 ::obj ::JsGlobalObject)
	   (js-toint32::int32 ::obj ::JsGlobalObject)

	   (js-toindex::uint32 ::obj)
	   (js-isindex?::bool ::uint32)
	   
	   (generic js-tostring::bstring ::obj ::JsGlobalObject)
	   (js-tojsstring::JsStringLiteral ::obj ::JsGlobalObject)
	   
	   (js-toobject::obj ::JsGlobalObject ::obj)
	   (js-toobject/debug::obj ::JsGlobalObject loc ::obj)
	   
	   (generic js-toprimitive ::obj ::symbol ::JsGlobalObject)
	   
	   (inline js-equal? ::obj ::obj ::JsGlobalObject)
	   (js-equality? ::obj ::obj ::JsGlobalObject)
	   (inline js-strict-equal? ::obj ::obj)
	   (js-eq? ::obj ::obj)
	   
	   (make-pcache ::int)
	   (inline pcache-ref ::vector ::int)
	   
	   (%js-eval-hss ::input-port ::JsGlobalObject ::obj ::obj)
	   (%js-direct-eval ::obj ::bool ::JsGlobalObject ::obj ::JsObject)
	   (%js-eval ::input-port ::symbol ::JsGlobalObject ::obj ::JsObject)
	   
	   (js-raise ::JsError)
	   (js-throw ::obj ::JsStringLiteral ::long)

	   (js-raise-type-error ::JsGlobalObject ::bstring ::obj)
	   (js-raise-type-error/loc ::JsGlobalObject ::obj ::bstring ::obj)
	   (js-raise-range-error ::JsGlobalObject ::bstring ::obj)
	   (js-raise-uri-error ::JsGlobalObject ::bstring ::obj)
	   (js-raise-syntax-error ::JsGlobalObject ::bstring ::obj . ::obj)
	   (js-raise-reference-error ::JsGlobalObject ::bstring ::obj . ::obj)
	   (js-raise-error ::JsGlobalObject ::bstring ::obj . ::obj)

	   (generic js-cast-object obj ::JsGlobalObject ::bstring)
	   (generic js-inspect::JsStringLiteral ::obj ::int)

	   (js-html-head ::JsGlobalObject)
	   ))

;*---------------------------------------------------------------------*/
;*    js-new/function ...                                              */
;*---------------------------------------------------------------------*/
(define (js-new/function %this::JsGlobalObject f::JsFunction args::pair-nil)
   (with-access::JsFunction f (construct arity alloc name)
      (let ((o (alloc f)))
	 (let ((r (js-apply% %this f construct o args)))
	    (if (isa? r JsObject) r o)))))

;*---------------------------------------------------------------------*/
;*    js-new ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.2.2       */
;*---------------------------------------------------------------------*/
(define (js-new %this f . args)
   (if (isa? f JsFunction)
       (js-new/function %this f args)
       (js-raise-type-error %this "new: object is not a function ~s" f)))

;*---------------------------------------------------------------------*/
;*    js-new/debug ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.2.2       */
;*---------------------------------------------------------------------*/
(define (js-new/debug %this loc f . args)
   (if (isa? f JsFunction)
       (js-new/function %this f args)
       (js-raise-type-error/loc %this loc "new: object is not a function ~s" f)))

;*---------------------------------------------------------------------*/
;*    js-new-return ...                                                */
;*---------------------------------------------------------------------*/
(define (js-new-return f r o)
   (with-access::JsFunction f (constrsize)
      (if (isa? r JsObject)
	  (with-access::JsObject r (elements)
	     (when (vector? elements)
		(set! constrsize (vector-length elements)))
	     r)
	  (with-access::JsObject o (elements)
	     (when (vector? elements)
		(set! constrsize (vector-length elements)))
	     o))))

;*---------------------------------------------------------------------*/
;*    js-new0 ...                                                      */
;*---------------------------------------------------------------------*/
(define (js-new0 %this f)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc name)
	  (let ((o (alloc f)))
	     ;; CARE ARITY
	     (let ((r (js-call0% %this f construct o)))
		(js-new-return f r o))))
       (js-raise-type-error %this "new: object is not a function ~s" f)))

(define (js-new1 %this f a0)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc name)
	  (let ((o (alloc f)))
	     ;; CARE ARITY
	     (let ((r (js-call1% %this f construct o a0)))
		(js-new-return f r o))))
       (js-raise-type-error %this "new: object is not a function ~s" f)))

(define (js-new2 %this f a0 a1)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc)
	  (let ((o (alloc f)))
	     ;; CARE ARITY
	     (let ((r (js-call2% %this f construct o a0 a1)))
		(js-new-return f r o))))
       (js-raise-type-error %this "new: object is not a function ~s" f)))

(define (js-new3 %this f a0 a1 a2)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc name)
	  (let ((o (alloc f)))
	     ;; CARE ARITY
	     (let ((r (js-call3% %this f construct o a0 a1 a2)))
		(js-new-return f r o))))
       (js-raise-type-error %this "new: object is not a function ~s" f)))

(define (js-new4 %this f a0 a1 a2 a3)
   (if (isa? f JsFunction)
       (with-access::JsFunction f (construct alloc)
	  (let ((o (alloc f)))
	     ;; CARE ARITY
	     (let ((r (js-call4% %this f construct o a0 a1 a2 a3)))
		(js-new-return f r o))))
       (js-raise-type-error %this "new: object is not a function ~s" f)))

;*---------------------------------------------------------------------*/
;*    js-object-alloc ...                                              */
;*---------------------------------------------------------------------*/
(define (js-object-alloc constructor::JsFunction %this::JsGlobalObject)
   (with-access::JsFunction constructor (constrsize constrmap)
      (let ((cproto (js-get constructor 'prototype %this)))
	 (instantiate::JsObject
	    (cmap constrmap)
	    (elements (make-vector constrsize (js-undefined)))
	    (__proto__ (if (isa? cproto JsObject)
			   cproto
			   (with-access::JsGlobalObject %this (__proto__)
			      __proto__)))))))

;*---------------------------------------------------------------------*/
;*    js-apply ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-apply %this fun obj args::pair-nil)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "apply: argument not a function ~s" fun)
       (with-access::JsFunction fun (procedure rest)
	  (js-apply% %this fun procedure obj args))))

;*---------------------------------------------------------------------*/
;*    js-apply% ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-apply% %this fun::JsFunction proc::procedure obj args::pair-nil)
   (with-access::JsFunction fun (arity rest len minlen)
      (let ((n (+fx 1 (length args))))
	 (cond
	    ((=fx arity n)
	     (apply proc obj args))
	    ((>fx arity n)
	     (if (and (>=fx minlen 0) (>fx minlen (-fx n 1)))
		 (js-raise-arity-error %this fun (-fx n 1))
		 (let ((rest (make-list (-fx arity n) (js-undefined))))
		    (apply proc obj (append args rest)))))
	    ((>=fx arity 0)
	     (if (>=fx minlen 0)
		 (js-raise-arity-error %this fun (-fx n 1))
		 (apply proc obj (take args (-fx arity 1)))))
	    (rest
	     (js-apply-rest% %this proc obj args len n))
	    (else
	     (let ((-arity (-fx (negfx arity) 1)))
		(if (<=fx -arity n)
		    (apply proc obj args)
		    (let ((rest (make-list (-fx -arity n) (js-undefined))))
		       (apply proc obj (append args rest))))))))))

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
(define (js-raise-arity-error %this fun n)
   (with-access::JsFunction fun (name minlen arity rest len)
      ;; (tprint "name=" name " minlen=" minlen " arity=" arity " len=" len " rest=" rest " n=" n)
      (js-raise-type-error %this
	 (format "~a: wrong number of arguments ~a provided, ~a expected"
	    (if (string? name) name "")
	    n
	    (cond
	       ((or (<fx minlen 0) (and (=fx minlen len) (not rest))) len)
	       (rest (format ">= ~a" minlen))
	       (else (format "[~a..~a]" minlen len))))
	 fun)))

;*---------------------------------------------------------------------*/
;*    gen-calln ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (gen-calln . args)
   (let ((n (+fx 1 (length args))))
      `(with-access::JsFunction fun (arity len rest minlen src name)
	  (case arity
	     ((-1)
	      (if (not rest)
		  (proc this ,@args)
		  (case len
		     ((0)
		      (proc this (js-vector->jsarray (vector ,@args) %this)))
		     ,@(map (lambda (i)
			       `((,i)
				 ,(if (<=fx n i)
				      `(if (and (<=fx ,n minlen) (>=fx minlen 0))
					   (js-raise-arity-error %this fun ,(-fx n 1))
					   (proc this ,@args ,@(make-list (-fx (+fx i 1) n) '(js-undefined))
					      (js-vector->jsarray '#() %this)))
				      `(proc this ,@(take args i)
					  (js-vector->jsarray (vector ,@(drop args i)) %this)))))
			(iota 8 1))
		     (else
		      (cond
			 ((and (<=fx ,n minlen) (>=fx minlen 0))
			  (js-raise-arity-error %this fun ,(-fx n 1)))
			 ((<=fx ,(-fx n 1) len)
			  (apply proc this ,@args
			     (js-rest-args %this (-fx (+fx len 1) ,n))))
			 (else
			  (let ((args (list ,@args)))
			     (apply proc this
				(append (take args len)
				   (list
				      (js-vector->jsarray
					 (apply vector (drop args len)) %this)))))))))))
	     ,@(map (lambda (i)
		       `((,i) 
			 ,(cond
			     ((=fx i n)
			      `(proc this ,@args))
			     ((<fx i n)
			      `(if (>=fx minlen 0)
				   (js-raise-arity-error %this fun ,(-fx n 1))
				   (proc this ,@(take args (-fx i 1)))))
			     (else
			      `(if (or (>fx ,n minlen) (<fx minlen 0))
				   (proc this ,@args
				      ,@(make-list (-fx i n) '(js-undefined)))
				   (js-raise-arity-error %this fun ,(-fx n 1)))))))
		(iota 8 1))
	     (else
	      (cond
		 ((<fx arity 0)
		  (let ((min (-fx (negfx arity) 1)))
		     (apply proc this ,@args
			(make-list (-fx min ,n) (js-undefined)))))
		 ((=fx arity ,n)
		  (proc this ,@args))
		 ((>=fx minlen 0)
		  (js-raise-arity-error %this fun ,(-fx n 1)))
		 (else
		  (apply proc this ,@args
		     (make-list (-fx arity ,n) (js-undefined))))))))))

;*---------------------------------------------------------------------*/
;*    js-calln ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-call0% %this fun::JsFunction proc::procedure this)
   (gen-calln))
(define (js-call1% %this fun::JsFunction proc::procedure this a0)
   (gen-calln a0))
(define (js-call2% %this fun::JsFunction proc::procedure this a0 a1)
   (gen-calln a0 a1))
(define (js-call3% %this fun::JsFunction proc::procedure this a0 a1 a2)
   (gen-calln a0 a1 a2))
(define (js-call4% %this fun::JsFunction proc::procedure this a0 a1 a2 a3)
   (gen-calln a0 a1 a2 a3))
(define (js-call5% %this fun::JsFunction proc::procedure this a0 a1 a2 a3 a4)
   (gen-calln a0 a1 a2 a3 a4))
(define (js-call6% %this fun::JsFunction proc::procedure this a0 a1 a2 a3 a4 a5)
   (gen-calln a0 a1 a2 a3 a4 a5))
(define (js-call7% %this fun::JsFunction proc::procedure this a0 a1 a2 a3 a4 a5 a6)
   (gen-calln a0 a1 a2 a3 a4 a5 a6))
(define (js-call8% %this fun::JsFunction proc::procedure this a0 a1 a2 a3 a4 a5 a6 a7)
   (gen-calln a0 a1 a2 a3 a4 a5 a6 a7))

(define (js-call0 %this fun this)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call0: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call0% %this fun procedure this))))
(define (js-call1 %this fun this a0)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call1: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call1% %this fun procedure this a0))))
(define (js-call2 %this fun this a0 a1)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call2: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call2% %this fun procedure this a0 a1))))
(define (js-call3 %this fun this a0 a1 a2)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call3: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call3% %this fun procedure this a0 a1 a2))))
(define (js-call4 %this fun this a0 a1 a2 a3)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call4: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call4% %this fun procedure this a0 a1 a2 a3))))
(define (js-call5 %this fun this a0 a1 a2 a3 a4)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call5: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call5% %this fun procedure this a0 a1 a2 a3 a4))))
(define (js-call6 %this fun this a0 a1 a2 a3 a4 a5)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call6: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call6% %this fun procedure this a0 a1 a2 a3 a4 a5))))
(define (js-call7 %this fun this a0 a1 a2 a3 a4 a5 a6)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call7: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call7% %this fun procedure this a0 a1 a2 a3 a4 a5 a6))))
(define (js-call8 %this fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call8: not a function ~s" fun)
       (with-access::JsFunction fun (procedure)
	  (js-call8% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7))))

(define (js-calln% %this fun this args)
   (with-access::JsFunction fun (procedure arity minlen rest len)
      (let ((n (+fx 1 (length args))))
	 (cond
	    ((=fx arity n)
	     (apply procedure this args))
	    ((>fx arity n)
	     (if (>fx minlen 0)
		 (js-raise-type-error %this
		    "wrong number of arguments" fun)
		 (apply procedure this
		    (append args
		       (make-list (-fx arity n)
			  (js-undefined))))))
	    ((>=fx arity 0)
	     (if (>fx minlen 0)
		 (js-raise-type-error %this
		    "wrong number of arguments" fun)
		 (apply procedure this (take args (-fx arity 1)))))
	    ((not rest)
	     (apply procedure this args))
	    (else
	     (cond
		((and (<=fx (-fx n 1) minlen) (>fx minlen 0))
		 (js-raise-type-error %this
		    "wrong number of arguments" fun))
		((<=fx (-fx n 1) len)
		 (apply procedure this
		    (append args (js-rest-args %this (-fx (+fx len 1) n)))))
		(else
		 (apply procedure this
		    (append (take args len)
		       (list
			  (js-vector->jsarray
			     (apply vector (drop args len)) %this)))))))))))

(define (js-calln %this fun this . args)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error %this "call: not a function ~s" fun)
       (js-calln% %this fun this args)))

(define (js-call0/debug %this loc fun this)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call0: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call0% %this fun procedure this)))
		($env-pop-trace env)
		aux)))))
(define (js-call1/debug %this loc fun this a0)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call1: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure (fname name))
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call1% %this fun procedure this a0)))
		($env-pop-trace env)
		aux)))))
(define (js-call2/debug %this loc fun this a0 a1)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call2: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call2% %this fun procedure this a0 a1)))
		($env-pop-trace env)
		aux)))))
(define (js-call3/debug %this loc fun this a0 a1 a2)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call3: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call3% %this fun procedure this a0 a1 a2)))
		($env-pop-trace env)
		aux)))))
(define (js-call4/debug %this loc fun this a0 a1 a2 a3)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call4: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call4% %this fun procedure this a0 a1 a2 a3)))
		($env-pop-trace env)
		aux)))))
(define (js-call5/debug %this loc fun this a0 a1 a2 a3 a4)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call5: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call5% %this fun procedure this a0 a1 a2 a3 a4)))
		($env-pop-trace env)
		aux)))))
(define (js-call6/debug %this loc fun this a0 a1 a2 a3 a4 a5)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call6: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call6% %this fun procedure this a0 a1 a2 a3 a4 a5)))
		($env-pop-trace env)
		aux)))))
(define (js-call7/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call7: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call7% %this fun procedure this a0 a1 a2 a3 a4 a5 a6)))
		($env-pop-trace env)
		aux)))))
(define (js-call8/debug %this loc fun this a0 a1 a2 a3 a4 a5 a6 a7)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call8: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-call8% %this fun procedure this a0 a1 a2 a3 a4 a5 a6 a7)))
		($env-pop-trace env)
		aux)))))
(define (js-calln/debug %this loc fun this . args)
   (if (not (isa? fun JsFunction))
       (js-raise-type-error/loc %this loc
	  (format "call: not a function ~~s ~a" loc) fun)
       (with-access::JsFunction fun (procedure)
	  (let ((env (current-dynamic-env))
		(name (js-function-debug-name fun)))
	     ($env-push-trace env name loc)
	     (let ((aux (js-calln% %this fun this args)))
		($env-pop-trace env)
		aux)))))

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
;*    js-instanceof? ...                                               */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.6       */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.4.5.3   */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.3.5.3     */
;*---------------------------------------------------------------------*/
(define (js-instanceof? %this v f)
   (if (not (isa? f JsFunction))
       (js-raise-type-error %this
	  "instanceof: not a function ~s" f)
       (when (isa? v JsObject)
	  (let ((o (js-get f 'prototype %this)))
	     (if (not (isa? o JsObject))
		 (js-raise-type-error %this
		    "instanceof: no prototype ~s" v)
		 (let loop ((v v))
		    (with-access::JsObject v ((v __proto__))
		       (cond
			  ((eq? o v) #t)
			  ((eq? v (js-null)) #f)
			  (else (loop v))))))))))

(define (js-instanceof?/debug %this loc v f)
   (if (not (isa? f JsFunction))
       (js-raise-type-error/loc %this loc
	  "instanceof: not a function ~s" f)
       (when (isa? v JsObject)
	  (let ((o (js-get f 'prototype %this)))
	     (if (not (isa? o JsObject))
		 (js-raise-type-error/loc %this loc
		    "instanceof: no prototype ~s" v)
		 (let loop ((v v))
		    (with-access::JsObject v ((v __proto__))
		       (cond
			  ((eq? o v) #t)
			  ((eq? v (js-null)) #f)
			  (else (loop v))))))))))

;*---------------------------------------------------------------------*/
;*    js-in? ...                                                       */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.8.7       */
;*---------------------------------------------------------------------*/
(define (js-in? %this field obj)
   (if (not (isa? obj JsObject))
       (js-raise-type-error %this "in: not a object ~s" obj)
       (js-has-property obj (js-toname field %this) %this)))

(define (js-in?/debug %this loc field obj)
   (if (not (isa? obj JsObject))
       (js-raise-type-error/loc %this loc "in: not a object ~s" obj)
       (js-has-property obj (js-toname field %this) %this)))

;*---------------------------------------------------------------------*/
;*    js-make-let ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-make-let)
   (make-cell '__undefined__))

;*---------------------------------------------------------------------*/
;*    js-let-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define (js-let-ref cell ident loc %this)
   (let ((v (cell-ref cell)))
      (if (eq? v '__undefined__)
	  (js-raise-reference-error/loc %this loc
	     (format "\"~a\" is not defined" ident) cell)
	  v)))

;*---------------------------------------------------------------------*/
;*    js-let-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (js-let-set! cell val)
   (cell-set! cell val))

;*---------------------------------------------------------------------*/
;*    js-totest ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-inline (js-totest obj)
   (if (boolean? obj) obj (js-toboolean obj)))
      
;*---------------------------------------------------------------------*/
;*    js-toboolean ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.2          */
;*---------------------------------------------------------------------*/
(define (js-toboolean obj)
   (cond
      ((boolean? obj) obj)
      ((eq? obj (js-undefined)) #f)
      ((eq? obj (js-null)) #f)
      ((number? obj) (not (or (= obj 0) (and (flonum? obj) (nanfl? obj)))))
      ((js-jsstring? obj) (js-jsstring->bool obj))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    js-tonumber ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.3          */
;*---------------------------------------------------------------------*/
(define-generic (js-tonumber obj %this::JsGlobalObject)
   (let loop ((obj obj))
      (cond
	 ((number? obj)
	  obj)
	 ((eq? obj (js-undefined))
	  +nan.0)
	 ((eq? obj (js-null))
	  0)
	 ((eq? obj #t)
	  1)
	 ((eq? obj #f)
	  0)
	 ((js-jsstring? obj)
	  (js-tonumber obj %this))
	 ((symbol? obj)
	  (loop (symbol->string! obj)))
	 (else
	  (bigloo-type-error "toNumber" "JsObject" obj)))))

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
	  ((<fl obj 0.)
	   (*fl -1. (floor (abs obj))))
	  (else
	   (floor obj))))
      ((or (js-jsstring? obj) (symbol? obj))
       (js-tointeger (js-tonumber obj %this) %this))
      ((eq? obj #t)
       1)
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
;*    js-touint32 ::obj ...                                            */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.6          */
;*---------------------------------------------------------------------*/
(define (js-touint32::uint32 obj %this)
   
   (define 2^32 (exptfl 2. 32.))
   
   (define (positive-double->uint32::uint32 obj::double)
      (if (<fl obj 2^32)
	  (flonum->uint32 obj)
	  (flonum->uint32 (remainderfl obj 2^32))))

   (define (double->uint32::uint32 obj::double)
      (cond
	 ((or (= obj +inf.0) (= obj -inf.0) (not (= obj obj)))
	   #u32:0)
	 ((<fl obj 0.)
	  (positive-double->uint32 (+fl 2^32 (*fl -1. (floor (abs obj))))))
	 (else
	  (positive-double->uint32 obj))))
   
   (cond
      ((uint32? obj)
       (let ((r::uint32 obj)) r))
      ((int32? obj)
       (int32->uint32 obj))
      ((fixnum? obj)
       (cond-expand
	  (bint30
	   (fixnum->uint32 obj))
	  (bint32
	   (int32->uint32 (fixnum->int32 obj)))
	  (else
	   (if (<=fx obj (-fx (bit-lsh 1 32) 1))
	       (fixnum->uint32 obj)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		      (int32bit (modulofx posint ^32)))
		  (fixnum->uint32 int32bit))))))
      ((flonum? obj)
       (double->uint32 obj))
      (else
       (js-touint32 (js-tointeger obj %this) %this))))
		  
;*---------------------------------------------------------------------*/
;*    js-toint32 ::obj ...                                             */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.5          */
;*---------------------------------------------------------------------*/
(define (js-toint32::int32 obj %this)

   (define (int64->int32::int32 obj::int64)
      (cond-expand
	 ((or bint30 bint32)
	  (let* ((i::llong (int64->llong obj))
		 (^31 (*llong #l8 (fixnum->llong (bit-lsh 1 28))))
		 (^32 (*llong #l2 ^31))
		 (posint (if (<llong i #l0) (+llong ^32 i) i))
		 (int32bit (modulollong posint ^32))
		 (n (if (>=llong int32bit ^31)
			(-llong int32bit ^32)
			int32bit)))
	     (llong->int32 n)))
	 (else
	  (let* ((i::elong (int64->elong obj))
		 (^31 (fixnum->elong (bit-lsh 1 31)))
		 (^32 (fixnum->elong (bit-lsh 1 32)))
		 (posint (if (<elong i #e0) (+elong ^32 i) i))
		 (int32bit (moduloelong posint ^32))
		 (n (if (>=elong int32bit ^31)
			(-elong int32bit ^32)
			int32bit)))
	     (elong->int32 n)))))

   (cond
      ((int32? obj)
       obj)
      ((uint32? obj)
       (uint32->int32 obj))
      ((fixnum? obj)
       (cond-expand
	  ((or bint30 bint32)
	   (fixnum->int32 obj))
	  (bint61
	   (if (and (<=fx obj (-fx (bit-lsh 1 31) 1))
		    (>=fx obj (negfx (bit-lsh 1 31))))
	       (fixnum->int32 obj)
	       (let* ((^31 (bit-lsh 1 31))
		      (^32 (bit-lsh 1 32))
		      (posint (if (<fx obj 0) (+fx ^32 obj) obj))
		      (int32bit (modulofx posint ^32))
		      (n (if (>=fx int32bit ^31)
			     (-fx int32bit ^32)
			     int32bit)))
		  (fixnum->int32 n))))
	  (else
	   (int64->int32 (fixnum->int64 obj)))))
      ((flonum? obj)
       (cond
	  ((or (= obj +inf.0) (= obj -inf.0) (nanfl? obj))
	   (fixnum->int32 0))
	  ((<fl obj 0.)
	   (let ((i (*fl -1. (floor (abs obj)))))
	      (if (>=fl i (negfl (exptfl 2. 31.)))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))
	  (else
	   (let ((i (floor obj)))
	      (if (<=fl i (-fl (exptfl 2. 31.) 1.))
		  (fixnum->int32 (flonum->fixnum i))
		  (int64->int32 (flonum->int64 i)))))))
      ((elong? obj)
       (error "js-toint32" "unexpected elong" obj))
      (else
       (js-toint32 (js-tonumber obj %this) %this))))

;*---------------------------------------------------------------------*/
;*    js-toindex ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-15.4         */
;*    -------------------------------------------------------------    */
;*    Performance demands this function not to returned a boxed        */
;*    result. So, false is here denoted 1^32-1, as an uint32.          */
;*---------------------------------------------------------------------*/
(define (js-toindex p)
   
   (define false (-u32 #u32:0 #u32:1))
   
   (define (string->index p::bstring)
      (let ((num (string->number p)))
	 (if (bignum? num)
	     (if (and (>=bx num #z0) (<bx num (exptbx #z2 #z32)))
		 (llong->uint32 (bignum->llong num))
		 false)
	     (js-toindex num))))

   (cond
      ((uint32? p)
       p)
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
      ((isa? p JsNumber)
       (with-access::JsNumber p (val) (js-toindex val)))
      ((js-jsstring? p)
       (string->index (js-jsstring->string p)))
      ((string? p)
       (string->index p))
      ((symbol? p)
       (string->index (symbol->string! p)))
      ((isa? p JsString)
       (with-access::JsString p (val) (string->index (js-jsstring->string val))))
      (else
       false)))

;*---------------------------------------------------------------------*/
;*    js-isindex? ...                                                  */
;*---------------------------------------------------------------------*/
(define (js-isindex? u32::uint32)
   (<u32 u32 (-u32 (fixnum->uint32 0) (fixnum->uint32 1))))

;*---------------------------------------------------------------------*/
;*    js-tostring ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.8          */
;*---------------------------------------------------------------------*/
(define-generic (js-tostring obj %this::JsGlobalObject)
   (cond
      ((eq? obj (js-undefined))
       "undefined")
      ((eq? obj #t)
       "true")
      ((eq? obj #f)
       "false")
      ((eq? obj (js-null))
       "null")
      ((number? obj)
       (js-number->string obj))
      ((symbol? obj)
       (symbol->string! obj))
      ((string? obj)
       (bigloo-type-error "js-tostring" "JsStringLiteral" obj))
      (else
       (typeof obj))))

;*---------------------------------------------------------------------*/
;*    js-tostring ::JsWrapper ...                                      */
;*---------------------------------------------------------------------*/
(define-method (js-tostring obj::JsWrapper %this::JsGlobalObject)
   (with-access::JsWrapper obj (obj)
      (js-tostring obj %this)))

;*---------------------------------------------------------------------*/
;*    js-toprimitive ::JsWrapper ...                                   */
;*---------------------------------------------------------------------*/
(define-method (js-toprimitive obj::JsWrapper preferredtype %this::JsGlobalObject)
   (with-access::JsWrapper obj (obj)
      obj))

;*---------------------------------------------------------------------*/
;*    js-tojsstring ...                                                */
;*---------------------------------------------------------------------*/
(define (js-tojsstring obj %this)
   (if (js-jsstring? obj)
       obj
       (js-string->jsstring (js-tostring obj %this))))

;*---------------------------------------------------------------------*/
;*    js-toobject-failsafe ...                                         */
;*---------------------------------------------------------------------*/
(define (js-toobject-failsafe %this::JsGlobalObject o)
   (cond
      ((js-jsstring? o)
       (with-access::JsGlobalObject %this (js-string)
	  (js-new1 %this js-string o)))
      ((number? o)
       (with-access::JsGlobalObject %this (js-number)
	  (js-new1 %this js-number o)))
      ((boolean? o)
       (with-access::JsGlobalObject %this (js-boolean)
	  (js-new1 %this js-boolean o)))
      ((isa? o JsObject)
       o)
      ((pair? o)
       o)
      ((isa? o object)
       o)
      (else
       #f)))
   
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
;*    js-toprimitive ::obj ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-9.1          */
;*---------------------------------------------------------------------*/
(define-generic (js-toprimitive obj preferredtype %this::JsGlobalObject)
   obj)

;*---------------------------------------------------------------------*/
;*    js-equal? ...                                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.1       */
;*---------------------------------------------------------------------*/
(define-inline (js-equal? o1 o2 %this::JsGlobalObject)
   (or (and (not (flonum? o1)) (eq? o1 o2)) (js-equality? o1 o2 %this)))

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
	 ((number? x)
	  (cond
	     ((number? y)
	      (= x y))
	     ((js-jsstring? y)
	      (if (= x 0)
		  (or (js-jsstring-null? y) (equality? x (js-tonumber y %this)))
		  (equality? x (js-tonumber y %this))))
	     ((isa? y JsObject)
	      (equality? x (js-toprimitive y 'any %this)))
	     ((boolean? y)
	      (equality? x (js-tonumber y %this)))
	     (else #f)))
	 ((js-jsstring? x)
	  (cond
	     ((js-jsstring? y)
	      (js-jsstring=? x y))
	     ((number? y)
	      (if (= y 0)
		  (or (js-jsstring-null? x) (equality? (js-tonumber x %this) y))
		  (equality? (js-tonumber x %this) y)))
	     ((isa? y JsObject)
	      (equality? x (js-toprimitive y 'any %this)))
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
	 ((isa? x JsObject)
	  (cond
	     ((js-jsstring? y) (equality? (js-toprimitive x 'any %this) y))
	     ((number? y) (equality? (js-toprimitive x 'any %this) y))
	     (else #f)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    js-strict-equal?                                                 */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.4       */
;*---------------------------------------------------------------------*/
(define-inline (js-strict-equal? o1 o2)
   (or (and (eq? o1 o2) (not (flonum? o1))) (js-eq? o1 o2)))

;*---------------------------------------------------------------------*/
;*    js-eq? ...                                                       */
;*---------------------------------------------------------------------*/
(define (js-eq? x y)
   (cond
      ((number? x) (and (number? y) (= x y)))
      ((js-jsstring? x) (and (js-jsstring? y) (js-jsstring=? x y)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    make-pcache ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-pcache len)
   (let ((pcache ($make-vector-uncollectable len #unspecified)))
      (let loop ((i 0))
	 (if (=fx i len)
	     pcache
	     (begin
		(vector-set-ur! pcache i (instantiate::JsPropertyCache))
		(loop (+fx i 1)))))))
      
;*---------------------------------------------------------------------*/
;*    pcache-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (pcache-ref pcache index)
   (vector-ref-ur pcache index))

;*---------------------------------------------------------------------*/
;*    %js-hss ...                                                      */
;*---------------------------------------------------------------------*/
(define (%js-eval-hss ip::input-port %this %worker scope)
   (js-worker-exec %worker "eval-hss"
      (lambda ()
	 (let ((v (%js-eval ip 'repl %this (js-get scope 'this %this) scope)))
	    (if (isa? v JsStringLiteral)
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
(define (%js-direct-eval s strict %this this scope)
   (if (not (js-jsstring? s))
       s
       (let* ((s (js-jsstring->string s))
	      (evals (if strict (string-append "'use strict';\n" s) s)))
	  (call-with-input-string evals
	     (lambda (ip)
		(%js-eval ip 'eval %this
		   (if strict (js-undefined) this)
		   scope))))))

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
		     :driver (j2s-eval-driver)
		     :parser parser))
	       (m (js-get scope 'module scope)))
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
   (with-access::JsGlobalObject %this (js-object)
      (let ((obj (js-new %this js-object)))
	 (js-put! obj 'filename (js-string->jsstring "") #f %this)
	 obj)))

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
	 (set! fname f)
	 (set! location l)))
   (raise err))

;*---------------------------------------------------------------------*/
;*    error-obj->string ...                                            */
;*---------------------------------------------------------------------*/
(define (error-obj->string::bstring %this obj)
   (cond
      ((isa? obj JsObject)
       (with-handler
	  (lambda (e)
	     (js-jsstring->string (js-typeof obj)))
	  (js-jsstring->string
	     (js-call0 %this (js-get obj 'toString %this) obj))))
      ((eq? obj #unspecified)
       "undefined")
      ((eq? obj #f)
       "false")
      ((eq? obj #t)
       "true")
      ((js-jsstring? obj)
       (js-jsstring->string obj))
      ((symbol? obj)
       (symbol->string! obj))
      (else
       (typeof obj))))

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
      ((isa? obj JsObject)
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
;*    js-html-head ...                                                 */
;*    -------------------------------------------------------------    */
;*    Normally overriden by nodejs-head@__nodejs_require               */
;*    (see nodejs/require.scm).                                        */
;*---------------------------------------------------------------------*/
(define (js-html-head %this)
   (js-make-function %this
      (lambda (this attrs . nodes)
	 (apply <HEAD> :idiom "javascript" :context %this
	    (when (isa? attrs JsObject)
	       (js-object->keyword-arguments* attrs %this))
	    (filter (lambda (n)
		       (or (isa? n xml-tilde) (isa? n xml-markup)))
	       nodes)))
      2 'HEAD))


