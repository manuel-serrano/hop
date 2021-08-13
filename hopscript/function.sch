;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/function.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec  7 06:32:41 2019                          */
;*    Last change :  Thu Aug 12 18:25:41 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function macros for js2scheme                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-function-apply-arguments ...                                  */
;*---------------------------------------------------------------------*/
(define-macro (js-function-apply-arguments %this this thisarg vec arguments cache)
   `(if (and (or (not (js-object? ,this)) (js-object-mode-plain? ,this))
	     (not (object? ,arguments)))
	(js-function-apply-vec ,%this ,this ,thisarg ,vec (fixnum->uint32 (vector-length ,vec)))
	(begin
	   (set! ,arguments (js-materialize-arguments ,%this ,vec ,arguments))
	   (js-function-apply %this ,this ,thisarg ,arguments ,cache))))
	   
;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply-arguments ...                            */
;*---------------------------------------------------------------------*/
(define-macro (js-function-maybe-apply-arguments %this this thisarg vec arguments cache)
   `(if (js-function? ,this)
	(js-function-apply-arguments ,%this ,this ,thisarg ,vec ,arguments ,cache)
	(begin
	   (set! ,arguments (js-materialize-arguments ,%this ,vec ,arguments))
	   (js-function-maybe-apply %this ,this ,thisarg ,arguments ,cache))))
   
;*---------------------------------------------------------------------*/
;*    js-function-info ...                                             */
;*---------------------------------------------------------------------*/
(define-expander js-function-info
   (lambda (x e)
      (match-case x
	 ((?- :name (and (? string?) ?name)
	     :len (and (? fixnum?) ?len)
	     :tostring (and (? string?) ?tostring))
	  (e `'#(,name ,len ,tostring "?" -1 -1 #f) e))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? fixnum?) ?len)
	     :tostring #f
	     :path (and (? string?) ?path)
	     :start (and (? fixnum?) ?start)
	     :end (and (? fixnum?) ?end))
	  (e `'#(,name ,len #f ,path ,start ,end 100 #f) e))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? integer?) ?len)
	     :path (and (? string?) ?path)
	     :start (and (? fixnum?) ?start)
	     :end (and (? fixnum?) ?end))
	  (e `'#(,name ,len #f ,path ,start ,end 100 3f) e))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? integer?) ?len)
	     :path (and (? string?) ?path)
	     :start (and (? fixnum?) ?start)
	     :end (and (? fixnum?) ?end)
	     :new-target ?new-target)
	  (e `'#(,name ,len #f ,path ,start ,end 100 ,new-target) e))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? integer?) ?len))
	  (if (epair? x)
	      (e `'#(,name ,len ,(format "function ~a() { [native code] }" name) ,(cadr (cer x)) ,(caddr (cer x)) -1 100 #F) e)
	      (e `'#(,name ,len ,(format "function ~a() { [native code] }" name) "?" -1 -1 100 #f) e)))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? integer?) ?len)
	     :maxconstrsize (and (? fixnum?) ?maxconstrsize))
	  (if (epair? x)
	      (e `'#(,name ,len ,(format "function ~a() { [native code] }" name) ,(cadr (cer x)) ,(caddr (cer x)) -1 ,maxconstrsize #f) e)
	      (e `'#(,name ,len ,(format "function ~a() { [native code] }" name) "?" -1 -1 ,maxconstrsize #f) e)))
	 ((?- :name ?name :len ?len)
	  (let ((tmp (gensym 'name)))
	     (if (epair? x)
		 (e `(let ((,tmp ,name))
			(vector ,tmp ,len (format "function ~a() { [native code] }" ,tmp)
			   ,(cadr (cer x)) ,(caddr (cer x)) -1 100 #f))
		    e)
		 (e `(let ((,tmp ,name))
			(vector ,tmp ,len (format "function ~a() { [native code] }" ,tmp) "?" -1 -1 100 #f))
		    e))))
	 (else
	  (error "js-function-info" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    js-function-arity ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (js-function-arity req . opt-protocol)
   (if (null? opt-protocol)
       `(procedure-arity ,req)
       (let ((opt (car opt-protocol))
	     (protocol (cdr opt-protocol)))
	  (if (and (integer? req) (integer? opt))
	      (if (null? protocol)
		  (if (=fx opt 0)
		      (+ req 1)
		      (error "js-function-arity"
			 "illegal optional for fix args"
			 (cons req opt-protocol)))
		  (match-case (car protocol)
		     (((kwote quote) arguments-lazy)
		      -2047)
		     (((kwote quote) arguments-eager)
		      -2047)
		     (((kwote quote) arguments-eager)
		      -2048)
		     (((kwote quote) arguments)
		      0)
		     (((kwote quote) rest-lazy)
		      (let ((offset (if (=fx opt 0) 2049 4049)))
			 (negfx (+fx offset (-fx req 1)))))
		     (((kwote quote) rest)
		      (let ((offset (if (=fx opt 0) 3049 5049)))
			 (negfx (+fx offset (-fx req 1)))))
		     (((kwote quote) scheme)
		      (cond
			 ((=fx opt 0)
			  (+fx req 1))
			 ((=fx opt -1)
			  (negfx (+fx (+fx 1 req) 1)))
			 (else
			  (negfx (+fx (+fx 1 req) opt)))))
		     (((kwote quote) scheme-optional)
		      (cond
			 ((and (=fx opt 1) (=fx req 0))
			  -512)
			 (else
			  (error "js-function-arity"
			     "Illegal scheme-optional"
			     (cons req opt-protocol)))))
		     (((kwote quote) optional)
		      (if (=fx opt 0)
			  (+fx req 1)
			  (negfx (+fx req 1024))))
		     (((kwote quote) fix)
		      (if (=fx opt 0)
			  (+fx req 1)
			  (error "js-function-arity"
			     "illegal optional for fix args"
			     (cons req opt-protocol))))
		     (else
		      (error "js-function-arity"
			 "illegal protocol" (cons req opt-protocol)))))
	      (error "js-function-arity"
		 "illegal arity" (cons req opt-protocol))))))

;*---------------------------------------------------------------------*/
;*    js-function-info-maxconstrsize ...                               */
;*---------------------------------------------------------------------*/
(define-macro (js-function-info-maxconstrsize info)
   `(vector-ref ,info 6))
