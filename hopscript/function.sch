;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/function.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec  7 06:32:41 2019                          */
;*    Last change :  Mon Jul 24 16:24:39 2023 (serrano)                */
;*    Copyright   :  2019-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function macros for js2scheme                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-function-apply-arguments ...                                  */
;*---------------------------------------------------------------------*/
(define-macro (js-function-apply-arguments %this fun thisarg vec %arguments mode cache)
   `(with-access::JsGlobalObject %this (js-function-prototype)
       (cond
	  (,%arguments
	   (js-function-apply ,%this ,fun ,thisarg ,%arguments ,cache))
	  ((and (js-object-mode-plain? ,fun)
		(js-object-mode-plain? js-function-prototype))
	   (js-function-apply-vec ,%this ,fun ,thisarg
	      ,vec #u32:0 (fixnum->uint32 (vector-length ,vec))))
	  (else
	   (set! ,%arguments 
	      ,(if (eq? mode 'strict)
		   `(js-strict-arguments ,%this (vector-copy ,vec))
		   `(js-sloppy-arguments ,%this (vector-copy ,vec))))
	   (js-function-apply %this ,fun ,thisarg ,%arguments ,cache)))))
	   
;*---------------------------------------------------------------------*/
;*    js-function-spread-arguments ...                                 */
;*---------------------------------------------------------------------*/
(define-macro (js-function-spread-arguments %this fun thisarg vec %arguments)
   `(with-access::JsGlobalObject %this (js-function-prototype)
       (cond
	  (%arguments
	   (js-apply-array ,%this ,fun ,thisarg %arguments))
	  (else
	   (js-function-apply-vec ,%this ,fun ,thisarg
	      ,vec #u32:0 (fixnum->uint32 (vector-length ,vec)))))))
	   
;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply-arguments ...                            */
;*---------------------------------------------------------------------*/
(define-macro (js-function-maybe-apply-arguments %this fun thisarg vec %arguments mode cache)
   (if (symbol? fun)
       (if (symbol? thisarg)
	   `(if (js-function? ,fun)
		(js-function-apply-arguments ,%this ,fun ,thisarg ,vec ,%arguments mode ,cache)
		(begin
		   (set! %arguments 
		      ,(if (eq? mode 'strict)
			   `(js-strict-arguments %this (vector-copy ,vec))
			   `(js-sloppy-arguments %this (vector-copy ,vec))))
		   (js-function-maybe-apply %this ,fun ,thisarg ,%arguments ,cache)))
	   (let ((tmpt (gensym 'tmpt)))
	      `(let ((,tmpt ,thisarg))
		  (js-function-maybe-apply-arguments ,%this ,fun ,tmpt
		     ,vec ,%arguments ,mode ,cache))))
       (let ((tmpf (gensym 'tmpf)))
	  `(let ((,tmpf ,fun))
	      (js-function-maybe-apply-arguments ,%this ,tmpf ,thisarg
		 ,vec ,%arguments ,mode ,cache)))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-apply-arguments-slice ...                      */
;*---------------------------------------------------------------------*/
(define-macro (js-function-maybe-apply-arguments-slice %this this thisarg vec offset len %arguments mode cache)
   `(with-access::JsGlobalObject ,%this (js-function-prototype)
      (cond
	 (,%arguments
	  (js-function-maybe-apply ,%this ,this ,thisarg
	     (js-arguments-slice ,%arguments ,offset ,len ,%this)
	     ,cache))
	 ((and (js-procedure? ,this)
	       (js-object-mode-plain? ,this)
	       (js-object-mode-plain? js-function-prototype))
	  (js-function-apply-vec ,%this ,this ,thisarg ,vec ,offset ,len))
	 (else
	  (set! ,%arguments 
	     ,(if (eq? mode 'strict)
		  `(js-strict-arguments ,%this (vector-copy ,vec))
		  `(js-sloppy-arguments ,%this (vector-copy ,vec))))
	  (js-function-maybe-apply ,%this ,this ,thisarg
	     (js-arguments-vector-slice ,vec ,offset ,len ,%this)
	     ,cache)))))

;*---------------------------------------------------------------------*/
;*    js-function-spread-arguments-slice ...                           */
;*---------------------------------------------------------------------*/
(define-macro (js-function-spread-arguments-slice %this this thisarg vec offset len %arguments mode cache)
   `(with-access::JsGlobalObject %this (js-function-prototype)
       (cond
	  (,%arguments
	   (js-apply-array ,%this ,this ,thisarg
	      (js-arguments-slice ,%arguments ,offset ,len ,%this)))
	  ((js-procedure? ,this) 
	   (js-function-apply-vec ,%this ,this ,thisarg ,vec ,offset ,len))
	  (else
	   (set! ,%arguments 
	      ,(if (eq? mode 'strict)
		   `(js-strict-arguments ,%this (vector-copy ,vec))
		   `(js-sloppy-arguments ,%this (vector-copy ,vec))))
	   (js-apply-array ,%this ,this ,thisarg
	      (js-arguments-vector-slice ,vec ,offset ,len ,%this))))))

;*---------------------------------------------------------------------*/
;*    js-function-maybe-spread-arguments ...                           */
;*---------------------------------------------------------------------*/
(define-macro (js-function-maybe-spread-arguments %this fun thisarg vec %arguments)
   `(if (js-function? ,fun)
	(js-function-spread-arguments ,%this ,fun ,thisarg ,vec ,%arguments)
	(js-raise-type-error ,%this "spread: this is not a function ~s" ,fun)))
   
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
	     :path ?path
	     :start (and (? fixnum?) ?start)
	     :end (and (? fixnum?) ?end))
	  (e (cond
		((string? path)
		 `'#(,name ,len #f ,path ,start ,end 100 #f))
		((eq? path '%sourcepath)
		 `(let ((v '#(,name ,len #f "" ,start ,end 100 #f)))
		     (vector-set! v 3 %sourcepath)
		     v))
		(else
		 `(vector ,name ,len #f ,path ,start ,end 100 #f)))
	     e))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? integer?) ?len)
	     :path ?path
	     :start (and (? fixnum?) ?start)
	     :end (and (? fixnum?) ?end))
	  (e (cond
		((string? path)
		 `'#(,name ,len #f ,path ,start ,end 100 #f))
		((eq? path '%sourcepath)
		 `(let ((v '#(,name ,len #f "" ,start ,end 100 #f)))
		     (vector-set! v 3 %sourcepath)
		     v))
		(else
		 `(vector ,name ,len #f ,path ,start ,end 100 #f)))
	     e))
	 ((?- :name (and (? string?) ?name)
	     :len (and (? integer?) ?len)
	     :path ?path
	     :start (and (? fixnum?) ?start)
	     :end (and (? fixnum?) ?end)
	     :new-target ?new-target)
	  (e (cond
		((string? path)
		 `'#(,name ,len #f ,path ,start ,end 100 ,new-target))
		((eq? path '%sourcepath)
		 `(let ((v '#(,name ,len #f "" ,start ,end 100 ,new-target)))
		     (vector-set! v 3 %sourcepath)
		     v))
		(else
		 `(vector ,name ,len #f ,path ,start ,end 100 ,new-target)))
	     e))
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
	 ((?- :name ?name :len ?len :new-target ?new-target)
	  (let ((tmp (gensym 'name)))
	     (if (epair? x)
		 (e `(let ((,tmp ,name))
			(vector ,tmp ,len (format "function ~a() { [native code] }" ,tmp)
			   ,(cadr (cer x)) ,(caddr (cer x)) -1 100 ,new-target))
		    e)
		 (e `(let ((,tmp ,name))
			(vector ,tmp ,len (format "function ~a() { [native code] }" ,tmp) "?" -1 -1 100 ,new-target))
		    e))))
	 (else
	  (error "js-function-info" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    js-function-arity ...                                            */
;*    -------------------------------------------------------------    */
;*    See also function.scm                                            */
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
		     (((kwote quote) arguments-stack)
		      -2047)
		     (((kwote quote) arguments-eager)
		      -2048)
		     (((kwote quote) arguments-lonly)
		      (-fx -8192 req))
		     (((kwote quote) arguments)
		      -2048)
		     (((kwote quote) rest-stack)
		      (let ((offset (if (=fx opt 0) 2049 4049)))
			 (negfx (+fx offset (-fx req 1)))))
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
