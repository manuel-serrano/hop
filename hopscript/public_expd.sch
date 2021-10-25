;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/public_expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Mon Oct 25 08:04:30 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript public expanders                                       */
;*    -------------------------------------------------------------    */
;*    See expanders.sch                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-let-set!-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (js-let-set!-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?id) ?val ?loc ?%this)
       (e `(if (eq? ,id #\Z)
	       (js-raise-reference-error/loc ,%this ,loc "dead-zone access" ',id)
	       (set! ,id ,val))
	  e))
      (else
       (error "js-let-set!" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-null-or-undefined?-expander ...                               */
;*---------------------------------------------------------------------*/
(define (js-null-or-undefined?-expander x e)
   (match-case x
      ((?- ?expr)
       (cond-expand
	  (bigloo4.3c
	   (if (symbol? expr)
	       (e `(or (eq? ,expr (js-undefined)) (eq? ,expr (js-null))) e)
	       (let ((tmp (gensym)))
		  (e `(let ((,tmp ,expr))
			 (or (eq? ,expr (js-undefined)) (eq? ,expr (js-null))))
		     e))))
	  (else
	   (e `(null-or-unspecified? ,expr) e))))))

;*---------------------------------------------------------------------*/
;*    js-tonumber-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (js-tonumber-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?expr) ?%this)
       (e `(if (js-number? ,expr)
	       ,expr
	       ((@ js-tonumber __hopscript_public) ,expr ,%this))
	  e))
      ((?- ?expr ?%this)
       (let ((tmp (gensym '%e)))
	  (e `(let ((,tmp ,expr)) (js-tonumber ,tmp ,%this)) e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-tonumber-for-flonum-expander ...                              */
;*---------------------------------------------------------------------*/
(define (js-tonumber-for-flonum-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?expr) ?%this)
       (e `(cond
	      ((flonum? ,expr)
	       ,expr)
	      ((fixnum? ,expr)
	       (fixnum->flonum ,expr))
	      (else
	       ((@ js-toflonum __hopscript_arithmetic)
		((@ js-tonumber __hopscript_public) ,expr ,%this))))
	  e))
      ((?- ?expr ?%this)
       (let ((tmp (gensym '%e)))
	  (e `(let ((,tmp ,expr)) (js-tonumber-for-flonum ,tmp ,%this)) e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-math-floorfl-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (js-math-floorfl-expander x e)
   (match-case x
      ((?- (and ?f (js-tonumber-for-flonum (and (? symbol?) ?val) %this)))
       (e `(if (fixnum? ,val)
	       (overflowfx ,val)
	       ((@ js-math-floorfl __hopscript_math) ,f))
	  e))
      ((?- (js-tonumber-for-flonum ?val %this))
       (let ((tmp (gensym 'tmp)))
	  (e `(let ((,tmp ,val))
		 (js-math-floorfl (js-tonumber-for-flonum ,tmp %this)))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))
      
;*---------------------------------------------------------------------*/
;*    js-toprimitive-for-string ...                                    */
;*    -------------------------------------------------------------    */
;*    See JS-TOPRIMITIVE-FOR-STRING in public.scm                      */
;*---------------------------------------------------------------------*/
(define (js-toprimitive-for-string-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?s) ?this)
       (e `(if (js-jsstring? ,s)
	       ,s
	       ((@ js-toprimitive-for-string __hopscript_public) ,s ,this))
	  e))
      ((?- ?s ?this)
       (let ((t (gensym 's)))
	  (e `(let ((,t ,s))
		 (if (js-jsstring? ,t)
		     ,t
		     ((@ js-toprimitive-for-string __hopscript_public) ,t ,this)))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-nullish-expander                                              */
;*    -------------------------------------------------------------    */
;*    https://tc39.es/ecma262/#prod-CoalesceExpression                 */
;*---------------------------------------------------------------------*/
(define (js-nullish-expander x e)
   (match-case x
      ((?- (and ?lhs (?- . ?-)) ?y)
       (let ((x (gensym 'lhs)))
	  (e `(let ((,x ,(e lhs e))) (js-nullish ,x ,y)) e)))
      ((?- ?x ?y)
       (e `(if (js-null-or-undefined? ,x) ,y ,x) e))
      (else
       (error "js-nullish" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-with-handler-no-unwind ...                                    */
;*    -------------------------------------------------------------    */
;*    Simplied version of with-handler used only when no               */
;*    "RETURN", "BREAK", or "CONTINUE" is used in the body.            */
;*---------------------------------------------------------------------*/
(define (js-with-handler-no-unwind-expander x e)
   (cond-expand
      ((or bigloo4.4a bigloo4.4b)
       (match-case x
	  ((?- ?hdl ?body)
	   (e `(with-handler ,hdl ,body) e))
	  (else
	   (error "js-with-handler-no-unwind" "bad syntax" x))))
      (else
       (match-case x
	  ((?- ?hdl ?body)
	   (let ((ohs (gensym 'ohs))
		 (esc (gensym 'esc))
		 (val (gensym 'val))
		 (hds (gensym 'hdl))
		 (env (gensym 'env)))
	      (e `(cond-expand
		     ((not bigloo-compile)
		      (with-handler ,hdl ,body))
		     (else
		      (let* ((,env (current-dynamic-env))
			     (,ohs (env-get-error-handler ,env)))
			 (let ((,hds ($acons #unspecified ,env)))
			    (bind-exit :env ,env (,esc)
			       (begin
				  (set-car! ,hds ,esc)
				  (env-set-error-handler! ,env ,hds)
				  (let ((,val ,body))
				     (env-set-error-handler! ,env ,ohs)
				     ,val))
			       (begin
				  (sigsetmask 0)
				  (env-set-error-handler! ,env ,ohs)
				  (,hdl (cdr ,hds))))))))
		 e)))
	  ((no-cell-but-incorrect ?- ?hdl ?body)
	   (let ((ohs (gensym 'ohs))
		 (esc (gensym 'esc))
		 (val (gensym 'val))
		 (hds (gensym 'hdl))
		 (cell (gensym 'cell))
		 (env (gensym 'env)))
	      (e `(cond-expand
		     ((not bigloo-compile)
		      (with-handler ,hdl ,body))
		     (else
		      (let* ((,env (current-dynamic-env))
			     (,ohs (env-get-error-handler ,env)))
			 (bind-exit :env ,env (,esc)
			    (let ((,hds (cons ,esc ,env)))
			       (env-set-error-handler! ,env ,hds)
			       (let ((,val ,body))
				  (env-set-error-handler! ,env ,ohs)
				  ,val))
			    (begin
			       (sigsetmask 0)
			       (env-set-error-handler! ,env ,ohs)
			       (,hdl (env-get-exitd-val ,env)))))))
		 e)))
	  ((old ?- ?hdl ?body)
	   (e `(with-handler ,hdl ,body) e))
	  (else
	   (error "js-with-handler-no-unwind" "bad syntax" x))))))

;*---------------------------------------------------------------------*/
;*    js-call% ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-call%-expander x e)
   (match-case x
      ((js-call% ?%this (and (? symbol?) ?fun) ?procedure ?this . ?args)
       (let ((len (length args)))
	  (if (<=fx len 10)
	      (let ((call (string->symbol (format "js-call~a%" len))))
		 (e `(,call ,%this ,fun ,this ,procedure ,@args) e))
	      (e `(js-calln% ,%this ,fun ,this (list ,@args)) e))))
      ((js-call% ?%this ?fun ?procedure ?this . ?args)
       (let ((f (gensym)))
	  (e `(let ((,f ,fun)) (js-call% ,%this ,f ,procedure ,this ,@args)) e)))
      (else
       (error "js-call%" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-call ...                                                      */
;*---------------------------------------------------------------------*/
(define (js-call-expander x e)
   (match-case x
      ((js-call ?%this (and (? symbol?) ?fun) ?this . ?args)
       (let ((len (length args)))
	  (if (<=fx len 10)
	      (let ((call (string->symbol (format "js-call~a" len))))
		 (e `(,call ,%this ,fun ,this ,@args) e))
	      (e `(js-calln ,%this ,fun ,this (list ,@args)) e))))
      ((js-call ?%this ?fun ?this . ?args)
       (let ((f (gensym)))
	  (e `(let ((,f ,fun)) (js-call ,%this ,f ,this ,@args)) e)))
      (else
       (error "js-call" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-call-procedure ...                                            */
;*---------------------------------------------------------------------*/
(define (js-call-procedure-expander x e)
   (match-case x
      ((js-call-procedure (and (? symbol?) ?fun) ?this . ?args)
       (let ((len (length args)))
	  (if (<=fx len 10)
	      (let ((call (string->symbol (format "js-call~a-procedure" len))))
		 (e `(,call ,fun ,this ,@args) e))
	      (e `(js-calln-procedure ,fun ,this (list ,@args)) e))))
      ((js-call-procedure ?fun ?this . ?args)
       (let ((f (gensym)))
	  (e `(let ((,f ,fun)) (js-call-procedure ,f ,this ,@args)) e)))
      (else
       (error "js-call-procedure" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-call-jsprocedure ...                                          */
;*---------------------------------------------------------------------*/
(define (js-call-jsprocedure-expander x e)
   (match-case x
      ((js-call-jsprocedure ?%this (and (? symbol?) ?fun) ?this . ?args)
       (let ((len (length args)))
	  (if (<=fx len 10)
	      (let ((call (string->symbol (format "js-call~a-jsprocedure" len))))
		 (e `(,call ,%this ,fun ,this ,@args) e))
	      (e `(js-calln-jsprocedure ,fun ,this (list ,@args)) e))))
      ((js-call-jsprocedure ?%this ?fun ?this . ?args)
       (let ((f (gensym)))
	  (e `(let ((,f ,fun)) (js-call-jsprocedure ,%this ,f ,this ,@args)) e)))
      (else
       (error "js-call-jsprocedure" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-new ...                                                       */
;*---------------------------------------------------------------------*/
(define (js-new-expander x e)
   (match-case x
      ((js-new ?%this (? symbol?) . ?args)
       (let ((len (length args)))
	  (if (<=fx len 8)
	      (set-car! x (string->symbol (format "js-new~a" len)))
	      (set-car! x '(@ js-new __hopscript_public)))
	  (e x e)))
      ((js-new ?%this ?fun . ?args)
       (let ((f (gensym)))
	  (e `(let ((,f ,fun)) (js-new ,%this ,f ,@args)) e)))
      (else
       (error "js-new" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-call-with-stack-yield ...                                     */
;*    -------------------------------------------------------------    */
;*    Yield values stack allocation (when supported by the back-end).  */
;*---------------------------------------------------------------------*/
(define (js-call-with-stack-yield-expander x e)
   (match-case x
      ((?- (and ?yield (js-make-yield ?val ?done ?%this))
	  (and ?proc (lambda (?y) . ?body)))
       (cond-expand
	  ((and bigloo-c (not devel) (not debug))
	   (let ((tmp (gensym 'aux)))
	      (e `(let ()
		     (pragma ,(format "struct BgL_jsyieldz00_bgl ~a;" tmp))
		     ($bgl-init-jsyield-object! (pragma::obj ,(format "(obj_t)(&~a)" tmp)))
		     (let ((,y (pragma::obj ,(format "(obj_t)BNANOBJECT(&~a)" tmp))))
			(js-init-yield! ,y %this)
			,@body))
		 e)))
	  (else
	   (e `(,proc ,yield) e))))
      (else
       (error "js-call-with-stack-yield" "bad form" ',x))))

