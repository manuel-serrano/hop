;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/public_expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Tue Aug 31 13:42:14 2021 (serrano)                */
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
       (e `(if (flonum? ,expr)
	       ,expr
	       ((@ js-toflonum __hopscript_arithmetic)
		((@ js-tonumber __hopscript_public) ,expr ,%this)))
	  e))
      ((?- ?expr ?%this)
       (let ((tmp (gensym '%e)))
	  (e `(let ((,tmp ,expr)) (js-tonumber-for-flonum ,tmp ,%this)) e)))
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
