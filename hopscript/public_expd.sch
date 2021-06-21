;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/public_expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Sat Jun 19 06:42:26 2021 (serrano)                */
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
;*    Simplied version of with-handle that is used when no             */
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
		 (cell (gensym 'cell))
		 (env (gensym 'env)))
	      (e `(cond-expand
		     ((not bigloo-compile)
		      (with-handler ,hdl ,body))
		     (else
		      (let* ((,env (current-dynamic-env))
			     (,ohs (env-get-error-handler ,env)))
			 (bind-exit :env ,env (,esc)
			    (begin
			       (env-set-error-handler! ,env ,esc)
			       (let ((,val ,body))
				  (env-set-error-handler! ,env ,ohs)
				  ,val))
			    (begin
			       (sigsetmask 0)
			       (env-set-error-handler! ,env ,ohs)
			       (,hdl (env-get-exitd-val ,env)))))))
		 e)))
	  ((env ?- ?hdl ?body)
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
	  ((orig ?- ?hdl ?body)
	   (e `(with-handler ,hdl ,body) e))
	  ((old bind-exit ?- ?hdl ?body)
	   (let ((ohs (gensym 'ohs))
		 (esc (gensym 'esc))
		 (val (gensym 'val))
		 (hds (gensym 'hdl))
		 (cell (gensym 'cell))
		 (env (gensym 'env)))
	      (e `(let* ((,cell ($make-stack-cell #unspecified))
			 (,env (current-dynamic-env))
			 (,ohs (env-get-error-handler ,env)))
		     (let ((,val (bind-exit :env ,env (,esc)
				    (let ((,hds (cons ,esc ,cell)))
				       (env-set-error-handler! ,env ,hds)
				       ,body))))
			(env-set-error-handler! ,env ,ohs)
			(if (eq? ,val ,cell)
			    (begin
			       (sigsetmask 0)
			       (,hdl (cell-ref ,val)))
			    ,val)))
		 e)))
	  ((cell ?- ?hdl ?body)
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
		      (let* ((,cell ($make-stack-cell #unspecified))
			     (,env (current-dynamic-env))
			     (,ohs (env-get-error-handler ,env)))
			 (bind-exit :env ,env (,esc)
			    (let ((,hds (cons ,esc ,cell)))
			       (env-set-error-handler! ,env ,hds)
			       (let ((,val ,body))
				  (env-set-error-handler! ,env ,ohs)
				  ,val))
			    (begin
			       (sigsetmask 0)
			       (env-set-error-handler! ,env ,ohs)
			       (,hdl (cell-ref ,cell)))))))
		 e)))
	  (else
	   (error "js-with-handler-no-unwind" "bad syntax" x))))))
