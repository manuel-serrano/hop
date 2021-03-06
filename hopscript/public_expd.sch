;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/public_expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 23 07:35:40 2017                          */
;*    Last change :  Wed Dec 18 09:18:57 2019 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
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
