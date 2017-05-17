;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/call.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  7 18:28:45 2017                          */
;*    Last change :  Sat May  6 06:32:31 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    js-call expansion                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-call0 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-call0 %this fun this)
   (if (symbol? fun)
       `(if (and (object? ,fun) (eq? (object-class ,fun) JsFunction1))
	    (with-access::JsFunction1 ,fun (procedure)
	       (procedure ,this))
	    ((@ js-call0 __hopscript_public) ,%this ,fun ,this))
       (let ((fsym (gensym 'fun)))
	  `(let ((,fsym ,fun))
	      (js-call0 ,%this ,fsym ,this)))))

;*---------------------------------------------------------------------*/
;*    js-call1 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-call1 %this fun this a0)
   (if (symbol? fun)
       (let ((t0 (gensym 'a0)))
	  `(let ((,t0 ,a0))
	      (if (and (object? ,fun) (eq? (object-class ,fun) JsFunction2))
		  (with-access::JsFunction2 ,fun (procedure)
		     (procedure ,this ,t0))
		  ((@ js-call1 __hopscript_public) ,%this ,fun ,this ,t0))))
       (let ((fsym (gensym 'fun)))
	  `(let ((,fsym ,fun))
	      (js-call1 ,%this ,fsym ,this ,a0)))))

;*---------------------------------------------------------------------*/
;*    js-call2 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-call2 %this fun this a0 a1)
   (if (symbol? fun)
       (let ((t0 (gensym 'a0))
	     (t1 (gensym 'a1)))
	  `(let* ((,t0 ,a0)
		  (,t1 ,a1))
	      (if (and (object? ,fun) (eq? (object-class ,fun) JsFunction3))
		   (with-access::JsFunction3 ,fun (procedure)
		      (procedure ,this ,t0 ,t1))
		   ((@ js-call2 __hopscript_public)
		    ,%this ,fun ,this ,t0 ,t1))))
       (let ((fsym (gensym 'fun)))
	  `(let ((,fsym ,fun))
	      (js-call2 ,%this ,fsym ,this ,a0 ,a1)))))

;*---------------------------------------------------------------------*/
;*    js-call3 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-call3 %this fun this a0 a1 a2)
   (if (symbol? fun)
       (let ((t0 (gensym 'a0))
	     (t1 (gensym 'a1))
	     (t2 (gensym 'a2)))
	  `(let* ((,t0 ,a0)
		  (,t1 ,a1)
		  (,t2 ,a2))
	      (if (and (object? ,fun) (eq? (object-class ,fun) JsFunction4))
		  (with-access::JsFunction4 ,fun (procedure)
		     (procedure ,this ,t0 ,t1 ,t2))
		  ((@ js-call3 __hopscript_public)
		   ,%this ,fun ,this ,t0 ,t1 ,t2))))
       (let ((fsym (gensym 'fun)))
	  `(let ((,fsym ,fun))
	      (js-call3 ,%this ,fsym ,this ,a0 ,a1 ,a2)))))

;*---------------------------------------------------------------------*/
;*    js-call4 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-call4 %this fun this a0 a1 a2 a3)
   (if (symbol? fun)
       (let ((t0 (gensym 'a0))
	     (t1 (gensym 'a1))
	     (t2 (gensym 'a2))
	     (t3 (gensym 'a3)))
	  `(let* ((,t0 ,a0)
		  (,t1 ,a1)
		  (,t2 ,a2)
		  (,t3 ,a3))
	      (if (and (object? ,fun) (eq? (object-class ,fun) JsFunction5))
		  (with-access::JsFunction5 ,fun (procedure)
		     (procedure ,this ,t0 ,t1 ,t2 ,t3))
		  ((@ js-call4 __hopscript_public)
		   ,%this ,fun ,this ,t0 ,t1 ,t2 ,t3))))
       (let ((fsym (gensym 'fun)))
	  `(let ((,fsym ,fun))
	      (js-call4 ,%this ,fsym ,this ,a0 ,a1 ,a2 ,a3)))))

;* {*---------------------------------------------------------------------*} */
;* {*    js-call ...                                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define-macro (js-call %this fun this . args)                       */
;*    (match-case args                                                 */
;*       (() `(js-call0 ,%this ,fun ,this))                            */
;*       ((?a0) `(js-call1 ,%this ,fun ,this ,a0))                     */
;*       ((?a0 ?a1) `(js-call2 ,%this ,fun ,this ,a0 ,a1))             */
;*       ((?a0 ?a1 ?a2) `(js-call3 ,%this ,fun ,this ,a0 ,a1 ,a2))     */
;*       ((?a0 ?a1 ?a2 ?a3) `(js-call4 ,%this ,fun ,this ,a0 ,a1 ,a2 ,a3)) */
;*       ((?a0 ?a1 ?a2 ?a3 ?a4) `(js-call5 ,%this ,fun ,this ,a0 ,a1 ,a2 ,a3 ,a4)) */
;*       ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5) `(js-call6 ,%this ,fun ,this ,a0 ,a1 ,a2 ,a3 ,a4 ,a5)) */
;*       ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6) `(js-call7 ,%this ,fun ,this ,a0 ,a1 ,a2 ,a3 ,a4 ,a5 ,a6)) */
;*       ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7) `(js-call8 ,%this ,fun ,this ,a0 ,a1 ,a2 ,a3 ,a4 ,a5 ,a6 ,a7)) */
;*       (else `(js-calln ,%this ,fun ,this ,@args))))                 */

;*---------------------------------------------------------------------*/
;*    js-call-method ...                                               */
;*---------------------------------------------------------------------*/
(define-expander js-call-method
   (lambda (x e)
      (match-case x
	 ((?- ?%this ?obj ?prop)
	  (e `(js-call-method0 ,%this ,obj ,prop)) e)
	 ((?- ?%this ?obj ?prop ?a0)
	  (e `(js-call-method1 ,%this ,obj ,prop ,a0)) e)
	 ((?- ?%this ?obj ?prop ?a0 ?a1)
	  (e `(js-call-method2 ,%this ,obj ,prop ,a0 ,a1)) e)
	 ((?- ?%this ?obj ?prop ?a0 ?a1 ?a2)
	  (e `(js-call-method3 ,%this ,obj ,prop ,a0 ,a1 ,a2)) e)
	 ((?- ?%this ?obj ?prop ?a0 ?a1 ?a2 ?a3)
	  (e `(js-call-method4 ,%this ,obj ,prop ,a0 ,a1 ,a2 ,a3)) e)
	 (else
	  (map (lambda (x) (e x e) x))))))
