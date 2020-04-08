;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/call.sch                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  7 18:28:45 2017                          */
;*    Last change :  Tue Apr  7 07:25:39 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js-call expansion                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-call0 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (js-call0 %this fun this)
   (if (symbol? fun)
       `(if (and (js-procedure? ,fun) (=fx (js-procedure-arity ,fun) 1))
	    (with-access::JsProcedure ,fun (procedure)
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
	      (if (and (js-procedure? ,fun) (=fx (js-procedure-arity ,fun) 2))
		  (with-access::JsProcedure ,fun (procedure)
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
	      (if (and (js-procedure? ,fun) (=fx (js-procedure-arity ,fun) 3))
		   (with-access::JsProcedure ,fun (procedure)
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
	      (if (and (js-procedure? ,fun) (=fx (js-procedure-arity ,fun) 4))
		  (with-access::JsProcedure ,fun (procedure)
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
	      (if (and (js-procedure? ,fun) (=fx (js-procedure-arity ,fun) 5))
		  (with-access::JsProcedure ,fun (procedure)
		     (procedure ,this ,t0 ,t1 ,t2 ,t3))
		  ((@ js-call4 __hopscript_public)
		   ,%this ,fun ,this ,t0 ,t1 ,t2 ,t3))))
       (let ((fsym (gensym 'fun)))
	  `(let ((,fsym ,fun))
	      (js-call4 ,%this ,fsym ,this ,a0 ,a1 ,a2 ,a3)))))

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
