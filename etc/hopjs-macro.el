;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/etc/hopjs-macro.el                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  4 15:33:55 2018                          */
;*    Last change :  Sun May  1 09:44:32 2022 (serrano)                */
;*    Copyright   :  2018-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    hopjs internal macros                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'hopjs-macro)

;*---------------------------------------------------------------------*/
;*    letn                                                             */
;*---------------------------------------------------------------------*/
(defmacro letn (name bindings &rest body)
  `(letrec ((,name #'(lambda ,(mapcar 'car bindings) ,@body)))
     (funcall ,name ,@(mapcar 'cadr bindings))))

;*---------------------------------------------------------------------*/
;*    loop                                                             */
;*---------------------------------------------------------------------*/
(defmacro loop (test &rest args)
  (let ((l (list 'cl-block nil (cons 'while (cons test args)))))
    (message "l=%s" l)
    l))

;*---------------------------------------------------------------------*/
;*    mcond ...                                                        */
;*---------------------------------------------------------------------*/
(defmacro mcond (&rest clauses)
  (if (consp clauses)
      (let ((clause (car clauses)))
	(if (eq (cadr clause) '=>)
	    `(let ((__tmp ,(car clause)))
	       (if __tmp
		   (funcall ,(caddr clause) __tmp)
		 (cond ,@(cdr clauses))))
	  (if (eq (car clause) t)
	      (cadr clause)
	    `(if ,(car clause)
		 ,(cadr clause)
	       (cond ,@(cdr clauses))))))
    '()))

;*---------------------------------------------------------------------*/
;*    case ...                                                         */
;*---------------------------------------------------------------------*/
(defmacro case (expr &rest clauses)
  `(cl-case ,expr ,@clauses))

;*---------------------------------------------------------------------*/
;*    orn ...                                                          */
;*---------------------------------------------------------------------*/
(defmacro orn (expr &rest exprs)
  ;; or-numberp
  (if (nullp exprs)
      expr
    `(let ((_e ,expr))
       (if (numberp _e)
	   _e
	 (orn ,@exprs)))))

;*---------------------------------------------------------------------*/
;*    with-debug ...                                                   */
;*---------------------------------------------------------------------*/
(defmacro with-debug (fmt &rest args)
  (message "with-debug %s" (or debug-on-error (getenv "EMACSDEBUG")))
  (if (or debug-on-error (getenv "EMACSDEBUG"))
      `(progn
	 (hopjs-debug 1 ,fmt ,@(reverse (cdr (reverse args))))
	 (let ((__r (progn ,@(last args))))
	   (hopjs-debug -1 (concat ,fmt " -> %s") ,@(reverse (cdr (reverse args))) __r)
	   __r))
    `(progn ,@(last args))))
