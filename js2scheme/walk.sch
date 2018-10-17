;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/ast.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 20 08:04:16 2014                          */
;*    Last change :  Fri Jun 20 08:04:17 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Ast tree traversal generator                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-walk-method ...                                           */
;*    -------------------------------------------------------------    */
;*    (define-walk-method (optim! this::While x y) BODY)               */
;*    =>                                                               */
;*    (define-method (optim! this::While x y)                          */
;*      (define (default-walk! n x y)                                  */
;*         (walk2! n optim! x y))                                      */
;*      (define (walk! n x y)                                          */
;*         (optim! n x y))                                             */
;*      BODY)                                                          */
;*---------------------------------------------------------------------*/
(define-macro (define-walk-method args . body)
   
   (define (parse-ident id::symbol)
      (let* ((string (symbol->string id))
	     (len (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		(values id #f))
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(values (string->symbol (substring string 0 walker))
		   (string->symbol (substring string (+fx walker 2)))))
	       (else
		(loop (+fx walker 1)))))))

   (define (id-without-type id)
      (multiple-value-bind (id type)
	 (parse-ident id)
	 id))

   (define (id-type id)
      (multiple-value-bind (id type)
	 (parse-ident id)
	 type))

   (let* ((name (car args))
	  (sname (symbol->string name))
	  (i (string-contains sname "::"))
	  (c (string-ref sname (-fx (or i (string-length sname)) 1)))
	  (nb-method-args (-fx (length args) 2))
	  (short-walk (case c
			 ((#\!) 'walk!)
			 ((#\*) 'walk*)
			 (else 'walk)))
	  (tname (case c
		    ((#\!) (string->symbol (string-append sname "::J2SNode")))
		    ((#\*) (string->symbol (string-append sname "::pair-nil")))
		    (else name)))
	  (long-walk (case c
			((#\!)
			 (string->symbol (format "walk~a!" nb-method-args)))
			((#\*)
			 (string->symbol (format "walk~a*" nb-method-args)))
			(else
			 (string->symbol (format "walk~a" nb-method-args)))))
	  (define-gen/met (if (eq? (id-type (cadr args)) 'J2SNode)
			      'define-generic
			      'define-method))
	  (default-walk (symbol-append 'default- short-walk)))
      `(,define-gen/met	(,tname ,@(cdr args))
	  (define (call-default-walker)
	     (,long-walk ,(id-without-type (cadr args)) ,(id-without-type name)
		,@(map id-without-type (cddr args))))
	  (define (,default-walk ,(id-without-type (cadr args)) ,@(cddr args))
	     (,long-walk ,(id-without-type (cadr args)) ,(id-without-type name)
		,@(map id-without-type (cddr args))))
	  (define (,short-walk ,(id-without-type (cadr args)) ,@(cddr args))
	     (,name ,@(map id-without-type (cdr args))))
	  ,@body)))

