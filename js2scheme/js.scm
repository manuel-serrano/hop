;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/js.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 23 09:28:30 2013                          */
;*    Last change :  Fri Apr 18 09:39:48 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Js->Js (for tilde expressions).                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_js

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_scheme)
   
   (export (generic j2s-js::pair-nil ::J2SNode tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (j2s-js this::J2SNode tildec dollarc mode evalp)
   (list (format "\"~a\"" (typeof this))))

;*---------------------------------------------------------------------*/
;*    j2s-js* ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-js* start end sep nodes tildec dollarc mode evalp)
   (if (null? nodes)
       (list start end)
       (cons start
	  (let loop ((nodes nodes))
	     (append (j2s-js (car nodes) tildec dollarc mode evalp)
		(if (null? (cdr nodes))
		    (list end)
		    (cons sep (loop (cdr nodes)))))))))
				
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSeq ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSeq tildec dollarc mode evalp)
   (with-access::J2SSeq this (nodes)
      (j2s-js* "" "" ";" nodes tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBlock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBlock tildec dollarc mode evalp)
   (with-access::J2SBlock this (nodes)
      (j2s-js* "{" "}" ";" nodes tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SVarDecls ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SVarDecls tildec dollarc mode evalp)
   (with-access::J2SVarDecls this (decls)
      (j2s-js* "" "" ";" decls tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SParam ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SParam tildec dollarc mode evalp)
   (with-access::J2SParam this (id)
      (list (symbol->string id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SReturn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SReturn tildec dollarc mode evalp)
   (with-access::J2SReturn this (expr)
      (cons "return "
	 (append (j2s-js expr tildec dollarc mode evalp) '(";")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFun tildec dollarc mode evalp)
   (with-access::J2SFun this (id params body)
      (cons (format "function ~a" (if id id ""))
	 (append
	    (j2s-js* "(" ")" "," params tildec dollarc mode evalp)
	    (j2s-js body tildec dollarc mode evalp)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SObjInit ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SObjInit tildec dollarc mode evalp)
   (with-access::J2SObjInit this (inits)
      (j2s-js* "{" "}" "," inits tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDataPropertyInit ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDataPropertyInit tildec dollarc mode evalp)
   (with-access::J2SDataPropertyInit this (name val)
      (append (j2s-js name tildec dollarc mode evalp)
	 '(":")
	 (j2s-js val tildec dollarc mode evalp))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccessorPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccessorPropertyInit tildec dollarc mode evalp)
   (with-access::J2SAccessorPropertyInit this (name get set)
      (cond
	 ((and get set)
	  (append
	     (with-access::J2SFun get (id body)
		(append "get " (j2s-js name tildec dollarc mode evalp)
		   (j2s-js body tildec dollarc mode evalp)))
	     ", "
	     (with-access::J2SFun set (id body params)
		(append "set " (j2s-js name tildec dollarc mode evalp)
		   (j2s-js* "(" ")" "," params tildec dollarc mode evalp)
		   (j2s-js body tildec dollarc mode evalp)))))
	 (set
	  (with-access::J2SFun set (id body params)
	     (append "set " (j2s-js name tildec dollarc mode evalp)
		(j2s-js* "(" ")" "," params tildec dollarc mode evalp)
		(j2s-js body tildec dollarc mode evalp))))
	 (else
	  (with-access::J2SFun get (id body)
	     (append "get " (j2s-js name tildec dollarc mode evalp)
		(j2s-js body tildec dollarc mode evalp)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SWhile tildec dollarc mode evalp)
   (with-access::J2SWhile this (test body)
      (cons* "while ("
	 (append (j2s-js test tildec dollarc mode evalp)
	    '(") ") (j2s-js body tildec dollarc mode evalp)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDo tildec dollarc mode evalp)
   (with-access::J2SWhile this (test body)
      (cons* "do "
	 (append (j2s-js body tildec dollarc mode evalp)
	    '("while( ") (j2s-js test tildec dollarc mode evalp)
	    '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDecl tildec dollarc mode evalp)
   (with-access::J2SDecl this (id)
      (list "var " (symbol->string id))))
					     
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclInit tildec dollarc mode evalp)
   (with-access::J2SDeclInit this (id val)
      (cons* "var " (symbol->string id) "="
	 (append (j2s-js val tildec dollarc mode evalp)))))
					     
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SStmtExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SStmtExpr tildec dollarc mode evalp)
   (with-access::J2SStmtExpr this (expr)
      (j2s-js expr tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SIf tildec dollarc mode evalp)
   (with-access::J2SIf this (test then else)
      (cons "if( "
	 (append (j2s-js test tildec dollarc mode evalp)
	    (cons ") " 
	       (append (j2s-js then tildec dollarc mode evalp)
		  (cons " else "
		     (j2s-js else tildec dollarc mode evalp))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSequence tildec dollarc mode evalp)
   (with-access::J2SSequence this (exprs)
      (j2s-js* "(" ")" "," exprs tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnresolvedRef ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnresolvedRef tildec dollarc mode evalp)
   (with-access::J2SUnresolvedRef this (id)
      (list (symbol->string id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SThis ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SThis tildec dollarc mode evalp)
   '("this"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCond ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCond tildec dollarc mode evalp)
   (with-access::J2SCond this (test then else)
      (cons "(("
	 (append (j2s-js test tildec dollarc mode evalp)
	    (cons ") ? ("
	       (append (j2s-js then tildec dollarc mode evalp)
		  (cons ") : ("
		     (append (j2s-js else tildec dollarc mode evalp)
			'("))")))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLiteralValue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLiteralValue tildec dollarc mode evalp)
   (with-access::J2SLiteralValue this (val)
      (list (format "~a" val))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SString ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SString tildec dollarc mode evalp)
   (with-access::J2SString this (val)
      (list (string-append "\"" (string-replace val #\" #\') "\""))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRegExp ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRegExp tildec dollarc mode evalp)
   (with-access::J2SRegExp this (val flags)
      (list (string-append "/" val "/" flags))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNull ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNull tildec dollarc mode evalp)
   (list "null"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUndefined ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUndefined tildec dollarc mode evalp)
   (list "undefined"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCall tildec dollarc mode evalp)
   (with-access::J2SCall this (fun args)
      (append (j2s-js fun tildec dollarc mode evalp)
	 (j2s-js* "(" ")" "," args tildec dollarc mode evalp))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccess ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccess tildec dollarc mode evalp)
   (with-access::J2SAccess this (obj field)
      (append (j2s-js obj tildec dollarc mode evalp)
	 '("[")
	 (j2s-js field tildec dollarc mode evalp)
	 '("]"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SXml ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SXml tildec dollarc mode evalp)
   (with-access::J2SXml this (tag attrs body)
      (cons*
	 "dom_create( \""
	 (cons* (string-downcase
		   (let ((s (symbol->string tag)))
		      (substring s 1 (-fx (string-length s) 1))))
	    "\" "
	    (append
	       ;; attributes
	       (if (null? attrs)
		   '()
		   (let loop ((attrs attrs))
		      (let ((p::J2SDataPropertyInit (car attrs)))
			 (with-access::J2SDataPropertyInit p (name val)
			    (with-access::J2SString name ((s val))
			       (cons*
				  (format ",sc_jsstring2keyword(~s)," s)
				  (append
				     (j2s-js val j2s-js-attribute-tilde dollarc mode evalp)
				     (if (null? (cdr attrs))
					 '()
					 (loop (cdr attrs))))))))))
	       ;; body
	       (cond
		  ((isa? body J2SBool)
		   '(")"))
		  ((isa? body J2SSequence)
		   (with-access::J2SSequence body (exprs)
		      (j2s-js* "," ")" "," exprs tildec dollarc mode evalp)))
		  (else
		   (error "js2scheme" "Illegal tag expression" this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnary tildec dollarc mode evalp)
   (with-access::J2SUnary this (op expr)
      (cons* (symbol->string op) "("
	 (append (j2s-js expr tildec dollarc mode evalp) '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBinary ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBinary tildec dollarc mode evalp)

   (define (j2s-op op)
      (case op
	 ((OR) '("||"))
	 ((BIT_OR) '("|"))
	 (else (list (symbol->string op)))))
   
   (with-access::J2SBinary this (lhs rhs op)
      (append (j2s-js lhs tildec dollarc mode evalp)
	 (j2s-op op)
	 (j2s-js rhs tildec dollarc mode evalp))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STilde ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STilde tildec dollarc mode evalp)
   (if (procedure? tildec)
       (tildec this tildec dollarc mode evalp)
       (j2s-js-script-tilde this tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js-script-tilde ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-js-script-tilde this::J2STilde tildec dollarc mode evalp)
   (with-access::J2STilde this (loc stmt)
      (cons "<script>"
	 (append (j2s-js stmt tildec j2s-js-client-dollar mode evalp)
	    '("</script>")))))

;*---------------------------------------------------------------------*/
;*    j2s-js-attribute-tilde ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-js-attribute-tilde this::J2STilde tildec dollarc mode evalp)
   (with-access::J2STilde this (loc stmt)
      (cons "function( event ) { "
	 (append (j2s-js stmt tildec j2s-js-client-dollar mode evalp)
	    '("}")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDollar ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDollar tildec dollarc mode evalp)
   (if (procedure? dollarc)
       (dollarc this tildec dollarc mode evalp)
       (j2s-js-default-dollar this tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js-default-dollar ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-js-default-dollar this::J2SDollar tildec dollarc mode evalp)
   (with-access::J2SDollar this (expr)
      `((call-with-output-string
	   (lambda (op)
	      (obj->javascript-attr ,(j2s-scheme expr mode evalp) op))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js-client-dollar ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-js-client-dollar this::J2SDollar tildec dollarc mode evalp)
   (with-access::J2SDollar this (expr)
      (j2s-js expr tildec dollarc mode evalp)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNew ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNew tildec dollarc mode evalp)
   (with-access::J2SNew this (clazz args)
      (cons "new "
	 (append (j2s-js clazz tildec dollarc mode evalp)
	    (j2s-js* "(" ")" "," args tildec dollarc mode evalp)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAssig tildec dollarc mode evalp)
   (with-access::J2SAssig this (lhs rhs)
      (append (j2s-js lhs tildec dollarc mode evalp)
	 '("=")
	 (j2s-js rhs tildec dollarc mode evalp))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAssigOp ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAssigOp tildec dollarc mode evalp)
   (with-access::J2SAssigOp this (op lhs rhs)
      (append (j2s-js lhs tildec dollarc mode evalp)
	 (list op)
	 (j2s-js rhs tildec dollarc mode evalp))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPrefix ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPrefix tildec dollarc mode evalp)
   (with-access::J2SPrefix this (lhs op)
      (cons op (j2s-js lhs tildec dollarc mode evalp))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPostfix ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPostfix tildec dollarc mode evalp)
   (with-access::J2SPostfix this (lhs op)
      (append (j2s-js lhs tildec dollarc mode evalp) (list op))))



