;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/js.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 23 09:28:30 2013                          */
;*    Last change :  Thu Dec 10 18:48:42 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
	   __js2scheme_scheme
	   __js2scheme_stmtassign
	   __js2scheme_stage)
   
   (export j2s-javascript-stage
	   (generic j2s-js::pair-nil ::J2SNode tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-stage ...                                         */
;*---------------------------------------------------------------------*/
(define j2s-javascript-stage
   (instantiate::J2SStageProc
      (name "javascript")
      (comment "JavaScript code generation")
      (proc (lambda (ast conf)
	       (call-with-eval-module (eval! `(module ,(gensym)))
		  (lambda ()
		     (eval! `(define %this ,(config-get conf :%this)))
		     (eval! `(define %resource ,(config-get conf :resource)))
		     (eval! `(define %source ,(config-get conf :source)))
		     (eval! `(define %worker ,(config-get conf :worker)))
		     (eval! `(define %scope %this))
		     (eval! `(define this %this))
		     (j2s-js ast #f
			(lambda (this::J2SDollar tildec dollarc mode evalp conf)
			   (with-access::J2SDollar this (node)
			      (list
				 (call-with-output-string
				    (lambda (op)
				       (let ((expr (j2s-scheme node mode evalp conf)))
					  (write (eval! expr) op)))))))
			'normal (lambda (x) x) conf)))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js-id ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-js-id::bstring id::symbol)
   (let ((s (symbol->string! id)))
      (if (char=? (string-ref s 0) #\%)
	  (string-append "_$" (substring s 1))
	  s)))

;*---------------------------------------------------------------------*/
;*    j2s-js* ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-js* this::J2SNode start end sep nodes tildec dollarc mode evalp conf)
   (if (null? nodes)
       (list this start end)
       (cons* this start
	  (let loop ((nodes nodes))
	     (append (j2s-js (car nodes) tildec dollarc mode evalp conf)
		(if (null? (cdr nodes))
		    (list end)
		    (cons sep (loop (cdr nodes)))))))))
				
;*---------------------------------------------------------------------*/
;*    j2s-js*/beforeeend ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-js-ellipsis this::J2SNode start end sep ellipsis nodes tildec dollarc mode evalp conf)
   (cond
      ((null? nodes)
       (list this start ellipsis end))
      ((null? (cdr nodes))
       (cons* this start ellipsis
	  (append (j2s-js (car nodes) tildec dollarc mode evalp conf)
	     (list end)  )))
      (else
       (cons* this start
	  (let loop ((nodes nodes))
	     (append (j2s-js (car nodes) tildec dollarc mode evalp conf)
		(cond
		   ((null? (cddr nodes))
		    (cons* sep ellipsis
		       (append
			  (j2s-js (cadr nodes) tildec dollarc mode evalp conf)
			  (list end))))
		   (else
		    (cons sep (loop (cdr nodes)))))))))))
				
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNode ...                                             */
;*    -------------------------------------------------------------    */
;*    This function returns a list of tokens and nodes. The nodes      */
;*    are meant to used for generating source-map tables. When         */
;*    generating JS code, they should be filtered out.                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-js::pair-nil this::J2SNode tildec dollarc mode evalp conf)
   (list this (format "\"~a\"" (typeof this))))

;*---------------------------------------------------------------------*/
;*    j2s-js::pair-nil ::J2SProgram ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-js::pair-nil this::J2SProgram tildec dollarc mode evalp conf)
   (with-access::J2SProgram this (headers decls nodes mode)
      (let* ((body (append headers decls nodes))
	     (prgm (j2s-js* this "" "" "" body tildec dollarc mode evalp conf)))
	 (case mode
	    ((normal) prgm)
	    ((strict hopscript) (cons "\"use strict\";\n" prgm))
	    (else prgm)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSeq ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSeq tildec dollarc mode evalp conf)
   (with-access::J2SSeq this (nodes)
      (j2s-js* this "" "" "" nodes tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBlock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBlock tildec dollarc mode evalp conf)
   (with-access::J2SBlock this (nodes)
      (j2s-js* this "{" "}" "" nodes tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLabel ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLabel tildec dollarc mode evalp conf)
   (with-access::J2SLabel this (id body)
      (cons* this (format "{ ~a: " id)
	 (append (j2s-js body tildec dollarc mode evalp conf)
	    '("}")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SVarDecls ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SVarDecls tildec dollarc mode evalp conf)
   (with-access::J2SVarDecls this (decls)
      (j2s-js* this "" "" "" decls tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLetBlock tildec dollarc mode evalp conf)
   (with-access::J2SLetBlock this (decls nodes)
      (append
	 (j2s-js* this "{" "" "" decls tildec dollarc mode evalp conf)
	 (j2s-js* this "" "}" "" nodes tildec dollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLet ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLet tildec dollarc mode evalp conf)
   (with-access::J2SLet this (isconst id val)
      (list this (if isconst "const " "let ") (j2s-js-id id) ";")))
   
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SParam ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SParam tildec dollarc mode evalp conf)
   (with-access::J2SParam this (id)
      (list this (j2s-js-id id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SReturn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SReturn tildec dollarc mode evalp conf)
   (with-access::J2SReturn this (expr)
      (cons* this "return "
	 (append (j2s-js expr tildec dollarc mode evalp conf) '(";")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFun tildec dollarc mode evalp conf)
   (with-access::J2SFun this (params body idthis vararg)
      (let ((ellipsis (if (eq? vararg 'rest) "... " "")))
	 (if (eq? idthis '%)
	     ;; an arrow function
	     (cons* this "("
		(append
		   (j2s-js-ellipsis this "(" ") => " "," ellipsis
		      params tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf)
		   '(")")))
	     ;; a regular function
	     (cons* this (format "function ~a" (or (j2sfun-id this) ""))
		(append
		   (j2s-js-ellipsis this "(" ")" "," ellipsis
		      params tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SObjInit ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SObjInit tildec dollarc mode evalp conf)
   (with-access::J2SObjInit this (inits)
      (j2s-js* this "{" "}" "," inits tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDataPropertyInit ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDataPropertyInit tildec dollarc mode evalp conf)
   (with-access::J2SDataPropertyInit this (name val)
      (cons this
	 (append (j2s-js name tildec dollarc mode evalp conf)
	    '(":")
	    (j2s-js val j2s-js-attribute-tilde dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccessorPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccessorPropertyInit tildec dollarc mode evalp conf)
   (with-access::J2SAccessorPropertyInit this (name get set)
      (cons this
	 (cond
	    ((and get set)
	     (append
		(with-access::J2SFun get (id body)
		   (append "get " (j2s-js name tildec dollarc mode evalp conf)
		      (j2s-js body tildec dollarc mode evalp conf)))
		", "
		(with-access::J2SFun set (id body params)
		   (append "set " (j2s-js name tildec dollarc mode evalp conf)
		      (j2s-js* this "(" ")" "," params tildec dollarc mode evalp conf)
		      (j2s-js body tildec dollarc mode evalp conf)))))
	    (set
	     (with-access::J2SFun set (id body params)
		(append "set " (j2s-js name tildec dollarc mode evalp conf)
		   (j2s-js* this "(" ")" "," params tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf))))
	    (else
	     (with-access::J2SFun get (id body)
		(append "get " (j2s-js name tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SWhile tildec dollarc mode evalp conf)
   (with-access::J2SWhile this (test body)
      (cons* this "while ("
	 (append (j2s-js test tildec dollarc mode evalp conf)
	    '(") ") (j2s-js body tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDo tildec dollarc mode evalp conf)
   (with-access::J2SWhile this (test body)
      (cons* this "do "
	 (append (j2s-js body tildec dollarc mode evalp conf)
	    '("while (") (j2s-js test tildec dollarc mode evalp conf)
	    '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFor tildec dollarc mode evalp conf)
   
   (define (join sep el)
      (if (null? el)
	  '()
	  (let loop ((el el))
	     (if (null? (cdr el))
		 (car el)
		 (append (car el) (list sep) (loop (cdr el)))))))
   
   (define (var-decl decl::J2SDecl)
      (with-access::J2SDecl decl (id)
	 (cons (j2s-js-id id)
	    (if (isa? decl J2SDeclInit)
		(with-access::J2SDeclInit decl (id val)
		   (cons "=" (j2s-js val tildec dollarc mode evalp conf)))
		'()))))
   
   (define (stmt-expr stmt::J2SStmtExpr)
      (with-access::J2SStmtExpr stmt (expr)
	 (j2s-js expr tildec dollarc mode evalp conf)))
   
   (with-access::J2SFor this (init test incr body)
      (cons* this "for( "
	 (append
	    (cond
	       ((isa? init J2SVarDecls)
		(with-access::J2SVarDecls init (decls)
		   (cons "var " (join "," (map var-decl decls)))))
	       ((isa? init J2SSeq)
		(with-access::J2SSeq init (nodes)
		   (if (every (lambda (n) (isa? n J2SStmtExpr)) nodes)
		       (join "," (map stmt-expr nodes))
		       (j2s-js init tildec dollarc mode evalp conf))))
	       (else
		(j2s-js init tildec dollarc mode evalp conf)))
	    '(";")
	    (j2s-js test tildec dollarc mode evalp conf)
	    '(";")
	    (j2s-js incr tildec dollarc mode evalp conf)
	    '(") ")
	    (j2s-js body tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SForIn tildec dollarc mode evalp conf)
   (with-access::J2SForIn this (lhs obj body)
      (cons* this "for("
         (append (if (isa? lhs J2SVarDecls)
		     (with-access::J2SVarDecls lhs (decls)
			(with-access::J2SDecl (car decls) (id)
			   (list "var " (j2s-js-id id))))
		     (j2s-js lhs tildec dollarc mode evalp conf))
	    '(" in ")
	    (j2s-js obj tildec dollarc mode evalp conf)
	    '(") ")
	    (j2s-js body tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDecl tildec dollarc mode evalp conf)
   (with-access::J2SDecl this (id)
      (list this "var " (j2s-js-id id) ";")))
                                             
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclInit tildec dollarc mode evalp conf)
   (with-access::J2SDeclInit this (id val)
      (cons* this "var " (j2s-js-id id) "="
         (append (j2s-js val tildec dollarc mode evalp conf) '(";")))))
                                             
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclFun tildec dollarc mode evalp conf)
   (with-access::J2SDeclInit this (id val)
      (j2s-js val tildec dollarc mode evalp conf)))
                                             
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SStmtExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SStmtExpr tildec dollarc mode evalp conf)
   (with-access::J2SStmtExpr this (expr)
      (cons this
	 (append (j2s-js expr tildec dollarc mode evalp conf) '(";")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SIf tildec dollarc mode evalp conf)
   (with-access::J2SIf this (test then else)
      (cons* this "if( "
	 (append (j2s-js test tildec dollarc mode evalp conf)
	    (cons ") " 
	       (append (j2s-js then tildec dollarc mode evalp conf)
		  (if (isa? else J2SNop)
		      '()
		      (cons " else "
			 (j2s-js else tildec dollarc mode evalp conf)))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSequence tildec dollarc mode evalp conf)
   (with-access::J2SSequence this (exprs)
      (j2s-js* this "(" ")" "," exprs tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRef tildec dollarc mode evalp conf)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id global)
	 (list this (j2s-js-id id)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnresolvedRef ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnresolvedRef tildec dollarc mode evalp conf)
   (with-access::J2SUnresolvedRef this (id)
      (list this (j2s-js-id id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SHopRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SHopRef tildec dollarc mode evalp conf)
   ;; MS 1 jul 2014: not quit sure, what about client-side scheme modules?
   (with-access::J2SHopRef this (id)
      (list this (j2s-js-id id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SThis ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SThis tildec dollarc mode evalp conf)
   (list this "this"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCond ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCond tildec dollarc mode evalp conf)
   (with-access::J2SCond this (test then else)
      (cons* this "(("
	 (append (j2s-js test tildec dollarc mode evalp conf)
	    (cons ") ? ("
	       (append (j2s-js then tildec dollarc mode evalp conf)
		  (cons ") : ("
		     (append (j2s-js else tildec dollarc mode evalp conf)
			'("))")))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSwitch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSwitch tildec dollarc mode evalp conf)
   (with-access::J2SSwitch this (key cases)
      (cons* this "switch("
	 (append
	    (j2s-js key tildec dollarc mode evalp conf)
	    '(") {")
	    (append-map (lambda (n) (j2s-js n tildec dollarc mode evalp conf))
	       cases)
	    '("}")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCase ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCase tildec dollarc mode evalp conf)
   (with-access::J2SCase this (expr body)
      (cons* this "case "
	 (append
	    (j2s-js expr tildec dollarc mode evalp conf)
	    '(": ")
	    (j2s-js body tildec dollarc mode evalp conf)
	    '("\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDefault ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDefault tildec dollarc mode evalp conf)
   (with-access::J2SDefault this (expr body)
      (cons* this "default: "
	 (append
	    (j2s-js body tildec dollarc mode evalp conf)
	    '("\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBreak ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBreak tildec dollarc mode evalp conf)
   (with-access::J2SBreak this (target id)
      (if id
	  (list this "break " id ";")
	  (list this "break;"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SContinue ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SContinue tildec dollarc mode evalp conf)
   (with-access::J2SContinue this (target id)
      (if id
	  (list this "continue " id ";")
	  (list this "continue;"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SComprehension ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SComprehension tildec dollarc mode evalp conf)
   (with-access::J2SComprehension this (decls iterables test expr ast)
      
      (define (comprehension names fun cond)
	 (cons* this "hop_comprehension" "( ["
	    (append
	       (j2s-js (car iterables) tildec dollarc mode evalp conf)
	       (append-map (lambda (iterable)
		       (cons ", "
			  (j2s-js iterable tildec dollarc mode evalp conf)))
		  (cdr iterables))
	       `("], " ,@fun ", " ,@cond ", ["
		   ,(format "~(, )" (map (lambda (n) (format "~s" n)) names))
		   "], "
		   ,(format "~s"
		       (call-with-output-string
			  (lambda (op) (ast->json test op))))
		   ", "
		   ,(format "~s"
		       (call-with-output-string
			  (lambda (op) (ast->json expr op))))
		   ", "
		   ,(format "~(, )"
		       (map (lambda (decl)
			       (call-with-output-string
				  (lambda (op) (ast->json decl op))))
			  decls)))
	       '(")"))))
      
      (let ((names (map (lambda (decl)
			   (with-access::J2SDecl decl (id)
			      (j2s-js-id id)))
		      decls)))
	 (if (not (isa? test J2SBool))
	     (comprehension names
		(cons* "function" "(" (format "~(, )" names) ")"
		   "{" " return "
		   (append (j2s-js expr tildec dollarc mode evalp conf)
		      '("}")))
		(cons* "function" "(" (format "~(, )" names) ")"
		   "{" " return "
		   (append (j2s-js test tildec dollarc mode evalp conf)
		      '("}"))))
	     (with-access::J2SBool test (val)
		(if (eq? val #t)
		    (comprehension names
		       (cons* "function" "(" (format "~(, )" names) ")"
			  "{" " return "
			  (append
			     (j2s-js expr tildec dollarc mode evalp conf)
			     '("}")))
		       '("true"))
		    '("[]")))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLiteralValue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLiteralValue tildec dollarc mode evalp conf)
   (with-access::J2SLiteralValue this (val)
      (list this (format "~a" (if (and (flonum? val) (nanfl? val)) "NaN" val)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBool ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBool tildec dollarc mode evalp conf)
   (with-access::J2SBool this (val)
      (list this (if val "true" "false"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNativeString ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNativeString tildec dollarc mode evalp conf)
   (with-access::J2SNativeString this (val)
      (list this (string-append "\"" (string-for-read val) "\""))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SString ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SString tildec dollarc mode evalp conf)
   (with-access::J2SString this (val)
      (list this (string-append "\"" (string-for-read val) "\""))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STemplate ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STemplate tildec dollarc mode evalp conf)
   (with-access::J2STemplate this (exprs)
      (let loop ((nodes exprs))
	 (cons "("
	    (append (j2s-js (car nodes) tildec dollarc mode evalp conf)
	       (cons ")"
		  (if (null? (cdr nodes))
		      '()
		      (cons "+" (loop (cdr nodes))))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SArray ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SArray tildec dollarc mode evalp conf)
   (with-access::J2SArray this (exprs)
      (j2s-js* this "[" "]" "," exprs tildec dollarc mode evalp conf)))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRegExp ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRegExp tildec dollarc mode evalp conf)
   (with-access::J2SRegExp this (val flags)
      (list this (string-append "/" val "/" flags))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNull ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNull tildec dollarc mode evalp conf)
   (list this "null"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUndefined ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUndefined tildec dollarc mode evalp conf)
   (list this "undefined"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCall tildec dollarc mode evalp conf)
   (with-access::J2SCall this (fun args)
      (cons this
	 (append (j2s-js fun tildec dollarc mode evalp conf)
	    (j2s-js* this "(" ")" "," args tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccess ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccess tildec dollarc mode evalp conf)
   (with-access::J2SAccess this (obj field)
      (cons this
	 (append (j2s-js obj tildec dollarc mode evalp conf)
	    '("[")
	    (j2s-js field tildec dollarc mode evalp conf)
	    '("]")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SParen ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SParen tildec dollarc mode evalp conf)
   (with-access::J2SParen this (expr)
      (cons* this "("
	 (append (j2s-js expr tildec dollarc mode evalp conf) '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnary tildec dollarc mode evalp conf)
   (with-access::J2SUnary this (op expr)
      (cons* this (symbol->string op) "("
	 (append (j2s-js expr tildec dollarc mode evalp conf) '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-op ...                                                       */
;*---------------------------------------------------------------------*/
(define (j2s-op op)
   (case op
      ((OR) "||")
      ((BIT_OR) "|")
      (else (symbol->string op))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBinary ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBinary tildec dollarc mode evalp conf)
   (with-access::J2SBinary this (lhs rhs op)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp conf)
	    (list " " (j2s-op op) " ")
	    (j2s-js rhs tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STilde ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STilde tildec dollarc mode evalp conf)
   (cons this
      (if (procedure? tildec)
	  (tildec this tildec dollarc mode evalp conf)
	  (j2s-js-script-tilde this tildec dollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js-script-tilde ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-js-script-tilde this::J2STilde tildec dollarc mode evalp conf)
   (with-access::J2STilde this (loc stmt)
      (cons* this "<script>"
	 (append (j2s-js stmt tildec j2s-js-client-dollar mode evalp conf)
	    '("</script>")))))

;*---------------------------------------------------------------------*/
;*    j2s-js-attribute-tilde ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-js-attribute-tilde this::J2STilde tildec dollarc mode evalp conf)
   (with-access::J2STilde this (loc stmt)
      (let* ((temp (gensym))
	     (assign (j2s-stmt-assign stmt temp))
	     (ndollarc (j2s-js-client-dollar dollarc)))
	 (cons* this "function( event ) { var "
	    (j2s-js-id temp) "; "
	    (append (j2s-js assign tildec ndollarc mode evalp conf)
	       `(,(format "\nreturn ~a}" temp)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDollar ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDollar tildec dollarc mode evalp conf)
   (cons this
      (if (procedure? dollarc)
	  (dollarc this tildec dollarc mode evalp conf)
	  (j2s-js-default-dollar this tildec dollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js-default-dollar ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-js-default-dollar this::J2SDollar tildec dollarc mode evalp conf)
   (with-access::J2SDollar this (node)
      (list this
	 `(call-with-output-string
	     (lambda (op)
		(obj->javascript-attr ,(j2s-scheme node mode evalp conf) op))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js-client-dollar ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-js-client-dollar odollarc)
   (lambda (this::J2SDollar tildec dollarc mode evalp conf)
      (with-access::J2SDollar this (node)
	 (j2s-js node tildec odollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNew ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNew tildec dollarc mode evalp conf)
   (with-access::J2SNew this (clazz args)
      (cons* this "new "
	 (append (j2s-js clazz tildec dollarc mode evalp conf)
	    (j2s-js* this "(" ")" "," args tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAssig tildec dollarc mode evalp conf)
   (with-access::J2SAssig this (lhs rhs)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp conf)
	    '("=")
	    (j2s-js rhs tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAssigOp ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAssigOp tildec dollarc mode evalp conf)
   (with-access::J2SAssigOp this (op lhs rhs)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp conf)
	    (list (j2s-js-id op) "=")
	    (j2s-js rhs tildec dollarc mode evalp conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPrefix ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPrefix tildec dollarc mode evalp conf)
   (with-access::J2SPrefix this (lhs op)
      (cons* this (symbol->string op)
	 (j2s-js lhs tildec dollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPostfix ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPostfix tildec dollarc mode evalp conf)
   (with-access::J2SPostfix this (lhs op)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp conf)
	    (list (symbol->string op))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SThrow ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SThrow tildec dollarc mode evalp conf)
   (with-access::J2SThrow this (expr)
      (cons* this "throw "
	 (append (j2s-js expr tildec dollarc mode evalp conf) '(";")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNop ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNop tildec dollarc mode evalp conf)
   '())

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STry ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STry tildec dollarc mode evalp conf)
   (with-access::J2STry this (body catch finally)
      (cons* this "try "
	 (append (j2s-js body tildec dollarc mode evalp conf)
	    (if (isa? catch J2SNop)
		'()
		(j2s-js catch tildec dollarc mode evalp conf) )
	    (if (isa? finally J2SNop)
		'()
		(cons* this "finally "
		   (j2s-js finally tildec dollarc mode evalp conf)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCatch ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCatch tildec dollarc mode evalp conf)
   (with-access::J2SCatch this (param body)
      (cons* this "catch("
	 (append (j2s-js param tildec dollarc mode evalp conf)
	    '(")")
	    (j2s-js body tildec dollarc mode evalp conf)))))
	    
	 
	    

