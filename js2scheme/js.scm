;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/js.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 23 09:28:30 2013                          */
;*    Last change :  Fri Aug  5 13:40:03 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
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
	       (add-header! ast conf)
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
				       (let ((expr (j2s-scheme node mode evalp conf '())))
					  (write (eval! expr) op)))))))
			'normal (lambda (x) x) conf)))))))
	 
;*---------------------------------------------------------------------*/
;*    add-header! ...                                                  */
;*---------------------------------------------------------------------*/
(define (add-header! ast conf)
   (let ((header (config-get conf :header #f)))
      (when (and header (isa? ast J2SProgram))
	 (with-access::J2SProgram ast (headers loc)
	    (set! headers (cons (instantiate::J2SPragma
				   (loc loc)
				   (lang 'javascript)
				   (expr header))
			     headers))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js-id ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-js-id::bstring decl::J2SDecl)
   (with-access::J2SDecl decl (_scmid id key)
      (if (symbol? _scmid)
	  (string-append (string-replace (symbol->string! _scmid) #\% #\$)
	     "$$" (fixnum->string key))
	  (let ((s (symbol->string! id)))
	     (if (char=? (string-ref s 0) #\%)
		 (string-append (string-replace s #\% #\$)
		    "$$" (fixnum->string key))
		 s)))))

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
;*    j2s-js-ellipsis ...                                              */
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
      (match-case nodes
	 (((and ?node (? (lambda (n) (isa? n J2SBlock)))))
	  (j2s-js node tildec dollarc mode evalp conf))
	 (else
	  (j2s-js* this "{" "}" "" nodes tildec dollarc mode evalp conf)))))

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
;*    j2s-binder ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-binder binder)
   (case binder
      ((let let-opt) "let ")
      ((const const-opt) "const ")
      ((var) "var ")
      ((param) "")
      (else (error "js" "Illegal binder" binder))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDecl tildec dollarc mode evalp conf)
   (with-access::J2SDecl this (binder)
      (if (j2s-param? this)
	  (list this (j2s-binder binder) (j2s-js-id this))
	  (list this (j2s-binder binder) (j2s-js-id this) ";"))))
                                             
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclInit tildec dollarc mode evalp conf)
   (with-access::J2SDeclInit this (val binder)
      (if (or (j2s-let-opt? this) (not (isa? val J2SUndefined)))
	  (cons* this (j2s-binder binder) (j2s-js-id this) "="
	     (append (j2s-js val tildec dollarc mode evalp conf)
		(if (j2s-param? this) '() '(";"))))
	  (list this (j2s-binder binder) (j2s-js-id this) ";"))))
                                             
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SReturn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SReturn tildec dollarc mode evalp conf)
   (with-access::J2SReturn this (expr tail)
      (cons* this "return "
	 (append (j2s-js expr tildec dollarc mode evalp conf) '(";")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SReturnYield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SReturnYield tildec dollarc mode evalp conf)
   
   (define (identity-kont? kont)
      (or (not (isa? kont J2SKont))
	  (with-access::J2SKont kont (body param)
	     (when (isa? body J2SStmtExpr)
		(with-access::J2SStmtExpr body (expr)
		   (when (isa? expr J2SRef)
		      (with-access::J2SRef expr (decl)
			 (eq? decl param))))))))
   
   (with-access::J2SReturnYield this (expr kont generator)
      (cond
	 ((identity-kont? kont)
	  (cond
	     (generator
	      (cons* this
		 "return $GEN.YieldS(" 
		 (append (j2s-js expr tildec dollarc mode evalp conf)
		    '(",$GEN.kid);"))))
	     ((isa? kont J2SUndefined)
	      (cons* this
		 "return $GEN.Return("
		 (append (j2s-js expr tildec dollarc mode evalp conf)
		    '(");"))))
	     (else
	      (cons* this
		 "return $GEN.Yield("
		 (append (j2s-js expr tildec dollarc mode evalp conf)
		    '(",$GEN.kid);"))))))
	 (generator
	  (cons* this
	     "return $GEN.YieldS("
	     (append (j2s-js expr tildec dollarc mode evalp conf)
		'(",")
		(if (isa? kont J2SUndefined) '("true") '("false"))
		'(",")
		(if (isa? kont J2SUndefined)
		    '("$GEN.done")
		    (j2s-js kont tildec dollarc mode evalp conf))
		'(");"))))
	 ((isa? kont J2SUndefined)
	  (cons* this "return $GEN.Return("
	     (append (j2s-js expr tildec dollarc mode evalp conf)
		'(");"))))
	 (else
	  (cons* this
	     "return $GEN.Yield("
	     (append (j2s-js expr tildec dollarc mode evalp conf)
		'(",")
		(j2s-js kont tildec dollarc mode evalp conf)
		'(");")))))))

;*---------------------------------------------------------------------*/
;*    j2s-js-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-js-fun this::J2SFun tildec dollarc mode evalp conf funid)
   
   (define (merge-blocks this::J2SBlock)
      this)
;*       (with-access::J2SBlock this (nodes)                           */
;* 	 ;; check the pattern (j2sblock decl decl ... decl j2sletblock) */
;* 	 (let loop ((lnodes nodes)                                     */
;* 		    (vdecls '()))                                      */
;* 	    (cond                                                      */
;* 	       ((null? lnodes)                                         */
;* 		this)                                                  */
;* 	       ((isa? (car lnodes) J2SDecl)                            */
;* 		(loop (cdr lnodes) (cons (car lnodes) vdecls)))        */
;* 	       ((isa? (car lnodes) J2SLetBlock)                        */
;* 		(when (pair? vdecls)                                   */
;* 		   ;; merge the decls into the j2sletblock             */
;* 		   (with-access::J2SLetBlock (car lnodes) (decls)      */
;* 		      (for-each (lambda (decl)                         */
;* 				   (when (isa? decl J2SDeclFun)        */
;* 				      (with-access::J2SDeclFun decl (scope) */
;* 					 (set! scope 'letblock))))     */
;* 			 vdecls)                                       */
;* 		      (set! decls (append vdecls decls))))             */
;* 		(with-access::J2SLetBlock (car lnodes) (nodes)         */
;* 		   (set! nodes (append nodes (cdr lnodes))))           */
;* 		(car lnodes))                                          */
;* 	       (else                                                   */
;* 		this)))))                                              */
   
   (with-access::J2SFun this (params body idthis vararg generator)
      (let ((body (merge-blocks body))
	    (ellipsis (if (eq? vararg 'rest) "... " "")))
	 (cond
	    ((eq? idthis '%)
	     ;; an arrow function
	     (cons* this "("
		(append
		   (j2s-js-ellipsis this "(" ") => " "," ellipsis
		      params tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf)
		   '(")"))))
	    ((not generator)
	     ;; a regular function
	     (cons* this (format "function ~a" funid)
		(append
		   (j2s-js-ellipsis this "(" ")" "," ellipsis
		      params tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf)
		   '("\n"))))
	    ((not (eq? (config-get conf :target) 'es5))
	     ;; a generator, for es6
	     (cons* this (format "function *~a" funid)
		(append
		   (j2s-js-ellipsis this "(" ")" "," ellipsis
		      params tildec dollarc mode evalp conf)
		   (j2s-js body tildec dollarc mode evalp conf)
		   '("\n"))))
	    (else
	     ;; a generator, for es5
	     (let ((predecls (collect-generator-predecls! body)))
		(cons* this (format "function ~a" funid)
		   (append
		      (j2s-js-ellipsis this "(" ")" "," ellipsis
			 params tildec dollarc mode evalp conf)
		      '("{")
		      (append-map! (lambda (decl)
				      (j2s-js decl tildec dollarc mode evalp conf))
			 predecls)
		      '("var $GEN=new hop.Generator(function(_$v,_$e)")
		      (j2s-js body tildec dollarc mode evalp conf)
		      '(");return $GEN;}\n")))))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFun tildec dollarc mode evalp conf)

   (define (fun-id this)
      (with-access::J2SFun this (decl)
	 (if (isa? decl J2SDecl)
	     (j2s-js-id decl)
	     "")))

   (j2s-js-fun this tildec dollarc mode evalp conf (fun-id this)))

;*---------------------------------------------------------------------*/
;*    collect-generator-predecls! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function collects from BODY, the list of declaration        */
;*    that can be executed before the generator.                       */
;*---------------------------------------------------------------------*/
(define (collect-generator-predecls! body)
   
   (define (predecl? this)
      (with-access::J2SDeclInit this (val binder)
	 ;; see j2s-js ::J2SDeclInit
	 (not (or (j2s-let-opt? this) (not (isa? val J2SUndefined))))))

   (define (loop* node predecls)
      (with-access::J2SBlock node (nodes)
	 (let liip ((n nodes)
		    (predecls predecls))
	    (cond
	       ((null? n)
		(values #t predecls))
	       ((and (isa? (car n) J2SDeclInit) (predecl? (car n)))
		(let ((decl (car n))
		      (next (cdr n)))
		   (set! nodes next)
		   (liip nodes (cons decl predecls))))
	       ((isa? (car n) J2SBlock)
		(multiple-value-bind (complete predecls)
		   (loop (car n) predecls)
		   (if complete
		       (liip (cdr nodes) predecls)
		       (values #f predecls))))
	       (else
		(values #f predecls))))))

   (define (loop body predecls)
      (let liip ((body body)
		 (predecls predecls))
	 (cond
	    ((isa? body J2SLetBlock)
	     (with-access::J2SLetBlock body (decls nodes)
		(let liip ((d decls)
			   (predecls predecls))
		   (cond
		      ((null? d)
		       (loop* body predecls))
		      ((not (isa? (car d) J2SDeclInit))
		       (values #f predecls))
		      ((predecl? (car d))
		       (let ((decl (car d))
			     (next (cdr d)))
			  (set! decls next)
			  (liip next (cons decl predecls))))
		      (else
		       (values #f predecls))))))
	    ((isa? body J2SBlock)
	     (loop* body predecls))
	    (else
	     (values #f predecls)))))

   (multiple-value-bind (complete predecls)
      (loop body '())
      predecls))

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
	    '(") ")
	    (if (isa? body J2SNop)
		'(";")
		(j2s-js body tildec dollarc mode evalp conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDo tildec dollarc mode evalp conf)
   (with-access::J2SWhile this (test body)
      (cons* this "do "
	 (append
	    (if (isa? body J2SNop)
		'("{}")
		(j2s-js body tildec dollarc mode evalp conf))
	    '("while (") (j2s-js test tildec dollarc mode evalp conf)
	    '(")")))))

;*---------------------------------------------------------------------*/
;*    join ...                                                         */
;*---------------------------------------------------------------------*/
(define (join sep el)
   (if (null? el)
       '()
       (let loop ((el el))
	  (if (null? (cdr el))
	      (car el)
	      (append (car el) (list sep) (loop (cdr el)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFor tildec dollarc mode evalp conf)
   
   (define (var-decl decl::J2SDecl)
      (cons (j2s-js-id decl)
	 (if (isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val)
		(cons "=" (j2s-js val tildec dollarc mode evalp conf)))
	     '())))
   
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
	    (if (isa? body J2SNop)
		'(";")
		(j2s-js body tildec dollarc mode evalp conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SForIn tildec dollarc mode evalp conf)
   (with-access::J2SForIn this (lhs obj body)
      (cons* this "for("
         (append (if (isa? lhs J2SVarDecls)
		     (with-access::J2SVarDecls lhs (decls)
			(list "var " (j2s-js-id (car decls))))
		     (j2s-js lhs tildec dollarc mode evalp conf))
	    '(" in ")
	    (j2s-js obj tildec dollarc mode evalp conf)
	    '(") ")
	    (if (isa? body J2SNop)
		'(";")
		(j2s-js body tildec dollarc mode evalp conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclFun tildec dollarc mode evalp conf)
   (with-access::J2SDeclInit this (id val)
      (j2s-js-fun val tildec dollarc mode evalp conf id)))
                                             
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
	       (append
		  (if (isa? then J2SNop)
		      '("{}") (j2s-js then tildec dollarc mode evalp conf))
		  (if (isa? else J2SNop)
		      '()
		      (cons " else "
			 (j2s-js else tildec dollarc mode evalp conf)))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSequence tildec dollarc mode evalp conf)
   (with-access::J2SSequence this (exprs)
      (j2s-js* this "" "" "," exprs tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRef tildec dollarc mode evalp conf)
   (with-access::J2SRef this (decl)
      (list this (j2s-js-id decl))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnresolvedRef ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnresolvedRef tildec dollarc mode evalp conf)
   (with-access::J2SUnresolvedRef this (id)
      (list this (symbol->string! id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SHopRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SHopRef tildec dollarc mode evalp conf)
   ;; MS 1 jul 2014: not quit sure, what about client-side scheme modules?
   (with-access::J2SHopRef this (id)
      (list this (symbol->string! id))))

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
      
      (let ((names (map j2s-js-id decls)))
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
      (let ((f (j2s-js fun tildec dollarc mode evalp conf)))
	 (cons this
	    (append (if (or (isa? fun J2SRef)
			    (isa? fun J2SAccess)
			    (isa? fun J2SUnresolvedRef))
			f
			(cons "(" (append f '(")"))))
	       (j2s-js* this "(" ")" "," args tildec dollarc mode evalp conf))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccess ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccess tildec dollarc mode evalp conf)

   (define (id-string? field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (pregexp-match "^[a-zA-Z0-9_$]+$" val))))
      
   (with-access::J2SAccess this (obj field)
      (if (id-string? field)
	  (cons this
	     (append (j2s-js obj tildec dollarc mode evalp conf)
		(list "." (with-access::J2SString field (val) val))))
	  (cons this
	     (append (j2s-js obj tildec dollarc mode evalp conf)
		'("[")
		(j2s-js field tildec dollarc mode evalp conf)
		'("]"))))))

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
      (if (memq op '(void typeof delete))
	  (cons* this (symbol->string op) " "
	     (j2s-js expr tildec dollarc mode evalp conf))
	  (cons* this (symbol->string op) 
	     (j2s-js expr tildec dollarc mode evalp conf)))))

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
   ;;(tprint "tildec=" tildec " " (j2s->list this))
   (cons this
      (cond
	 ((procedure? tildec)
	  (tildec this tildec dollarc mode evalp conf))
	 ((eq? tildec #t)
	  (j2s-js-script-tilde this tildec dollarc mode evalp conf))
	 (else
	  (with-access::J2STilde this (loc stmt)
	     (cons* this "<script>"
		(append (j2s-js stmt
			   j2s-js-script-tilde
			   dollarc
			   mode evalp conf)
		   '("</script>"))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js-script-tilde ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-js-script-tilde this::J2STilde tildec dollarc mode evalp conf)
   (with-access::J2STilde this (loc stmt)
      (let ((ndollarc (j2s-js-client-dollar dollarc)))
	 (cons* this "SCRIPT( undefined, '"
	    (string-for-read
	       (call-with-output-string
		  (lambda (port)
		     (for-each (lambda (n)
				  (unless (isa? n J2SNode)
				     (display n port)))
			(j2s-js stmt tildec ndollarc mode evalp conf)))))
	    '("')")))))

;*---------------------------------------------------------------------*/
;*    j2s-js-attribute-tilde ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-js-attribute-tilde this::J2STilde tildec dollarc mode evalp conf)
   (with-access::J2STilde this (loc stmt)
      (let* ((temp (gensym))
	     (assign (j2s-stmt-assign stmt temp))
	     (id (gensym "$"))
	     (env (cons 0 '()))
	     (ndollarc (j2s-js-client-dollar-env dollarc id env))
	     (body (j2s-js assign tildec ndollarc mode evalp conf)))
	 (cons* this (format "new hop_xml_tilde( function( event, ~a ) { var " id)
	    (symbol->string! temp) "; "
	    (append body
	       (list (format "\nreturn ~a}, [" temp))
	       (join "," (cdr env))
	       '("])"))))))

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
		(obj->javascript-attr ,(j2s-scheme node mode evalp conf '()) op))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js-client-dollar ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-js-client-dollar odollarc)
   (lambda (this::J2SDollar tildec dollarc mode evalp conf)
      (with-access::J2SDollar this (node)
	 ;;(tprint "DOLLAR=" (j2s->list node))
	 (j2s-js node tildec odollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js-client-dollar-env ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-js-client-dollar-env odollarc id env)
   (lambda (this::J2SDollar tildec dollarc mode evalp conf)
      (with-access::J2SDollar this (node)
	 ;;(tprint "DOLLAR=" (j2s->list node))
	 (let ((expr (j2s-js node tildec odollarc mode evalp conf)))
	    (let ((count (car env)))
	       (set-car! env (+fx count 1))
	       (set-cdr! env (cons expr (cdr env)))
	       (list this (format "~a[~a]" id count)))))))

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
	    (list (j2s-op op) "=")
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

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPragma tildec dollarc mode evalp conf)
   (with-access::J2SPragma this (expr lang)
      (if (eq? lang 'javascript)
	  (list this expr)
	  (list this "undefined"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SYield ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SYield tildec dollarc mode evalp conf)
   (with-access::J2SYield this (expr kont generator)
      (cons* this (if generator "yield* " "yield ")
	 (j2s-js expr tildec dollarc mode evalp conf))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SKont ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SKont tildec dollarc mode evalp conf)
   (with-access::J2SKont this (param exn body)
      (cons* this "function( "
	 (append (j2s-js param tildec dollarc mode evalp conf)
	    '(", ")
	    (j2s-js exn tildec dollarc mode evalp conf)
	    '(") {")
	    (j2s-js body tildec dollarc mode evalp conf)
	    '("}")))))
