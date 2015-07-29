;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/parser.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep  8 07:38:28 2013                          */
;*    Last change :  Wed Jul 29 19:47:12 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript parser                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_parser

   (include "token.sch")

   (import __js2scheme_lexer
	   __js2scheme_html
	   __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils)

   (export (j2s-parser ::input-port ::pair-nil)
	   (j2s-tag->expr ::pair)))

;*---------------------------------------------------------------------*/
;*    j2s-parser ...                                                   */
;*    -------------------------------------------------------------    */
;*    JavaScript parser                                                */
;*---------------------------------------------------------------------*/
(define (j2s-parser input-port conf::pair-nil)

   (define tilde-level 0)

   (define (with-tilde proc)
      (set! tilde-level (+fx tilde-level 1))
      (let ((res (proc)))
	 (set! tilde-level (-fx tilde-level 1))
	 res))
	    
   (define (with-dollar proc)
      (set! tilde-level (-fx tilde-level 1))
      (let ((res (proc)))
	 (set! tilde-level (+fx tilde-level 1))
	 res))
	    
   (define (current-loc)
      `(at ,(input-port-name input-port) ,(input-port-position input-port)))

   (define (parse-token-error msg token::pair)
      (match-case (token-loc token)
	 ((at ?fname ?loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "j2s-parser")
		(msg (if (eq? (token-tag token) 'BAD) (cadr token) msg))
		(obj (if (eq? (token-tag token) 'BAD) (cddr token) (token-value token)))
		(fname fname)
		(location loc))))
	 (else
	  (raise
	     (instantiate::&io-parse-error
		(proc "j2s-parser")
		(msg (if (eq? (token-tag token) 'BAD) (cadr token) msg))
		(obj (if (eq? (token-tag token) 'BAD) (cddr token) (token-value token))))))))

   (define (parse-node-error msg node::J2SNode)
      (with-access::J2SNode node (loc)
	 (let ((l (read-line input-port)))
	    (match-case loc
	       ((at ?fname ?loc)
		(raise
		   (instantiate::&io-parse-error
		      (proc "j2s-parser")
		      (msg msg)
		      (obj (j2s->list node))
		      (fname fname)
		      (location loc))))
	       (else
		(raise
		   (instantiate::&io-parse-error
		      (proc "j2s-parser")
		      (msg msg)
		      (obj (j2s->list node)))))))))

   (define (parse-error msg obj)
      (let ((fname (input-port-name input-port))
	    (loc (input-port-position input-port)))
	 (raise
	    (instantiate::&io-parse-error
	       (proc "j2s-parser")
	       (msg msg)
	       (obj (read-line input-port))
	       (fname fname)
	       (location loc)))))
   
   (define *peeked-tokens* '())
   (define *previous-token-type* #unspecified)
   (define *open-tokens* #unspecified)
   
   (define (peek-token)
      (if (null? *peeked-tokens*)
	  (begin
	     (set! *peeked-tokens* (list (read/rp (j2s-lexer) input-port)))
	     (if (eq? (caar *peeked-tokens*) 'NEWLINE)
		 (begin
		    (set! *previous-token-type* 'NEWLINE)
		    (set! *peeked-tokens* (cdr *peeked-tokens*))
		    (peek-token))
		 (car *peeked-tokens*)))
	  (car *peeked-tokens*)))
   
   (define (token-push-back! token)
      (if (at-new-line-token?)
	  (set! *peeked-tokens*
	     (cons* token (econs 'NEWLINE 'NEWLINE (token-loc token))
		*peeked-tokens*))
	  (set! *peeked-tokens*
	     (cons token *peeked-tokens*))))
   
   (define (peek-token-type)
      (car (peek-token)))
   
   (define (peek-token-value)
      (cdr (peek-token)))
   
   (define (at-new-line-token?)
      (eq? *previous-token-type* 'NEWLINE))

   (define (push-open-token token)
      (set! *open-tokens* (cons token *open-tokens*))
      token)

   (define (pop-open-token)
      (set! *open-tokens* (cdr *open-tokens*)))

   (define (consume-token! type)
      (let ((token (consume-any!)))
	 (if (eq? (token-tag token) type)
	     token
	     (parse-token-error 
		(format "expected \"~a\" got \"~a\"" type
		   (token-tag token))
		token))))
   
   (define (consume! type)
      (cdr (consume-token! type)))
   
   (define (consume-any!)
      (let ((res (peek-token)))
	 (set! *previous-token-type* (car res))
	 (set! *peeked-tokens* (cdr *peeked-tokens*))
	 ;; (peek-token) ;; prepare new token.
	 res))
   
   (define (consume-statement-semicolon! where)
      (cond
	 ((eq? (peek-token-type) 'SEMICOLON)
	  (consume-any!))
	 ((eq? (peek-token-type) 'NEWLINE)
	  (consume-any!))
	 ((or (eq? (peek-token-type) 'RBRACE)
	      (at-new-line-token?)
	      (eq? (peek-token-type) 'EOF))
	  (peek-token))
	 (else
	  (parse-token-error (format "~a, \"\;\" or newline expected" where)
	     (peek-token)))))
   
   (define (eof?)
      (eq? (peek-token-type) 'EOF))
   
   (define (read-regexp intro-token)
      (when (eq? intro-token '/=)
	 (unread-char! #\= input-port))
      (let ((token (read/rp (j2s-regex-lexer) input-port)))
	 (case (token-tag token)
	    ((EOF)
	     (parse-token-error "Unfinished regular expression literal" token))
	    ((ERROR)
	     (parse-error "Bad regular-expression literal" token))
	    (else
	     token))))
   
   (define (source-elements::J2SBlock)
      (let loop ((rev-ses '()))
	 (if (eof?)
	     (if (null? rev-ses)
		 (instantiate::J2SBlock
		    (loc `(at ,(input-port-name input-port) 0))
		    (endloc `(at ,(input-port-name input-port) 0))
		    (nodes '()))
		 (let* ((fnode (car rev-ses))
			(nodes (reverse! rev-ses)))
		    (with-access::J2SNode (car nodes) (loc)
		       (with-access::J2SNode fnode ((endloc loc))
			  (instantiate::J2SBlock
			     (loc loc)
			     (endloc endloc)
			     (nodes nodes))))))
	     (loop (cons (source-element) rev-ses)))))
   
   (define (source-element)
      (case (peek-token-type)
	 ((function)
	  (function-declaration))
	 ((service)
	  (service-declaration))
	 ((RESERVED)
	  (if (eq? (peek-token-value) 'import)
	      (begin
		 (consume-any!)
		 (service-import))
	      (statement)))
	 ((EOF)
	  (parse-token-error "unexpected end of file"
	     (if (pair? *open-tokens*) (car *open-tokens*) (consume-any!))))
	 ((ERROR)
	  (parse-token-error "error" (consume-any!)))
	 (else
	  (statement))))

   (define (absolute-file-name? path)
      (and (>fx (string-length path) 0)
	   (char=? (string-ref path 0) (file-separator))))
   
   (define (repl-element)
      (case (peek-token-type)
	 ((function) (function-declaration))
	 ((service) (service-declaration))
	 ((EOF) (cdr (consume-any!)))
	 ((ERROR) (parse-token-error "error" (consume-any!)))
	 (else (statement))))
   
   (define (statement)
      (case (peek-token-type)
	 ((LBRACE) (block))
	 ((var) (var-decl-list #f))
	 ((let) (let-decl-list #f))
	 ((const) (const-decl-list #f))
	 ((SEMICOLON) (empty-statement))
	 ((if) (iff))
	 ((for while do) (iteration))
	 ((continue) (continue))
	 ((break) (break))
	 ((return) (return))
	 ((with) (with))
	 ((switch) (switch))
	 ((throw) (throw))
	 ((try) (trie))
	 ((ID) (labeled-or-expr))
	 ;; ECMA 262 recommends implementations not to support
	 ;; function declarations in statements. See
	 ;;   http://www.ecma-international.org/ecma-262/5.1/#sec-12
	 ;; However, it looks like main implementation do. For compatibility
	 ;; we mimic this behavior.
	 ((function) (function-declaration))
	 ((debugger) (debugger-statement))
	 (else (expression-statement))))
   
   (define (block)
      (let ((token (push-open-token (consume-token! 'LBRACE))))
	 (let loop ((rev-stats '()))
	    (case (peek-token-type)
	       ((RBRACE)
		(let ((etoken (consume-any!)))
		   (pop-open-token)
		   (instantiate::J2SBlock
		      (loc (token-loc token))
		      (endloc (token-loc etoken))
		      (nodes (reverse! rev-stats)))))
	       (else
		(loop (cons (statement) rev-stats)))))))

   (define (decl-list token in-for-init? constrinit constr)
      (let loop ((rev-vars (list (var in-for-init? constrinit constr))))
	 (case (peek-token-type)
	    ((SEMICOLON)
	     (unless in-for-init? (consume-any!))
	     (instantiate::J2SVarDecls
		(loc (token-loc token))
		(decls (reverse! rev-vars))))
	    ((COMMA)
	     (consume-any!)
	     (loop (cons (var in-for-init? constrinit constr) rev-vars)))
	    ((in)
	     (cond
		((not in-for-init?)
		 (parse-token-error "illegal variable declaration"
		    (peek-token)))
		(else
		 (instantiate::J2SVarDecls
		    (loc (token-loc token))
		    (decls rev-vars)))))
	    (else
	     (if (and (not in-for-init?)
		      (or (at-new-line-token?)
			  (eq? (peek-token-type) 'RBRACE)
			  (eq? (peek-token-type) 'EOF)))
		 (instantiate::J2SVarDecls
		    (loc (token-loc token))
		    (decls (reverse! rev-vars)))
		 (parse-token-error "illegal variable declaration"
		    (consume-any!)))))))
   
   (define (var-decl-list in-for-init?)
      (decl-list (consume-token! 'var) in-for-init?
	 (lambda (loc id val)
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id id)
	       (val val)))
	 (lambda (loc id)
	    (instantiate::J2SDecl
	       (loc loc)
	       (id id)))))
   
   (define (let-decl-list in-for-init?)
      ;; ES6 let block
      (decl-list (consume-token! 'let) in-for-init?
	 (lambda (loc id val)
	    (instantiate::J2SLetInit
	       (loc loc)
	       (id id)
	       (val val)))
	 (lambda (loc id)
	    (instantiate::J2SLetInit
	       (loc loc)
	       (id id)
	       (val (instantiate::J2SUndefined (loc loc)))))))

   (define (const-decl-list in-for-init?)
      ;; ES6 const block
      (decl-list (consume-token! 'const) in-for-init?
	 (lambda (loc id val)
	    (instantiate::J2SLetInit
	       (const #t)
	       (loc loc)
	       (id id)
	       (val val)))
	 (lambda (loc id)
	    (parse-token-error "const-block" (consume-any!)))))
   
   (define (var in-for-init? constrinit constr)
      (let ((id (consume-any!)))
	 (case (car id)
	    ((ID)
	     (case (peek-token-type)
		((=)
		 (let* ((token (consume-any!))
			(expr (assig-expr in-for-init?)))
		    (constrinit (token-loc token) (cdr id) expr)))
		(else
		 (constr (token-loc id) (cdr id)))))
	    ((undefined NaN Infinity)
	     (case (peek-token-type)
		((=)
		 (consume-any!)
		 (assig-expr in-for-init?))
		(else
		 (parse-token-error "Illegal variable declaration" id))))
	    (else
	     (parse-token-error "Illegal lhs" id)))))
   
   (define (empty-statement)
      (instantiate::J2SNop
	 (loc (token-loc (consume-token! 'SEMICOLON)))))
   
   (define (iff)
      (let ((tif (consume-any!)))
	 (push-open-token (consume! 'LPAREN))
	 (let ((test (expression #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((then (statement)))
	       (case (peek-token-type)
		  ((else)
		   (let* ((tok (consume-any!))
			  (otherwise (statement)))
		      (instantiate::J2SIf
			 (loc (token-loc tif))
			 (test test)
			 (then then)
			 (else otherwise))))
		  (else
		   (instantiate::J2SIf
		      (loc (token-loc tif))
		      (test test)
		      (then then)
		      (else (instantiate::J2SNop
			       (loc (token-loc tif)))))))))))
   
   (define (iteration)
      (case (peek-token-type)
	 ((for) (for))
	 ((while) (while))
	 ((do) (do-while))))
   
   (define (for)
      
      (define (init-first-part)
	 (case (peek-token-type)
	    ((var) (var-decl-list #t))
	    ((SEMICOLON) #f)
	    (else (expression #t))))
      
      (let ((loc (token-loc (consume-token! 'for))))
	 (push-open-token (consume! 'LPAREN))
	 (let ((first-part (init-first-part)))
	    (case (peek-token-type)
	       ((SEMICOLON)
		(for-init/test/incr loc
		   (or first-part (instantiate::J2SNop (loc loc)))))
	       ((in)
		(for-in loc first-part ))))))
   
   ;; for (init; test; incr)
   (define (for-init/test/incr loc init::J2SNode)
      (consume! 'SEMICOLON)
      (let ((test (case (peek-token-type)
		     ((SEMICOLON) #f)
		     (else (expression #f)))))
	 (consume! 'SEMICOLON)
	 (let ((incr (case (peek-token-type)
			((RPAREN) #f)
			(else (expression #f)))))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let* ((body (statement)))
	       (instantiate::J2SFor
		  (loc loc)
		  (init (or init (instantiate::J2SNop (loc loc))))
		  (test (or test (instantiate::J2SBool (val #t) (loc loc))))
		  (incr (or incr (instantiate::J2SUndefined (loc loc))))
		  (body body))))))
   
   ;; for (lhs/var x in obj)
   (define (for-in loc lhs)
      ;; TODO: weed out bad lhs
      (consume! 'in)
      (let ((error-token (peek-token))
	    (obj (expression #f))
	    (ignore-RPAREN (consume! 'RPAREN))
	    (body (statement)))
	 (cond
	    ((isa? lhs J2SVarDecls)
	     (with-access::J2SVarDecls lhs (decls)
		(cond
		   ((null? lhs)
		    (parse-error "Illegal emtpy declaration"
		       error-token))
		   ((not (null? (cdr decls)))
		    (parse-error "Only one declaration allowed"
		       error-token)))))
	    ((not (isa? lhs J2SUnresolvedRef))
	     (parse-error "Variable reference or declaration required"
		error-token)))
	 (instantiate::J2SForIn
	    (loc loc)
	    (lhs lhs)
	    (obj obj)
	    (body body))))
   
   (define (while)
      (let ((token (consume-token! 'while)))
	 (push-open-token (consume! 'LPAREN))
	 (let ((test (expression #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((body (statement)))
	       (instantiate::J2SWhile
		  (loc (token-loc token))
		  (test test)
		  (body body))))))
   
   (define (do-while)
      (let* ((loc (token-loc (consume-token! 'do)))
	     (body (statement)))
	 (consume! 'while)
	 (push-open-token (consume! 'LPAREN))
	 (let ((test (expression #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (instantiate::J2SDo
	       (loc loc)
	       (test test)
	       (body body)))))
   
   (define (continue)
      (let ((loc (token-loc (consume-token! 'continue))))
	 (if (and (eq? (peek-token-type) 'ID)
		  (not (at-new-line-token?)))
	     (let ((id (consume! 'ID)))
		(consume-statement-semicolon! "continue")
		(instantiate::J2SContinue
		   (loc loc)
		   (id id)))
	     (begin
		(consume-statement-semicolon! "continue")
		(instantiate::J2SContinue
		   (loc loc)
		   (id #f))))))
   
   (define (break)
      (let ((loc (token-loc (consume-token! 'break))))
	 (if (and (eq? (peek-token-type) 'ID)
		  (not (at-new-line-token?)))
	     (let ((id (consume! 'ID)))
		(consume-statement-semicolon! "break")
		(instantiate::J2SBreak
		   (loc loc)
		   (id id)))
	     (begin
		(consume-statement-semicolon! "break")
		(instantiate::J2SBreak
		   (loc loc))))))
   
   (define (return)
      (let ((loc (token-loc (consume-token! 'return))))
	 (cond
	    ((or (case (peek-token-type)
		    ((EOF ERROR SEMICOLON) #t)
		    (else #f))
		 (at-new-line-token?))
	     (consume-statement-semicolon! "return")
	     (instantiate::J2SReturn
		(loc loc)
		(expr (instantiate::J2SUndefined
			 (loc loc)))))
	    ((eq? (peek-token-type) 'RBRACE)
	     ;; many javascript libs use the pattern "return}", so hopc
	     ;; accepts it.
	     (instantiate::J2SReturn
		(loc loc)
		(expr (instantiate::J2SUndefined
			 (loc loc)))))
	    (else
	     (let ((expr (expression #f)))
		(consume-statement-semicolon! "return")
		(instantiate::J2SReturn
		   (loc loc)
		   (expr expr)))))))
   
   (define (with)
      (let ((token (consume-token! 'with)))
	 (push-open-token (consume! 'LPAREN))
	 (let ((expr (expression #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((body (statement)))
	       (instantiate::J2SWith
		  (loc (token-loc token))
		  (obj expr)
		  (block body))))))
   
   (define (switch)
      (let ((token (consume-token! 'switch)))
	 (push-open-token (consume! 'LPAREN))
	 (let ((key (expression #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((cases (case-block)))
	       (instantiate::J2SSwitch
		  (loc (token-loc token))
		  (key key)
		  (cases cases))))))
   
   (define (case-block)
      (push-open-token (consume! 'LBRACE))
      (let loop ((rev-cases '())
		 (default-case-done? #f))
	 (case (peek-token-type)
	    ((RBRACE)
	     (pop-open-token)
	     (consume-any!)
	     (reverse! rev-cases))
	    ((case)
	     (loop (cons (case-clause) rev-cases) default-case-done?))
	    ((default)
	     (if default-case-done?
		 (error "Only one default-clause allowed"
		    (peek-token)
		    (peek-token))
		 (loop (cons (default-clause) rev-cases) #t))))))
   
   (define (case-clause)
      (let ((token (consume-token! 'case)))
	 (let ((expr (expression #f)))
	    (consume! ':)
	    (let ((body (switch-clause-statements (token-loc token))))
	       (instantiate::J2SCase
		  (loc (token-loc token))
		  (expr expr)
		  (body body))))))
   
   (define (default-clause)
      (let ((token (consume-token! 'default)))
	 (consume! ':)
	 (instantiate::J2SDefault
	    (loc (token-loc token))
	    (expr (class-nil J2SExpr))
	    (body (switch-clause-statements (token-loc token))))))
   
   (define (switch-clause-statements loc)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE EOF ERROR default case)
	     (instantiate::J2SBlock
		(loc loc)
		(endloc (token-loc (peek-token)))
		(nodes (reverse! rev-stats))))
	    (else
	     (loop (cons (statement) rev-stats))))))
   
   (define (throw)
      (let ((token (consume-token! 'throw)))
	 (when (or (at-new-line-token?) (eq? (peek-token-type) 'NEWLINE))
	    (parse-token-error "throw must have a value" (peek-token)))
	 (peek-token)
	 (when (or (at-new-line-token?) (eq? (peek-token-type) 'NEWLINE))
	    (parse-token-error "throw must have a value" (peek-token)))
	 (let ((expr (expression #f)))
	    (consume-statement-semicolon! "throw")
	    (instantiate::J2SThrow
	       (loc (token-loc token))
	       (expr expr)))))
   
   (define (trie)
      (let ((loc (token-loc (consume-token! 'try))))
	 (let ((body (block)))
	    (let ((catch-part #f)
		  (finally-part #f))
	       (when (eq? (peek-token-type) 'catch)
		  (set! catch-part (catch)))
	       (when (eq? (peek-token-type) 'finally)
		  (set! finally-part (finally)))
	       (instantiate::J2STry
		  (loc loc)
		  (body body)
		  (catch (or catch-part
			     (instantiate::J2SNop (loc loc))))
		  (finally (or finally-part
			       (instantiate::J2SNop (loc loc)))))))))
   
   (define (catch)
      (let ((loc (token-loc (consume-token! 'catch))))
	 (push-open-token (consume! 'LPAREN))
	 (let ((id (consume! 'ID)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((body (block)))
	       ;; not sure, if 'Param' is a really good choice.
	       ;; we'll see...
	       (instantiate::J2SCatch
		  (loc loc)
		  (param (instantiate::J2SParam
			    (loc loc)
			    (id id)))
		  (body body))))))
   
   (define (finally)
      (consume! 'finally)
      (block))
   
   (define (labeled-or-expr)
      (let* ((id-token (consume-token! 'ID))
	     (next-token-type (peek-token-type)))
	 (if (eq? next-token-type  ':)
	     (begin
		(consume-any!)
		(instantiate::J2SLabel
		   (loc (token-loc id-token))
		   (id (cdr id-token))
		   (body (statement))))
	     (begin
		(token-push-back! id-token)
		(expression-statement)))))

   (define (debugger-statement)
      (let ((token (consume-token! 'debugger)))
	 (instantiate::J2SNop
	    (loc (token-loc token)))))
   
   (define (expression-statement)
      (let ((expr (expression #f)))
	 (consume-statement-semicolon! "expression")
	 (with-access::J2SExpr expr (loc)
	    (instantiate::J2SStmtExpr
	       (loc loc)
	       (expr expr)))))
   
   (define (function-declaration)
      (function #t))
   
   (define (function-expression)
      (function #f))
   
   (define (service-declaration)
      (service #t))
   
   (define (service-expression)
      (service #f))

   (define (arrow-params params)
      (map (lambda (t)
	      (instantiate::J2SParam
		 (loc (token-loc t))
		 (id (cdr t))))
	 params))

   (define (arrow-body)
      (if (eq? (peek-token-type) 'LBRACE)
	  ;; a statement
	  (fun-body)
	  ;; an expression
	  (let ((expr (expression #f)))
	     (with-access::J2SNode expr (loc)
		(instantiate::J2SBlock
		   (loc loc)
		   (endloc loc)
		   (nodes (list (instantiate::J2SReturn
				   (loc loc)
				   (expr expr)))))))))
   
   (define (arrow-function params)
      ;; ES6 arrow functions
      (let ((=> (consume-any!)))
	 (instantiate::J2SArrow
	    (idthis '%)
	    (loc (token-loc =>))
	    (name '||)
	    (mode 'strict)
	    (params (arrow-params params))
	    (body (arrow-body)))))
	    
   (define (function declaration?)
      (let* ((token (consume-token! 'function))
	     (id (when (or declaration? (eq? (peek-token-type) 'ID))
		    (consume-token! 'ID)))
	     (params (params))
	     (body (fun-body))
	     (mode (or (javascript-mode body) 'normal)))
	 (cond
	    (declaration?
	     (instantiate::J2SDeclFun
		(loc (token-loc token))
		(id (cdr id))
		(val (instantiate::J2SFun
			(loc (token-loc token))
			(params params)
			(name (cdr id))
			(mode mode)
			(body body)
			(decl (instantiate::J2SDecl
				 (loc (token-loc token))
				 (id (cdr id))
				 (name (cdr id))
				 (writable #f)
				 (ronly #t)
				 (global #t)))))))
	    (id
	     (co-instantiate ((fun (instantiate::J2SFun
				      (loc (token-loc token))
				      (decl decl)
				      (params params)
				      (name (cdr id))
				      (mode mode)
				      (body body)))
			      (decl (instantiate::J2SDeclCnstFun
				       (loc (token-loc id))
				       (id (cdr id))
				       (name (cdr id))
				       (writable #f)
				       (ronly #t)
				       (global #t)
				       (fun fun))))
		fun))
	    (else
	     (instantiate::J2SFun
		(loc (token-loc token))
		(params params)
		(name '||)
		(mode mode)
		(body body))))))

   (define (init->params init)
      (if (isa? init J2SPropertyInit)
	  (with-access::J2SPropertyInit init (loc name)
	     (if (isa? name J2SString)
		 (with-access::J2SString name (val)
		    (instantiate::J2SParam
		       (loc loc)
		       (id (string->symbol val))))
		 (parse-node-error "Illegal parameter declaration" init)))
	  (parse-node-error "Illegal parameter declaration" init)))

   (define (service declaration?)
      (let* ((token (consume-token! 'service))
	     (id (when (or declaration? (eq? (peek-token-type) 'ID))
		    (consume-token! 'ID)))
	     (inits (service-params))
	     (params (if (isa? inits J2SObjInit)
			 (with-access::J2SObjInit inits (inits)
			    (map init->params inits))
			 inits))
	     (init (if (isa? inits J2SObjInit)
		       inits
		       (instantiate::J2SNop
			  (loc (token-loc token)))))
	     (body (fun-body)))
	 (cond
	    (declaration?
	     (instantiate::J2SDeclSvc
		(loc (token-loc token))
		(id (cdr id))
		(val (instantiate::J2SSvc
			(loc (token-loc token))
			(params params)
			(name (cdr id))
			(init init)
			(mode 'strict)
			(body body)
			(decl (instantiate::J2SDecl
				 (loc (token-loc token))
				 (id (cdr id))
				 (name (cdr id))
				 (writable #f)
				 (ronly #t)
				 (global #t)))))))
	    (id
	     (co-instantiate ((fun (instantiate::J2SSvc
				      (loc (token-loc id))
				      (decl decl)
				      (params params)
				      (name (cdr id))
				      (init init)
				      (mode 'strict)
				      (body body)))
			      (decl (instantiate::J2SDeclCnstFun
				       (loc (token-loc id))
				       (id (cdr id))
				       (name (cdr id))
				       (writable #f)
				       (ronly #t)
				       (global #t)
				       (fun fun))))
		fun))
	    (else
	     (instantiate::J2SSvc
		(loc (token-loc token))
		(params params)
		(name (gensym))
		(init init)
		(mode 'strict)
		(body body))))))

   (define (service-import)
      (let* ((token (consume-token! 'service))
	     (loc (token-loc token))
	     (id (consume-token! 'ID))
	     (inits (service-params)))
	 (unless (null? inits)
	    (parse-node-error "Imported service must not declare parameters"
	       (car inits)))
	 (instantiate::J2SDeclSvc
	    (loc loc)
	    (id (cdr id))
	    (val (instantiate::J2SSvc
		    (decl (instantiate::J2SDecl
			     (loc loc)
			     (id (cdr id))))
		    (loc loc)
		    (params inits)
		    (name (cdr id))
		    (init (instantiate::J2SNop
			     (loc (token-loc token))))
		    (register #f)
		    (mode 'strict)
		    (body (instantiate::J2SBlock
			     (loc loc)
			     (endloc loc)
			     (nodes (list
				       (instantiate::J2SPragma
					  (loc loc)
					  (expr "(current-request)")))))))))))

   (define (consume-param!)
      (let ((token (consume-token! 'ID)))
	 (instantiate::J2SParam
	    (loc (token-loc token))
	    (id (token-value token)))))
      
   (define (params)
      (push-open-token (consume! 'LPAREN))
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     (pop-open-token)
	     '())
	  (let loop ((rev-params (list (consume-param!))))
	     (if (eq? (peek-token-type) 'COMMA)
		 (begin
		    (consume-any!)
		    (loop (cons (consume-param!) rev-params)))
		 (begin
		    (consume! 'RPAREN)
		    (pop-open-token)
		    (reverse! rev-params))))))

   (define (service-params)
      (push-open-token (consume! 'LPAREN))
      (case (peek-token-type)
	 ((RPAREN)
	  (consume-any!)
	  '())
	 ((LBRACE)
	  (let ((o (object-literal)))
	     (consume! 'RPAREN)
	     (pop-open-token)
	     o))
	 (else
	  (let loop ((rev-params (list (consume-param!))))
	     (if (eq? (peek-token-type) 'COMMA)
		 (begin
		    (consume-any!)
		    (loop (cons (consume-param!) rev-params)))
		 (begin
		    (consume! 'RPAREN)
		    (pop-open-token)
		    (reverse! rev-params)))))))

   (define (fun-body)
      (let ((token (push-open-token (consume-token! 'LBRACE))))
	 (let ((loc (current-loc)))
	    (let loop ((rev-ses '()))
	       (if (eq? (peek-token-type) 'RBRACE)
		   (let ((etoken (consume-any!)))
		      (pop-open-token)
		      (instantiate::J2SBlock
			 (loc (token-loc token))
			 (endloc (token-loc etoken))
			 (nodes (reverse! rev-ses))))
		   (loop (cons (source-element) rev-ses)))))))
   
   (define (expression::J2SExpr in-for-init?)
      (let ((assig (assig-expr in-for-init?)))
	 (let loop ((rev-exprs (list assig)))
	    (if (eq? (peek-token-type) 'COMMA)
		(begin
		   (consume-any!)
		   (loop (cons (assig-expr in-for-init?) rev-exprs)))
		(if (null? (cdr rev-exprs))
		    (car rev-exprs)
		    (let ((exprs (reverse! rev-exprs)))
		       (with-access::J2SNode (car exprs) (loc)
			  (instantiate::J2SSequence
			     (loc loc)
			     (exprs exprs)))))))))
   
   (define (assig-operator? x)
      (case x
	 ((= *= /= %= += -= <<= >>= >>>= &= ^= BIT_OR=)
	  #t)
	 (else #f)))
   
   (define (assig-expr in-for-init?)
      
      (define (with-out-= op=)
	 (let* ((s= (symbol->string op=))
		(s=-length (string-length s=))
		(s (substring s= 0 (- s=-length 1)))
		(op (string->symbol s)))
	    op))
      
      (let ((expr (cond-expr in-for-init?)))
	 (if (assig-operator? (peek-token-type))
	     (let* ((op (consume-any!))
		    (rhs (assig-expr in-for-init?)))
		(cond
		   ((and (eq? op '=) (isa? expr J2SRef))
		    (instantiate::J2SAssig
		       (loc (token-loc op))
		       (lhs expr)
		       (rhs rhs)))
		   ((and (eq? op '=) (isa? expr J2SAccess))
		    `(instantiate::J2SAssig
		       (lhs ,expr)
		       (rhs ,rhs)))
		   ((eq? (car op) '=)
		    (instantiate::J2SAssig
		       (loc (token-loc op))
		       (lhs expr)
		       (rhs rhs)))
		   (else
		    (instantiate::J2SAssigOp
		       (loc (token-loc op))
		       (lhs expr)
		       (op (with-out-= (car op)))
		       (rhs rhs)))))
	     expr)))
   
   (define (cond-expr in-for-init?)
      (let ((expr (binary-expr in-for-init?))
	    (token (peek-token)))
	 (if (eq? (token-tag token) '?)
	     (let* ((ignore-? (consume-any!))
		    (then (assig-expr #f))
		    (ignore-colon (consume! ':))
		    (else (assig-expr in-for-init?)))
		(instantiate::J2SCond
		   (loc (token-loc token))
		   (test expr)
		   (then then)
		   (else else)))
	     expr)))
   
   (define (op-level op)
      (case op
	 ((OR) 1)
	 ((&&) 2)
	 ((BIT_OR) 3)
	 ((^) 4)
	 ((&) 5)
	 ((== != === !==) 6)
	 ((< > <= >= instanceof in) 7)
	 ((<< >> >>>) 8)
	 ((+ -) 9)
	 ((* / %) 10)
	 (else #f)))

   (define (binary-op-type op)
      (case op
	 ((< > <= >= instanceof in == ===) 'bool)
	 (else #f)))

   ;; left-associative binary expressions
   (define (binary-expr in-for-init?)
      (define (binary-aux level)
	 (if (> level 10)
	     (unary)
	     (let loop ((expr (binary-aux (+fx level 1))))
		(let* ((type (peek-token-type))
		       (new-level (op-level type)))
		   (cond
		      ((and in-for-init? (eq? type 'in))
		       expr)
		      ((not new-level)
		       expr)
		      ((=fx new-level level)
		       (let ((token (consume-any!)))
			  (when (eq? type 'OHTML)
			     ;; < operator
			     (let* ((val (token-value token))
				    (id (substring val 1)))
				(token-push-back!
				   (make-token 'ID id
				      (token-loc token)))
				(token-tag-set! token '<)))
			  (loop (instantiate::J2SBinary
				   (loc (token-loc token))
				   (lhs expr)
				   (op (token-tag token))
				   (type (binary-op-type (token-tag token)))
				   (rhs (binary-aux (+fx level 1)))))))
		      (else
		       expr))))))
      (binary-aux 1))
   
   (define (unary)
      (case (peek-token-type)
	 ((++ --)
	  (let* ((token (consume-any!))
		 (expr (unary)))
	     (if (or (isa? expr J2SUnresolvedRef)
		     (isa? expr J2SAccess)
		     (isa? expr J2SParen))
		 (instantiate::J2SPrefix
		       (loc (token-loc token))
		       (rhs (class-nil J2SExpr))
		       (lhs expr)
		       (op (token-tag token)))
		 (parse-token-error
		    "Invalid left-hand side expression in prefix operation"
		    token))))
	 ((delete)
	  (let ((token (consume-any!))
		(expr (unary)))
	     (cond
		((or (isa? expr J2SAccess) (isa? expr J2SUnresolvedRef))
		 (instantiate::J2SUnary
		    (op (token-tag token))
		    (loc (token-loc token))
		    (expr expr)))
		(else
		 (instantiate::J2SUnary
		    (op (token-tag token))
		    (loc (token-loc token))
		    (expr expr))))))
	 ((void typeof ~ ! + -)
	  (let ((token (consume-any!)))
	     (instantiate::J2SUnary
		(loc (token-loc token))
		(op (token-tag token))
		(expr (unary)))))
	 (else
	  (postfix (token-loc (peek-token))))))
   
   (define (postfix loc)
      (let ((expr (lhs loc)))
	 (if (not (at-new-line-token?))
	     (case (peek-token-type)
		((++ --)
		 (let ((token (consume-any!)))
		    (if (or (isa? expr J2SUnresolvedRef)
			    (isa? expr J2SAccess)
			    (isa? expr J2SParen))
			(instantiate::J2SPostfix
			   (loc (token-loc token))
			   (rhs (class-nil J2SExpr))
			   (lhs expr)
			   (op (token-tag token)))
			(parse-token-error
			   "Invalid left-hand side expression in postfix operation"
			   token))))
		(else
		 expr))
	     expr)))
   
   ;; we start by getting all news (new-expr)
   ;; the remaining access and calls are then caught by the access-or-call
   ;; invocation allowing call-parenthesis.
   ;;
   ;; the access-or-call in new-expr does not all any parenthesis to be
   ;; consumed as they would be part of the new-expr.
   (define (lhs loc)
      (access-or-call (new-expr loc) loc #t))
   
   (define (new-expr loc)
      (if (eq? (peek-token-type) 'new)
	  (let* ((ignore (consume-any!))
		 (clazz (new-expr (token-loc ignore)))
		 (args (if (eq? (peek-token-type) 'LPAREN)
			   (arguments)
			   '())))
	     (instantiate::J2SNew
		(loc (token-loc ignore))
		(clazz clazz)
		(args args)))
	  (access-or-call (primary) loc #f)))
   
   (define (access-or-call expr loc call-allowed?)
      (let loop ((expr expr))
	 (case (peek-token-type)
	    ((LBRACKET)
	     (let* ((ignore (push-open-token (consume-any!)))
		    (field (expression #f))
		    (ignore-too (consume! 'RBRACKET)))
		(pop-open-token)
		(loop (instantiate::J2SAccess
			 (loc (token-loc ignore))
			 (obj expr)
			 (field field)))))
	    ((NEWLINE)
	     (consume-any!)
	     (loop expr))
	    ((DOT)
	     (let* ((ignore (consume-any!))
		    (field (consume-any!))
		    (key (car field))
		    (field-str (format "~a" (cdr field))))
		(if (or (eq? key 'ID)
			(eq? key 'RESERVED)
			(j2s-reserved-id? key))
		    (loop (instantiate::J2SAccess
			     (loc (token-loc ignore))
			     (obj expr)
			     (field (instantiate::J2SString
				       (loc (token-loc field))
				       (val field-str)))))
		    (parse-token-error "Wrong property name" field))))
	    ((LPAREN)
	     (if call-allowed?
		 (loop (instantiate::J2SCall
			  (loc loc)
			  (fun expr)
			  (args (arguments))))
		 expr))
	    (else
	     expr))))
   
   (define (arguments)
      (push-open-token (consume! 'LPAREN))
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-args (list (assig-expr #f))))
	     (if (eq? (peek-token-type) 'RPAREN)
		 (begin
		    (consume-any!)
		    (pop-open-token)
		    (reverse! rev-args))
		 (let* ((ignore (consume! 'COMMA))
			(arg (assig-expr #f)))
		    (loop (cons arg rev-args)))))))

   
   
   (define (js-xml-expression tag)
      
      (define (xml-err msg token)
	 (parse-token-error msg token))

      (let ((token (push-open-token (consume-token! 'LBRACE))))
	 (let loop ((state 1)
		    (attributes '())
		    (nodes '()))
	    (case (peek-token-type)
	       ((ID STRING RESERVED)
		(let ((token (consume-any!)))
		   (cond
		      ((and (eq? (peek-token-type) ':) (=fx state 1))
		       ;; an attribute
		       (begin
			  (consume-any!)
			  (let* ((name (instantiate::J2SString
					  (loc (token-loc token))
					  (val (if (eq? (token-tag token) 'STRING)
						   (token-value token)
						   (symbol->string (token-value token))))))
				 (attr (instantiate::J2SDataPropertyInit
					  (loc (token-loc token))
					  (name name)
					  (val (assig-expr #f)))))
			     (loop (negfx state) (cons attr attributes) '()))))
		      ((>fx state 0)
		       (token-push-back! token)
		       (loop (negfx state) attributes
			  (cons (assig-expr #f) nodes)))
		      (else
		       (xml-err "Illegal xml expression, unexpected \",\""
			  (peek-token))))))
	       ((COMMA)
		(if (<fx state 0)
		    (begin
		       (consume-any!)
		       (loop (negfx state) attributes nodes))
		    (xml-err "Illegal xml expression, unexpected \",\""
		       (peek-token))))
	       ((RBRACE)
		(consume-any!)
		(pop-open-token)
		(let ((ctag (peek-token)))
		   (when (eq? (car ctag) 'CTAG)
		      ;; optional closing tag
		      (if (eq? (cdr ctag) (cdr tag))
			  (consume-any!)
			  (xml-err
			     (format "Illegal xml expression, tag \"~a\" found, \"~a\" expected"
				(cdr ctag) (cdr tag))
			     (peek-token))))
		   (let ((attrs (instantiate::J2SObjInit
				   (loc (token-loc token))
				   (inits (reverse! attributes)))))
		      (instantiate::J2SCall
			 (loc (token-loc tag))
			 (fun (j2s-tag->expr tag))
			 (args (cons attrs (reverse! nodes)))))))
	       (else
		(if (>fx state 0)
		    (let ((expr (assig-expr #f)))
		       (loop -2 attributes (cons expr nodes)))
		    (xml-err
		       "Illegal xml expression, attribute or expression expected"
		       (peek-token))))))))

   (define (html-expression tag)
      (html-parser input-port conf tag))

   (define (xml-expression tag)
      (if (lbrace-following? input-port)
	  (js-xml-expression tag)
	  (html-expression tag)))

   (define (tilde token)
      (with-tilde
	 (lambda ()
	    (let loop ((rev-stats '()))
	       (case (peek-token-type)
		  ((RBRACE)
		   (consume-any!)
		   (instantiate::J2SSeq
		      (loc (token-loc token))
		      (nodes (reverse! rev-stats))))
		  (else
		   (loop (cons (statement) rev-stats))))))))

   (define (tilde-expression)
      (let ((token (consume-any!)))
	 (instantiate::J2STilde
	    (loc (token-loc token))
	    (stmt (tilde token)))))

   (define (dollar-expression)
      (let* ((ignore (consume-any!))
	    (expr (expression #f))
	    (ignore-too (consume! 'RBRACE)))
	 expr))
   
   (define (primary)
      (case (peek-token-type)
	 ((PRAGMA)
	  (jspragma))
	 ((function)
	  (function-expression))
	 ((service)
	  (service-expression))
	 ((this)
	  (instantiate::J2SThis
	     (loc (token-loc (consume-any!)))))
	 ((ID RESERVED)
	  (let ((token (consume-any!)))
	     (if (eq? (peek-token-type) '=>)
		 (arrow-function (list token))
		 (instantiate::J2SUnresolvedRef
		    (loc (token-loc token))
		    (id (token-value token))))))
	 ((HOP)
	  (let ((token (consume-token! 'HOP)))
	     (instantiate::J2SHopRef
		(loc (token-loc token))
		(id (token-value token)))))
	 ((LPAREN)
	  (let ((token (push-open-token (consume-any!)))
		(expr (expression #f))
		(ignore-too (consume! 'RPAREN)))
	     (pop-open-token)
	     (if (eq? (peek-token-type) '=>)
		 (let ((ps (map (lambda (n)
				   (if (isa? n J2SUnresolvedRef)
				       (with-access::J2SUnresolvedRef n (id loc)
					  (econs 'ID id loc))
				       (parse-node-error "unexpected token" n)))
			      (with-access::J2SSequence expr (exprs)
				 exprs))))
		    (arrow-function ps))
		 (instantiate::J2SParen
		    (loc (token-loc token))
		    (expr expr)))))
	 ((LBRACKET)
	  (array-literal))
	 ((LBRACE)
	  (object-literal))
	 ((NaN)
	  (let ((token (consume-any!)))
	     (instantiate::J2SNumber
		(loc (token-loc token))
		(val +nan.0))))
	 ((Infinity)
	  (let ((token (consume-any!)))
	     (instantiate::J2SNumber
		(loc (token-loc token))
		(val +inf.0))))
	 ((null)
	  (let ((token (consume-any!)))
	     (instantiate::J2SNull
		(loc (token-loc token)))))
	 ((undefined)
	  (let ((token (consume-token! 'undefined)))
	     (instantiate::J2SUndefined
		(loc (token-loc token)))))
	 ((true false)
	  (let ((token (consume-any!)))
	     (instantiate::J2SBool
		(loc (token-loc token))
		(val (eq? (token-tag token) 'true)))))
	 ((NUMBER)
	  (let ((token (consume-token! 'NUMBER)))
	     (instantiate::J2SNumber
		(loc (token-loc token))
		(val (token-value token)))))
	 ((OCTALNUMBER)
	  (let ((token (consume-token! 'OCTALNUMBER)))
	     (instantiate::J2SOctalNumber
		(loc (token-loc token))
		(val (token-value token)))))
	 ((STRING)
	  (let ((token (consume-token! 'STRING)))
	     (instantiate::J2SString
		(escape '())
		(loc (token-loc token))
		(val (token-value token)))))
	 ((ESTRING)
	  (let ((token (consume-token! 'ESTRING)))
	     (instantiate::J2SString
		(escape '(escape))
		(loc (token-loc token))
		(val (token-value token)))))
	 ((OSTRING)
	  (let ((token (consume-token! 'OSTRING)))
	     (instantiate::J2SString
		(escape '(escape octal))
		(loc (token-loc token))
		(val (token-value token)))))
	 ((EOF)
	  (parse-token-error "unexpected end of file" (peek-token)))
	 ((/ /=)
	  (let ((pattern (read-regexp (peek-token-type))))
	     ;; consume-any *must* be after having read the reg-exp,
	     ;; so that the read-regexp works. Only then can we remove
	     ;; the peeked token.
	     (let ((token (consume-any!)))
		(instantiate::J2SRegExp
		   (loc (token-loc token))
		   (val (car (cdr pattern)))
		   (flags (cdr (cdr pattern)))))))
	 ((OTAG)
	  (xml-expression (consume-any!)))
	 ((HTML)
	  (let ((tag (consume-any!)))
	     (instantiate::J2SCall
		(loc (token-loc tag))
		(fun (j2s-tag->expr tag))
		(args '()))))
	 ((OHTML)
;* 	  (if (html-following? input-port)                             */
	      (html-expression (consume-any!)))
;* 	      (let* ((t (consume-any!))                                */
;* 		     (s (symbol->string (token-value t)))              */
;* 		     (tokid (make-token 'ID (string->symbol (substring s 1)) */
;* 			       (token-loc t)))                         */
;* 		     (tokop (make-token '< "<" (token-loc t))))        */
;* 		 (token-push-back! tokid)                              */
;* 		 (token-push-back! tokop)                              */
;* 		 )))                                                   */
	 ((TILDE)
	  (let ((token (consume-any!)))
	     (instantiate::J2STilde
		(loc (token-loc token))
		(stmt (tilde token)))))
	 ((DOLLAR)
	  (if (>fx tilde-level 0)
	      (with-dollar
		 (lambda ()
		    (let* ((ignore (consume-any!))
			   (expr (expression #f))
			   (ignore-too (consume! 'RBRACE)))
		       (instantiate::J2SDollar
			  (loc (token-loc ignore))
			  (node expr)))))
	      (parse-token-error
		 "Invalid ${ ... } statement"
		 (consume-any!))))
	 (else
	  (parse-token-error "unexpected token" (peek-token)))))
   
   (define (jspragma)
      (let* ((token (consume-token! 'PRAGMA))
	     (LPAREN (push-open-token (consume-token! 'LPAREN)))
	     (str (consume-any!))
	     (RPAREN (consume-token! 'RPAREN)))
	 (pop-open-token)
	 (if (memq (car str) '(STRING ESTRING OSTRING))
	     (call-with-input-string (cdr str)
		(lambda (ip)
		   (instantiate::J2SPragma
		      (loc (token-loc token))
		      (expr (read ip)))))
	     (parse-token-error "unexpected token" str))))
   
   (define (array-literal)
      (let ((token (push-open-token (consume-token! 'LBRACKET))))
	 (let loop ((rev-els '())
		    (length 0))
	    (case (peek-token-type)
	       ((RBRACKET)
		(consume-any!)
		(pop-open-token)
		(instantiate::J2SArray
		   (loc (token-loc token))
		   (exprs (reverse! rev-els))
		   (len length)))
	       ((COMMA)
		(let ((token (consume-any!)))
		   (loop (cons (instantiate::J2SArrayAbsent
				  (loc (token-loc token)))
			    rev-els)
		      (+fx length 1))))
	       ((for)
		(if (pair? rev-els)
		    (parse-token-error "unexpected token" (consume-any!))
		    (comprehension-literal (token-loc token))))
	       (else
		(let ((array-el (assig-expr #f)))
		   (case (peek-token-type)
		      ((COMMA)
		       (consume-any!)
		       (loop (cons array-el rev-els)
			  (+fx length 1)))
		      ((RBRACKET)
		       (consume! 'RBRACKET)
		       (instantiate::J2SArray
			  (loc (token-loc token))
			  (exprs (reverse! (cons array-el rev-els)))
			  (len (+fx length 1))))
		      (else
		       (parse-token-error "unexpected token"
			  (consume-any!))))))))))

   (define (comprehension-literal loc)
      ;; ECMAScript 7 array comprehension
      (let loop ((decls '())
		 (iterables '())
		 (test (instantiate::J2SBool (val #t) (loc loc))))
	 (case (peek-token-type)
	    ((for)
	     (consume-any!)
	     (multiple-value-bind (decl iterable)
		(comprehension-binding loc)
		(loop (cons decl decls) (cons iterable iterables) test)))
	    ((if)
	     (let ((tok (consume-any!)))
		(let ((ntest (comprehension-test)))
		   (loop decls iterables
		      (if (isa? test J2SBool)
			  ntest
			  (instantiate::J2SBinary
			     (loc (token-loc tok))
			     (lhs test)
			     (op '&&)
			     (type 'bool)
			     (rhs ntest)))))))
	    (else
	     (instantiate::J2SComprehension
		(loc loc)
		(decls (reverse! decls))
		(iterables (reverse! iterables))
		(test test)
		(expr (comprehension-expression)))))))

   (define (comprehension-binding loc)
      ;; parses the comprehension binder (<id> of <expression>)
      (push-open-token (consume-token! 'LPAREN))
      (let* ((id (consume-token! 'ID))
	     (of (consume-any!)))
	 (unless (and (eq? (car of) 'ID) (eq? (cdr of) 'of))
	    (parse-token-error
	       (format "expected \"of\" got \"~a\"" (cdr of)) of))
	 (let* ((iterable (expression #f))
		(loc (token-loc id))
		(decl (instantiate::J2SLetOpt
			 (id (cdr id))
			 (val (instantiate::J2SUndefined (loc loc)))
			 (loc loc))))
	    (consume-token! 'RPAREN)
	    (pop-open-token)
	    (values decl iterable))))

   (define (comprehension-expression::J2SExpr)
      (let ((expr (expression #f)))
	 (consume-token! 'RBRACKET)
	 expr))

   (define (comprehension-test::J2SExpr)
      (push-open-token (consume-token! 'LPAREN))
      (let ((test (expression #f)))
	 (consume-token! 'RPAREN)
	 (pop-open-token)
	 test))
   
   (define (object-literal)
      
      (define (property-name)
	 (case (peek-token-type)
	    ;; IDs are automatically transformed to strings.
	    ((ID RESERVED)
	     (let ((token (consume-any!)))
		(case (token-value token)
		   ((get set)
		    token)
		   (else
		    (instantiate::J2SString
		       (loc (token-loc token))
		       (val (symbol->string (token-value token))))))))
	    ((STRING)
	     (let ((token (consume-token! 'STRING)))
		(instantiate::J2SString
		   (escape '())
		   (loc (token-loc token))
		   (val (token-value token)))))
	    ((ESTRING)
	     (let ((token (consume-token! 'ESTRING)))
		(instantiate::J2SString
		   (escape '(escape))
		   (loc (token-loc token))
		   (val (token-value token)))))
	    ((OSTRING)
	     (let ((token (consume-token! 'OSTRING)))
		(instantiate::J2SString
		   (escape '(escape octal))
		   (loc (token-loc token))
		   (val (token-value token)))))
	    ((NUMBER)
	     (let ((token (consume-token! 'NUMBER)))
		(instantiate::J2SNumber
		   (loc (token-loc token))
		   (val (token-value token)))))
	    ((OCTALNUMBER)
	     (let ((token (consume-token! 'OCTALNUMBER)))
		(instantiate::J2SOctalNumber
		   (loc (token-loc token))
		   (val (token-value token)))))
	    ((true false null)
	     (let ((token (consume-any!)))
		(instantiate::J2SString
		   (loc (token-loc token))
		   (val (symbol->string (token-value token))))))
	    (else
	     (if (j2s-reserved-id? (peek-token-type))
		 (let ((token (consume-any!)))
		    (case (token-value token)
		       ((get set)
			token)
		       (else
			(instantiate::J2SString
			   (loc (token-loc token))
			   (val (symbol->string (token-value token)))))))
		 (parse-token-error "Wrong property name" (peek-token))))))
      
      (define (find-prop name props)
	 (find (lambda (prop)
		  (when (isa? prop J2SAccessorPropertyInit)
		     (with-access::J2SAccessorPropertyInit prop ((pname name))
			(with-access::J2SString pname (val)
			   (string=? val name)))))
	    props))
      
      (define (property-accessor tokname name props)
	 (let* ((id (consume-any!))
		(params (params))
		(body (fun-body))
		(mode (or (javascript-mode body) 'normal))
		(fun (instantiate::J2SFun
			(mode mode)
			(loc (token-loc tokname))
			(params params)
			(name (cdr id))
			(body body)))
		(oprop (find-prop (symbol->string! (cdr id)) props))
		(prop (or oprop
			  (instantiate::J2SAccessorPropertyInit
			     (loc (token-loc tokname))
			     (get (instantiate::J2SUndefined
				     (loc (token-loc id))))
			     (set (instantiate::J2SUndefined
				     (loc (token-loc id))))
			     (name (instantiate::J2SString
				      (loc (token-loc id))
				      (val (symbol->string (cdr id)))))))))
	    (with-access::J2SAccessorPropertyInit prop (get set)
	       (if (eq? name 'get)
		   (if (isa? get J2SUndefined)
		       (set! get fun)
		       (parse-token-error "Wrong property" (peek-token)))
		   (if (isa? set J2SUndefined)
		       (set! set fun)
		       (parse-token-error "Wrong property" (peek-token))))
	       ;; return a prop only if new
	       (unless oprop prop))))
      
      (define (property-init props)
	 (let* ((tokname (property-name))
		(name (when (pair? tokname) (cdr tokname))))
	    (case name
	       ((get set)
		(case (peek-token-type)
		   ((ID RESERVED)
		    (property-accessor tokname name props))
		   ((:)
		    (let* ((ignore (consume-any!))
			   (val (assig-expr #f)))
		       (with-access::J2SLiteral ignore (loc)
			  (instantiate::J2SDataPropertyInit
			     (loc (token-loc tokname))
			     (name (instantiate::J2SString
				      (loc (token-loc tokname))
				      (val (symbol->string name))))
			     (val val)))))
		   (else
		    (if (j2s-reserved-id? (peek-token-type))
			(property-accessor tokname name props)
			(parse-token-error "Wrong property name" (peek-token))))))
	       (else
		(let* ((ignore (consume! ':))
		       (val (assig-expr #f)))
		   (with-access::J2SLiteral tokname (loc)
		      (instantiate::J2SDataPropertyInit
			 (loc loc)
			 (name tokname)
			 (val val))))))))
      
      (push-open-token (consume! 'LBRACE))
      (if (eq? (peek-token-type) 'RBRACE)
	  (let ((token (consume-any!)))
	     (pop-open-token)
	     (instantiate::J2SObjInit
		(loc (token-loc token))
		(inits '())))
	  (let loop ((rev-props (list (property-init '()))))
	     (if (eq? (peek-token-type) 'RBRACE)
		 (let ((token (consume-any!)))
		    (pop-open-token)
		    (instantiate::J2SObjInit
		       (loc (token-loc token))
		       (inits (reverse! rev-props))))
		 (begin
		    (consume! 'COMMA)
		    ;; MS: I'm not sure this is demanded by the official
		    ;; JS syntax test case uses it (15.2.3.6-4-293-3.js)
		    (if (eq? (peek-token-type) 'RBRACE)
			(loop rev-props)
			(let ((newp (property-init rev-props)))
			   ;; property-init returns a new props except
			   ;; for accessor properties which might set
			   ;; a field of an already existing prop
			   (loop (if newp (cons newp rev-props) rev-props)))))))))

   (define (abspath)
      (let ((path (input-port-name input-port)))
	 (if (absolute-file-name? path)
	     path
	     (file-name-canonicalize! (make-file-name (pwd) path)))))

   (define (nodes-mode nodes)
      (let ((mode (when (pair? nodes) (javascript-mode-nodes nodes))))
	 (if (symbol? mode) mode 'normal)))
   
   (define (program dp)
      (set! tilde-level (if dp 1 0))
      (with-access::J2SBlock (source-elements) (loc endloc nodes name)
	 (let ((module (javascript-module-nodes nodes))
	       (mode (nodes-mode nodes)))
	    (instantiate::J2SProgram
	       (loc loc)
	       (endloc endloc)
	       (path (abspath))
	       (module module)
	       (path (config-get conf :filename (abspath)))
	       (main (config-get conf :module-main #f))
	       (name (config-get conf :module-name #f))
	       (mode mode)
	       (nodes (map! (lambda (n) (disable-es6 n mode conf)) nodes))))))
   
   (define (eval)
      (set! tilde-level 0)
      (with-access::J2SBlock (source-elements) (loc endloc nodes)
	 (let ((mode (nodes-mode nodes)))
	    (instantiate::J2SProgram
	       (loc loc)
	       (endloc endloc)
	       (path (config-get conf :filename (abspath)))
	       (name (config-get conf :module-name #f))
	       (mode mode)
	       (nodes (map! (lambda (n) (disable-es6 n mode conf)) nodes))))))

   (define (repl)
      (let ((el (repl-element)))
	 (if (isa? el J2SNode)
	     (with-access::J2SNode el (loc)
		(instantiate::J2SProgram
		   (loc loc)
		   (endloc loc)
		   (main (config-get conf :module-main #f))
		   (name (config-get conf :module-name #f))
		   (path (config-get conf :filename (abspath)))
		   (nodes (list (disable-es6 el 'normal conf)))))
	     el)))

   (case (config-get conf :parser #f)
      ((tilde-expression) (tilde-expression))
      ((dollar-expression) (dollar-expression))
      ((module) (program #f))
      ((repl) (repl))
      ((eval) (eval))
      ((client-program) (program #t))
      (else (program #f))))

;*---------------------------------------------------------------------*/
;*    j2s-tag->expr ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-tag->expr tag::pair)
      (let* ((type (symbol->string! (cdr tag)))
	     (i (string-index type #\.)))
	 (if i
	     ;; a variable access
	     (instantiate::J2SAccess
		(loc (token-loc tag))
		(obj (instantiate::J2SUnresolvedRef
			(loc (token-loc tag))
			(id (string->symbol (substring type 1 i)))))
		(field (instantiate::J2SString
			  (loc (token-loc tag))
			  (val (substring type (+fx i 1) (-fx (string-length type) 1))))))
	     ;; a tag name
	     (instantiate::J2SUnresolvedRef
		(loc (token-loc tag))
		(id (string->symbol (substring type 1 (-fx (string-length type) 1))))))))

;*---------------------------------------------------------------------*/
;*    javascript-mode-nodes ...                                        */
;*---------------------------------------------------------------------*/
(define (javascript-mode-nodes nnodes::pair-nil)
   
   (define (octal-error s)
      (with-access::J2SString s (val loc)
	 (raise
	    (instantiate::&io-parse-error
	       (proc "js-symbol")
	       (msg "Octal literals are not allowed in strict mode")
	       (obj val)
	       (fname (cadr loc))
	       (location (caddr loc))))))

   (define (check-octal-string n)
      (cond
	 ((isa? n J2SStmtExpr)
	  (with-access::J2SStmtExpr n (expr)
	     (check-octal-string expr)))
	 ((isa? n J2SString)
	  (with-access::J2SString n (escape loc)
	     (when (memq 'octal escape)
		(octal-error n))))
	 (else
	  #f)))
   
   (let loop ((nodes nnodes))
      (when (pair? nodes)
	 (let ((mode (javascript-mode (car nodes))))
	    (cond
	       ((symbol? mode)
		(when (eq? mode 'strict)
		   (for-each check-octal-string nnodes))
		mode)
	       (mode
		(loop (cdr nodes))))))))

;*---------------------------------------------------------------------*/
;*    lbrace-following? ...                                            */
;*---------------------------------------------------------------------*/
(define (lbrace-following? port)
   (read/rp
      (regular-grammar ()
	 ((: (* (in " \t\n")) #\{)
	  (rgc-buffer-insert-substring! port (the-string) 0 (the-length))
	  #t)
	 (else
	  #f))
      port))

;*---------------------------------------------------------------------*/
;*    javascript-mode ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (javascript-mode node::J2SNode)
   #f)

;*---------------------------------------------------------------------*/
;*    javascript-mode ::J2SSeq ...                                     */
;*---------------------------------------------------------------------*/
(define-method (javascript-mode node::J2SSeq)
   (with-access::J2SSeq node (nodes)
      (javascript-mode-nodes nodes)))

;*---------------------------------------------------------------------*/
;*    javascript-mode ::J2SStmtExpr ...                                */
;*---------------------------------------------------------------------*/
(define-method (javascript-mode node::J2SStmtExpr)
   (with-access::J2SStmtExpr node (expr)
      (when (isa? expr J2SString)
	 (with-access::J2SString expr (val escape)
	    (cond
	       ((pair? escape) (memq 'octal escape))
	       ((string=? "use strict" val) 'strict)
	       ((string=? "use hopscript" val) 'hopscript)
	       (else #t))))))
   
;*---------------------------------------------------------------------*/
;*    javascript-module-nodes ...                                      */
;*---------------------------------------------------------------------*/
(define (javascript-module-nodes nnodes::pair-nil)
   (let loop ((nodes nnodes))
      (when (pair? nodes)
	 (or (javascript-module (car nodes)) (loop (cdr nodes))))))

;*---------------------------------------------------------------------*/
;*    javascript-module ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (javascript-module node::J2SNode)
   #f)

;*---------------------------------------------------------------------*/
;*    javascript-module ::J2SSeq ...                                   */
;*---------------------------------------------------------------------*/
(define-method (javascript-module node::J2SSeq)
   (with-access::J2SSeq node (nodes)
      (javascript-module-nodes nodes)))

;*---------------------------------------------------------------------*/
;*    javascript-module ::J2SStmtExpr ...                              */
;*---------------------------------------------------------------------*/
(define-method (javascript-module node::J2SStmtExpr)
   (with-access::J2SStmtExpr node (expr)
      (when (isa? expr J2SString)
	 (with-access::J2SString expr (val)
	    (when (string-prefix? "(module " val)
	       (call-with-input-string val read))))))

;*---------------------------------------------------------------------*/
;*    disable-es6 ...                                                  */
;*---------------------------------------------------------------------*/
(define (disable-es6 node::J2SNode mode conf)
   (unless (memq mode '(hopscript ecmascript6))
      (unless (config-get conf :es6-let #f)
	 (disable-es6-let node))
      (unless (config-get conf :es6-arrow-function #f)
	 (disable-es6-arrow node)))
   node)

;*---------------------------------------------------------------------*/
;*    disable-es6-let ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-let this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    disable-es6-let ::J2SFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-let this::J2SFun)
   (with-access::J2SFun this (mode)
      (unless (memq mode '(hopscript ecmascript6))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    disable-es6-let ::J2SArrow ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-let this::J2SLetInit)
   (with-access::J2SLetInit this (loc id)
      (raise
	 (instantiate::&io-parse-error
	    (proc "js-parser")
	    (msg "arrow/const block disabled")
	    (obj id)
	    (fname (cadr loc))
	    (location (caddr loc))))))

;*---------------------------------------------------------------------*/
;*    disable-es6-arrow ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-arrow this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    disable-es6-arrow ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-arrow this::J2SFun)
   (with-access::J2SFun this (mode)
      (unless (memq mode '(hopscript ecmascript6))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    disable-es6-arrow ::J2SArrow ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-arrow this::J2SArrow)
   (with-access::J2SArrow this (loc)
      (raise
	 (instantiate::&io-parse-error
	    (proc "js-parser")
	    (msg "arrow function disabled")
	    (obj '=>)
	    (fname (cadr loc))
	    (location (caddr loc))))))

