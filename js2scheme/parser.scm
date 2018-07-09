;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/parser.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep  8 07:38:28 2013                          */
;*    Last change :  Mon Jul  9 09:02:20 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript parser                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_parser

   (include "token.sch"
	    "ast.sch")

   (import __js2scheme_lexer
	   __js2scheme_html
	   __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils)

   (export (j2s-parser ::input-port ::pair-nil)
	   (j2s-tag->expr ::pair ::bool)))

;*---------------------------------------------------------------------*/
;*    j2s-parser ...                                                   */
;*    -------------------------------------------------------------    */
;*    JavaScript parser                                                */
;*---------------------------------------------------------------------*/
(define (j2s-parser input-port conf::pair-nil)
   
   (define tilde-level (config-get conf :tilde-level 0))
   (define lang (config-get conf :language "hopscript"))
   (define current-mode 'normal)
   (define source-map (config-get conf :source-map #f))
   
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
   
   (define _this
      (instantiate::J2SDecl
	 (loc `(at ,(input-port-name input-port) 0))
	 (vtype 'object)
	 (id '%this)
	 (_scmid '%this)))
   
   (define-macro (with-this id loc . body)
      `(let ((othis _this))
	  (set! _this
	     (instantiate::J2SDecl
		(loc ,loc)
		(id ,id)
		(_scmid ,id)))
	  (unwind-protect
	     (begin ,@body)
	     (set! _this othis))))
   
   (define (current-this) _this)
   
   (define (current-loc)
      `(at ,(input-port-name input-port) ,(input-port-position input-port)))
   
   (define (parse-token-error msg token::pair)
      (match-case (token-loc token)
	 ((at ?fname ?loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "hopscript")
		(msg (if (eq? (token-tag token) 'BAD) (cadr token) msg))
		(obj (if (eq? (token-tag token) 'BAD) (cddr token) (token-value token)))
		(fname fname)
		(location loc))))
	 (else
	  (raise
	     (instantiate::&io-parse-error
		(proc "hopscript")
		(msg (if (eq? (token-tag token) 'BAD) (cadr token) msg))
		(obj (if (eq? (token-tag token) 'BAD) (cddr token) (token-value token))))))))
   
   (define (parse-token-warning msg token)
      (with-handler
	 (lambda (e)
	    (exception-notify e)
	    (display "ignoring...\n" (current-error-port))
	    #f)
	 (parse-token-error msg token)))
   
   (define (parse-error msg obj)
      (let ((fname (input-port-name input-port))
	    (loc (input-port-position input-port)))
	 (raise
	    (instantiate::&io-parse-error
	       (proc "hopscript")
	       (msg msg)
	       (obj (read-line input-port))
	       (fname fname)
	       (location loc)))))

   (define (parse-node-error msg node)
      (with-access::J2SNode node (loc)
	 (match-case loc
	    ((at ?fname ?loc)
	     (raise
		(instantiate::&io-parse-error
		   (proc "hopscript")
		   (msg msg)
		   (obj (typeof node))
		   (fname fname)
		   (location loc))))
	    (else
	     (raise
		(instantiate::&io-parse-error
		   (proc "hopscript")
		   (msg msg)
		   (obj (typeof node))))))))
   
   (define *peeked-tokens* '())
   (define *previous-token-type* #unspecified)
   (define *open-tokens* '())
   
   (define (peek-token)
      (if (null? *peeked-tokens*)
	  (begin
	     (set! *peeked-tokens* (list (read/rp (j2s-lexer) input-port lang conf)))
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
      (token-tag (peek-token)))
   
   (define (peek-token-value)
      (token-value (peek-token)))
   
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
		(format "Expected \"~a\" got \"~a\"" type
		   (token-tag token))
		token))))
   
   (define (consume! type)
      (cdr (consume-token! type)))
   
   (define (consume-any!)
      (let ((res (peek-token)))
	 (set! *previous-token-type* (car res))
	 (set! *peeked-tokens* (cdr *peeked-tokens*))
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
	  (parse-token-error (format "~a, \"\;\", or newline expected" where)
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
   
   (define (source-element-mode! node::J2SNode)
      (let ((mode (if (eq? (config-get conf :parser #f) 'eval-strict)
		      'strict
		      (javascript-mode node))))
	 (when mode (set! current-mode mode))))
   
   (define (source-elements::J2SBlock)
      (let loop ((rev-ses '())
		 (first #t))
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
	     (let ((el (source-element)))
		(if (eq? el 'source-map)
		    (loop rev-ses #f)
		    (begin
		       (when first (source-element-mode! el))
		       (loop (cons el rev-ses) #f)))))))
   
   (define (source-element)
      (case (peek-token-type)
	 ((function)
	  (function-declaration))
	 ((ID)
	  (if (eq? (peek-token-value) 'async)
	      (let* ((token (consume-any!))
		     (next (peek-token-type)))
		 (token-push-back! token)
		 (if (eq? next 'function)
		     (async-declaration)
		     (statement)))
	      (statement)))
	 ((service)
	  (service-declaration))
	 ((class)
	  (class-declaration))
	 ((RESERVED)
	  (if (eq? (peek-token-value) 'import)
	      (import (consume-any!))
	      (statement)))
	 ((EOF)
	  (parse-token-error "Unexpected end of file"
	     (if (pair? *open-tokens*)
		 (car (last-pair *open-tokens*))
		 (consume-any!))))
	 ((ERROR)
	  (parse-token-error "Error" (consume-any!)))
	 ((SOURCEMAP)
	  (let ((tok (consume-any!)))
	     (if (eof?)
		 (begin
		    (set! source-map (token-value tok))
		    'source-map)
		 (parse-token-error "Unexpected source-map" tok))))
	 (else
	  (statement))))
   
   (define (absolute-file-name? path)
      (and (>fx (string-length path) 0)
	   (char=? (string-ref path 0) (file-separator))))
   
   (define (repl-element)
      (case (peek-token-type)
	 ((function) (function-declaration))
	 ((service) (service-declaration))
	 ((async) (async-declaration))
	 ((class) (class-declaration))
	 ((EOF) (cdr (consume-any!)))
	 ((ERROR) (parse-token-error "Error" (consume-any!)))
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
	 ((async) (async-declaration))
	 ((class) (class-declaration))
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
      (let loop ((vars (var in-for-init? constrinit constr)))
	 (case (peek-token-type)
	    ((SEMICOLON)
	     (unless in-for-init? (consume-any!))
	     (instantiate::J2SVarDecls
		(loc (token-loc token))
		(decls vars)))
	    ((COMMA)
	     (consume-any!)
	     (loop (append vars (var in-for-init? constrinit constr))))
	    ((in)
	     (cond
		((not in-for-init?)
		 (parse-token-error "Illegal variable declaration"
		    (peek-token)))
		(else
		 (instantiate::J2SVarDecls
		    (loc (token-loc token))
		    (decls vars)))))
	    (else
	     (cond
		((and (eq? (peek-token-type) 'ID)
		      (eq? (token-value (peek-token)) 'of)
		      in-for-init?)
		 (instantiate::J2SVarDecls
		    (loc (token-loc token))
		    (decls vars)))
		((and (not in-for-init?)
		      (or (at-new-line-token?)
			  (eq? (peek-token-type) 'RBRACE)
			  (eq? (peek-token-type) 'EOF)))
		 (instantiate::J2SVarDecls
		    (loc (token-loc token))
		    (decls vars)))
		(else
		 (parse-token-error "Illegal variable declaration"
		    (consume-any!))))))))
   
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
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id id)
	       (binder 'let)
	       (val val)))
	 (lambda (loc id)
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id id)
	       (binder 'let)
	       (val (J2SUndefined))))))
   
   (define (const-decl-list in-for-init?)
      ;; ES6 const block
      (decl-list (consume-token! 'const) in-for-init?
	 (lambda (loc id val)
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id id)
	       (writable #f)
	       (binder 'let)
	       (val val)))
	 (lambda (loc id)
	    (instantiate::J2SDeclInit
	       (loc loc)
	       (id id)
	       (writable #f)
	       (binder 'let)
	       (val (J2SUndefined))))))
   
   (define (var::pair in-for-init? constrinit constr)
      (let ((id (peek-token)))
	 (case (token-tag id)
	    ((ID static)
	     (consume-any!)
	     (if (or (eq? (token-tag id) 'ID) (eq? current-mode 'normal))
		 (case (peek-token-type)
		    ((=)
		     (let* ((token (consume-any!))
			    (expr (assig-expr in-for-init? #f)))
			(list (constrinit (token-loc token) (cdr id) expr))))
		    (else
		     (list (constr (token-loc id) (cdr id)))))
		 (parse-token-error "Illegal lhs" id))) 
	    ((undefined NaN Infinity)
	     (consume-any!)
	     (case (peek-token-type)
		((=)
		 (consume-any!)
		 (list (assig-expr in-for-init? #f)))
		(else
		 (parse-token-error "Illegal variable declaration" id))))
	    ((LBRACE LBRACKET)
	     (let* ((objectp (eq? (peek-token-type) 'LBRACE))
		    (loc (token-loc id))
		    (lhs (if objectp (object-literal #t) (array-literal #t)))
		    (decl (constrinit loc (gensym '%obj) (J2SUndefined)))
		    (bindings (j2s-destructure lhs decl #t)))
		(if in-for-init?
		    (let* ((tmp (instantiate::J2SDecl
				   (loc loc)
				   (id (gensym '%i))))
			   (rhs (J2SRef tmp)))
		       (with-access::J2SDeclInit decl (val id %info binder)
			  (set! binder 'let)
			  (set! val (instantiate::J2SDProducer
				       (loc loc)
				       (size (if objectp -1 (length bindings)))
				       (decl decl)
				       (type (if objectp 'object 'array))
				       (expr rhs))))
		       (cons* tmp decl bindings))
		    (let* ((assig (consume-token! '=))
			   (rhs (assig-expr in-for-init? #f)))
		       (with-access::J2SDeclInit decl (val id %info binder)
			  (set! binder 'let)
			  (set! val (instantiate::J2SDProducer
				       (loc loc)
				       (size (if objectp -1 (length bindings)))
				       (type (if objectp 'object 'array))
				       (decl decl)
				       (expr rhs))))
		       (cons decl bindings)))))
	    (else
	     (parse-token-error "Illegal lhs" id)))))
   
   (define (empty-statement)
      (instantiate::J2SNop
	 (loc (token-loc (consume-token! 'SEMICOLON)))))
   
   (define (iff)
      (let ((tif (consume-any!)))
	 (push-open-token (consume-token! 'LPAREN))
	 (let ((test (expression #f #f)))
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
      
      (define (init-first-part tok0)
	 (case tok0
	    ((var) (var-decl-list #t))
	    ((let) (let-decl-list #t))
	    ((const) (const-decl-list #t))
	    ((SEMICOLON) #f)
	    (else (expression #t #f))))
      
      (let ((loc (token-loc (consume-token! 'for))))
	 (push-open-token (consume-token! 'LPAREN))
	 (let* ((tok0 (peek-token-type))
		(first-part (init-first-part tok0)))
	    (case (peek-token-type)
	       ((SEMICOLON)
		(for-init/test/incr loc
		   (or first-part (instantiate::J2SNop (loc loc)))))
	       ((in)
		(for-in loc first-part))
	       ((ID)
		(when (eq? (token-value (peek-token)) 'of)
		   (for-in loc first-part)))))))
   
   ;; for (init; test; incr)
   (define (for-init/test/incr loc init::J2SNode)
      (consume! 'SEMICOLON)
      (let ((test (case (peek-token-type)
		     ((SEMICOLON) #f)
		     (else (expression #f #f)))))
	 (consume! 'SEMICOLON)
	 (let ((incr (case (peek-token-type)
			((RPAREN) #f)
			(else (expression #f #f)))))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let* ((body (statement)))
	       (instantiate::J2SFor
		  (loc loc)
		  (init (or init (instantiate::J2SNop (loc loc))))
		  (test (or test (instantiate::J2SBool (val #t) (loc loc))))
		  (incr (or incr (J2SUndefined)))
		  (body body))))))
   
   ;; for (lhs/var x in obj)
   (define (for-in loc lhs)
      ;; TODO: weed out bad lhs
      (let ((op (token-tag (consume-any!)))
	    (error-token (peek-token))
	    (obj (expression #f #f))
	    (ignore-RPAREN (consume! 'RPAREN))
	    (body (statement)))
	 (cond
	    ((isa? lhs J2SVarDecls)
	     (with-access::J2SVarDecls lhs (decls)
		(cond
		   ((null? lhs)
		    (parse-error "Illegal emtpy declaration"
		       error-token))
		   ((null? (cdr decls))
		    (instantiate::J2SForIn
		       (loc loc)
		       (op op)
		       (lhs lhs)
		       (obj obj)
		       (body body)))
		   ((and (isa? (cadr decls) J2SDeclInit)
			 (with-access::J2SDeclInit (cadr decls) (val)
			    (isa? val J2SDProducer)))
		    ;; a destructuring for-loop header
		    (let ((tmps (cdr decls)))
		       (set! decls (list (car decls)))
		       (let ((body (instantiate::J2SBlock
				      (loc loc)
				      (endloc loc)
				      (nodes (list (instantiate::J2SVarDecls
						      (loc loc)
						      (decls tmps))
						body)))))
			  (instantiate::J2SForIn
			     (loc loc)
			     (op op)
			     (lhs lhs)
			     (obj obj)
			     (body body)))))
		   (else
		    (parse-error "Only one declaration allowed"
		       error-token)))))
	    ((not (isa? lhs J2SUnresolvedRef))
	     (parse-error "Variable reference or declaration required"
		error-token))
	    (else
	     (instantiate::J2SForIn
		(loc loc)
		(op op)
		(lhs lhs)
		(obj obj)
		(body body))))))
   
   (define (while)
      (let ((token (consume-token! 'while)))
	 (push-open-token (consume-token! 'LPAREN))
	 (let ((test (expression #f #f)))
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
	 (push-open-token (consume-token! 'LPAREN))
	 (let ((test (expression #f #f)))
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
	     (let ((expr (expression #f #f)))
		(consume-statement-semicolon! "return")
		(instantiate::J2SReturn
		   (loc loc)
		   (expr expr)))))))

   (define (with)
      (let ((token (consume-token! 'with)))
	 (push-open-token (consume-token! 'LPAREN))
	 (let ((expr (expression #f #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((body (statement)))
	       (instantiate::J2SWith
		  (loc (token-loc token))
		  (obj expr)
		  (block body))))))
   
   (define (switch)
      (let ((token (consume-token! 'switch)))
	 (push-open-token (consume-token! 'LPAREN))
	 (let ((key (expression #f #f)))
	    (consume! 'RPAREN)
	    (pop-open-token)
	    (let ((cases (case-block)))
	       (instantiate::J2SSwitch
		  (loc (token-loc token))
		  (key key)
		  (cases cases))))))

   (define (case-block)
      (push-open-token (consume-token! 'LBRACE))
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
	 (let ((expr (expression #f #f)))
	    (consume! ':)
	    (let ((body (switch-clause-statements (token-loc token))))
	       (instantiate::J2SCase
		  (loc (token-loc token))
		  (expr expr)
		  (body body))))))
   
   (define (default-clause)
      (let* ((token (consume-token! 'default))
	     (loc (token-loc token)))
	 (consume! ':)
	 (instantiate::J2SDefault
	    (loc loc)
	    (expr (J2SUndefined))
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
	    (parse-token-error "Throw must have a value" (peek-token)))
	 (peek-token)
	 (when (or (at-new-line-token?) (eq? (peek-token-type) 'NEWLINE))
	    (parse-token-error "Throw must have a value" (peek-token)))
	 (let ((expr (expression #f #f)))
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
	 (push-open-token (consume-token! 'LPAREN))
	 (case (peek-token-type)
	    ((ID)
	     (let ((id (consume! 'ID)))
		(consume! 'RPAREN)
		(pop-open-token)
		(let ((body (block)))
		   ;; not sure, if 'Param' is a really good choice.
		   ;; we'll see...
		   (instantiate::J2SCatch
		      (loc loc)
		      (param (instantiate::J2SDecl
				(loc loc)
				(id id)
				(binder 'param)))
		      (body body)))))
	    ((LBRACE LBRACKET)
	     (let ((vars (var #t
			    (lambda (loc id val)
			       (instantiate::J2SDeclInit
				  (loc loc)
				  (id id)
				  (val val)))
			    (lambda (loc id)
			       (instantiate::J2SDecl
				  (loc loc)
				  (id id))))))
		(consume! 'RPAREN)
		(pop-open-token)
		(with-access::J2SDecl (car vars) (binder)
		   (set! binder 'param))
		(instantiate::J2SCatch
		   (loc loc)
		   (param (car vars))
		   (body (instantiate::J2SBlock
			    (loc loc)
			    (endloc loc)
			    (nodes (list (instantiate::J2SVarDecls
					    (loc loc)
					    (decls (cdr vars)))
				      (block))))))))
	    (else
	     (parse-token-error "Unexpected token" (consume-any!))))))
   
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
      (let ((expr (expression #f #f)))
	 (consume-statement-semicolon! "expression")
	 (with-access::J2SExpr expr (loc)
	    (instantiate::J2SStmtExpr
	       (loc loc)
	       (expr expr)))))
   
   (define (function-declaration)
      (function #t (consume-token! 'function) #f))

   (define (async-declaration)
      (let* ((tok (consume-any!))
	     (fun (function-declaration)))
	 (if (isa? fun J2SDeclFun)
	     (with-access::J2SDeclFun fun (val)
		(set! val (async->generator val))
		fun)
	     (parse-token-error "Illegal async function declaration" tok))))

   (define (class-declaration)
      (clazz #t))
   
   (define (function-expression)
      (function #f (consume-token! 'function)))
   
   (define (service-declaration)
      (service #t))
   
   (define (service-expression)
      (service #f))

   (define (class-expression)
      (clazz #f))

   (define (async->generator fun::J2SFun)
      ;; generates the async function body by the following transformation
      ;; async function NAME( a0, ... ) { BODY }
      ;;   =>
      ;; function NAME( a0, ... ) { return spawn( function*() { BODY }, this); }
      ;; For additional details, see:
      ;;   https://tc39.github.io/ecmascript-asyncawait
      (with-access::J2SFun fun (generator body mode thisp)
	 (cond
	    ((and (not (config-get conf :es2017-async))
		  (not (string=? lang "hopscript")))
	     (parse-node-error
		"Async function requires hopscript or ecmascript2017 mode" fun))
	    (generator
	     (parse-node-error
		"Wrong async function declaration" fun))
	    (else
	     (with-access::J2SBlock body (loc endloc)
		(let ((gen (instantiate::J2SFun
			      (thisp thisp)
			      (loc loc)
			      (generator #t)
			      (mode 'strict)
			      (body body))))
		   (set! body
		      (J2SBlock
			 (J2SReturn #t
			    (J2SCall (J2SHopRef 'js-spawn)
			       gen (J2SThis (current-this))
			       (J2SHopRef '%this)))))
		   fun))))))
      
   (define (async-expression)
      (let* ((tok (consume-any!))
	     (fun (primary #f)))
	 (if (isa? fun J2SFun)
	     (async->generator fun)
	     (parse-token-error "Illegal async function expression" tok))))
   
   (define (arrow-params args)
      (map (lambda (p idx)
	      (cond
		 ((pair? p)
		  (let ((loc (token-loc p)))
		     (instantiate::J2SDecl
			(loc loc)
			(id (token-value p))
			(binder 'param))))
		 ((isa? p J2SAssig)
		  (with-access::J2SAssig p (loc lhs rhs)
		     (with-access::J2SUnresolvedRef lhs (id)
			(instantiate::J2SDeclInit
			   (val rhs)
			   (loc loc)
			   (id id)
			   (binder 'param)))))
		 ((isa? p J2SUnresolvedRef)
		  (with-access::J2SUnresolvedRef p (id loc)
		     (instantiate::J2SDecl
			(loc loc)
			(id id)
			(binder 'param))))
		 ((or (isa? p J2SObjInit) (isa? p J2SArray))
		  (with-access::J2SExpr p (loc)
		     (let ((id (string->symbol (format "%~a" idx))))
			(instantiate::J2SDecl
			   (loc loc)
			   (id id)
			   (_scmid id)
			   (binder 'param)))))
		 (else
		  (parse-node-error "Unexpected token" p))))
	 args (iota (length args))))

   (define (arrow-body params::pair-nil args::pair-nil)
      (if (eq? (peek-token-type) 'LBRACE)
	  ;; a statement
	  (fun-body params args current-mode)
	  ;; an expression
	  (let* ((expr (assig-expr #f #f))
		 (endloc (token-loc (peek-token) -1)))
	     (with-access::J2SNode expr (loc)
		(destructure-fun-params params args
		   (instantiate::J2SBlock
		      (loc loc)
		      (endloc endloc)
		      (nodes (append (fun-body-params-defval params)
				(list (instantiate::J2SReturn
					 (loc loc)
					 (expr expr)))))))))))
   
   (define (arrow-function args::pair-nil loc)
      ;; ES6 arrow functions
      (let* ((=> (consume-any!))
	     (params (arrow-params args)))
	 (instantiate::J2SArrow
	    (idthis '%)
	    (loc loc)
	    (name '||)
	    (mode 'strict)
	    (params params)
	    (body (arrow-body params args)))))

   (define (rest-params params)
      (when (pair? params)
	 (with-access::J2SDecl (car (last-pair params)) (usage)
	    (when (equal? usage '(rest)) 'rest))))
   
   (define (rest-params params)
      (when (pair? params)
	 (with-access::J2SDecl (car (last-pair params)) (usage)
	    (when (equal? usage '(rest)) 'rest))))

   (define (function declaration? token #!optional methodof)
      (let ((loc (token-loc token)))
	 (with-this 'this loc
	    (let* ((gen (when (eq? (peek-token-type) '*)
			   (consume-any!) '*))
		   (id (when (or declaration?
				 (memq (peek-token-type) '(ID service)))
			  (consume-any!))))
	       (multiple-value-bind (params args)
		  (function-params)
		  (let* ((body (fun-body params args current-mode))
			 (mode (or (javascript-mode body) current-mode)))
		     (cond
			(declaration?
			 (co-instantiate ((val (instantiate::J2SFun
						  (loc loc)
						  (thisp (current-this))
						  (params params)
						  (name (cdr id))
						  (mode mode)
						  (generator gen)
						  (ismethodof methodof)
						  (body body)
						  (vararg (rest-params params))))
					  (decl (instantiate::J2SDeclFun
						   (loc loc)
						   (writable (not (eq? mode 'hopscript)))
						   (ronly (eq? mode 'hopscript))
						   (id (cdr id))
						   (val val))))
			    decl))
			(id
			 (co-instantiate ((fun (instantiate::J2SFun
						  (loc loc)
						  (decl decl)
						  (mode mode)
						  (generator gen)
						  (name (cdr id))
						  (thisp (current-this))
						  (params params)
						  (ismethodof methodof)
						  (vararg (rest-params params))
						  (body body)))
					  (decl (instantiate::J2SDeclFun
						   (loc (token-loc id))
						   (id (cdr id))
						   (immutable #t)
						   (writable #f)
						   (ronly #t)
						   (expression #t)
						   (scope 'global)
						   (val fun))))
			    fun))
			(else
			 (instantiate::J2SFun
			    (loc loc)
			    (name '||)
			    (mode mode)
			    (generator gen)
			    (thisp (current-this))
			    (params params)
			    (vararg (rest-params params))
			    (ismethodof methodof)
			    (body body))))))))))

   (define (service-create token id this::J2SDecl params args body mode register declaration? import?::bool)
      (let ((loc (token-loc token)))
	 (cond
	    (declaration?
	     (instantiate::J2SDeclSvc
		(loc loc)
		(id (cdr id))
		(val (instantiate::J2SSvc
			(loc loc)
			(register register)
			(import import?)
			(thisp this)
			(params params)
			(vararg (rest-params params))
			(name (cdr id))
			(init (J2SNop))
			(mode mode)
			(path (cdr id))
			(body body)
			(decl (instantiate::J2SDecl
				 (loc loc)
				 (id (cdr id))
				 (writable #f)
				 (ronly #t)
				 (scope 'global)))))))
	    (id
	     (co-instantiate ((fun (instantiate::J2SSvc
				      (loc (token-loc id))
				      (register register)
				      (import import?)
				      (decl decl)
				      (thisp this)
				      (params params)
				      (vararg (rest-params params))
				      (name (cdr id))
				      (init (J2SNop))
				      (mode mode)
				      (path (cdr id))
				      (body body)))
			      (decl (instantiate::J2SDeclFun
				       (loc (token-loc id))
				       (id (cdr id))
				       (immutable #t)
				       (writable #f)
				       (ronly #t)
				       (expression #t)
				       (scope  'global)
				       (val fun))))
		fun))
	    (else
	     (instantiate::J2SSvc
		(loc loc)
		(register register)
		(import import?)
		(thisp this)
		(params params)
		(vararg (rest-params params))
		(name (gensym))
		(init (J2SNop))
		(mode mode)
		(body body))))))
   
   (define (service-import token id params args declaration?)
      (let ((loc (token-loc id)))
	 (unless (null? params)
	    (parse-node-error "Imported service must not declare parameters"
	       (car params)))
	 (let ((body (instantiate::J2SBlock
			(loc loc)
			(endloc loc)
			(nodes (list (instantiate::J2SNop
					(loc loc))))))
	       (init (instantiate::J2SNop
			(loc loc))))
	    (with-this 'this loc
	       (service-create token id (current-this) params args body 'strict
		  #f declaration? #t)))))
      
   (define (service-implement token id params args declaration?)
      (with-this 'this (token-loc token)
	 (let* ((body (fun-body params args 'strict))
		(mode (or (if (eq? (javascript-mode body) 'hopscript)
			      'hopscript 'strict))))
	    (service-create token id (current-this) params args body mode
	       #t declaration? #f))))

   (define (import token)
      (parse-token-error "Illegal import declaration" token))

   (define (service declaration?)
      (let* ((token (consume-token! 'service))
	     (id (when (or declaration? (eq? (peek-token-type) 'ID))
		    (consume-token! 'ID))))
	 (multiple-value-bind (params args)
	    (function-params)
	    (if (and (null? params) (not (eq? (peek-token-type) 'LBRACE)))
		(if (not id)
		    (parse-token-error "Bad service import" token)
		    (service-import token id params args declaration?))
		(service-implement token id params args declaration?)))))

   (define (consume-param! idx)
      (case (peek-token-type)
	 ((ID)
	  (let* ((token (consume-any!))
		 (loc (token-loc token))
		 (hint '())
		 (typ (if (eq? (peek-token-type) 'TYPE)
			  (token-value (consume-any!))
			  'unknown)))
	     (if (eq? (peek-token-type) '=)
		 ;; a parameter with a default value
		 (begin
		    (consume-any!)
		    (values
		       (instantiate::J2SDeclInit
			  (binder 'param)
			  (val (assig-expr #f #f))
			  (loc loc)
			  (id (token-value token))
			  (utype typ)
			  (hint hint))
		       #f))
		 ;; no default value
		 (values 
		    (instantiate::J2SDecl
		       (binder 'param)
		       (loc loc)
		       (id (token-value token))
		       (utype typ)
		       (hint hint))
		    #f))))
	 ((LBRACE)
	  (let ((id (string->symbol (format "%~a" idx)))
		(loc (token-loc (peek-token))))
	     (values
		(instantiate::J2SDecl
		   (binder 'param)
		   (loc loc)
		   (id id)
		   (_scmid id))
		(object-literal #t))))
	 ((LBRACKET)
	  (let ((id (string->symbol (format "%~a" idx)))
		(loc (token-loc (peek-token))))
	     (values
		(instantiate::J2SDecl
		   (binder 'param)
		   (loc loc)
		   (id id)
		   (_scmid id))
		(array-literal #f))))
	 (else
	  (parse-error "Unexpected token" (consume-any!)))))

   (define (consume-rest-param!)
      (let* ((token (consume-token! 'ID))
	     (loc (token-loc token)))
	 (instantiate::J2SDecl
	    (binder 'param)
	    (loc loc)
	    (usage '(rest))
	    (id (token-value token)))))
      
   (define (function-params)
      (push-open-token (consume-token! 'LPAREN))
      (case (peek-token-type)
	 ((RPAREN)
	  (consume-any!)
	  (pop-open-token)
	  (values '() '()))
	 ((DOTS)
	  (consume-any!)
	  (let ((param (consume-rest-param!)))
	     (consume-token! 'RPAREN)
	     (pop-open-token)
	     (values (list param) '())))
	 (else
	  (multiple-value-bind (param arg)
	     (consume-param! 0)
	     (let loop ((rev-params (list param))
			(rev-args (if arg (list arg) '()))
			(idx 1))
		(if (eq? (peek-token-type) 'COMMA)
		    (begin
		       (consume-any!)
		       (if (eq? (peek-token-type) 'DOTS)
			   (begin
			      (consume-any!)
			      (let ((param (consume-rest-param!)))
				 (consume! 'RPAREN)
				 (pop-open-token)
				 (values
				    (reverse! (cons param rev-params))
				    (reverse! rev-args))))
			   (multiple-value-bind (param arg)
			      (consume-param! idx)
			      (loop (cons param rev-params)
				 (if arg (cons arg rev-args) rev-args)
				 (+fx idx 1)))))
		    (begin
		       (consume! 'RPAREN)
		       (pop-open-token)
		       (values
			  (reverse! rev-params)
			  (reverse! rev-args)))))))))

   (define (param-defval p::J2SDecl)
      ;; generate (if (eq? id undefined) (set! id defval))
      (when (isa? p J2SDeclInit)
	 (with-access::J2SDeclInit p (val loc)
	    (unless (or (isa? val J2SUndefined)
			(and (isa? val J2SUnresolvedRef)
			     (with-access::J2SUnresolvedRef val (id)
				(eq? id 'undefined))))
	       (let* ((rhs (instantiate::J2SUndefined
			      (loc loc)))
		      (lhs (instantiate::J2SRef
			      (loc loc)
			      (decl p)))
		      (test (instantiate::J2SBinary
			       (loc loc)
			       (op '===)
			       (lhs lhs)
			       (rhs rhs)))
		      (then (instantiate::J2SStmtExpr
			       (loc loc)
			       (expr (instantiate::J2SAssig
					(loc loc)
					(lhs (duplicate::J2SRef lhs))
					(rhs val)))))
		      (else (instantiate::J2SNop
			       (loc loc))))
		  (instantiate::J2SIf
		     (loc loc)
		     (test test)
		     (then then)
		     (else else)))))))
   
   (define (fun-body-params-defval params::pair-nil)
      (filter-map param-defval params))

   (define (fun-body params::pair-nil args mode)
      (let ((cmode current-mode))
	 (set! current-mode mode)
	 (unwind-protect
	    (let ((token (push-open-token (consume-token! 'LBRACE))))
	       (let ((loc (current-loc)))
		  (let loop ((rev-ses '())
			     (first #t))
		     (if (eq? (peek-token-type) 'RBRACE)
			 (let ((etoken (consume-any!)))
			    (pop-open-token)
			    (destructure-fun-params params args
			       (instantiate::J2SBlock
				  (loc (token-loc token))
				  (endloc (token-loc etoken))
				  (nodes (append (fun-body-params-defval params)
					    (reverse! rev-ses))))))
			 (let ((el (source-element)))
			    (source-element-mode! el)
			    (loop (cons el rev-ses) #f))))))
	    (set! current-mode cmode))))

   (define (clazz declaration?)
      (let* ((loc (current-loc))
	     (token (consume-token! 'class))
	     (id (when (or declaration? (eq? (peek-token-type) 'ID))
		    (consume-token! 'ID)))
	     (cname (when id (token-value id)))
	     (extends (if (eq? (peek-token-type) 'extends)
			  (begin
			     (consume-token! 'extends)
			     (assig-expr #f #f))
			  (J2SUndefined)))
	     (lbrace (push-open-token (consume-token! 'LBRACE))))
	 (let loop ((rev-ses '()))
	    (case (peek-token-type)
	       ((RBRACE)
		(let ((etoken (consume-any!)))
		   (pop-open-token)
		   (let ((clazz (instantiate::J2SClass
				   (endloc (token-loc etoken))
				   (name cname)
				   (loc (token-loc token))
				   (super extends)
				   (elements (reverse! rev-ses)))))
		      (cond
			 (declaration?
			  (let ((vdecl (instantiate::J2SDeclInit
					  (id (token-value id))
					  (loc loc)
					  (binder 'class)
					  (val clazz)))
				(cdecl (instantiate::J2SDeclClass
					  (loc (token-loc id))
					  (id (token-value id))
					  (immutable #t)
					  (writable #f)
					  (ronly #t)
					  (scope 'global)
					  (binder 'let)
					  (val clazz))))
			     (with-access::J2SClass clazz (decl)
				(set! decl cdecl))
			     (instantiate::J2SVarDecls
				(loc loc)
				(decls (list vdecl)))))
			 (id
			  (let ((cdecl (instantiate::J2SDeclClass
					  (loc (token-loc id))
					  (id (token-value id))
					  (immutable #t)
					  (writable #f)
					  (ronly #t)
					  (scope 'global)
					  (binder 'let)
					  (val clazz))))
			     (with-access::J2SClass clazz (decl)
				(set! decl cdecl)
				clazz)))
			 (else
			  clazz)))))
	       ((SEMICOLON)
		(consume-any!)
		(loop rev-ses))
	       (else
		(loop
		   (cons (class-element (not (isa? extends J2SUndefined)))
		      rev-ses)))))))

   (define (class-element super?)
      (if (eq? (peek-token-type) 'static)
	  (begin
	     (consume-token! 'static)
	     (class-method #t super?))
	  (class-method #f super?)))

   (define (class-method static? super?)
      (let* ((loc (token-loc (peek-token)))
	     (gen (when (eq? (peek-token-type) '*)
		     (consume-any!) '*))
	     (name-or-get (property-name)))
	 (with-this 'this loc
	    (if (isa? name-or-get J2SNode)
		(multiple-value-bind (params args)
		   (function-params)
		   (let* ((body (fun-body params args 'strict))
			  (fun (instantiate::J2SFun
				  (loc loc)
				  (thisp (current-this))
				  (params params)
				  (mode 'strict)
				  (generator gen)
				  (body body)
				  (ismethodof super?)
				  (vararg (rest-params params))))
			  (prop (instantiate::J2SDataPropertyInit
				   (loc loc)
				   (name name-or-get)
				   (val fun))))
		      (instantiate::J2SClassElement
			 (loc loc)
			 (static static?)
			 (prop prop))))
		(let ((name (property-name)))
		   (multiple-value-bind (params args)
		      (function-params)
		      (let* ((body (fun-body params args 'strict))
			     (fun (instantiate::J2SFun
				     (loc loc)
				     (thisp (current-this))
				     (params params)
				     (mode 'strict)
				     (generator gen)
				     (body body)
				     (vararg (rest-params params))))
			     (prop (instantiate::J2SAccessorPropertyInit
				      (loc loc)
				      (name name)
				      (get (if (eq? (token-value name-or-get) 'get)
					       fun
					       (instantiate::J2SUndefined
						  (loc loc))))
				      (set (if (eq? (token-value name-or-get) 'set)
					       fun
					       (instantiate::J2SUndefined
						  (loc loc)))))))
			 (instantiate::J2SClassElement
			    (loc loc)
			    (static static?)
			    (prop prop)))))))))
   
   (define (expression::J2SExpr in-for-init? destructuring?)
      (let ((assig (assig-expr in-for-init? destructuring?)))
	 (let loop ((rev-exprs (list assig)))
	    (if (eq? (peek-token-type) 'COMMA)
		(begin
		   (consume-any!)
		   (loop (cons (assig-expr in-for-init? #f) rev-exprs)))
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
   
   (define (assig-expr in-for-init? destructuring?)
      
      (define (with-out-= op=)
	 (let* ((s= (symbol->string op=))
		(s=-length (string-length s=))
		(s (substring s= 0 (- s=-length 1)))
		(op (string->symbol s)))
	    op))
      
      (let ((lhs (cond-expr in-for-init?)))
	 (if (assig-operator? (peek-token-type))
	     (let* ((op (consume-any!))
		    (rhs (assig-expr in-for-init? #f)))
		(cond
;* 		   ((and (eq? op '=) (isa? lhs J2SRef))               */
;* 		    (instantiate::J2SAssig                             */
;* 		       (loc (token-loc op))                            */
;* 		       (lhs lhs)                                      */
;* 		       (rhs rhs)))                                     */
		   ((eq? (car op) '=)
		    (cond
;* 		       ((isa? lhs J2SAccess)                          */
;* 			`(instantiate::J2SAssig                        */
;* 			    (lhs ,lhs)                                */
;* 			    (rhs ,rhs)))                               */
		       ((or (isa? lhs J2SArray) (isa? lhs J2SObjInit))
			(let* ((loc (token-loc op))
			       (endloc loc)
			       (objectp (isa? lhs J2SObjInit))
			       (decl (J2SDeclInit '(init ref get) (gensym '%obj)
					(J2SUndefined)))
			       (inits (j2s-destructure lhs decl #f))
			       (bindings (filter (lambda (i)
						    (isa? i J2SDecl))
					    inits))
			       (nodes (filter-map (lambda (i)
						     (unless (isa? i J2SDecl)
							(J2SStmtExpr i)))
					 inits)))
			   (with-access::J2SDeclInit decl (binder val)
			      (set! binder 'let-opt)
			      (set! val (instantiate::J2SDProducer
					   (loc loc)
					   (size (if objectp -1 (length inits)))
					   (type (if objectp 'object 'array))
					   (decl decl)
					   (expr rhs))))
			   (J2SBindExit #f
			      (J2SBlock*
				 (cons
				    (instantiate::J2SVarDecls
				       (loc loc)
				       (decls (cons decl bindings)))
				    (append nodes
				       (list
					  (J2SStmtExpr (J2SRef decl)))))))))
		       (else
			(instantiate::J2SAssig
			   (loc (token-loc op))
			   (lhs lhs)
			   (rhs rhs)))))
		   (else
		    (instantiate::J2SAssigOp
		       (loc (token-loc op))
		       (lhs lhs)
		       (op (with-out-= (car op)))
		       (rhs rhs)))))
	     lhs)))
   
   (define (cond-expr in-for-init?)
      (let ((expr (binary-expr in-for-init? #t))
	    (token (peek-token)))
	 (if (eq? (token-tag token) '?)
	     (let* ((ignore-? (consume-any!))
		    (then (assig-expr #f #f))
		    (ignore-colon (consume! ':))
		    (else (assig-expr in-for-init? #f)))
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

   ;; left-associative binary expressions
   (define (binary-expr in-for-init? destructuring?)
      (let binary-aux ((level 1))
	 (if (> level 10)
	     (unary destructuring?)
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
				   (rhs (binary-aux (+fx level 1)))))))
		      (else
		       expr)))))))
   
   (define (unary destructuring?)
      (case (peek-token-type)
	 ((++ --)
	  (let* ((token (consume-any!))
		 (expr (unary #f))
		 (loc (token-loc token)))
	     (if (or (isa? expr J2SUnresolvedRef)
		     (isa? expr J2SAccess)
		     (isa? expr J2SParen))
		 (let* ((op (if (eq? (token-tag token) '++) '+ '-))
			(rhs (instantiate::J2SBinary
				(loc loc)
				(op op)
				(lhs expr)
				(rhs (instantiate::J2SNumber
					(loc loc)
					(val 1))))))
		    ;; see POSTFIX nodes for RHS 
		    (instantiate::J2SPrefix
		       (loc loc)
		       (lhs expr)
		       (rhs (dup-expr rhs))
		       (op (token-tag token))))
		 (parse-token-error
		    "Invalid left-hand side expression in prefix operation"
		    token))))
	 ((delete)
	  (let ((token (consume-any!))
		(expr (unary #f)))
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
	 ((void typeof ~ !)
	  (let ((token (consume-any!)))
	     (instantiate::J2SUnary
		(loc (token-loc token))
		(op (token-tag token))
		(expr (unary #f)))))
	 ((+ -)
	  (let ((token (consume-any!))
		(expr (unary #f)))
	     (if (isa? expr J2SNumber)
		 (with-access::J2SNumber expr (val)
		    (duplicate::J2SNumber expr
		       (loc (token-loc token))
		       (val (if (eq? (token-tag token) '+)
				(if (= val 0) 0.0 val)
				(if (= val 0) -0.0 (- val))))))
		 (instantiate::J2SUnary
		    (loc (token-loc token))
		    (op (token-tag token))
		    (expr expr)))))
	 (else
	  (postfix (token-loc (peek-token)) destructuring?))))

   (define (postfix loc destructuring?)
      (let ((expr (lhs loc destructuring?)))
	 (if (not (at-new-line-token?))
	     (case (peek-token-type)
		((++ --)
		 (let ((token (consume-any!)))
		    (if (or (isa? expr J2SUnresolvedRef)
			    (isa? expr J2SAccess)
			    (isa? expr J2SParen))
			(let* ((op (if (eq? (token-tag token) '++) '+ '-))
			       (rhs (instantiate::J2SBinary
				       (loc loc)
				       (op op)
				       (lhs expr)
				       (rhs (instantiate::J2SNumber
					       (loc loc)
					       (val 1))))))
			   ;; the compiler uses the rhs part only when
			   ;; it computes static analysis (e.g., type inference)
			   ;; of the AST
			   (instantiate::J2SPostfix
			      (loc loc)
			      (lhs (dup-expr expr))
			      (rhs rhs)
			      (op (token-tag token))))
			(parse-token-error
			   "Invalid left-hand side expression in postfix operation"
			   token))))
		(else
		 expr))
	     expr)))
   
   ;; we start by getting all news (new-expr)
   ;; the remaining accesses and calls are then caught by the access-or-call
   ;; invocation allowing call-parenthesis.
   (define (lhs loc destructuring?)
      (access-or-call (new-expr loc destructuring?) loc #t))
   
   (define (new-expr loc destructuring?)
      (case (peek-token-type)
	 ((new)
	  (let* ((ignore (consume-any!))
		 (clazz (new-expr (token-loc ignore) #f))
		 (args (if (eq? (peek-token-type) 'LPAREN)
			   (arguments)
			   '())))
	     (instantiate::J2SNew
		(loc (token-loc ignore))
		(clazz clazz)
		(args args))))
	 ((yield await)
	  (yield-expr))
	 (else
	  (access-or-call (primary destructuring?) loc #f))))

   (define (yield-expr)
      (let ((loc (token-loc (consume-any!)))
	    (gen (when (eq? (peek-token-type) '*)
		    (consume-any!)
		    #t)))
	 (cond
	    ((or (case (peek-token-type)
		    ((EOF ERROR SEMICOLON RPAREN RBRACKET) #t)
		    (else #f))
		 (at-new-line-token?))
	     (instantiate::J2SYield
		(loc loc)
		(generator gen)
		(expr (instantiate::J2SUndefined
			 (loc loc)))))
	    (else
	     (let ((expr (assig-expr #f #f)))
		(instantiate::J2SYield
		   (loc loc)
		   (generator gen)
		   (expr expr)))))))
   
   (define (tag-call-arguments loc)
      (let* ((exprs (template-expressions #t))
	     (strs (filter (lambda (e) (isa? e J2SString)) exprs))
	     (strse (map (lambda (s::J2SString)
			    (with-access::J2SString s (val)
			       (duplicate::J2SString s
				  (val (token-value
					  (j2s-escape-js-string val input-port))))))
		       strs))
	     (vals (filter-map (lambda (e)
				  (when (cell? e)
				     (cell-ref e)))
		      exprs)))
	 (cons (instantiate::J2SCall
		  (loc loc)
		  (fun (instantiate::J2SHopRef
			  (loc loc)
			  (id 'js-template-raw)))
		  (thisarg (list (J2SUndefined)))
		  (args (list (instantiate::J2SArray
				 (loc loc)
				 (exprs strse)
				 (len (length strs)))
			   (instantiate::J2SArray
			      (loc loc)
			      (exprs strs)
			      (len (length strs)))
			   (instantiate::J2SHopRef
			      (loc loc)
			      (id '%this)))))
	    vals)))
	      
   (define (access-or-call expr loc call-allowed?)
      (let loop ((expr expr))
	 (case (peek-token-type)
	    ((LBRACKET)
	     (let* ((ignore (push-open-token (consume-any!)))
		    (field (expression #f #f))
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
			(eq? key 'service)
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
			  ;; don't explicit thisarg as the code generator
			  ;; this check the lhs access to find the proper
			  ;; receiver
			  (thisarg (list (J2SUndefined)))
			  (args (arguments))))
		 expr))
	    ((TSTRING TEMPLATE)
	     (instantiate::J2SCall
		(loc loc)
		(fun expr)
		(thisarg (list (J2SUndefined)))
		(args (tag-call-arguments loc))))
	    (else
	     expr))))
   
   (define (arguments)
      (push-open-token (consume-token! 'LPAREN))
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-args (list (assig-expr #f #f))))
	     (if (eq? (peek-token-type) 'RPAREN)
		 (begin
		    (consume-any!)
		    (pop-open-token)
		    (reverse! rev-args))
		 (let* ((ignore (consume! 'COMMA))
			(arg (assig-expr #f #f)))
		    (loop (cons arg rev-args)))))))

   (define (xml-expression tag delim)
      (html-parser input-port (cons* :tilde-level tilde-level conf) tag delim))

   (define (doctype-expression)
      (html-parser input-port (cons* :tilde-level tilde-level conf)))

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
		  ((CTAG)
		   (if (eq? (token-value (peek-token)) '<script>)
		       (if (eq? (config-get conf :parser #f) 'script-expression)
			   (instantiate::J2SSeq
			      (loc (token-loc token))
			      (nodes (reverse! rev-stats)))
			   (loop (cons (statement) rev-stats)))
		       (loop (cons (statement) rev-stats))))
		  (else
		   (loop (cons (statement) rev-stats))))))))

   (define (tilde-expression)
      (let ((token (consume-any!)))
	 (instantiate::J2STilde
	    (loc (token-loc token))
	    (stmt (tilde token)))))

   (define (dollar-expression)
      (let* ((ignore (consume-any!))
	     (expr (expression #f #f))
	     (ignore-too (consume! 'RBRACE)))
	 expr))

   (define (template-expressions::pair cellp::bool)

      (define (block->expresion stmt)
	 (when (isa? stmt J2SBlock)
	    (with-access::J2SBlock stmt (nodes)
	       (when (and (pair? nodes) (null? (cdr nodes)))
		  (when (isa? (car nodes) J2SStmtExpr)
		     (with-access::J2SStmtExpr (car nodes) (expr)
			expr))))))
      
      (let loop ((tok (consume-any!))
		 (vals '()))
	 (let ((val (instantiate::J2SString
		       (loc (token-loc tok))
		       (val (token-value tok)))))
	    (case (token-tag tok)
	       ((TSTRING)
		(reverse! (cons val vals)))
	       ((TEMPLATE)
		(let* ((stmt (statement))
		       (expr (block->expresion stmt)))
		   (if expr
		       (loop (read/rp (j2s-template-lexer) input-port)
			  (cons* (if cellp (make-cell expr) expr) val vals))
		       (parse-node-error "Expression expected" stmt))))
	       (else
		(parse-token-error
		   (format "Invalid template string (~a)" (token-tag tok))
		   tok))))))
   
   (define (template-expression)
      (instantiate::J2STemplate
	 (loc (token-loc (peek-token)))
	 (exprs (template-expressions #f))))

   (define (number val conf)
      (cond
	 ((flonum? val)
	  val)
	 ((or (>llong (fixnum->llong val) (conf-max-int conf))
	      (<llong (fixnum->llong val) (conf-min-int conf)))
	  (fixnum->flonum val))
	 (else
	  val)))
	      
   (define (primary destructuring?)
      (case (peek-token-type)
	 ((PRAGMA)
	  (jspragma))
	 ((function)
	  (function-expression))
	 ((service)
	  (service-expression))
	 ((async)
	  (async-expression))
	 ((class)
	  (class-expression))
	 ((this)
	  (instantiate::J2SThis
	     (decl (current-this))
	     (loc (token-loc (consume-any!)))))
	 ((super)
	  (let* ((tok (consume-any!))
		 (loc (token-loc tok)))
	     (unless (memq (peek-token-type) '(DOT LPAREN LBRACKET))
		(parse-token-error "'super' keyword unexpected here" tok))
	     (instantiate::J2SSuper
		(decl (current-this))
		(loc loc))))
	 ((ID RESERVED)
	  (let ((token (consume-any!)))
	     (if (eq? (peek-token-type) '=>)
		 (arrow-function (list token) (token-loc token))
		 (instantiate::J2SUnresolvedRef
		    (loc (token-loc token))
		    (id (token-value token))))))
	 ((HOP)
	  (let ((token (consume-token! 'HOP)))
	     (instantiate::J2SHopRef
		(loc (token-loc token))
		(id (token-value token)))))
	 ((LPAREN)
	  (let ((token (push-open-token (consume-any!))))
	     (if (eq? (peek-token-type) 'RPAREN)
		 (let ((tok (consume-any!)))
		    (pop-open-token)
		    (if (eq? (peek-token-type) '=>)
			;; zero-argument arrow function
			(arrow-function '() (token-loc token))
			(parse-token-error "Unexpected token" tok)))
		 (let ((expr (expression #f #t)))
		    (consume! 'RPAREN)
		    (pop-open-token)
		    (if (eq? (peek-token-type) '=>)
			(cond
			   ((isa? expr J2SAssig)
			    (arrow-function (list expr) (token-loc token)))
			   ((isa? expr J2SUnresolvedRef)
			    (arrow-function (list expr) (token-loc token)))
			   ((isa? expr J2SSequence)
			    (with-access::J2SSequence expr (exprs)
			       (arrow-function exprs (token-loc token))))
			   ((or (isa? expr J2SObjInit) (isa? expr J2SArray))
			    (arrow-function (list expr) (token-loc token)))
			   (else
			    (parse-node-error "bad arrow parameter" expr)))
			(instantiate::J2SParen
			   (loc (token-loc token))
			   (expr expr)))))))
	 ((LBRACKET)
	  (array-literal destructuring?))
	 ((LBRACE)
	  (object-literal destructuring?))
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
		(val (number (token-value token) conf)))))
	 ((OCTALNUMBER)
	  (let ((token (consume-token! 'OCTALNUMBER)))
	     (instantiate::J2SOctalNumber
		(loc (token-loc token))
		(val (number (token-value token) conf)))))
	 ((STRING TSTRING)
	  (let ((token (consume-any!)))
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
	 ((TEMPLATE)
	  (template-expression))
	 ((EOF)
	  (parse-token-error "Unexpected end of file" (peek-token)))
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
	  (xml-expression (consume-any!) #f))
	 ((HTML)
	  (let* ((tag (consume-any!))
		 (loc (token-loc tag)))
	     (instantiate::J2SCall
		(loc loc)
		(fun (j2s-tag->expr tag #t))
		(thisarg (list (J2SUndefined)))
		(args '()))))
	 ((HTMLCOMMENT)
	  (let* ((tag (consume-any!))
		 (loc (token-loc tag)))
	     (instantiate::J2SCall
		(loc (token-loc tag))
		(fun (j2s-tag->expr (make-token tag '<!--> loc) #t))
		(thisarg (list (J2SUndefined)))
		(args (list (instantiate::J2SNativeString
			       (loc loc)
			       (val (token-value tag))))))))
	 ((OHTML)
	  (xml-expression (consume-any!) #t))
	 ((DOCTYPE)
	  (consume-any!)
	  (doctype-expression))
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
			   (expr (expression #f #f))
			   (ignore-too (consume! 'RBRACE)))
		       (instantiate::J2SDollar
			  (loc (token-loc ignore))
			  (node expr)))))
	      (parse-token-error
		 "Invalid ${ ... } statement"
		 (consume-any!))))
	 (else
	  (parse-token-error "Unexpected token" (peek-token)))))
   
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
	     (parse-token-error "Unexpected token" str))))
   
   (define (array-literal destructuring?)
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
	       ((DOTS)
		(let* ((token (consume-any!))
		       (lhs (cond-expr #f))
		       (dots (instantiate::J2SDots
				(loc (token-loc token))
				(lhs lhs)))
		       (rb (consume-token! 'RBRACKET)))
		   (pop-open-token)
		   (instantiate::J2SArray
		      (loc (token-loc token))
		      (exprs (reverse! (cons* dots rev-els)))
		      (len (+ 1 length)))))
	       (else
		(let ((array-el (assig-expr #f #f)))
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
		       (parse-token-error "Unexpected token"
			  (consume-any!))))))))))

   (define (property-name)
      (case (peek-token-type)
	 ;; IDs are automatically transformed to strings.
	 ((ID RESERVED service)
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
		(val (number (token-value token) conf)))))
	 ((OCTALNUMBER)
	  (let ((token (consume-token! 'OCTALNUMBER)))
	     (instantiate::J2SOctalNumber
		(loc (token-loc token))
		(val (number (token-value token) conf)))))
	 ((true false null)
	  (let ((token (consume-any!)))
	     (instantiate::J2SString
		(loc (token-loc token))
		(val (symbol->string (token-value token))))))
	 ((LBRACKET)
	  (let* ((token (push-open-token (consume-token! 'LBRACKET)))
		 (expr (expression #f #f)))
	     (consume! 'RBRACKET)
	     (pop-open-token)
	     expr))
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
   
   (define (object-literal destructuringp::bool)
      
      (define (find-prop name props)
	 (find (lambda (prop)
		  (when (isa? prop J2SAccessorPropertyInit)
		     (with-access::J2SAccessorPropertyInit prop ((pname name))
			(with-access::J2SString pname (val)
			   (string=? val name)))))
	    props))
      
      (define (property-accessor tokname name props)
	 (with-this 'this (token-loc tokname)
	    (let ((id (consume-any!)))
	       (multiple-value-bind (params args)
		  (function-params)
		  (let* ((body (fun-body params args current-mode))
			 (mode (or (javascript-mode body) current-mode))
			 (loc (token-loc tokname))
			 (fun (instantiate::J2SFun
				 (mode mode)
				 (loc loc)
				 (thisp (current-this))
				 (params params)
				 (vararg (rest-params params))
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
				(parse-token-error "Wrong property"
				   (peek-token)))
			    (if (isa? set J2SUndefined)
				(set! set fun)
				(parse-token-error "Wrong property"
				   (peek-token))))
			;; return a prop only if new
			(unless oprop prop)))))))

      (define (dynamic-property-accessor loc propname name props)
	 (with-this 'this loc
	    (multiple-value-bind (params args)
	       (function-params)
	       (let* ((body (fun-body params args current-mode))
		      (mode (or (javascript-mode body) current-mode))
		      (fun (instantiate::J2SFun
			      (mode mode)
			      (loc loc)
			      (thisp (current-this))
			      (params params)
			      (vararg (rest-params params))
			      (body body)))
		      (prop (instantiate::J2SAccessorPropertyInit
			       (loc loc)
			       (get (instantiate::J2SUndefined
				       (loc loc)))
			       (set (instantiate::J2SUndefined
				       (loc loc)))
			       (name propname))))
		  (with-access::J2SAccessorPropertyInit prop (get set)
		     (if (eq? name 'get)
			 (if (isa? get J2SUndefined)
			     (set! get fun)
			     (parse-token-error "Wrong property" (peek-token)))
			 (if (isa? set J2SUndefined)
			     (set! set fun)
			     (parse-token-error "Wrong property" (peek-token))))
		     ;; return a prop only if new
		     prop)))))
      
      (define (property-init props)
	 (let* ((token (peek-token))
		(tokname (property-name))
		(name (when (pair? tokname) (cdr tokname))))
	    (case name
	       ((get set)
		(case (peek-token-type)
		   ((ID RESERVED service)
		    (property-accessor tokname name props))
		   ((:)
		    (let* ((ignore (consume-any!))
			   (loc (token-loc ignore))
			   (val (assig-expr #f #f)))
		       (instantiate::J2SDataPropertyInit
			  (loc loc)
			  (name (instantiate::J2SString
				   (loc loc)
				   (val (symbol->string name))))
			  (val val))))
		   ((LBRACKET)
		    (consume-any!)
		    (let ((expr (expression #f #f)))
		       (consume-token! 'RBRACKET)
		       (dynamic-property-accessor (token-loc token) expr name props)))
		   ((LPAREN)
		    (instantiate::J2SDataPropertyInit
		       (loc (token-loc token))
		       (name (instantiate::J2SString
				(loc (token-loc token))
				(val (symbol->string (token-value token)))))
		       (val (function #f token '__proto__))))
		   (else
		    (if (j2s-reserved-id? (peek-token-type))
			(property-accessor tokname name props)
			(parse-token-error "Wrong property name" (peek-token))))))
	       (else
		(let* ((loc (token-loc token))
		       (val (case (peek-token-type)
			       ((COMMA RBRACE)
				(if (eq? (token-tag token) 'ID)
				    (instantiate::J2SUnresolvedRef
				       (loc loc)
				       (id (token-value token)))
				    (parse-token-error "Unexpected token"
				       token)))
			       ((LPAREN)
				(function #f token '__proto__))
			       ((:)
				(consume-any!)
				(assig-expr #f #f))
			       ((=)
				(if destructuringp
				    (begin
				       (consume-any!)
				       (J2SBinary 'OR
					  (J2SUnresolvedRef (token-value token))
					  (assig-expr #f #f)))
				    (parse-token-error "Unexpected token"
				       (peek-token))))
			       (else
				(parse-token-error "Unexpected token"
				   (peek-token))))))
		   (instantiate::J2SDataPropertyInit
		      (loc loc)
		      (name tokname)
		      (val val)))))))
      
      (push-open-token (consume-token! 'LBRACE))
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
	       (source-map (config-get conf :source-mapping-url source-map))
	       (path (config-get conf :filename (abspath)))
	       (main (config-get conf :module-main #f))
	       (name (config-get conf :module-name #f))
	       (mode mode)
	       (nodes (map! (lambda (n) (dialect n mode conf)) nodes))))))
   
   (define (eval mode)
      (set! tilde-level 0)
      (with-access::J2SBlock (source-elements) (loc endloc nodes)
	 (let ((mode (or mode (nodes-mode nodes))))
	    (instantiate::J2SProgram
	       (loc loc)
	       (endloc endloc)
	       (path (config-get conf :filename (abspath)))
	       (name (config-get conf :module-name #f))
	       (mode mode)
	       (nodes (map! (lambda (n) (dialect n mode conf)) nodes))))))

   (define (eval-strict)
      (set! current-mode 'strict)
      (eval 'strict))

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
		   (nodes (list (dialect el 'normal conf)))))
	     el)))

   (case (config-get conf :parser #f)
      ((script-expression) (with-tilde tilde-expression))
      ((tilde-expression) (with-tilde tilde-expression))
      ((dollar-expression) (dollar-expression))
      ((module) (program #f))
      ((repl) (repl))
      ((eval) (eval #f))
      ((eval-strict) (eval-strict))
      ((client-program) (program #t))
      (else (program #f))))

;*---------------------------------------------------------------------*/
;*    j2s-tag->expr ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-tag->expr tag::pair force-case)
   (let ((s (symbol->string! (cdr tag))))
      (let loop ((type (substring s 1 (-fx (string-length s) 1)))
		 (fc force-case))
	 (let ((i (string-index-right type #\.)))
	    (if i
		;; a property access
		(let ((s (substring type (+fx i 1) (string-length type))))
		   (instantiate::J2SAccess
		      (loc (token-loc tag))
		      (obj (loop (substring type 0 i) #f))
		      (field (instantiate::J2SString
				(loc (token-loc tag))
				(val (if fc (string-upcase s) s))))))
		;; a tag name
		(instantiate::J2SUnresolvedRef
		   (loc (token-loc tag))
		   (id (string->symbol (if fc (string-upcase type) type)))))))))

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
   
   (define (stricter-mode mode m)
      (cond
	 ((not mode) m)
	 ((not m) mode)
	 ((eq? mode 'hopscript) mode)
	 ((eq? m 'hopscript) m)
	 ((eq? mode 'strict) mode)
	 ((eq? m 'strict) m)
	 (else m)))

   (let loop ((nodes nnodes)
	      (mode #f))
      (if (pair? nodes)
	  (let ((m (javascript-mode (car nodes))))
	     (cond
		((symbol? m)
		 (when (eq? m 'strict)
		    (for-each check-octal-string nnodes))
		 (loop (cdr nodes) (stricter-mode mode m)))
		(m
		 (loop (cdr nodes) mode))
		(else
		 mode)))
	  mode)))

;*---------------------------------------------------------------------*/
;*    parse-node-error ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-node-error msg node::J2SNode)
   (with-access::J2SNode node (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (raise
	     (instantiate::&io-parse-error
		(proc "hopscript")
		(msg msg)
		(obj (j2s->list node))
		(fname fname)
		(location loc))))
	 (else
	  (raise
	     (instantiate::&io-parse-error
		(proc "hopscript")
		(msg msg)
		(obj (j2s->list node))))))))

;*---------------------------------------------------------------------*/
;*    destructure-fun-params ...                                       */
;*---------------------------------------------------------------------*/
(define (destructure-fun-params params::pair-nil args::pair-nil body::J2SBlock)
   (if (find (lambda (a) (isa? a J2SObjInit)) args)
       (with-access::J2SBlock body (loc nodes)
	  (let* ((decls (append-map (lambda (p a)
				       (if (isa? a J2SUnresolvedRef)
					   '()
					   (j2s-destructure a p #t)))
			   params args))
		 (vdecls (instantiate::J2SVarDecls
			    (loc loc)
			    (decls decls))))
	     (check-unique (append params decls) parse-node-error)
	     (set! nodes (cons vdecls nodes))
	     body))
       body))

;*---------------------------------------------------------------------*/
;*    j2s-destructure ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-destructure lhs::J2SExpr decl::J2SDecl bind::bool)
   (with-access::J2SExpr lhs (loc)
      (destructure-path lhs decl '* bind)))

;*---------------------------------------------------------------------*/
;*    destructure-path ...                                             */
;*    -------------------------------------------------------------    */
;*    Useful information located at:                                   */
;*      http://exploringjs.com/es6/ch_destructuring.html               */
;*---------------------------------------------------------------------*/
(define (destructure-path::pair-nil lhs::J2SExpr decl::J2SDecl path bind::bool)

   (define (J2SDeclAssig loc id val::J2SExpr)
      (if bind
	  (instantiate::J2SDeclInit
	     (loc loc)
	     (id id)
	     (val val))
	  (instantiate::J2SAssig
	     (loc loc)
	     (lhs (J2SUnresolvedRef id))
	     (rhs val))))

   (define (destructure-unref lhs src path)
      (with-access::J2SUnresolvedRef lhs (id loc)
	 (list (J2SDeclAssig loc id (J2SDConsumer decl path src)))))

   (define (destructure-access lhs src path)
      (if (not bind)
	  (with-access::J2SAccess lhs (loc)
	     (list (J2SAssig lhs src)))
	  (parse-node-error "Bad access declaration" lhs)))

   (define (destructure-obj lhs::J2SExpr tmp path)
      (with-access::J2SObjInit lhs (inits)
	 (append-map (lambda (init)
			(cond
			   ((isa? init J2SDataPropertyInit)
			    (with-access::J2SDataPropertyInit init (name val loc)
			       (cond
				  ((isa? val J2SUnresolvedRef)
				   ;; { ...., id, ... }
				   ;; { ...., id:alias, ... }
				   (destructure val (J2SAccess tmp name)
				      `(get ,path ,name) bind))
				  ((isa? val J2SBinary)
				   ;; { ...., id = def, ... }
				   (with-access::J2SBinary val (lhs rhs)
				      (set! lhs (J2SAccess tmp name))
				      (with-access::J2SString name ((id val))
					 (destructure
					    (J2SUnresolvedRef
					       (string->symbol id))
					    val
					    `(get-default ,path ,rhs)
					    bind))))
				  ((isa? val J2SAssig)
				   ;; { ...., id:alias = def, ... }
				   (with-access::J2SAssig val (lhs rhs)
				      (destructure lhs
					 (J2SBinary 'OR
					    (J2SAccess tmp name) rhs)
					 `(get-alias-default ,rhs)
					 bind)))
				  (else
				   ;; { ..., id:pat, ... }
				   (destructure val (J2SAccess tmp name)
				      `(get ,path ,name) bind)))))
			   (else
			    (parse-node-error "Bad object argument" init))))
	    inits)))

   (define (destructure-array lhs::J2SExpr tmp path)

      (define (destructure-aref e i tmp path)
	 (cond
	    ((isa? e J2SArrayAbsent)
	     ;; [ ..., , ... ]
	     '())
	    ((isa? e J2SDots)
	     ;; [ ..., ... id ]
	     (with-access::J2SDots e (loc lhs)
		(let ((decl (instantiate::J2SDeclInit
			       (binder 'let)
			       (loc loc)
			       (id (gensym '%dots))
			       (val (J2SDConsumer decl path
				       (J2SCall (J2SAccess tmp
						   (J2SString "slice"))
					  (J2SNumber i)))))))
		   (cons decl
		      (destructure lhs (J2SRef decl)
			 `(spread ,path) bind)))))
	    ((isa? e J2SAssig)
	     ;; [ ...., id = def, ... ]
	     (with-access::J2SAssig e (lhs rhs loc)
		(destructure lhs
		   (J2SBinary 'OR (J2SAccess tmp (J2SNumber i)) rhs)
		   `(get-alias-default ,rhs)
		   bind)))
	    (else
	     ;; [ ..., id, ... ]
	     (with-access::J2SExpr e (loc)
		(destructure e (J2SAccess tmp (J2SNumber i))
		   `(aref ,path ,i) bind)))))
      
      (with-access::J2SArray lhs (exprs len)
	 (append-map (lambda (e i) (destructure-aref e i tmp path))
	    exprs (iota len))))

   (define (destructure-tmp lhs::J2SExpr src path destructure/tmp)
      (if (isa? src J2SUnresolvedRef)
	  (destructure/tmp lhs src path)
	  (with-access::J2SExpr lhs (loc)
	     (let ((decl (instantiate::J2SDeclInit
			    (binder 'let)
			    (loc loc)
			    (id (gensym '%tmp))
			    (val src))))
		(cons decl (destructure/tmp lhs (J2SRef decl) path))))))

   (define (destructure lhs src path bind)
      (cond
	 ((isa? lhs J2SUnresolvedRef)
	  (destructure-unref lhs src path))
	 ((isa? lhs J2SObjInit)
	  (destructure-tmp lhs src path destructure-obj))
	 ((isa? lhs J2SArray)
	  (destructure-tmp lhs src path destructure-array))
	 ((isa? lhs J2SAccess)
	  (destructure-access lhs src path))
	 (else
	  (parse-node-error "Bad declaration" lhs))))

   (with-access::J2SDecl decl (loc)
      (destructure lhs (J2SRef decl) path bind)))

;*---------------------------------------------------------------------*/
;*    lbrace-following? ...                                            */
;*---------------------------------------------------------------------*/
(define (lbrace-following? port)
   (read/rp
      (regular-grammar ()
	 ((: (* (or (in " \t\n") (or "\xc2\xa0"))) #\{)
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
;*    dialect ...                                                      */
;*    -------------------------------------------------------------    */
;*    Modify the AST according to the compilation dialect (which is    */
;*    specified by the MODE argument).                                 */
;*---------------------------------------------------------------------*/
(define (dialect node::J2SNode mode conf)
   ;; propage function definition modes
   (hopscript-mode-fun! node mode)
   ;; make hopscript function constant
   (hopscript-cnst-fun! node)
   (unless (memq mode '(strict hopscript ecmascript6 ecmascript2017))
      (unless (config-get conf :es6-let #f)
	 (disable-es6-let node))
      (unless (config-get conf :es6-default-value #f)
	 (disable-es6-default-value node))
      (unless (config-get conf :es6-arrow-function #f)
	 (disable-es6-arrow node))
      (unless (config-get conf :es6-rest-argument #f)
	 (disable-es6-rest-argument node)))
   node)

;*---------------------------------------------------------------------*/
;*    hopscript-mode-fun! ...                                          */
;*    -------------------------------------------------------------    */
;*    Propagate the JavaScript mode into the funtion definitions       */ 
;*---------------------------------------------------------------------*/
(define-walk-method (hopscript-mode-fun! this::J2SNode mode::symbol)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    hopscript-mode-fun! ::J2SFun ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (hopscript-mode-fun! this::J2SFun mode)
   (with-access::J2SFun this ((fmode mode) body name)
      (when (eq? fmode 'normal) (set! fmode mode))
      (hopscript-mode-fun! body fmode)
      this))

;*---------------------------------------------------------------------*/
;*    hopscript-mode-fun! ::J2SDeclFun ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (hopscript-mode-fun! this::J2SDeclFun mode)
   (with-access::J2SDeclFun this (val immutable)
      (if immutable
	  (begin
	     (hopscript-mode-fun! val mode)
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    hopscript-cnst-fun! ...                                          */
;*    -------------------------------------------------------------    */
;*    In HopScript mode, function declarations are read-only. To       */
;*    implement this, this walker siwtches J2SDeclFun ronly attribute  */
;*    to #t.                                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hopscript-cnst-fun! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    hopscript-cnst-fun! ::J2SDeclFun ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (hopscript-cnst-fun! this::J2SDeclFun)
   (with-access::J2SDeclFun this (val mode ronly writable immutable)
      (with-access::J2SFun val (mode)
	 (when (eq? mode 'hopscript)
	    (set! writable #f)
	    (set! ronly #t)))
      (if immutable
	  this
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    hopscript-cnst-fun! ::J2SFun ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (hopscript-cnst-fun! this::J2SFun)
   (with-access::J2SFun this (decl)
      (when (isa? decl J2SDeclFun)
	 (with-access::J2SDeclFun decl (immutable)
	    (when immutable
	       (hopscript-cnst-fun! decl)))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    disable-es6-let ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-let this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    disable-es6-let ::J2SFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-let this::J2SFun)
   (with-access::J2SFun this (mode name)
      (unless (memq mode '(hopscript ecmascript6 ecmascript2017))
	 (call-default-walker))))

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
      (unless (memq mode '(hopscript ecmascript6 ecmascript2017))
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

;*---------------------------------------------------------------------*/
;*    disable-es6-default-value ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-default-value this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    disable-es6-default-value ::J2SFun ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-let this::J2SFun)
   (with-access::J2SFun this (mode)
      (unless (memq mode '(hopscript ecmascript6 ecmascript2017))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    disable-es6-default-value ::J2SDeclInit ...                      */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-default-value this::J2SDeclInit)
   (when (j2s-param? this)
      (with-access::J2SDeclInit this (val id loc)
	 (unless (nodefval? val)
	    (raise
	       (instantiate::&io-parse-error
		  (proc "js-parser")
		  (msg "default parameter values disabled")
		  (obj id)
		  (fname (cadr loc))
		  (location (caddr loc))))))))

;*---------------------------------------------------------------------*/
;*    disable-es6-rest-argument ::J2SNode ...                          */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-rest-argument this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    disable-es6-rest-argument ::J2SFun ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (disable-es6-rest-argument this::J2SFun)
   (with-access::J2SFun this (vararg name loc mode)
      (if (and (eq? vararg 'rest) (not (eq? mode 'hopscript)))
	  (raise
	     (instantiate::&io-parse-error
		(proc "js-parser")
		(msg "rest arguments values disabled")
		(obj name)
		(fname (cadr loc))
		(location (caddr loc))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    dup-expr ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (dup-expr e::J2SExpr)
   e)

;*---------------------------------------------------------------------*/
;*    dup-expr ::J2SUnresolvedRef ...                                  */
;*---------------------------------------------------------------------*/
(define-method (dup-expr e::J2SUnresolvedRef)
   (duplicate::J2SUnresolvedRef e))

;*---------------------------------------------------------------------*/
;*    dup-expr ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dup-expr e::J2SAccess)
   (with-access::J2SAccess e (obj field)
      (duplicate::J2SAccess e
	 (obj (dup-expr obj))
	 (field (dup-expr field)))))

;*---------------------------------------------------------------------*/
;*    dup-expr ::J2SParen ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dup-expr e::J2SParen)
   (with-access::J2SParen e (expr)
      (duplicate::J2SParen e
	 (expr (dup-expr expr)))))

;*---------------------------------------------------------------------*/
;*    check-unique ...                                                 */
;*---------------------------------------------------------------------*/
(define (check-unique decl-list::pair-nil parse-error::procedure)
   (let loop ((l decl-list))
      (when (pair? l)
	 (with-access::J2SDecl (car l) (id)
	    (for-each (lambda (d)
			 (with-access::J2SDecl d ((oid id))
			    (when (eq? oid id)
			       (parse-error "Duplicate parameter name not allowed in this context" d))))
	       (cdr l)))
	 (loop (cdr l)))))
