;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-11 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module js-parser
   (import js-nodes
	   js-lexer)
   (export (parse::Node port::input-port next-pragma::procedure)))

(define (my-error ip msg obj token)
   (let ((l (read-line ip)))
      (let loop ((msg msg)
		 (obj obj)
		 (token token))
	 (cond
	    ((not token)
	     (error "parser" msg (format "~a \"~a\"" obj l)))
	    ((epair? token)
	     (match-case (cer token)
		((at ?fname ?loc)
		 (error/location "parser" msg (format "~a \"~a\"" obj l) fname loc))
		(else
		 (loop msg obj #f))))
	    (else
	     (loop msg obj #f))))))

(define (parse input-port next-pragma!)
   ;; fun-body at bottom of file
   
   (define *peeked-tokens* '())
   (define *previous-token-type* #unspecified) ;; including new-line
   
   
   (define (read-regexp intro-token)
      (let ((token (read/rp *Reg-exp-grammar* input-port)))
	 (if (eq? (car token) 'EOF)
	     (my-error input-port "unfinished regular expression literal" #f token))
	 (if (eq? (car token) 'ERROR)
	     (my-error input-port "bad regular-expression literal" (cdr token) token))
	 (string-append (symbol->string intro-token) (cdr token))))
   
   (define (peek-token)
      (if (null? *peeked-tokens*)
	  (begin
	     (set! *peeked-tokens* (list (read/rp *JS-grammar* input-port)))
	     ;(tprint (car *peeked-tokens*))
	     (if (eq? (caar *peeked-tokens*)
		      'NEW_LINE)
		 (begin
		    (set! *previous-token-type* 'NEW_LINE)
		    (set! *peeked-tokens* (cdr *peeked-tokens*))
		    (peek-token))
		 (car *peeked-tokens*)))
	  (car *peeked-tokens*)))
   
   (define (token-push-back! token)
      (set! *peeked-tokens* (cons token *peeked-tokens*)))
   
   (define (peek-token-type)
      (car (peek-token)))
   
   (define (at-new-line-token?)
      (eq? *previous-token-type* 'NEW_LINE))
   
   (define (consume! type)
      (let ((token (consume-any!)))
	 (if (eq? (car token) type)
	     (cdr token)
	     (my-error input-port
		       (format "unexpected token. expected ~a got: " type)
		       (car token)
		       token))))
   
   (define (consume-statement-semicolon!)
      (cond
	 ((eq? (peek-token-type) 'SEMICOLON)
	  (consume-any!))
	 ((or (eq? (peek-token-type) 'RBRACE)
	      (at-new-line-token?)
	      (eq? (peek-token-type) 'EOF))
	  'do-nothing)
	 (else
	  (my-error input-port "unexpected token: " (peek-token) (peek-token)))))
   
   (define (consume-any!)
      (let ((res (peek-token)))
	 (set! *previous-token-type* (car res))
	 (set! *peeked-tokens* (cdr *peeked-tokens*))
	 (peek-token) ;; prepare new token.
	 res))
   
   (define (eof?)
      (eq? (peek-token-type) 'EOF))
   
   (define (program)
      (instantiate::Program
	 (body (source-elements))))
   
   (define (source-elements)
      (let loop ((rev-ses '()))
	 (if (eof?)
	     (instantiate::Block
		(stmts (reverse! rev-ses)))
	     (loop (cons (source-element) rev-ses)))))

   (define (source-element)
      (case (peek-token-type)
	 ((function) (function-declaration))
	 ((ERROR EOF)
	  (let ((t (consume-any!)))
	     (my-error input-port "eof or error" t t)))
	 (else (statement))))

   (define (statement)
      (case (peek-token-type)
	 ((LBRACE) (block))
	 ((var) (var-decl-list #f))
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
	 ;; errors will be handled in the expr-clause.
	 (else (expression-statement))))

   (define (block)
      (consume! 'LBRACE)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE)
	     (consume-any!)
	     (instantiate::Block (stmts (reverse! rev-stats))))
	    ;; errors will be handled in the statemente-clause
	    (else (loop (cons (statement) rev-stats))))))
   
   (define (var-decl-list in-for-init?)
      (consume! 'var)
      (let loop ((rev-vars (list (var in-for-init?))))
	 (case (peek-token-type)
	    ((SEMICOLON) (if (not in-for-init?)
			     (consume-any!))
			 (instantiate::Var-Decl-List
			    (vars (reverse! rev-vars))))
	    ((COMMA) (consume-any!)
		     (loop (cons (var in-for-init?) rev-vars)))
	    ((in) (cond
		     ((not in-for-init?)
		      (my-error input-port "unexpected token"
				"in"
				(peek-token)))
		     (else
		      (instantiate::Var-Decl-List
			 (vars rev-vars)))))
	    (else (if (and (not in-for-init?)
			   (or (at-new-line-token?)
			       (eq? (peek-token-type) 'EOF)))
		      (instantiate::Var-Decl-List
			 (vars (reverse! rev-vars)))
		      (let ((t (consume-any!)))
			 (my-error input-port "unexpected token, error or EOF"
				   (cdr t)
				   t)))))))

   (define (var in-for-init?)
      (let ((id (consume! 'ID)))
	 (case (peek-token-type)
	    ((=) (consume-any!)
		 (let ((expr (assig-expr in-for-init?)))
		    (instantiate::Init
		       (lhs (instantiate::Decl (id id)))
		       (rhs expr))))
	    (else (instantiate::Decl (id id))))))

   (define (empty-statement)
      (consume! 'SEMICOLON)
      (instantiate::NOP))

   (define (iff)
      (consume-any!) ;; the 'if'
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (let ((then (statement)))
	    (case (peek-token-type)
	       ((else) (consume-any!)
		       (let ((else (statement)))
			  (instantiate::If
			     (test test)
			     (then then)
			     (else else))))
	       (else (instantiate::If
			(test test)
			(then then)
			(else (instantiate::NOP))))))))

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
      
      (consume! 'for)
      (consume! 'LPAREN)
      (let ((first-part (init-first-part)))
	 (case (peek-token-type)
	    ((SEMICOLON) (for-init/test/incr first-part))
	    ((in) (for-in first-part)))))
   
   ;; for (init; test; incr)
   (define (for-init/test/incr init)
      (consume! 'SEMICOLON)
      (let ((test (case (peek-token-type)
		     ((SEMICOLON) #f)
		     (else (expression #f)))))
	 (consume! 'SEMICOLON)
	 (let ((incr (case (peek-token-type)
			((RPAREN) #f)
			(else (expression #f)))))
	    (consume! 'RPAREN)
	    (let* ((body (statement)))
	       (instantiate::For
		  (init init)
		  (test test)
		  (incr incr)
		  (body body))))))

   ;; for (lhs/var x in obj)
   (define (for-in lhs)
      ;; TODO: weed out bad lhs
      (consume! 'in)
      (let ((error-token (peek-token))
	    (obj (expression #f))
	    (ignore-RPAREN (consume! 'RPAREN))
	    (body (statement)))
	 (cond
	    ((isa? lhs Var-Decl-List)
	     (let ((lhs-vars (with-access::Var-Decl-List lhs (vars) vars)))
		(unless (null? (cdr lhs-vars))
		   (my-error input-port "Only one variable allowed in 'for ... in' loop"
			     (with-access::Ref (cadr lhs-vars) (id) id)
			     error-token))
		(instantiate::For-In
		   (lhs lhs)
		   (obj obj)
		   (body body))))
	    ((or (isa? lhs Sequence)
		 (isa? lhs Assig)
		 (isa? lhs Binary)
		 (isa? lhs Unary)
		 (isa? lhs Postfix))
	     (my-error input-port "Bad left-hand side in 'for ... in' loop construct"
		       (class-name (object-class lhs))
		       error-token))
	    (else
	     (instantiate::For-In
		(lhs lhs)
		(obj obj)
		(body body))))))

   (define (while)
      (consume! 'while)
      (consume! 'LPAREN)
      (let ((test (expression #f)))
	 (consume! 'RPAREN)
	 (let ((body (statement)))
	    (instantiate::While
	       (test test)
	       (body body)))))

   (define (do-while)
      (consume! 'do)
      (let ((body (statement)))
	 (consume! 'while)
	 (consume! 'LPAREN)
	 (let ((test (expression #f)))
	    (consume! 'RPAREN)
	    (consume-statement-semicolon!)
	    (instantiate::Do
	       (body body)
	       (test test)))))

   (define (continue)
      (consume! 'continue)
      (if (and (eq? (peek-token-type) 'ID)
	       (not (at-new-line-token?)))
	  (let ((id (consume! 'ID)))
	     (consume-statement-semicolon!)
	     (instantiate::Continue (id id)))
	  (begin
	     (consume-statement-semicolon!)
	     (instantiate::Continue (id #f)))))

   (define (break)
      (consume! 'break)
      (if (and (eq? (peek-token-type) 'ID)
	       (not (at-new-line-token?)))
	  (let ((id (consume! 'ID)))
	     (consume-statement-semicolon!)
	     (instantiate::Break (id id)))
	  (begin
	     (consume-statement-semicolon!)
	     (instantiate::Break (id #f)))))

   (define (return)
      (consume! 'return)
      (if (or (case (peek-token-type) ((EOF ERROR SEMICOLON) #t) (else #f))
	      (at-new-line-token?))
	  (begin
	     (consume-statement-semicolon!)
	     (instantiate::Return (val #f)))
	  (let ((expr (expression #f)))
	     (consume-statement-semicolon!)
	     (instantiate::Return (val expr)))))

   (define (with)
      (consume! 'with)
      (consume! 'LPAREN)
      (let ((expr (expression #f)))
	 (consume! 'RPAREN)
	 (let ((body (statement)))
	    (instantiate::With
	       (obj expr)
	       (body body)))))

   (define (switch)
      (consume! 'switch)
      (consume! 'LPAREN)
      (let ((key (expression #f)))
	 (consume! 'RPAREN)
	 (let ((cases (case-block)))
	    (instantiate::Switch
	       (key key)
	       (cases cases)))))

   (define (case-block)
      (consume! 'LBRACE)
      (let loop ((rev-cases '())
		 (default-case-done? #f))
	 (case (peek-token-type)
	    ((RBRACE) (consume-any!)
		      (reverse! rev-cases))
	    ((case) (loop (cons (case-clause) rev-cases)
			  default-case-done?))
	    ((default) (if default-case-done?
			   (error "Only one default-clause allowed"
				  (peek-token)
				  (peek-token))
			   (loop (cons (default-clause) rev-cases)
				 #t))))))
   
   (define (case-clause)
      (consume! 'case)
      (let ((expr (expression #f)))
	 (consume! ':)
	 (let ((body (switch-clause-statements)))
	    (instantiate::Case
	       (expr expr)
	       (body body)))))
   
   (define (default-clause)
      (consume! 'default)
      (consume! ':)
      (instantiate::Default
	 (body (switch-clause-statements))))

   (define (switch-clause-statements)
      (let loop ((rev-stats '()))
	 (case (peek-token-type)
	    ((RBRACE EOF ERROR default case)
	     (instantiate::Block
		(stmts (reverse! rev-stats))))
	    (else (loop (cons (statement) rev-stats))))))
   
   (define (throw)
      (consume! 'throw)
      (when (at-new-line-token?)
	 (error "throw must have a value" #f (peek-token)))
      (let ((expr (expression #f)))
	 (consume-statement-semicolon!)
	 (instantiate::Throw
	    (expr expr))))

   (define (trie)
      (consume! 'try)
      (let ((body (block)))
	 (let ((catch-part #f)
	       (finally-part #f))
	    (if (eq? (peek-token-type) 'catch)
		(set! catch-part (catch)))
	    (if (eq? (peek-token-type) 'finally)
		(set! finally-part (finally)))
	    (instantiate::Try
	       (body body)
	       (catch catch-part)
	       (finally finally-part)))))

   (define (catch)
      (consume! 'catch)
      (consume! 'LPAREN)
      (let ((id (consume! 'ID)))
	 (consume! 'RPAREN)
	 (let ((body (block)))
	    ;; not sure, if 'Param' is a really good choice.
	    ;; we'll see...
	    (instantiate::Catch
	       (exception (instantiate::Param (id id)))
	       (body body)))))

   (define (finally)
      (consume! 'finally)
      (block))

   (define (labeled-or-expr)
      (let* ((id-token (consume-any!))
	     (next-token-type (peek-token-type)))
	 [assert (id-token) (eq? (car id-token) 'ID)]
	 (token-push-back! id-token)
	 (if (eq? next-token-type  ':)
	     (labeled)
	     (expression-statement))))
   
   (define (expression-statement)
      (let ((expr (expression #f)))
	 (consume-statement-semicolon!)
	 expr))
   
   (define (labeled)
      (let ((id (consume! 'ID)))
	 (consume! ':)
	 (instantiate::Labeled
	    (id id)
	    (body (statement)))))

   (define (function-declaration)
      (function #t))
   
   (define (function-expression)
      (function #f))
   
   (define (function declaration?)
      (consume! 'function)
      (let* ((id (if (or declaration?
			 (eq? (peek-token-type) 'ID))
		     (consume! 'ID)
		     #f))
	     (params (params))
	     (body (fun-body)))
	 (if declaration?
	     (instantiate::Fun-Binding
		(lhs (instantiate::Decl (id id)))
		(rhs (instantiate::Fun  (params params) (body body))))
	     (if id
		 (instantiate::Named-Fun
		    (name (instantiate::Decl (id id)))
		    (fun (instantiate::Fun  (params params) (body body))))
		 (instantiate::Fun (params params) (body body))))))
   
   (define (params)
      (consume! 'LPAREN)
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-params (list (instantiate::Param (id (consume! 'ID))))))
	     (if (eq? (peek-token-type) 'COMMA)
		 (begin
		    (consume-any!)
		    (loop (cons (instantiate::Param (id (consume! 'ID)))
				rev-params)))
		 (begin
		    (consume! 'RPAREN)
		    (reverse! rev-params))))))
   
   (define (fun-body)
      (consume! 'LBRACE)
      (let loop ((rev-ses '()))
	 (if (eq? (peek-token-type) 'RBRACE)
	     (begin
		(consume-any!)
		(instantiate::Block (stmts (reverse! rev-ses))))
	     (loop (cons (source-element) rev-ses)))))

   (define (expression in-for-init?)
      (let ((assig (assig-expr in-for-init?)))
	 (let loop ((rev-exprs (list assig)))
	    (if (eq? (peek-token-type) 'COMMA)
		(begin
		   (consume-any!)
		   (loop (cons (assig-expr in-for-init?) rev-exprs)))
		(if (null? (cdr rev-exprs))
		    (car rev-exprs)
		    (instantiate::Sequence (exprs (reverse! rev-exprs))))))))

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
	     (let* ((op (car (consume-any!))) ;; ops are in car
		    (rhs (assig-expr in-for-init?)))
		;; TODO: weed out bad lhs exprs
		(cond
		   ((and (eq? op '=) (isa? expr Access))
		    (instantiate::Accsig
		       (lhs expr)
		       (rhs rhs)))
		   ((eq? op '=)
		    (instantiate::Vassig
		       (lhs expr)
		       (rhs rhs)))
		   ((isa? expr Access)
		    (instantiate::Accsig-op
		       (lhs expr)
		       (op (with-out-= op))
		       (rhs rhs)))
		   (else
		    (instantiate::Vassig-op
		       (lhs expr)
		       (op (with-out-= op))
		       (rhs rhs)))))
	     expr)))

   (define (cond-expr in-for-init?)
      (let ((expr (binary-expr in-for-init?)))
	 (if (eq? (peek-token-type) '?)
	     (let* ((ignore-? (consume-any!))
		    (then (assig-expr #f))
		    (ignore-colon (consume! ':))
		    (else (assig-expr in-for-init?)))
		(instantiate::Cond
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
		       ;; ops are in car
		       (let ((token-op (car (consume-any!))))
			  (loop (instantiate::Binary
				   (lhs expr)
				   (op token-op)
				   (rhs (binary-aux (+fx level 1)))))))
		      (else
		       expr))))))
      (binary-aux 1))

   (define (unary)
      (case (peek-token-type)
	 ((delete void typeof ~ ! ++ -- + -)
	  (instantiate::Unary
	     (op (car (consume-any!)))
	     (expr (unary))))
	 (else
	  (postfix))))

   (define (postfix)
      (let ((expr (lhs)))
	 (if (not (at-new-line-token?))
	     (case (peek-token-type)
		((++ --)
		 (let ((op (car (consume-any!))))
		    (instantiate::Postfix
		       (expr expr)
		       (op op))))
		(else
		 expr))
	     expr)))

   ;; we start by getting all news (new-expr)
   ;; the remaining access and calls are then caught by the access-or-call
   ;; invocation allowing call-parenthesis.
   ;;
   ;; the access-or-call in new-expr does not all any parenthesis to be
   ;; consumed as they would be part of the new-expr.
   (define (lhs)
      (access-or-call (new-expr) #t))

   (define (new-expr)
      (if (eq? (peek-token-type) 'new)
	  (let* ((ignore (consume-any!))
		 (class (new-expr))
		 (args (if (eq? (peek-token-type) 'LPAREN)
			   (arguments)
			   '())))
	     (instantiate::New
		(class class)
		(args args)))
	  (access-or-call (primary) #f)))
   
   (define (access-or-call expr call-allowed?)
      (let loop ((expr expr))
	 (case (peek-token-type)
	    ((LBRACKET) (let* ((ignore (consume-any!))
			       (field (expression #f))
			       (ignore-too (consume! 'RBRACKET)))
			   (loop (instantiate::Access
				    (obj expr)
				    (field field)))))
	    ((DOT) (let* ((ignore (consume-any!))
			  (field (consume! 'ID))
			  (field-str (format "'~a'" field)))
		      (loop (instantiate::Access
			       (obj expr)
			       (field (instantiate::String
					 (val field-str)))))))
	    ((LPAREN) (if call-allowed?
			  (loop (instantiate::Call
				   (fun expr)
				   (args (arguments))))
			  expr))
	    (else expr))))

   (define (arguments)
      (consume! 'LPAREN)
      (if (eq? (peek-token-type) 'RPAREN)
	  (begin
	     (consume-any!)
	     '())
	  (let loop ((rev-args (list (assig-expr #f))))
	     (if (eq? (peek-token-type) 'RPAREN)
		 (begin
		    (consume-any!)
		    (reverse! rev-args))
		 (let* ((ignore (consume! 'COMMA))
			(arg (assig-expr #f)))
		    (loop (cons arg rev-args)))))))

   (define (primary)
      (case (peek-token-type)
	 ((PRAGMA) (js-pragma))
	 ((function) (function-expression))
	 ((this) (consume-any!)
		 (instantiate::This))
	 ((ID) (instantiate::Ref (id (consume! 'ID))))
	 ((LPAREN) (let ((ignore (consume-any!))
			 (expr (expression #f))
			 (ignore-too (consume! 'RPAREN)))
		      expr))
	 ((LBRACKET) (array-literal))
	 ((LBRACE) (object-literal))
	 ((null) (consume-any!)
		 (instantiate::Null (val 'null)))
	 ((true false) (instantiate::Bool (val (eq? (car (consume-any!)) 'true))))
	 ((NUMBER) (instantiate::Number (val (consume! 'NUMBER)))) ;; still as string!
	 ((STRING) (instantiate::String (val (consume! 'STRING))))
	 ((EOF) (my-error input-port "unexpected end of file" #f (peek-token)))
	 ((/ /=) (let ((pattern (read-regexp (peek-token-type))))
		    ;; consume-any *must* be after having read the reg-exp,
		    ;; so that the read-regexp works. Only then can we remove
		    ;; the peeked token.
		    (consume-any!) ;; the / or /=
		    (instantiate::RegExp (pattern pattern))))
	 (else
	  (let ((t (peek-token)))
	     (my-error input-port "unexpected token: " t t)))))

   (define (js-pragma)
      (consume! 'PRAGMA)
      (let ((prag (next-pragma!)))
	 (instantiate::Pragma (str prag))))

   (define (array-literal)
      (consume! 'LBRACKET)
      (let loop ((rev-els '())
		 (length 0))
	 (case (peek-token-type)
	    ((RBRACKET) (consume-any!)
			(instantiate::Array
			   (els (reverse! rev-els))
			   (len length)))
	    ((COMMA)
	     (consume-any!)
	     (loop rev-els (+fx length 1)))
	    (else (let ((array-el (instantiate::Array-Element
				     (index length)
				     (expr (assig-expr #f)))))
		     (if (eq? (peek-token-type) 'COMMA)
			 (begin
			    (consume-any!)
			    (loop (cons array-el rev-els)
				  (+fx length 1)))
			 (begin
			    (consume! 'RBRACKET)
			    (instantiate::Array
			       (els (reverse! (cons array-el rev-els)))
			       (len (+fx length 1))))))))))

   (define (object-literal)
      (define (property-name)
	 (case (peek-token-type)
	    ;; IDs are automatically transformed to strings.
	    ((ID)
	     (instantiate::String
		(val (string-append "\""
				    (symbol->string (consume! 'ID))
				    "\""))))
	    ((STRING) (instantiate::String (val (consume! 'STRING))))
	    ((NUMBER) (instantiate::Number (val (consume! 'NUMBER))))))
      
      (define (property-init)
	 (let* ((name (property-name))
		(ignore (consume! ':))
		(val (assig-expr #f)))
	    (instantiate::Property-Init
	       (name name)
	       (val val))))
      
      (consume! 'LBRACE)
      (if (eq? (peek-token-type) 'RBRACE)
	  (begin
	     (consume-any!)
	     (instantiate::Obj-Init (inits '())))
	  (let loop ((rev-props (list (property-init))))
	     (if (eq? (peek-token-type) 'RBRACE)
		 (begin
		    (consume-any!)
		    (instantiate::Obj-Init
		       (inits (reverse! rev-props))))
		 (begin
		    (consume! 'COMMA)
		    (loop (cons (property-init) rev-props)))))))

   ;; procedure entry point.
   ;; ----------------------
   (program))
