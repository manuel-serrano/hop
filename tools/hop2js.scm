;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/tools/hop2js.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Wed Sep 13 01:56:26 2023                          */
;*    Last change :  Wed Aug 27 08:51:07 2025 (serrano)                */
;*    Copyright   :  2023-25 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    A partial Hop-to-JS compiler.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop2js
   (include "ident.sch")
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let ((files '()))
      (args-parse (cdr args)
	 ((("-h" "--help") (help "This message"))
	  (usage args-parse-usage)
	  (exit 0))
	 (("-d" ?ident (help "Only compile definition IDENT"))
	  (set! idents (cons (string->symbol ident) idents)))
	 (("-i" ?ident (help "Ignore definition IDENT"))
	  (set! ignores (cons (string->symbol ident) ignores)))
	 ((("-r" "--rename") ?old ?new (help "Rename Ident"))
	  (hashtable-put! var-ident-table old new))
	 (else 
	  (set! files (cons else files))))
      (if (pair? files)
	  (for-each (lambda (p) (call-with-input-file p compile-port))
	     (reverse files))
	  (compile-port (current-input-port)))))

;*---------------------------------------------------------------------*/
;*    idents ...                                                       */
;*---------------------------------------------------------------------*/
(define idents '())
(define ignores '())
(define renames '())

;*---------------------------------------------------------------------*/
;*    active-ident? ...                                                */
;*---------------------------------------------------------------------*/
(define (active-ident? id)

   (define (idents? id)
      (or (null? idents)
	  (let* ((s (symbol->string! id))
		 (i (string-index s #\:)))
	     (if i
		 (let ((n (if i (substring s 0 i) s)))
		    (memq n idents))
		 (memq id idents)))))

   (define (ignores? id)
      (when (pair? ignores)
	  (let* ((s (symbol->string! id))
		 (i (string-index s #\:)))
	     (if i
		 (let ((n (if i (substring s 0 i) s)))
		    (memq n ignores))
		 (memq id ignores)))))

   (unless (ignores? id) (idents? id)))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "usage: hop2js [options] ...")
   (print "       hop2js [options] src1.scm src2.scm ...")
   (args-parse-usage #f))

;*---------------------------------------------------------------------*/
;*    compile-port ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-port port)
   (let loop ()
      (let ((expr (read port)))
	 (unless (eof-object? expr)
	    (let ((res (hop2js expr)))
	       (when res
		  (print res)))
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    expander-table ...                                               */
;*---------------------------------------------------------------------*/
(define expander-table (create-hashtable :weak 'open-string))

;*---------------------------------------------------------------------*/
;*    var-ident-table (see ident.sch) ...                              */
;*---------------------------------------------------------------------*/
(hashtable-put! var-ident-table "file-exists?" "fs.existsSync")
(hashtable-put! var-ident-table "service" "$service")

;*---------------------------------------------------------------------*/
;*    semicolon ...                                                    */
;*---------------------------------------------------------------------*/
(define (semicolon e)
   (let ((l (string-ref e (-fx (string-length e) 1))))
      (if (or (char=? l #\;)
	      (char=? l #\})
	      (char=? l #\Newline))
	  e
	  (string-append e ";"))))

;*---------------------------------------------------------------------*/
;*    return-kont ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-kont expr)
   (if (substring-at? expr "throw " 0)
       (semicolon expr)
       (semicolon (format "return ~a" expr))))

;*---------------------------------------------------------------------*/
;*    id-kont ...                                                      */
;*---------------------------------------------------------------------*/
(define (id-kont x)
   x)

;*---------------------------------------------------------------------*/
;*    hop2js ...                                                       */
;*---------------------------------------------------------------------*/
(define (hop2js expr)
   
   (define (kont-init x)
      x)
   
   (match-case expr
      ((define (?name . ?args) . ?body)
       (when (active-ident? name)
	  (hop2js-function name args body '() semicolon)))
      ((define-generic (?name ?a0 . ?args) . ?body)
       (when (active-ident? name)
	  (hop2js-stmt-define-generic name a0 args body '() kont-init)))
      ((define-method (?name ?a0 . ?args) . ?body)
       (when (active-ident? name)
	  (hop2js-stmt-define-method name a0 args body '() kont-init)))
      ((module ?name . ?clauses)
       (hop2js-module name clauses '() kont-init))
      ((define-macro (?name . ?margs) . ?body)
       (hashtable-put! expander-table (symbol->string! name)
	  (lambda (fun args env)
	     (let ((x (eval (cons* 'lambda margs body))))
		(hop2js-expr (apply x args)  env))))
       #f)
      ((define (and (? symbol?) ?var) ?expr)
       (when (active-ident? var)
	  (hop2js-stmt-define var expr '() kont-init)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    hop2js-typed-ident ...                                           */
;*---------------------------------------------------------------------*/
(define (hop2js-typed-ident::bstring sym::symbol)
   (let* ((s (symbol->string! sym))
	  (i (string-index s #\:))
	  (n (if i (substring s 0 i) s)))
      (var-ident n)))

;*---------------------------------------------------------------------*/
;*    hop2js-typed-class ...                                           */
;*---------------------------------------------------------------------*/
(define (hop2js-typed-class sym)
   (let* ((str (symbol->string sym))
	  (i (string-index str #\:))
	  (id (substring str 0 i))
	  (clazz (substring str (+fx i 2) (string-length str))))
      (values id clazz)))

;*---------------------------------------------------------------------*/
;*    hop2js-ident ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop2js-ident sym)
   (var-ident (symbol->string! sym)))

;*---------------------------------------------------------------------*/
;*    hop2js-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define (hop2js-ref id::symbol env::pair-nil)
   (let ((c (assq id env)))
      (if (and c (pair? (cdr c)))
	  (let ((k (cadr c)))
	     (if (pregexp-match "^[_a-zA-Z0-9]+$" k)
		 (format "~a.~a" (caddr c) k)
		 (format "~a['~a']" (caddr c) k)))
	  (hop2js-ident id))))

;*---------------------------------------------------------------------*/
;*    instantiate? ...                                                 */
;*---------------------------------------------------------------------*/
(define (instantiate? ident)
   (when (symbol? ident)
      (let ((s (symbol->string! ident)))
	 (substring-at? s "instantiate::" 0))))

;*---------------------------------------------------------------------*/
;*    duplicate? ...                                                   */
;*---------------------------------------------------------------------*/
(define (duplicate? ident)
   (when (symbol? ident)
      (let ((s (symbol->string! ident)))
	 (substring-at? s "duplicate::" 0))))

;*---------------------------------------------------------------------*/
;*    instantiate-class ...                                            */
;*---------------------------------------------------------------------*/
(define (instantiate-class ident)
   (let ((s (symbol->string! ident)))
      (string->symbol (substring s (string-length "instantiate::")))))
   
;*---------------------------------------------------------------------*/
;*    with-access? ...                                                 */
;*---------------------------------------------------------------------*/
(define (with-access? ident)
   (when (symbol? ident)
      (let ((s (symbol->string! ident)))
	 (substring-at? s "with-access::" 0))))

;*---------------------------------------------------------------------*/
;*    hop2js-module ...                                                */
;*---------------------------------------------------------------------*/
(define (hop2js-module name clauses env::pair-nil kont::procedure)
   (let ((export (assq 'export clauses)))
      (if (pair? export)
	  (kont
	     (let ((es (filter-map (lambda (e)
				      (let ((id (if (pair? e) (car e) e)))
					 (unless (memq id '(class final-class abstract-class wide-class))
					    (when (active-ident? id)
					       (hop2js-typed-ident id)))))
			  (cdr export))))
		(if (pair? es)
		    (format "export {~(, )}"  es)
		    "")))
	  (kont ""))))

;*---------------------------------------------------------------------*/
;*    hop2js-function ...                                              */
;*---------------------------------------------------------------------*/
(define (hop2js-function name args expr* env::pair-nil kont::procedure)
   (let ((id (hop2js-typed-ident name)))
      (format "const ~a = ~a" id (hop2js-lambda args expr* env))))

;*---------------------------------------------------------------------*/
;*    hop2js-lambda ...                                                */
;*---------------------------------------------------------------------*/
(define (hop2js-lambda args expr* env::pair-nil)
   
   (define (fx-function args expr*)
      (let ((frame (map list args)))
	 (format "((~(, )) => ~a)"
	    (map hop2js-typed-ident args)
	    (hop2js-block expr* (append frame env) return-kont))))
   
   (define (optionals-function args expr*)
      (let* ((fmls (filter (lambda (a) (not (eq? a #!optional))) args))
	     (frame (map list fmls)))
	 (format "((~(, )) => ~a)"
	    (map hop2js-typed-ident fmls)
	    (hop2js-block expr* (append frame env) return-kont))))
   
   (define (va-function args expr*)
      (let* ((last (last-pair args))
	     (opt (cdr last))
	     (frame (cons (list opt) (map list args))))
	 (set-cdr! last '())
	 (format "((~(, ), ...~a) => ~a)"
	    (map hop2js-typed-ident args)
	    (hop2js-typed-ident opt)
	    (hop2js-block expr* (append frame env) return-kont))))
   
   (define (va-function0 opt expr*)
      (let ((frame (list opt)))
	 (format "((...~a) => ~a)"
	    (hop2js-typed-ident opt)
	    (hop2js-block expr* (cons frame env) return-kont))))
   
   (cond
      ((list? args)
       (if (memq #!optional args)
	   (optionals-function args expr*)
	   (fx-function args expr*)))
      ((pair? args)
       (va-function args expr*))
      (else
       (va-function0 args expr*))))

;*---------------------------------------------------------------------*/
;*    hop2js-block ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop2js-block expr* env kont)
   (match-case expr*
      (((let . ?-))
       (hop2js-stmt* expr* env kont))
      (else
       (format "{ ~a }\n" (hop2js-stmt* expr* env kont)))))
	 
;*---------------------------------------------------------------------*/
;*    hop2js-stmt* ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt* expr* env::pair-nil kont::procedure)
   (let loop ((expr* expr*))
      (cond
	 ((null? expr*)
	  (semicolon (kont "undefined")))
	 ((null? (cdr expr*))
	  (semicolon (hop2js-stmt (car expr*) env kont)))
	 (else
	  (string-append (semicolon (hop2js-stmt (car expr*) env id-kont))
	     (loop (cdr expr*)))))))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt expr env::pair-nil kont::procedure)
   (match-case expr
      ((begin ?expr)
       (hop2js-stmt expr env kont))
      ((begin . ?expr*)
       (hop2js-block expr* env kont))
      ((case ?expr . ?clauses)
       (hop2js-stmt-case expr clauses env kont))
      ((cond . ?clauses)
       (hop2js-stmt-cond clauses env kont))
      ((define (and (? symbol?) ?var) ?expr)
       (hop2js-stmt-define var expr env kont))
      ((define (?name . ?args) . ?body)
       (hop2js-stmt `(define ,name (lambda ,args ,@body)) env kont))
      ((if ?test ?yes)
       (hop2js-stmt-if test yes #f env kont))
      ((if ?test ?yes ?no)
       (hop2js-stmt-if test yes no env kont))
      ((let (and ?loop (? symbol?)) (and (? list?) ?bindings) . ?body)
       (hop2js-stmt-letn loop bindings body env kont))
      ((let (and (? pair?) ?bindings) . ?body)
       (hop2js-stmt-let bindings body env kont))
      ((let* (and (? pair?) ?bindings) . ?body)
       (hop2js-stmt-let bindings body env kont))
      ((letrec (and (? pair?) ?bindings) . ?body)
       (hop2js-stmt-let bindings body env kont))
      ((match-case ?expr . ?clauses)
       (hop2js-stmt-match-case expr clauses env kont))
      ((multiple-value-bind ?bindings ?expr . ?body)
       (hop2js-stmt-multiple-value-bind bindings expr body env kont))
      ((set! ?var ?expr)
       (kont (hop2js-expr-set! var expr env)))
      ((unless ?test . ?expr*)
       (hop2js-stmt-if `(not ,test) `(begin ,@expr*) #f env kont))
      ((unwind-protect ?stmt ?cleanup)
       (hop2js-stmt-unwind-protect stmt cleanup env kont))
      ((when ?test . ?expr*)
       (hop2js-stmt-if test `(begin ,@expr*) #f env kont))
      (((? with-access?) ?obj ?vars . ?expr*)
       (hop2js-stmt-with-access obj vars expr* env kont))
      (else
       (kont (hop2js-expr expr env)))))

;*---------------------------------------------------------------------*/
;*    hop2js-expr ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop2js-expr::bstring expr env::pair-nil)
   (match-case expr
      ((? number?)
       (cond
	  ((not (flonum? expr))
	   (number->string expr))
	  ((nanfl? expr)
	   "NaN")
	  ((infinitefl? expr)
	   (if (>=fl expr 0.0)
	       "+Infinity"
	       "-Infinity"))
	  (else
	   (number->string expr))))
      ((? string?)
       (string-append "\"" (hop2js-string expr) "\""))
      ((? keyword?)
       (string-append "'" (keyword->string expr) "'"))
      ((? char?)
       (let ((n (char->integer expr)))
	  (if (and (>fx n 0) (<=fx n 127))
	      (string-append "'" (string expr) "'")
	      (string-append "'" (format "\\u~4,0x" n) "'"))))
      ((? symbol?)
       (hop2js-ref expr env))
      (((kwote quote) (tprint . ?-))
       ;; commented debug
       "undefined")
      (((kwote quote) (and ?sym (? symbol?)))
       (string-append "'" (symbol->string sym) "'"))
      (((kwote quote) ())
       "null")
      (((kwote quote) (and ?els (? pair?)))
       (format "[~(, )]"
	  (map (lambda (e)
		  (cond
		     ((symbol? e)
		      (string-append "'" (symbol->string e) "'"))
		     ((or (string? e) (char? e) (boolean? e) (number? e))
		      (hop2js-expr e env))
		     (else
		      (error "kwote" "unsupported literal" e))))
	     els)))
      (((kwote quote) (and ?els (? vector?)))
       (format "[~(, )]"
	  (map (lambda (e)
		  (cond
		     ((symbol? e)
		      (string-append "'" (symbol->string e) "'"))
		     ((or (string? e) (char? e) (boolean? e) (number? e))
		      (hop2js-expr e env))
		     (else
		      (error "kwote" "unsupported literal" e))))
	     (vector->list els))))
      ((quasiquote . ?-)
       "undefined")
      ((? boolean?)
       (if expr "true" "false"))
      (#unspecified
       "undefined")
      ((begin ?expr)
       (hop2js-expr expr env))
      ((begin . ?expr*)
       (format "(~(, ))" (map (lambda (e) (hop2js-expr e env)) expr*)))
      ((case ?expr . ?clauses)
       (hop2js-expr-case expr clauses env))
      ((cond . ?clauses)
       (hop2js-expr-cond clauses env))
      ((define (and (? symbol?) ?var) ?val)
       (error "define expression" "not implemented" val))
;*        (hop2js-def var expr env kont))                              */
      ((define (?name . ?args) . ?body)
       (error "define expression" "not implemented" expr))
;*        (hop2js-expr `(define ,name (lambda ,args ,@body)) env))     */
      ((if ?test ?yes)
       (hop2js-expr-if test yes #f env))
      ((if ?test ?yes ?no)
       (hop2js-expr-if test yes no env))
      ((lambda ?args . ?body)
       (hop2js-lambda args body env))
      ((let (and ?loop (? symbol?)) (and (? list?) ?bindings) . ?body)
       (error "letn expression" "not implemented" expr))
;*        (hop2js-letn loop bindings body env kont))                   */
      ((let (and (? pair?) ?bindings) . ?body)
       (hop2js-expr-let bindings body env))
      ((let* (and (? pair?) ?bindings) . ?body)
       (hop2js-expr-let bindings body env))
      ((letrec (and (? pair?) ?bindings) . ?body)
       (hop2js-expr-let bindings body env))
      ((set! ?var ?expr)
       (hop2js-expr-set! var expr env))
      ((when ?test . ?expr*)
       (hop2js-expr `(if ,test (begin ,@expr*)) env))
      ((unless ?test . ?expr*)
       (hop2js-expr `(if (not ,test) (begin ,@expr*)) env))
      ((values . ?expr*)
       (hop2js-expr-values expr* env))
      (((? with-access?) ?obj ?vars . ?expr*)
       (hop2js-expr-with-access obj vars expr* env))
      ((?fun . ?args)
       (hop2js-call fun args env))
      (else
       "??")))

;*---------------------------------------------------------------------*/
;*    hop2js-string ...                                                */
;*---------------------------------------------------------------------*/
(define (hop2js-string str)
   
   (define (encchar c)
      (case c
	 ((#\Newline)
	  "\\n")
	 ((#\Return)
	  "\\r")
	 ((#\Tab)
	  "\\t")
	 ((#\")
	  "\\\"")
	 (else
	  (let ((n (char->integer c)))
	     (if (and (>fx n 0) (<=fx n 127))
		 (string c)
		 (format "\\u~4,0x" n))))))
      
   (let loop ((i (-fx (string-length str) 1)))
      (if (=fx i -1)
	  str
	  (let ((n (char->integer (string-ref str i))))
	     (if (and (>fx n 31) (<=fx n 127) (not (=fx n 34)))
		 (loop (-fx i 1))
		 (apply string-append (map encchar (string->list str))))))))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-case ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-case expr clauses env kont)
   
   (define (case-clause c)
      (match-case c
	 ((else . ?body)
	  (format "default: ~a"
	     (hop2js-stmt* body env kont)))
	 ((((and ?s (? symbol?))) . ?body)
	  (format "case '~a': ~a break;"
	     (symbol->string s)
	     (hop2js-stmt* body env kont)))
	 ((((and ?n (? integer?))) . ?body)
	  (format "case ~a: ~a break;"
	     n
	     (hop2js-stmt* body env kont)))
	 (((and ?symbols (? (lambda (v) (every symbol? v)))) . ?body)
	  (string-append
	     (apply string-append
		(map (lambda (s) (format "case '~a':" (symbol->string s)))
		   symbols))
	     (hop2js-stmt* body env kont)
	     "break;"))
	 (((and ?numbers (? (lambda (v) (every integer? v)))) . ?body)
	  (string-append
	     (apply string-append
		(map (lambda (n) (format "case ~a:" n))
		   numbers))
	     (hop2js-stmt* body env kont)
	     "break;"))
	 (((and ?chars (? (lambda (v) (every char? v)))) . ?body)
	  (string-append
	     (apply string-append
		(map (lambda (n) (format "case \"~a\":" (hop2js-string (string n))))
		   chars))
	     (hop2js-stmt* body env kont)
	     "break;"))
	 (else
	  (error "hop2js-stmt-case" "cannot compile" expr))))
   
   (format "switch (~a) { ~( ) }" (hop2js-expr expr env)
      (map case-clause clauses)))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-case ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-case expr clauses env)
   (format "(() => { ~a })()"
      (hop2js-stmt-case expr clauses env return-kont)))

;*---------------------------------------------------------------------*/
;*    cond->if ...                                                     */
;*---------------------------------------------------------------------*/
(define (cond->if clauses)
   (when (pair? clauses)
      (match-case (car clauses)
	 ((else . ?body)
	  `(begin ,@body))
	 ((?test => (lambda (?var) . ?body))
	  (let ((tmp (string->symbol (hop2js-ident var))))
	     `(let ((,tmp ,test))
		 (if ,tmp
		     (begin ,@body)
		     ,(cond->if (cdr clauses))))))
	 ((?test => ?proc)
	  (let ((tmp (gensym 'cond)))
	     `(let ((,tmp ,test))
		 (if ,tmp
		     (,proc ,tmp)
		     ,(cond->if (cdr clauses))))))
	 ((?test . ?body)
	  `(if ,test (begin ,@body) ,(cond->if (cdr clauses))))
	 (else
	  (error "cond" "illegal clause" (car clauses))))))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-cond ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-cond clauses env kont)
   (hop2js-stmt (cond->if clauses) env kont))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-cond ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-cond clauses env)
   (hop2js-expr (cond->if clauses) env))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-define ...                                           */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-define var expr env kont)
   (format "let ~a = ~a" (hop2js-typed-ident var) (hop2js-expr expr env)))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-define-generic ...                                   */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-define-generic name a0 args body env kont)
   (multiple-value-bind (id clazz)
      (hop2js-typed-class a0)
      (let ((gid (hop2js-typed-ident name)))
	 (string-append
	    ;; global function
	    (format "const ~a = (~(, )) => { return ~a.~a(~(, )); }"
	       gid (cons id args) id gid (cons id args))
	    "\n"
	    ;; default body
	    (format "~a.prototype.~a = ((~(, )) => { ~a })"
	       clazz (hop2js-typed-ident name) (cons id args)
	       (hop2js-stmt* body env return-kont))))))
	 
;*---------------------------------------------------------------------*/
;*    hop2js-stmt-define-method ...                                    */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-define-method name a0 args body env kont)
   (multiple-value-bind (id clazz)
      (hop2js-typed-class a0)
      (format "~a.prototype.~a = ((~(, )) => { ~a })"
	 clazz (hop2js-typed-ident name) (cons id args)
	 (hop2js-stmt* body env return-kont))))
	 
;*---------------------------------------------------------------------*/
;*    hop2js-stmt-if ...                                               */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-if test yes no env kont)
   (let ((t (hop2js-expr test env))
	 (y (semicolon (hop2js-stmt yes env kont))))
      (match-case no
	 ((if ?- . ?-)
	  (format "if (~a) ~a else ~a" t y
	     (semicolon (hop2js-stmt no env kont))))
	 (#f
	  (format "if (~a) ~a" t y))
	 (else
	  (format "if (~a) ~a else ~a" t y
	     (hop2js-stmt no env kont))))))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-if ...                                               */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-if test yes no env)
   (let ((t (hop2js-expr test env))
	 (y (hop2js-expr yes env)))
      (match-case no
	 (#f
	  (format "((~a) ? (~a) : undefined)" t y))
	 (else
	  (format "((~a) ? (~a) : (~a))" t y
	     (hop2js-expr no env))))))

;*---------------------------------------------------------------------*/
;*    hop2js-letn ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop2js-letn loop bindings body env::pair-nil kont::procedure)
   (let ((ids (map (lambda (b)
		      (hop2js-typed-ident (car b)))
		 bindings))
	 (vals (map (lambda (b)
		       (hop2js-expr (cadr b) env))
		  bindings))
	 (frame (map (lambda (b) (list (car b))) bindings)))
      (kont
	 (format "((function ~a(~(, )) { ~a })(~(, )))"
	    loop ids
	    (hop2js-stmt* body (append frame env) return-kont)
	    vals))))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-letn ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-letn loop bindings body env::pair-nil kont::procedure)
   (let* ((ids (map (lambda (b)
		       (hop2js-typed-ident (car b)))
		  bindings))
	  (vals (map (lambda (b)
			(hop2js-expr (cadr b) env))
		   bindings))
	  (frame (map (lambda (b) (list (car b))) bindings))
	  (id (string-append (hop2js-typed-ident loop)))
	  (id$ (string-append id "$")))
      (format "{ const ~a = ((function ~a(~(, )) { ~a })(~(, ))); ~a }"
	 id$ id ids
	 (hop2js-stmt* body (append frame env) return-kont)
	 vals
	 (kont id$))))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-let ...                                              */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-let bindings body env::pair-nil kont::procedure)
   (let ((bs (map (lambda (b)
		     (if (pair? b)
			 (format "~a = ~a" (hop2js-typed-ident (car b))
			    (hop2js-expr (cadr b) env))
			 (hop2js-typed-ident b)))
		bindings))
	 (frame (map (lambda (b) (list (if (pair? b) (car b) b))) bindings)))
      (format "{ let ~(, ); ~a }" bs
	 (hop2js-stmt* body (append frame env) kont))))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-let ...                                              */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-let bindings body env::pair-nil)
   (let ((bs (map (lambda (b)
		     (format "~a = ~a" (hop2js-typed-ident (car b))
			(hop2js-expr (cadr b) env)))
		bindings))
	 (frame (map (lambda (b) (list (car b))) bindings)))
      (format "(((~(, )) => { ~a })(~(, )))"
	 (map (lambda (b)
		 (hop2js-typed-ident (car b)))
	    bindings)
	 (hop2js-stmt* body (append frame env) return-kont)
	 (map (lambda (b)
		 (hop2js-expr (cadr b) env))
	    bindings))))

;*---------------------------------------------------------------------*/
;*    hop2js-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop2js-call::bstring fun args env)
   (let ((x (and (symbol? fun)
		 (hashtable-get expander-table (symbol->string! fun)))))
      (if x
	  (x fun args env)
	  (case fun
;* 	     ((-)                                                      */
;* 	      ;; unop/binop                                            */
;* 	      (if (pair? (cdr args))                                   */
;* 		  (let ((lhs (hop2js-expr (car args) env))             */
;* 			(rhs (hop2js-expr (cadr args) env)))           */
;* 		     (format "~a - ~a" lhs rhs))                       */
;* 		  (let ((lhs (hop2js-expr (car args) env)))            */
;* 		     (format "- ~a" lhs))))                            */
;* 	     ((> >= < <= +)                                            */
;* 	      ;; binop                                                 */
;* 	      (let ((lhs (hop2js-expr (car args) env))                 */
;* 		    (rhs (hop2js-expr (cadr args) env)))               */
;* 		 (format "~a ~a ~a" lhs fun rhs)))                     */
;* 	     ((+fx -fx >fx >=fx <fx <=fx                               */
;* 		 +fl -fl >fl >=fl <fl <=fl                             */
;* 		 +llong -llong >llong >=llong <llong <=llong)          */
;* 	      ;; binop                                                 */
;* 	      (let ((lhs (hop2js-expr (car args) env))                 */
;* 		    (rhs (hop2js-expr (cadr args) env))                */
;* 		    (op (substring (symbol->string fun) 0 1)))         */
;* 		 (format "~a ~a ~a" lhs op rhs)))                      */
;* 	     ((string=? char=?)                                        */
;* 	      (let ((lhs (hop2js-expr (car args) env))                 */
;* 		    (rhs (hop2js-expr (cadr args) env)))               */
;* 		 (format "~a === ~a" lhs rhs)))                        */
;* 	     ((eq?)                                                    */
;* 	      ;; binop                                                 */
;* 	      (let ((lhs (hop2js-expr (car args) env))                 */
;* 		    (rhs (hop2js-expr (cadr args) env))                */
;* 		    (op (substring (symbol->string fun) 0 1)))         */
;* 		 (format "~a === ~a" lhs rhs)))                        */
;* 	     ((and or)                                                 */
;* 	      (let ((lhs (hop2js-expr (car args) env)))                */
;* 		 (if (pair? (cdr args))                                */
;* 		     (let ((rhs (hop2js-expr (cadr args) env))         */
;* 			   (op (if (eq? fun 'and) "&&" "||")))         */
;* 			(format "~a ~a ~a" lhs op rhs))                */
;* 		     lhs)))                                            */
;* 	     ((not)                                                    */
;* 	      (let ((val (hop2js-expr (car args) env)))                */
;* 		 (if (symbol? (car args))                              */
;* 		     (string-append "!" val)                           */
;* 		     (string-append "!(" val ")"))))                   */
;* 	     ((J2SPragma)                                              */
;* 	      "J2SNotImplemented('J2SPragma')")                        */
;* 	     ((raise)                                                  */
;* 	      (format "throw ~a" (hop2js-expr (car args) env)))        */
;* 	     ((vector)                                                 */
;* 	      (format "[~(, )]"                                        */
;* 		 (map (lambda (e) (hop2js-expr e env)) args)))         */
;* 	     ((string-length)                                          */
;* 	      (format "~a.length" (hop2js-expr (car args) env)))       */
;* 	     ((string-ref)                                             */
;* 	      (format "~a[~a]" (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))) */
;* 	     ((isa?)                                                   */
;* 	      (format "~a instanceof ~a"                               */
;* 		 (hop2js-expr (car args) env)                          */
;* 		 (hop2js-expr (cadr args) env)))                       */
	     (else
	      (cond
		 ((eq? fun 'J2SPragma)
		  "J2SNotImplemented('J2SPragma')")
		 ((instantiate? fun)
		  ;; instantiate
		  (hop2js-instantiate fun args env))
		 ((duplicate? fun)
		  ;; instantiate
		  (hop2js-duplicate (car args) (cdr args) env))
		 (else
		  ;; regular calls
		  (format "~a(~(, ))" (hop2js-expr fun env)
		     (if (keyword-arguments? args)
			 (hop2js-keyword-args args env)
			 (map (lambda (e) (hop2js-expr e env)) args))))))))))

;*---------------------------------------------------------------------*/
;*    keyword-arguments? ...                                           */
;*---------------------------------------------------------------------*/
(define (keyword-arguments? args)
   (let loop ((args args))
      (cond
	 ((null? args)
	  #f)
	 ((keyword? (car args))
	  ;; every other remaining args must be a keyword
	  (let loop ((args args))
	     (cond
		((null? args) #t)
		((and (keyword? (car args)) (pair? (cdr args))) (loop (cddr args)))
		(else #f))))
	 (else
	  (loop (cdr args))))))
	     
;*---------------------------------------------------------------------*/
;*    hop2js-keyword-args ...                                          */
;*---------------------------------------------------------------------*/
(define (hop2js-keyword-args args env)
   
   (define (args->object args env)
      (string-append "{"
	 (let loop ((args args))
	    (let ((p (format "\"~a\": ~a"
			(keyword->string (car args))
			(hop2js-expr (cadr args) env))))
	       (string-append p
		  (if (pair? (cddr args))
		      (string-append ", " (loop (cddr args)))
		      "}"))))))
   
   (let loop ((args args))
      (cond
	 ((null? args)
	  '())
	 ((and (keyword? (car args)) (pair? (cdr args)))
	  (list (args->object args env)))
	 (else
	  (cons (hop2js-expr (car args) env)
	     (loop (cdr args)))))))

;*---------------------------------------------------------------------*/
;*    hop2js-instantiate ...                                           */
;*---------------------------------------------------------------------*/
(define (hop2js-instantiate fun args env)
   
   (define (instantiate-prop prop)
      (let ((name (car prop))
	    (val (cadr prop)))
	 (format "'~a': ~a" (prop-ident name) (hop2js-expr val env))))
   
   (let ((id (hop2js-ident (instantiate-class fun))))
      (format "new ~a({~(,)})" id (map instantiate-prop args))))

;*---------------------------------------------------------------------*/
;*    hop2js-duplicate ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-duplicate obj props env)
   
   (define (duplicate-prop prop)
      (let ((name (car prop))
	    (val (cadr prop)))
	 (format "'~a': ~a" (prop-ident name) (hop2js-expr val env))))
   
   (format "~a.duplicate({~(,)})" (hop2js-expr obj env)
      (map duplicate-prop props)))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-multiple-value-bind ...                              */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-multiple-value-bind bindings expr expr* env kont)
   (let ((frame (map (lambda (b) (list b)) bindings)))
      (format "{ let [~(, )] = ~a; ~a }"
	 (map hop2js-typed-ident bindings)
	 (hop2js-expr expr env)
	 (hop2js-stmt* expr* (append frame env) kont))))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-values ...                                           */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-values expr* env)
   (format "[~(, )]" (map (lambda (e) (hop2js-expr e env)) expr*)))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-match-case ...                                       */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-match-case expr clauses env kont)

   (define (pat-variable? e)
      (when (symbol? e)
	 (char=? (string-ref (symbol->string! e) 0) #\?)))
   
   (define (pat-variable-name e)
      (substring (symbol->string! e) 1))
   
   (define (hop2js-match-test id expr env)
      (match-case expr
	 ((at ?fname ?loc)
	  (values
	     (format "~a instanceof Location" id)
	     (format "const fname = ~a.filename, loc = ~a.offset;" id id)))
	 (else
	  (let loop ((expr expr)
		     (len 0)
		     (vars '()))
	     (cond
		((null? expr)
		 (values
		    (format "~a.length === ~a" id len)
		    (format "const ~(, );" vars)))
		((pair? expr)
		 (cond
		    ((eq? (car expr) '?-)
		     (loop (cdr expr) (+fx len 1) vars))
		    ((pat-variable? (car expr))
		     (let ((var (format "~a = ~a[~a]"
				   (pat-variable-name (car expr))
				   id len)))
			(loop (cdr expr) (+fx len 1) (cons var vars))))
		    (else
		     (error "hop2js-stmt-match-case"
			"Unsupported pattern" expr))))
		((eq? expr '?-)
		 (values
		    (format "~a.length >= ~a" id len)
		    (format "const ~(, );" vars)))
		(else
		 (error "hop2js-stmt-match-case"
		    "Unsupported pattern" expr)))))))
   
   (let ((val (hop2js-expr expr env))
	 (id (gensym 'mc)))
      (format "{ const ~a = ~a; ~a }"
	 id val
	 (let loop ((clauses clauses))
	    (cond
	       ((null? clauses)
		"")
	       ((eq? (caar clauses) 'else)
		(hop2js-stmt* (cdr (car clauses)) env kont))
	       (else
		(multiple-value-bind (test binder)
		   (hop2js-match-test id (car (car clauses)) env)
		   (format "if (~a) { ~a; ~a } else ~a"
		      test binder
		      (hop2js-stmt* (cdr (car clauses)) env kont)
		      (loop (cdr clauses))))))))))
		
;*---------------------------------------------------------------------*/
;*    hop2js-stmt-with-access ...                                      */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-with-access obj props expr* env kont)
   (let* ((id (gensym 'obj))
	  (frame (map (lambda (p)
			 (if (symbol? p)
			     (list p (prop-ident p) id)
			     (list (car p) (prop-ident (cadr p)) id)))
		    props)))
      (format "{ let ~a = ~a; ~a }" id (hop2js-expr obj env)
	 (hop2js-stmt* expr* (append frame env) kont))))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-with-access ...                                      */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-with-access obj props expr* env)
   (let* ((id (gensym 'obj))
	  (frame (map (lambda (p)
			 (if (symbol? p)
			     (list p (prop-ident p) id)
			     (list (car p) (prop-ident (cadr p)) id)))
		    props)))
      (format "((~a => { ~a })(~a))"
	 id (hop2js-stmt* expr* (append frame env) return-kont)
	 (hop2js-expr obj env))))

;*---------------------------------------------------------------------*/
;*    hop2js-stmt-unwind-protect ...                                   */
;*---------------------------------------------------------------------*/
(define (hop2js-stmt-unwind-protect stmt cleanup env kont)

   (define (block stmt)
      (if (char=? (string-ref stmt 0) #\{)
	  stmt
	  (string-append "{ " stmt " }")))
   
   (let ((stmt (hop2js-stmt stmt env kont))
	 (cleanup (hop2js-stmt cleanup env id-kont)))
      (format "try ~a finally ~a" (block stmt) (block cleanup))))

;*---------------------------------------------------------------------*/
;*    hop2js-expr-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (hop2js-expr-set! var expr env)
   (format "~a = ~a" (hop2js-ref var env) (hop2js-expr expr env)))

;*---------------------------------------------------------------------*/
;*    builtin-macros ...                                               */
;*---------------------------------------------------------------------*/
(define generic-binop-expander
   (lambda (fun args env)
      (let ((lhs (hop2js-expr (car args) env))
	    (rhs (if (pair? (cddr args))
		     (hop2js-expr `(,fun ,@(cdr args)) env)
		     (hop2js-expr (cadr args) env))))
	 (format "(~a ~a ~a)" lhs fun rhs))))

(define =-binop-expander
   (lambda (fun args env)
      (let ((lhs (hop2js-expr (car args) env))
	    (rhs (if (pair? (cddr args))
		     (hop2js-expr `(,fun ,@(cdr args)) env)
		     (hop2js-expr (cadr args) env))))
	 (format "(~a === ~a)" lhs rhs))))
(hashtable-put! expander-table "=" =-binop-expander)
(hashtable-put! expander-table ">" generic-binop-expander)
(hashtable-put! expander-table ">=" generic-binop-expander)
(hashtable-put! expander-table "<=" generic-binop-expander)
(hashtable-put! expander-table "<" generic-binop-expander)
(hashtable-put! expander-table "+" generic-binop-expander)
(hashtable-put! expander-table "*" generic-binop-expander)
(hashtable-put! expander-table "/" generic-binop-expander)

(define specific-binop-expander
   (lambda (fun args env)
      (let* ((lhs (hop2js-expr (car args) env))
	     (rhs (if (pair? (cddr args))
		      (hop2js-expr `(,fun ,@(cdr args)) env)
		      (hop2js-expr (cadr args) env)))
	     (str (symbol->string fun))
	     (op (if (char=? (string-ref str 1) #\=)
		     (substring str 0 2)
		     (substring str 0 1))))
	 (format "(~a ~a ~a)" lhs op rhs))))

(hashtable-put! expander-table "+fx" specific-binop-expander)
(hashtable-put! expander-table "-fx" specific-binop-expander)
(hashtable-put! expander-table "*fx" specific-binop-expander)
(hashtable-put! expander-table "/fx" specific-binop-expander)
(hashtable-put! expander-table ">=fx" specific-binop-expander)
(hashtable-put! expander-table ">fx" specific-binop-expander)
(hashtable-put! expander-table "<fx" specific-binop-expander)
(hashtable-put! expander-table "<=fx" specific-binop-expander)
(hashtable-put! expander-table "+fl" specific-binop-expander)
(hashtable-put! expander-table "-fl" specific-binop-expander)
(hashtable-put! expander-table "*fl" specific-binop-expander)
(hashtable-put! expander-table "/fl" specific-binop-expander)
(hashtable-put! expander-table ">=fl" specific-binop-expander)
(hashtable-put! expander-table ">fl" specific-binop-expander)
(hashtable-put! expander-table "<fl" specific-binop-expander)
(hashtable-put! expander-table "<=fl" specific-binop-expander)
(hashtable-put! expander-table "+llong" specific-binop-expander)
(hashtable-put! expander-table "-llong" specific-binop-expander)
(hashtable-put! expander-table "*llong" specific-binop-expander)
(hashtable-put! expander-table "/llong" specific-binop-expander)
(hashtable-put! expander-table ">=llong" specific-binop-expander)
(hashtable-put! expander-table ">llong" specific-binop-expander)
(hashtable-put! expander-table "<llong" specific-binop-expander)
(hashtable-put! expander-table "<=llong" specific-binop-expander)
(hashtable-put! expander-table "+elong" specific-binop-expander)
(hashtable-put! expander-table "-elong" specific-binop-expander)
(hashtable-put! expander-table "*elong" specific-binop-expander)
(hashtable-put! expander-table "/elong" specific-binop-expander)
(hashtable-put! expander-table ">=elong" specific-binop-expander)
(hashtable-put! expander-table ">elong" specific-binop-expander)
(hashtable-put! expander-table "<elong" specific-binop-expander)
(hashtable-put! expander-table "<=elong" specific-binop-expander)

(hashtable-put! expander-table "expt"
   (lambda (fun args env)
      (format "~a ** ~a"
	 (hop2js-expr (car args) env)
	 (hop2js-expr (cadr args) env))))

(define neg-expander
   (lambda (fun args env)
      (format "-~a" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "negfl" neg-expander)
(hashtable-put! expander-table "negfx" neg-expander)
(hashtable-put! expander-table "negelong" neg-expander)
(hashtable-put! expander-table "negllong" neg-expander)

(hashtable-put! expander-table "equal?"
   (lambda (fun args env)
      (let ((lhs (hop2js-expr (car args) env))
	    (rhs (hop2js-expr (cadr args) env))
	    (op (substring (symbol->string fun) 0 1)))
	 (format "~a == ~a" lhs rhs))))

(define eq-expander
   (lambda (fun args env)
      (let ((lhs (hop2js-expr (car args) env))
	    (rhs (hop2js-expr (cadr args) env)))
	 (format "~a === ~a" lhs rhs))))

(hashtable-put! expander-table "=fx" eq-expander)
(hashtable-put! expander-table "=elong" eq-expander)
(hashtable-put! expander-table "=llong" eq-expander)

(hashtable-put! expander-table "-"
   (lambda (fun args env)
      (if (pair? (cdr args))
	  (let ((lhs (hop2js-expr (car args) env))
		(rhs (hop2js-expr (cadr args) env)))
	     (format "~a - ~a" lhs rhs))
	  (let ((lhs (hop2js-expr (car args) env)))
	     (format "- ~a" lhs)))))

(hashtable-put! expander-table "eq?" eq-expander)
      
(hashtable-put! expander-table "not"
   (lambda (fun args env)
      (match-case args
	 (((eq? ?lhs ?rhs))
	  (let ((lhs (hop2js-expr lhs env))
		(rhs (hop2js-expr rhs env))
		(op (substring (symbol->string fun) 0 1)))
	     (format "~a !== ~a" lhs rhs)))
	 (else
	  (let ((val (hop2js-expr (car args) env)))
	     (if (symbol? (car args))
		 (string-append "!" val)
		 (string-append "!(" val ")")))))))

(define and-or-expander
   (lambda (fun args env)
      (let loop ((args args))
	 (let ((lhs (hop2js-expr (car args) env)))
	    (if (pair? (cdr args))
		(let ((rhs (loop (cdr args)))
		      (op (if (eq? fun 'and) "&&" "||")))
		   (format "(~a) ~a ~a" lhs op rhs))
		(format "(~a)" lhs))))))
(hashtable-put! expander-table "and" and-or-expander)
(hashtable-put! expander-table "or" and-or-expander)

(hashtable-put! expander-table "string?"
   (lambda (fun args env)
      (format "typeof(~a) === 'string'" (hop2js-expr (car args) env))))

(define string=-char=-expander
   (lambda (fun args env)
      (let ((lhs (hop2js-expr (car args) env))
	    (rhs (hop2js-expr (cadr args) env)))
	 (format "~a === ~a" lhs rhs))))

(hashtable-put! expander-table "string=?" string=-char=-expander)
(hashtable-put! expander-table "char=?" string=-char=-expander)

(hashtable-put! expander-table "string-length"
   (lambda (fun args env)
      (format "~a.length"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "string-ref"
   (lambda (fun args env)
      (format "~a[~a]"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "string-index"
   (lambda (fun args env)
      (format "~a.indexOf(~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "substring=?"
   (lambda (fun args env)
      (format "~a.substring(0, ~a) === ~a"
	 (hop2js-expr (car args) env)
	 (hop2js-expr (caddr args) env)
	 (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "getbuffer"
   (lambda (fun args env)
      (if (pair? (cddr args))
	  (format "Buffer.from(~a.substring(~a, ~a), \"binary\")"
	     (hop2js-expr (car args) env)
	     (hop2js-expr (cadr args) env)
	     (hop2js-expr (caddr args) env))
	  (format "Buffer.from(~a.substring(~a), \"binary\")"
	     (hop2js-expr (car args) env)
	     (hop2js-expr (caddr args) env)))))

(hashtable-put! expander-table "substring"
   (lambda (fun args env)
      (if (pair? (cddr args))
	  (format "~a.substring(~a, ~a)"
	     (hop2js-expr (car args) env)
	     (hop2js-expr (cadr args) env)
	     (hop2js-expr (caddr args) env))
	  (format "~a.substring(~a)"
	     (hop2js-expr (car args) env)
	     (hop2js-expr (cadr args) env)))))

(hashtable-put! expander-table "substring-at?"
   (lambda (fun args env)
      (format "~a.substring(~a, ~a + ~a) === ~a"
	 (hop2js-expr (car args) env)
	 (hop2js-expr (caddr args) env)
	 (hop2js-expr (caddr args) env)
	 (if (string? (cadr args))
	     (string-length (cadr args))
	     (format "~a.length" (hop2js-expr (cadr args) env)))
	 (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "string-prefix?"
   (lambda (fun args env)
      (format "~a.startsWith(~a)"
	 (hop2js-expr (cadr args) env)
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "isa?"
   (lambda (fun args env)
      (format "~a instanceof ~a"
	 (hop2js-expr (car args) env) 
	 (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "raise"
   (lambda (fun args env)
      (format "throw ~a"
	 (hop2js-expr (car args) env))))
      
(hashtable-put! expander-table "vector"
   (lambda (fun args env)
      (format "[~(, )]" 
	 (map (lambda (e) (hop2js-expr e env)) args))))

(hashtable-put! expander-table "vector-length"
   (lambda (fun args env)
      (format "~a.length"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "vector-ref"
   (lambda (fun args env)
      (format "~a[~a]"
	 (hop2js-expr (car args) env)
	 (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "vector-set!"
   (lambda (fun args env)
      (format "~a[~a] = ~a"
	 (hop2js-expr (car args) env)
	 (hop2js-expr (cadr args) env)
	 (hop2js-expr (caddr args) env))))

(hashtable-put! expander-table "memq"
   (lambda (fun args env)
      (format "~a?.includes(~a)"
	 (hop2js-expr (cadr args) env)
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "usage"
   (lambda (fun args env)
      (hop2js-expr (car args) env)))

(hashtable-put! expander-table "co-instantiate"
   (lambda (fun args env)

      (define (instantiate-prop prop)
	 (let ((name (car prop))
	       (val (cadr prop)))
	    (format "'~a': ~a" name (hop2js-expr val env))))
      
      (let* ((bindings (car args))
	     (body (cdr args))
	     (vs (map (lambda (b)
			 (hop2js-typed-ident (car b)))
		    bindings))
	     (frame (map list vs))
	     (allocs (map (lambda (b)
			     (format "~a = new ~a()"
				(hop2js-typed-ident (car b))
				(instantiate-class (car (cadr b)))))
			bindings))
	     (assigs (map (lambda (b)
			     (format "Object.assign(~a, {~(,)})"
				(hop2js-typed-ident (car b))
				(map instantiate-prop (cdr (cadr b)))))
			bindings)))
	 (format "(((~(, )) => { ~(; ); ~(; ); return ~a; } )(~(, )))"
	    vs
	    allocs assigs
	    (hop2js-expr `(begin ,@body) env)
	    (map (lambda (_) "undefined") bindings)))))

(hashtable-put! expander-table "fixnum?"
   (lambda (fun args env)
      (format "Number.isInteger(~a)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "integer?"
   (lambda (fun args env)
      (format "Number.isInteger(~a)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "number?"
   (lambda (fun args env)
      (format "typeof (~a) === 'number'" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "elong?"
   (lambda (fun args env)
      (format "Number.isInteger(~a)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "llong?"
   (lambda (fun args env)
      (format "Number.isInteger(~a)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "fixnum->flonum"
   (lambda (fun args env)
      (hop2js-expr (car args) env)))

(hashtable-put! expander-table "fixnum->elong"
   (lambda (fun args env)
      (hop2js-expr (car args) env)))

(hashtable-put! expander-table "fixnum->llong"
   (lambda (fun args env)
      (hop2js-expr (car args) env)))

(hashtable-put! expander-table "elong->fixnum"
   (lambda (fun args env)
      (hop2js-expr (car args) env)))

(hashtable-put! expander-table "elong->flonum"
   (lambda (fun args env)
      (hop2js-expr (car args) env)))

(hashtable-put! expander-table "char->integer"
   (lambda (fun args env)
      (format "~a.charCodeAt(0)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "bit-lsh"
   (lambda (fun args env)
      (format "(~a << ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-rsh"
   (lambda (fun args env)
      (format "(~a >> ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-or"
   (lambda (fun args env)
      (format "(~a | ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-and"
   (lambda (fun args env)
      (format "(~a & ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-xor"
   (lambda (fun args env)
      (format "(~a ^ ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-not"
   (lambda (fun args env)
      (format "(~~~a)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "bit-lshelong"
   (lambda (fun args env)
      (format "(~a << ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-rshelong"
   (lambda (fun args env)
      (format "(~a >> ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-orelong"
   (lambda (fun args env)
      (format "(~a | ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-andelong"
   (lambda (fun args env)
      (format "(~a & ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-xorelong"
   (lambda (fun args env)
      (format "(~a ^ ~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "bit-notelong"
   (lambda (fun args env)
      (format "(~~~a)" (hop2js-expr (car args) env))))

(hashtable-put! expander-table "tprint"
   (lambda (fun args env)
      (format "console.error(~(, ))"
	 (map (lambda (e) (hop2js-expr e env)) args))))

(hashtable-put! expander-table "open-mmap"
   (lambda (fun args env)
      (format "new Mmap(~(, ))"
	 (map (lambda (a) (hop2js-expr a env)) args))))

(hashtable-put! expander-table "string->mmap"
   (lambda (fun args env)
      (format "Mmap.stringToMmap(~(, ))"
	 (map (lambda (a) (hop2js-expr a env)) args))))

(hashtable-put! expander-table "close-mmap"
   (lambda (fun args env)
      (format "~a.close()"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "mmap-length"
   (lambda (fun args env)
      (format "~a.length"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "mmap-ref"
   (lambda (fun args env)
      (format "~a.ref(~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "mmap-get-string"
   (lambda (fun args env)
      (format "~a.toString(~a)"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "mmap-get-char"
   (lambda (fun args env)
      (format "~a.get()"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "mmap-substring"
   (lambda (fun args env)
      (format "~a.substring(~a, ~a)"
	 (hop2js-expr (car args) env)
	 (hop2js-expr (cadr args) env)
	 (hop2js-expr (caddr args) env))))

(hashtable-put! expander-table "mmap-read-position"
   (lambda (fun args env)
      (format "~a.rindex"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "mmap-read-position-set!"
   (lambda (fun args env)
      (format "~a.rindex = ~a"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "mmap-write-position"
   (lambda (fun args env)
      (format "~a.windex"
	 (hop2js-expr (car args) env))))

(hashtable-put! expander-table "mmap-write-position-set!"
   (lambda (fun args env)
      (format "~a.windex = ~a"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "format"
   (lambda (fun args env)
      (if (not (string? (car args)))
	  (error "hop2js" "unsupported \"format\"" `(format ,@args))
	  (if (string=? (car args) "~a")
	      (hop2js-expr (cadr args) env)
	      (let* ((i (string-split (car args) "~"))
		     (s (if (null? (cdr i))
			    (cons "" i)
			    i))
		     (l (append-map (lambda (p a)
				       (if (=fx (string-length p) 1)
					   (list "+" (hop2js-expr a env))
					   (list "+"
					      (hop2js-expr a env)
					      "+\""
					      (hop2js-string (substring p 1))
					      "\"")))
			   (cdr s) (cdr args))))
		 (apply string-append
		    (cons* "\"" (hop2js-string (car s)) "\"" l)))))))

(hashtable-put! expander-table "make-tuple"
   (lambda (fun args env)
      (format "[~a, ~a]"
	 (hop2js-expr (car args) env) (hop2js-expr (cadr args) env))))

(hashtable-put! expander-table "integer->string"
   (lambda (fun args env)
      (if (null? (cdr args))
	  (format "~a.toString()"
	     (hop2js-expr (car args) env))
	  (format "~a.toString(~a)"
	     (hop2js-expr (car args) env)
	     (hop2js-expr (cadr args) env)))))
