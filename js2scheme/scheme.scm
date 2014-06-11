;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/scheme.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 11:47:51 2013                          */
;*    Last change :  Wed Jun 11 08:27:06 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generate a Scheme program from out of the J2S AST.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_compile
	   __js2scheme_stage)
   
   (export j2s-scheme-stage
	   j2s-scheme-eval-stage
	   (generic j2s-scheme ::obj ::symbol ::procedure)
	   (j2s-scheme-id id)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-scheme-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation")
      (proc (lambda (ast args) (j2s-scheme ast 'normal comp-return)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-eval-stage ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-scheme-eval-stage
   (instantiate::J2SStageProc
      (name "scheme")
      (comment "Scheme code generation (eval)")
      (proc (lambda (ast args) (j2s-scheme ast 'normal (lambda (x) x))))))

;*---------------------------------------------------------------------*/
;*    comp-return ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-return x)
   x)

;*---------------------------------------------------------------------*/
;*    acc-return ...                                                   */
;*---------------------------------------------------------------------*/
(define (acc-return expr)
   `(set! %acc ,expr))

;*---------------------------------------------------------------------*/
;*    in-eval? ...                                                     */
;*---------------------------------------------------------------------*/
(define (in-eval? r)
   (not (eq? r comp-return)))

;*---------------------------------------------------------------------*/
;*    eval-return ...                                                  */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-8.9          */
;*---------------------------------------------------------------------*/
(define-macro (eval-return type value target)
   `(if return ,value ,value))

;*---------------------------------------------------------------------*/
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairify loc expr)
   (econs (car expr) (cdr expr) loc))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-id ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-id id)
   (if (memq id '(raise error eval quote module))
       (symbol-append '^ id)
       id))

;*---------------------------------------------------------------------*/
;*    j2s-new ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-new clazz args)
   (let ((new (case (length args)
		 ((0) 'js-new0)
		 ((1) 'js-new1)
		 ((2) 'js-new2)
		 ((3) 'js-new3)
		 ((4) 'js-new4)
		 (else 'js-new))))
      `(,new %this ,clazz ,@args)))

;*---------------------------------------------------------------------*/
;*    j2s-fast-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-fast-id id)
   (symbol-append '% id))

;*---------------------------------------------------------------------*/
;*    epairify-deep ...                                                */
;*---------------------------------------------------------------------*/
(define (epairify-deep loc expr)
   (if (or (epair? expr) (not (pair? expr)))
       expr
       (econs (epairify-deep loc (car expr))
	  (epairify-deep loc (cdr expr))
	  loc)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (j2s-scheme this mode return::procedure)
   (if (pair? this)
       (map (lambda (e) (j2s-scheme e mode return)) this)
       this))

;*---------------------------------------------------------------------*/
;*    flatten-stmt ...                                                 */
;*---------------------------------------------------------------------*/
(define (flatten-stmt stmt)
   (when (and (pair? stmt) (eq? (car stmt) 'begin))
      (set-cdr! stmt (flatten-nodes (cdr stmt))))
   stmt)

;*---------------------------------------------------------------------*/
;*    flatten-nodes ...                                                */
;*---------------------------------------------------------------------*/
(define (flatten-nodes nodes)
   (append-map
      (lambda (l)
	 (if (and (pair? l) (eq? (car l) 'begin))
	     (flatten-nodes (cdr l))
	     (list l)))
      nodes))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SProgram mode return)

   (define (unserialize)
      `(define (%unserialize alist)
	  (with-access::JsGlobalObject %this (js-object)
	     (let ((obj (js-new %this js-object)))
		(for-each (lambda (e)
			     (js-put! obj (keyword->symbol (car e))
				(cadr e) #f %this))
		   alist)
		obj))))
   
   (define (j2s-module module body)
      (with-access::J2SProgram this (nodes mode pcache-size)
	 (list
	    module
	    (when (>fx pcache-size 0)
	       `(define %PCACHE (make-pcache ,pcache-size)))
	    '(define %source (or (the-loading-file) "/"))
	    '(define %resource (dirname %source))
	    `(define (hopscript %this)
		,(unserialize)
		(define %worker (js-current-worker))
		(define this %this)
		,@body
		,(j2s-scheme-id 'module)))))

   (define (j2s-main-module name body)
      (let ((module `(module ,(string->symbol name)
			(eval (library hop)
			   (library hopscript)
			   (library nodejs)
			   (library js2scheme))
			(library hop hopscript nodejs js2scheme)
			(main main))))
	 (with-access::J2SProgram this (nodes mode pcache-size)
	    (list
	       module
	       (when (>fx pcache-size 0)
		  `(define %PCACHE (make-pcache ,pcache-size)))
	       '(define %this (nodejs-new-global-object))
	       '(define this %this)
	       '(define %source (or (the-loading-file) "/"))
	       '(define %resource (dirname %source))
	       (unserialize)
	       `(define (main args)
		   (define %worker (js-init-main-worker! %this))
		   (nodejs-auto-require! %worker %this)
		   ,@body
		   (js-worker-terminate! %worker)
		   (thread-join! (thread-start-joinable! %worker)))))))
	 

   (with-access::J2SProgram this (module main nodes mode name pcache-size)
      (let ((body (flatten-nodes (j2s-scheme nodes mode return))))
	 (cond
	    (module
	     ;; a module whose declaration is in the source
	     (j2s-module module body))
	    ((not name)
	     ;; a mere expression
	     `(lambda (%this)
		 ,(unserialize)
		 ,(when (>fx pcache-size 0)
		     `(define %PCACHE (make-pcache ,pcache-size)))
		 (define %worker (js-current-worker))
		 (define this %this)
		 '(define %source (or (the-loading-file) "/"))
		 '(define %resource (dirname %source))
		 (js-undefined)
		 ,@body))
	    (main
	     ;; generate a main hopscript module
	     (j2s-main-module name body))
	    (else
	     ;; generate the module clause
	     (let ((module `(module ,(string->symbol name)
			       (library hop hopscript js2scheme)
			       (export (hopscript ::JsGlobalObject)))))
		(j2s-module module body)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SVarDecls ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SVarDecls mode return)
   (illegal-node "j2s-scheme" this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-decl ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-decl this::J2SDecl value mode return)
   (with-access::J2SDecl this (loc id name global)
      (let ((ident (or name (j2s-scheme-id id))))
	 (epairify-deep loc
	    (if global
		(let ((fun-name (format "lambda ~a:~a" (cadr loc) (caddr loc))))
		   (if (in-eval? return)
		       (j2s-unresolved-put! '%this `',ident
			  value #f 'normal)
		       `(begin
			   (define ,ident ,value)
			   (js-bind! %this %this ',ident
			      :configurable #f
			      :get (js-make-function %this
				      (lambda (%) ,ident)
				      1 ,fun-name)
			      :set (js-make-function %this
				      (lambda (% %v)
					 (set! ,ident %v))
				      2 ,fun-name)))))
		`(define ,ident ,value))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDecl mode return)
   (with-access::J2SDecl this (loc id)
      (j2s-scheme-decl this '(js-undefined) mode return)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-set! lhs val result mode return)
   (cond
      ((isa? lhs J2SRef)
       (with-access::J2SRef lhs (decl)
	  (with-access::J2SDecl decl (writable global id)
	     (if writable
		 (cond
		    ((and global (in-eval? return))
		     `(begin
			 ,(j2s-put! '%this `',id
			     val (eq? mode 'strict) #f)
			 ,result))
		    (result
		     `(begin
			 (set! ,(j2s-scheme lhs mode return) ,val)
			 ,result))
		    (else
		     `(set! ,(j2s-scheme lhs mode return) ,val)))
		 val))))
      ((not result)
       `(set! ,(j2s-scheme lhs mode return) ,val))
      (else
       `(begin
	   (set! ,(j2s-scheme lhs mode return) ,val)
	   ,result))))
	      
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclInit mode return)
   (with-access::J2SDeclInit this (loc id name val writable)
      (let ((ident (or name (j2s-scheme-id id))))
	 (epairify loc
	    (if writable
		`(begin
		    (set! ,ident ,(j2s-scheme val mode return))
		    (js-undefined))
		`(begin
		    ,(j2s-scheme val mode return)
		    (js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclFun mode return)
   (with-access::J2SDeclFun this (loc id name val global)
      (with-access::J2SFun val (params mode)
	 (let* ((scmid (or name (j2s-scheme-id id)))
		(fastid (j2s-fast-id id)))
	    (epairify-deep loc
	       (if global 
		   `(begin
		       (define ,fastid
			  ,(jsfun->scheme val mode return))
		       (define ,scmid
			  (js-bind! %this %this ',scmid
			     :configurable #f
			     :value (js-make-function  %this
				       ,fastid
				       ,(length params) ,(symbol->string! id)
				       :strict ,(eq? mode 'strict)
				       :alloc (lambda (o)
						 (js-object-alloc o %this))
				       :construct ,fastid))))
		   `(begin
		       (define ,fastid
			  ,(jsfun->scheme val mode return))
		       (define ,scmid
			  (js-make-function %this
			     ,fastid ,(length params)
			     ',id
			     :strict ,(eq? mode 'strict)
			     :alloc (lambda (o) (js-object-alloc o %this))
			     :construct ,fastid)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclSvc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclSvc mode return)
   (with-access::J2SDeclSvc this (loc id name val)
      (let ((scmid (or name (j2s-scheme-id id))))
	 (epairify-deep loc
    `(define ,scmid ,(jssvc->scheme val scmid mode return))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclTag ...                                      */
;*---------------------------------------------------------------------*/
;* (define-method (j2s-scheme this::J2SDeclTag mode return)            */
;*                                                                     */
;*    (define (jstag->scheme this::J2STag id mode return)              */
;*                                                                     */
;*       (define (init->formal init::J2SDataPropertyInit)              */
;* 	 (with-access::J2SDataPropertyInit init (name val)             */
;* 	    (with-access::J2SString name ((name val))                  */
;* 	       (list (string->symbol name) (j2s-scheme val mode return))))) */
;*                                                                     */
;*       (define (tag-parameters-sans-attribute args)                  */
;* 	 (cond                                                         */
;* 	    ((null? args)                                              */
;* 	     '())                                                      */
;* 	    ((null? (cdr args))                                        */
;* 	     (with-access::J2SDecl (car args) (id)                     */
;* 		(list id)))                                            */
;* 	    (else                                                      */
;* 	     (list                                                     */
;* 		(with-access::J2SDecl (car args) (id) (list id))       */
;* 		(with-access::J2SDecl (cadr args) (id) id)))))         */
;*                                                                     */
;*       (define (tag-parameters args)                                 */
;* 	 (cond                                                         */
;* 	    ((null? args)                                              */
;* 	     '())                                                      */
;* 	    ((isa? (car args) J2SObjInit)                              */
;* 	     (with-access::J2SObjInit (car args) (inits)               */
;* 		(append (map init->formal inits)                       */
;* 		   (tag-parameters-sans-attribute (cdr args)))))       */
;* 	    (else                                                      */
;* 	     (tag-parameters-sans-attribute args))))                   */
;*                                                                     */
;*       (define (tag-parameters-attributes args)                      */
;* 	 (match-case args                                              */
;* 	    ((?- ?attributes ?-)                                       */
;* 	     (with-access::J2SDecl attributes (id)                     */
;* 		id))))                                                 */
;*                                                                     */
;*       (define (tag-parameters-nodes args)                           */
;* 	 (match-case args                                              */
;* 	    ((?- ?- ?nodes)                                            */
;* 	     (with-access::J2SDecl nodes (id)                          */
;* 		id))))                                                 */
;*                                                                     */
;*       (with-access::J2STag this (id inits body need-bind-exit-return) */
;* 	 (let ((attributes (tag-parameters-attributes inits))          */
;* 	       (nodes (tag-parameters-nodes inits)))                   */
;* 	    `(define-tag ,(symbol-append '< id '>) ,(tag-parameters inits) */
;* 		,(when attributes                                      */
;* 		    (let ((id (j2s-scheme-id attributes)))             */
;* 		       `(set! ,id                                      */
;* 			   (js-plist->jsobject ,id %this))))           */
;* 		,(when nodes                                           */
;* 		    (let ((id (j2s-scheme-id nodes)))                  */
;* 		       `(set! ,id                                      */
;* 			   (js-vector->jsarray (apply vector ,id) %this)))) */
;* 		,(if need-bind-exit-return                             */
;* 		     (with-access::J2SNode body (loc)                  */
;* 			(epairify loc                                  */
;* 			   (return-body                                */
;* 			      (flatten-stmt (j2s-scheme body mode return))))) */
;* 		     (flatten-stmt (j2s-scheme body mode return))))))) */
;*                                                                     */
;*    (with-access::J2SDeclTag this (loc id name val)                  */
;*       (let ((scmid (or name (j2s-scheme-id id))))                   */
;* 	 (epairify-deep loc                                            */
;* 	    (jstag->scheme val scmid mode return)))))                  */

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclExtern ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclExtern mode return)
   (with-access::J2SDeclExtern this (loc id name val bind)
      (if bind
	  (j2s-scheme-decl this (j2s-scheme val mode return) mode return)
	  (epairify loc
	     `(define ,(or name (j2s-scheme-id id)) ,(j2s-scheme val mode return))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return)
   (with-access::J2SInit this (lhs rhs loc)
      (epairify loc
	 (j2s-scheme-set! lhs (j2s-scheme rhs mode return) '(js-undefined) mode return))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRef mode return)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (id name global)
	 (if (and global (in-eval? return))
	     `(js-get-global-object-name %this ',id #f %this)
	     (or name (j2s-scheme-id id))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWithRef ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWithRef mode return)
   (with-access::J2SWithRef this (id withs expr loc)
      (epairify loc
	 (let loop ((withs withs))
	    (if (null? withs)
		(j2s-scheme expr mode return)
		`(if (js-in? ',id ,(car withs) %this)
		     (js-get ,(car withs) ',id %this)
		     ,(loop (cdr withs))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SHopRef ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SHopRef mode return)
   (with-access::J2SHopRef this (id)
      id))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThis ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThis mode return)
   (with-access::J2SThis this (loc)
      'this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCond mode return)
   (with-access::J2SCond this (loc test then else)
      (let ((tmp (gensym)))
	 (epairify loc
	    `(let ((,tmp ,(j2s-scheme test mode return)))
		(if (if (boolean? ,tmp) ,tmp (js-totest ,tmp))
		    ,(j2s-scheme then mode return)
		    ,(j2s-scheme else mode return)))))))

;*---------------------------------------------------------------------*/
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define (pcache cache)
   `(pcache-ref %PCACHE ,cache))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved name cache throw)
   (if cache
       `(js-get-global-object-name/cache %this ',name
	   ,(pcache cache)
	   ,(if (pair? throw) `',throw throw)
	   %this)
       `(js-get-global-object-name %this ',name
	   ,(if (pair? throw) `',throw throw)
	   %this)))

;*---------------------------------------------------------------------*/
;*    j2s-unresolved-put! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-unresolved-put! obj field expr throw::bool mode::symbol)
   (if (eq? mode 'strict)
       `(js-unresolved-put! ,obj ,field ,expr #t %this)
       `(js-put! ,obj ,field ,expr ,throw %this)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnresolvedRef ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnresolvedRef mode return)
   (with-access::J2SUnresolvedRef this (loc cache id)
      (epairify loc
	 (j2s-unresolved id cache loc))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArrayAbsent ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArrayAbsent mode return)
   (with-access::J2SArrayAbsent this (loc)
      (epairify loc '(js-absent))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLiteralValue ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLiteralValue mode return)
   (with-access::J2SLiteralValue this (val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNumber mode return)
   (with-access::J2SNumber this (val)
      (cond
	 ((elong? val)
	  (elong->flonum val))
	 ((llong? val)
	  (llong->flonum val))
	 ((bignum? val)
	  (bignum->flonum val))
	 ((fixnum? val)
	  (cond-expand
	     (bint30
	      val)
	     (else
	      (if (or (>=fx val (bit-lsh 1 30)) (<fx val (negfx (bit-lsh 1 30))))
		  (fixnum->flonum val)
		  val))))
	 (else val))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SString ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SString mode return)
   (with-access::J2SString this (loc val)
      val))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRegExp ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRegExp mode return)
   (with-access::J2SRegExp this (loc val flags)
      (epairify loc
	 `(with-access::JsGlobalObject %this (js-regexp)
	     ,(j2s-new 'js-regexp
		 (if (string-null? flags)
		     (list val)
		     (list val flags)))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return)
   (with-access::J2SArray this (loc exprs)
      (let ((sexprs (j2s-scheme exprs mode return)))
	 (if (every (lambda (x)
			(or (number? x) (string? x) (boolean? x)))
		sexprs)
	     (epairify loc `(js-vector->jsarray ',(list->vector sexprs) %this))
	     (epairify loc `(js-vector->jsarray (vector ,@sexprs) %this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNull ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNull mode return)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-null))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUndefined ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUndefined mode return)
   (with-access::J2SLiteral this (loc)
      (epairify loc '(js-undefined))))

;*---------------------------------------------------------------------*/
;*    return-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-body body)
   `(bind-exit (%return)
       ,body))

;*---------------------------------------------------------------------*/
;*    jsfun->scheme ...                                                */
;*---------------------------------------------------------------------*/
(define (jsfun->scheme this::J2SFun mode return)

   (define (lambda-or-labels id args body)
      (if id
	  (let ((%id (symbol-append '% id)))
	     `(labels ((,%id ,(cons 'this args) ,body)) ,%id))
	  `(lambda ,(cons 'this args)
	      ,body)))
   
   (define (param-scheme-id param)
      (with-access::J2SDecl param (id name)
	 (or name (j2s-scheme-id id))))
   
   (define (fixarg-lambda id params body)
      (let ((args (j2s-scheme params mode return)))
	 (lambda-or-labels id args body)))
   
   (define (init-alias-argument argument rest indx)
      (let ((id (param-scheme-id argument)))
	 `(begin
	     (set! ,id (car ,rest))
	     (js-arguments-define-own-property arguments ,indx
		(instantiate::JsAccessorDescriptor
		   (name (string->symbol (integer->string ,indx)))
		   (get (js-make-function %this
			   (lambda (%) ,id) 0 "get"))
		   (set (js-make-function %this
			   (lambda (% %v) (set! ,id %v)) 1 "set"))
		   (configurable #t)
		   (enumerable #t))))))
   
   (define (init-argument val indx)
      `(js-arguments-define-own-property arguments ,indx
	  (instantiate::JsValueDescriptor
	     (name (string->symbol (integer->string ,indx)))
	     (value ,val)
	     (writable #t)
	     (configurable #t)
	     (enumerable #t))))
   
   (define (normal-vararg-lambda id params::pair-nil body)
      ;; normal mode: arguments is an alias
      (let ((rest (gensym 'rest))
	    (id (or id (gensym 'fun))))
	 (lambda-or-labels id rest
	    `(let ((arguments
		      (js-arguments %this
			 (make-vector (length ,rest) (js-absent)))))
		,@(if (pair? params)
		      (map (lambda (param)
			      (with-access::J2SDecl param (id name loc)
				 (epairify loc
				    `(define ,(or name (j2s-scheme-id id))
					(js-undefined)))))
			 params)
		      '())
		,(when (pair? params)
		    `(when (pair? ,rest)
			,(init-alias-argument (car params) rest 0)
			(set! ,rest (cdr ,rest))
			,(let loop ((params (cdr params))
				    (i 1))
			    (if (null? params)
				#unspecified
				`(when (pair? ,rest)
				    ,(init-alias-argument (car params) rest i)
				    (set! ,rest (cdr ,rest))
				    ,(loop (cdr params) (+fx i 1)))))))
		(let loop ((,rest ,rest)
			   (i ,(length params)))
		   (when (pair? ,rest)
		      ,(init-argument `(car ,rest) 'i)
		      (loop (cdr ,rest) (+fx i 1))))
		(js-define-own-property arguments 'callee
		   (instantiate::JsValueDescriptor
		      (name 'callee)
		      (value (js-make-function %this
				,(symbol-append '% id) 0 ',id))
		      (writable #t)
		      (configurable #t)
		      (enumerable #f))
		   #f
		   %this)
		,body))))
   
   (define (strict-vararg-lambda id params::pair-nil body)
      ;; strict mode: arguments is initialized on entrance
      (let ((rest (gensym 'rest)))
	 (lambda-or-labels id rest
	    `(let ((arguments (js-strict-arguments %this ,rest)))
		,@(if (pair? params)
		      (map (lambda (param)
			      (with-access::J2SDecl param (id name loc)
				 (epairify loc
				    `(define ,(or name (j2s-scheme-id id))
					(js-undefined)))))
			 params)
		      '())
		,(when (pair? params)
		    `(when (pair? ,rest)
			(set! ,(param-scheme-id (car params)) (car ,rest))
			,(let loop ((params (cdr params)))
			    (if (null? params)
				#unspecified
				`(when (pair? (cdr ,rest))
				    (set! ,rest (cdr ,rest))
				    (set! ,(param-scheme-id (car params))
				       (car ,rest))
				    ,(loop (cdr params)))))))
		,body))))

   (with-access::J2SFun this (loc id params body need-bind-exit-return vararg mode)
      (let* ((body (if need-bind-exit-return
		       (with-access::J2SNode body (loc)
			  (epairify loc
			     (return-body
				(flatten-stmt (j2s-scheme body mode return)))))
		       (flatten-stmt (j2s-scheme body mode return))))
	     (fun (cond
		     ((not vararg)
		      (fixarg-lambda id params body))
		     ((eq? mode 'normal)
		      (normal-vararg-lambda id params body))
		     (else
		      (strict-vararg-lambda id params body)))))
	 (epairify-deep loc fun))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFun mode return)
   (with-access::J2SFun this (loc params mode id)
      (let* ((tmp (gensym))
	     (fundef `(let ((,tmp ,(jsfun->scheme this mode return)))
			 (js-make-function %this
			    ,tmp
			    ,(length params)
			    ,(format "lambda ~a: ~a" (cadr loc) (caddr loc))
			    :strict ,(eq? mode 'strict)
			    :alloc (lambda (o) (js-object-alloc o %this))
			    :construct ,tmp))))
	 (epairify-deep loc
	    (if id
		`(let ((,id (js-undefined)))
		    (set! ,id ,fundef)
		    ,id)
		fundef)))))

;*---------------------------------------------------------------------*/
;*    jssvc->scheme ::J2SSvc ...                                       */
;*---------------------------------------------------------------------*/
(define (jssvc->scheme this::J2SSvc id mode return)
   
   (define (j2sscheme-service this tmp id path args mode return)
      
      (define (jscript-funcall init)
	 ;; see runtime/service_expd.sch
	 (if (isa? init J2SObjInit)
	     "(sc_lambda = function ( argument ) { return new HopFrame( hop_apply_url( ~s, hop_object_to_dsssl_args( argument ) ) ); },
              sc_lambda.resource = function( file ) { return ~s + \"/\" + file; },
              sc_lambda)"
	     "(sc_lambda = function () { return new HopFrame( hop_apply_url( ~s, arguments ) ); },
              sc_lambda.resource = function( file ) { return ~s + \"/\" + file; },
              sc_lambda)"))
      
      (define (service-proc->scheme this)
	 (with-access::J2SSvc this (loc body need-bind-exit-return)
	    (let* ((body (if need-bind-exit-return
			     (with-access::J2SNode body (loc)
				(epairify loc
				   (return-body
				      (j2s-scheme body mode return))))
			     (j2s-scheme body mode return)))
		   (fun `(lambda ,args
			    (let ((req (current-request)))
			       (js-worker-exec %worker
				  (lambda ()
				     (thread-request-set! (current-thread) req)
				     ,(flatten-stmt body)))))))
	       (epairify-deep loc fun))))
      
      (with-access::J2SSvc this (init register)
	 `(js-make-service %this ,tmp ',id
	     (,(if register 'register-service! 'begin)
	      (instantiate::hop-service
		 (proc ,(service-proc->scheme this))
		 (javascript ,(jscript-funcall init))
		 (path ,path)
		 (id ',id)
		 (wid ',id)
		 (args ',args)
		 (resource %resource)
		 (source %source)
		 (decoder %unserialize))))))
   
   (define (init->formal init::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit init (name val)
	 (with-access::J2SString name ((name val))
	    (list (string->symbol name) (j2s-scheme val mode return)))))
   
   (define (init->actual init::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit init (name val)
	 (with-access::J2SString name ((name val))
	    (list (string->keyword name) (string->symbol name)))))
   
   (define (svc-proc-entry this params actuals)
      (with-access::J2SSvc this (loc)
	 (let ((tmpp (gensym 'servicep))
	       (tmps (gensym 'services)))
	    `(letrec* ((,tmpp (lambda (this ,@params #!rest rest)
				 (with-access::JsService ,tmps (svc)
				    (js-make-hopframe %this
				       (hop-apply-service-url svc 
					  (if (and (pair? rest)
						   (isa? (car rest) JsObject))
					      (js-object->keyword-arguments
						 (car rest) %this)
					      (list ,@actuals)))))))
		       (,tmps ,(j2sscheme-service this tmpp (or id tmpp)
				  (epairify loc
				     `(make-hop-url-name
					 ,(if (symbol? id)
					      (symbol->string id)
					      '(gen-service-url :public #t))))
				  params
				  mode return)))
		,tmps))))
   
   (define (svc-fix-proc-entry this)
      (with-access::J2SSvc this (params)
	 (let ((params (j2s-scheme params mode return)))
	    (svc-proc-entry this params params))))
   
   (define (svc-dsssl-proc-entry this)
      (with-access::J2SSvc this (init)
	 (with-access::J2SObjInit init (inits)
	    (let ((params (cons '#!key (map init->formal inits)))
		  (actuals (append-map init->actual inits)))
	       (svc-proc-entry this params actuals)))))
   
   (with-access::J2SSvc this (loc init)
      (epairify-deep loc
	 (if (isa? init J2SObjInit)
	     (svc-dsssl-proc-entry this)
	     (svc-fix-proc-entry this)))))
	   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSvc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSvc mode return)
   (with-access::J2SSvc this (loc)
      (epairify loc
	 (jssvc->scheme this #f mode return))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParam ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParam mode return)
   (with-access::J2SParam this (id name)
      (or name (j2s-scheme-id id))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SReturn mode return)
   (with-access::J2SReturn this (loc expr tail)
      (if tail
	  (j2s-scheme expr mode return)
	  (epairify loc
	     `(%return ,(j2s-scheme expr mode return))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SThrow ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SThrow mode return)
   (with-access::J2SThrow this (loc expr)
      (epairify loc
	 `(raise ,(j2s-scheme expr mode return)))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STry ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STry mode return)
   (with-access::J2STry this (loc body catch finally)
      (epairify-deep loc
	 (let* ((trybody (j2s-scheme body mode return))
		(trie (if (isa? catch J2SNop)
			  (j2s-scheme body mode return)
			  (with-access::J2SCatch catch (loc param body)
			     (epairify-deep loc
				`(with-handler
				    (lambda (,(j2s-scheme param mode return))
				       ,(j2s-scheme body mode return))
				    ,trybody))))))
	    (if (isa? finally J2SNop)
		trie
		`(unwind-protect
		    ,trie
		    ,(j2s-scheme finally mode return)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWith ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWith mode return)
   (with-access::J2SWith this (obj block id)
      `(let ((,id (js-toobject %this ,(j2s-scheme obj mode return))))
	  ,(j2s-scheme block mode return))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPragma mode return)
   (with-access::J2SPragma this (loc expr)
      (epairify-deep loc expr)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSequence mode return)
   (with-access::J2SSequence this (loc exprs)
      (if (pair? (cdr exprs))
	  (epairify loc `(begin ,@(j2s-scheme exprs mode return)))
	  (j2s-scheme (car exprs) mode return))))

;*---------------------------------------------------------------------*/
;*    js-binop ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-binop op lhsv rhsv)
   (case op
      ((OR)
       (let ((lhs (gensym 'lhs)))
	  `(let ((,lhs ,lhsv))
	      (if (js-toboolean ,lhs)
		  ,lhs
		  ,rhsv))))
      ((&&)
       (let ((lhs (gensym 'lhs)))
	  `(let ((,lhs ,lhsv))
	      (if (js-toboolean ,lhs)
		  ,rhsv
		  ,lhs))))
      (else
       (let ((lhs (gensym 'lhs))
	     (rhs (gensym 'rhs)))
	  `(let* ((,lhs ,lhsv)
		  (,rhs ,rhsv))
	      ,(case op
		  ((+)
		   `(js+ ,lhs ,rhs %this))
		  ((-)
		   `(js- ,lhs ,rhs %this))
		  ((*)
		   `(js* ,lhs ,rhs %this ))
		  ((/)
		   `(js/ ,lhs ,rhs %this))
		  ((%)
		   `(js% ,lhs ,rhs %this))
		  ((<)
		   `(js< ,lhs ,rhs %this))
		  ((<=)
		   `(js<= ,lhs ,rhs %this))
		  ((>)
		   `(js> ,lhs ,rhs %this))
		  ((>=)
		   `(js>= ,lhs ,rhs %this))
		  ((==)
		   `(js-equal? ,lhs ,rhs %this))
		  ((!=)
		   `(not (js-equal? ,lhs ,rhs %this)))
		  ((===)
		   `(js-strict-equal? ,lhs ,rhs))
		  ((!==)
		   `(not (js-strict-equal? ,lhs ,rhs)))
		  ((<-)
		   `(js<- ,lhs ,rhs %this))
		  ((instanceof)
		   `(js-instanceof? ,lhs ,rhs %this))
		  ((in)
		   `(js-in? ,lhs ,rhs %this))
		  ((&)
		   `(js-bitand ,lhs ,rhs %this))
		  ((BIT_OR)
		   `(js-bitor ,lhs ,rhs %this))
		  ((^)
		   `(js-bitxor ,lhs ,rhs %this))
		  ((>>)
		   `(js-bitrsh ,lhs ,rhs %this))
		  ((>>>)
		   `(js-bitursh ,lhs ,rhs %this))
		  ((<<)
		   `(js-bitlsh ,lhs ,rhs %this))
		  (else
		   `(,op ,lhs ,rhs %this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBinary mode return)
   (with-access::J2SBinary this (loc lhs rhs op)
      (epairify-deep loc
	 (js-binop op
	    (j2s-scheme lhs mode return) (j2s-scheme rhs mode return)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SParen mode return)
   (with-access::J2SParen this (expr)
      (j2s-scheme expr mode return)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SUnary mode return)

   (define (err id)
      (with-access::J2SUnary this (loc)
	 (match-case loc
	    ((at ?fname ?loc)
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(format "Delete of an unqualified identifier in strict mode: \"~a\"" id)
		       ,fname ,loc))))
	    (else
	     `(with-access::JsGlobalObject %this (js-syntax-error)
		 (js-raise
		    (js-new %this js-syntax-error
		       ,(format "Delete of an unqualified identifier in strict mode: \"~a\"" id))))))))

   (define (delete->scheme expr)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-8.12.7
      (cond
	 ((isa? expr J2SWithRef)
	  (with-access::J2SWithRef expr (id withs expr loc)
	     (let loop ((withs withs))
		(if (null? withs)
		    `(begin ,(j2s-scheme expr mode return) #f)
		    `(if (js-in? ',id ,(car withs) %this)
			 (js-delete! ,(j2s-scheme (car withs) mode return)
			    ',(j2s-scheme id mode return)
			    #f
			    %this)
			 ,(loop (cdr withs)))))))
	 ((isa? expr J2SAccess)
	  (with-access::J2SAccess expr (obj field)
	     `(js-delete! ,(j2s-scheme obj mode return)
		 ,(j2s-scheme field mode return)
		 ,(eq? mode 'strict)
		 %this)))
	 ((isa? expr J2SUnresolvedRef)
	  (if (eq? mode 'strict)
	      (with-access::J2SUnresolvedRef expr (id)
		 (err id))
	      (with-access::J2SUnresolvedRef expr (id)
		 `(js-delete! %this ',id #f %this))))
	 ((isa? expr J2SRef)
	  (if (eq? mode 'strict)
	      (with-access::J2SRef expr (decl)
		 (with-access::J2SDecl decl (id)
		    (err id)))
	      '(begin #f)))
	 (else
	  `(begin ,(j2s-scheme expr mode return) #t))))
   
   (with-access::J2SUnary this (loc expr op)
      (epairify loc
	 (case op
	    ((!)
	     `(if (js-totest ,(j2s-scheme expr mode return)) #f #t))
	    ((typeof)
	     (if (isa? expr J2SUnresolvedRef)
		 ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.3
		 (with-access::J2SUnresolvedRef expr (id loc cache)
		    `(js-typeof ,(j2s-unresolved id cache #f)))
		 `(js-typeof ,(j2s-scheme expr mode return))))
	    ((void)
	     `(begin ,(j2s-scheme expr mode return) (js-undefined)))
	    ((delete)
	     (delete->scheme expr))
	    ((+)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.6
	     (let ((expr (j2s-scheme expr mode return)))
		(if (eqv? expr 0)
		    `(begin +0.0)
		    `(js-tonumber ,expr %this))))
	    ((-)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.7
	     (let ((expr (j2s-scheme expr mode return)))
		(if (eqv? expr 0)
		    `(begin -0.0)
		    `(js-neg ,expr %this))))
	    ((~)
	     ;; http://www.ecma-international.org/ecma-262/5.1/#sec-11.4.8
	     `(js-bitnot ,(j2s-scheme expr mode return) %this))
	    (else
	     `(,op ,(j2s-scheme expr mode return)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPostfix ...                                      */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-11.3.1       */
;*    -------------------------------------------------------------    */
;*    !!! x++ not equivalent to x = x + 1 as x++ always converts       */
;*    to number.                                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPostfix mode return)
   (with-access::J2SPostfix this (loc lhs op)
      (let ((inc (if (eq? op '++) 1 -1)))
	 (cond
	    ((isa? lhs J2SRef)
	     (let ((tmp (gensym 'tmp)))
		(epairify-deep loc
		   `(let ((,tmp (js-tonumber ,(j2s-scheme lhs mode return) %this)))
		       ,(j2s-scheme-set! lhs
			   (epairify loc `(js+ ,inc ,tmp %this))
			   tmp
			   mode return)))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs ((o obj) field cache)
		(let ((tmp (gensym 'tmp))
		      (obj (gensym 'obj))
		      (pro (gensym 'prop))
		      (prov (j2s-scheme field mode return)))
		   (epairify-deep loc
		      `(let* ((,obj ,(j2s-scheme o mode return))
			      ,@(if (string? prov) '() (list `(,pro ,prov)))
			      (,tmp (js-tonumber
					,(j2s-get obj
					   (if (string? prov) prov pro)
					   cache)
					%this)))
			  (js-put! ,obj ,(if (string? prov) prov pro)
			     (js+ ,inc ,tmp %this) ,(eq? mode 'strict) %this)
			  ,tmp)))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id cache loc)
		(let ((tmp (gensym 'tmp)))
		   (epairify-deep loc
		      `(let ((,tmp (js-tonumber ,(j2s-unresolved id cache loc)
				      %this)))
			  ,(j2s-unresolved-put! '%this `',id
			      `(+ ,inc ,tmp)
			      #t mode)
			  ,tmp)))))
	    (else
	     (j2s-error "j2sscheme"
		(format "Illegal postfix \"~a\"" op)
		this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SPrefix ...                                       */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/5.1/#sec-11.3.1prefix        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SPrefix mode return)
   (with-access::J2SPrefix this (loc lhs op)
      (let ((inc (if (eq? op '++) 1 -1)))
	 (cond
	    ((isa? lhs J2SRef)
	     (epairify loc
		(j2s-scheme-set! lhs
		   (epairify loc
		      `(js+ ,inc (js-tonumber ,(j2s-scheme lhs mode return) %this)  %this))
		   (j2s-scheme lhs mode return)
		   mode return)))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs ((o obj) field cache)
		(let ((tmp (gensym 'tmp))
		      (obj (gensym 'obj))
		      (pro (gensym 'prop))
		      (prov (j2s-scheme field mode return)))
		   (epairify-deep loc
		      `(let* ((,obj ,(j2s-scheme o mode return))
			      ,@(if (string? prov) '() (list `(,pro ,prov)))
			      (,tmp (js-tonumber
				       ,(j2s-get obj
					   (if (string? prov) prov pro)
					   cache)
				       %this)))
			  (js-put! ,obj ,(if (string? prov) prov pro)
			     (js+ ,inc ,tmp %this) ,(eq? mode 'strict) %this)
			  (+ ,inc ,tmp))))))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id cache loc)
		(let ((tmp (gensym 'tmp)))
		   (epairify-deep loc
		      `(let ((,tmp (js+ ,inc
				      (js-tonumber ,(j2s-unresolved id cache loc)
					 %this )
				      %this)))
			  ,(j2s-unresolved-put! '%this `',id
			      tmp #t mode)
			  ,tmp)))))
	    (else
	     (j2s-error "j2sscheme"
		(format "Illegal prefix \"~a\"" op)
		this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmt ...                                         */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12           */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmt mode return)
   (return this))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSeq ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.1         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSeq mode return)
   (with-access::J2SSeq this (loc nodes)
      (let ((nodes nodes))
	 (cond
	    ((null? nodes)
	     (epairify loc
		(return '(js-undefined))))
	    ((pair? (cdr nodes))
	     (epairify loc
		`(begin ,@(j2s-scheme nodes mode return))))
	    (else
	     (j2s-scheme (car nodes) mode return))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNop ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.3         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNop mode return)
   (with-access::J2SNop this (loc)
      (epairify loc
	 (return '(js-undefined)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SStmtExpr ...                                     */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.4         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SStmtExpr mode return)
   (with-access::J2SStmtExpr this (expr)
      (if (isa? expr J2SIf)
	  (j2s-scheme expr mode return)
	  (return (j2s-scheme expr mode return)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SIf ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.5         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SIf mode return)
   (with-access::J2SIf this (loc test then else)
      (let ((tmp (gensym)))
	 (epairify loc
	    `(let ((,tmp ,(j2s-scheme test mode return)))
		(if (if (boolean? ,tmp) ,tmp (js-totest ,tmp))
		    ,(j2s-scheme then mode return)
		    ,(j2s-scheme else mode return)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDo ...                                           */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.1       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDo mode return)
   (with-access::J2SDo this (loc test body id
			       need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,(j2s-scheme body mode return)))
		    (j2s-scheme body mode return))
	       (if (js-totest ,(j2s-scheme test mode return))
		   (,loop)
		   '(js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       ,(if need-bind-exit-continue
		    (epairify-deep loc
		       `(bind-exit (,(escape-name '%continue id))
			   ,(j2s-scheme body mode acc-return)))
		    (j2s-scheme body mode acc-return))
	       (if (js-totest ,(j2s-scheme test mode return))
		   (,loop %acc)
		   %acc)))
      
      (let* ((doid (gensym 'do))
	     (loop (if (in-eval? return) (eval-loop doid) (comp-loop doid))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		(epairify-deep loc `(bind-exit (,(escape-name '%break id)) ,loop))
		(epairify loc loop))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SWhile ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.2       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SWhile mode return)
   (with-access::J2SWhile this (loc test body id
				  need-bind-exit-break need-bind-exit-continue)

      (define (comp-loop loop)
	 `(let ,loop ()
	       (if (js-totest ,(j2s-scheme test mode return))
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode return))
			       (,loop)))
			(epairify-deep loc
			   `(begin
			       ,(j2s-scheme body mode return)
			       (,loop))))
		   '(js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if (js-totest ,(j2s-scheme test mode return))
		   ,(if need-bind-exit-continue
			(epairify-deep loc
			   `(begin
			       (bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return))
			       (,loop %acc)))
			(epairify-deep loc
			   `(begin
			       ,(j2s-scheme body mode acc-return)
			       (,loop %acc))))
		   %acc)))
      
      (let* ((whileid (gensym 'while))
	     (loop (if (in-eval? return) (eval-loop whileid) (comp-loop whileid))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		(epairify-deep loc `(bind-exit (,(escape-name '%break id)) ,loop))
		(epairify loc loop))))))

;*---------------------------------------------------------------------*/
;*    escape-name ...                                                  */
;*---------------------------------------------------------------------*/
(define (escape-name escape id)
   (if (symbol? id)
       (symbol-append escape '- id)
       escape))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SFor ...                                          */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.3       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SFor mode return)
   (with-access::J2SFor this (loc init test incr body id
				need-bind-exit-break
				need-bind-exit-continue
				need-bind-exit-continue-label)
      
      (define (comp-loop loop)
	 `(let ,loop ()
	       (if (js-totest ,(j2s-scheme test mode return))
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode return)))
			   (j2s-scheme body mode return))
		      ,(j2s-scheme incr mode return)
		      (,loop))
		   (js-undefined))))

      (define (eval-loop loop)
	 `(let ,loop ((%acc (js-undefined)))
	       (if (js-totest ,(j2s-scheme test mode return))
		   (begin
		      ,(if need-bind-exit-continue
			   (epairify-deep loc
			      `(bind-exit (,(escape-name '%continue id))
				  ,(j2s-scheme body mode acc-return)))
			   (j2s-scheme body mode acc-return))
		      ,(j2s-scheme incr mode return)
		      (,loop %acc))
		   %acc)))

      (let* ((forid (gensym 'for))
	     (loop (if (in-eval? return) (eval-loop forid) (comp-loop forid))))
	 (epairify-deep loc
	    `(begin
		,(j2s-scheme init mode return)
		,(if need-bind-exit-break
		     (epairify-deep loc
			`(bind-exit (,(escape-name '%break id)) ,loop))
		     (epairify loc loop)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SForIn ...                                        */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/5.1/#sec-12.6.4       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SForIn mode return)

   (define (for-in/break-comp tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(let ((%acc (js-undefined)))
			(js-for-in ,(j2s-scheme obj mode return)
			   (lambda (,name)
			      ,set
			      ,(if need-bind-exit-continue
				   `(bind-exit (,(escape-name '%continue id))
				       ,(j2s-scheme body mode acc-return))
				   (j2s-scheme body mode acc-return)))
			   %this)
			%acc)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break-eval tmp name props obj body set)
      (with-access::J2SForIn this (need-bind-exit-break need-bind-exit-continue id)
	 (let ((for `(js-for-in ,(j2s-scheme obj mode return)
			(lambda (,name)
			   ,set
			   ,(if need-bind-exit-continue
				`(bind-exit (,(escape-name '%continue id))
				    ,(j2s-scheme body mode return))
				(j2s-scheme body mode return)))
			%this)))
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,for)
		for))))

   (define (for-in/break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/break-eval tmp name props obj body set)
	  (for-in/break-comp tmp name props obj body set)))

   (define (for-in/w-break-comp tmp name props obj body set)
      `(js-for-in ,(j2s-scheme obj mode return)
	  (lambda (,name)
	     ,set
	     ,(j2s-scheme body mode return))
	  %this))

   (define (for-in/w-break-eval tmp name props obj body set)
      `(let ((%acc (js-undefined)))
	  (js-for-in ,(j2s-scheme obj mode return)
	     (lambda (,name)
		,set
		,(j2s-scheme body mode acc-return))
	     %this)
	  %acc))

   (define (for-in/w-break tmp name props obj body set)
      (if (in-eval? return)
	  (for-in/w-break-eval tmp name props obj body set)
	  (for-in/w-break-comp tmp name props obj body set)))

   (define (set lhs name loc)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SRef)
	     (epairify loc (j2s-scheme-set! lhs name #f mode return)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! '%this `',id
		      name #f mode))))
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field)
		(epairify loc
		   `(js-put! ,(j2s-scheme obj mode return)
		       ,(j2s-scheme field mode return)
		       ,name
		       ,(eq? mode 'strict)
		       %this))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if (js-in? ',id ,(car withs) %this)
			       ,(j2s-put! (car withs) (symbol->string id)
				   name #f #f)
			       ,(liip (cdr withs))))))))
	    (else
	     (j2s-error "j2sscheme" "Illegal lhs" this)))))
   
   (with-access::J2SForIn this (loc lhs obj body
				  need-bind-exit-break need-bind-exit-continue)
      (let* ((tmp (gensym))
	     (name (gensym))
	     (props (gensym))
	     (set (set lhs name loc)))
	 (epairify-deep loc
	    (if (or need-bind-exit-continue need-bind-exit-break)
		(for-in/break tmp name props obj body set)
		(for-in/w-break tmp name props obj body set))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SLabel ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SLabel mode return)
   (with-access::J2SLabel this (body)
      (j2s-scheme body mode return)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SBreak mode return)
   (with-access::J2SBreak this (loc target)
      (with-access::J2SIdStmt target (id)
	 (epairify loc
	    `(,(escape-name '%break id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SContinue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SContinue mode return)
   (with-access::J2SContinue this (loc target)
      (with-access::J2SLoop target (id)
	 (epairify loc
	    `(,(escape-name '%continue id)
	      ,(if (in-eval? return) '%acc '(js-undefined)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSwitch mode return)
   (with-access::J2SSwitch this (loc key cases id need-bind-exit-break)
      
      (define (comp-switch)
	 (let ((elseclause #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode return))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((nil? expr)
					   (set! elseclause expr)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp ,(j2s-scheme expr mode return))
						,@(map (lambda (f) `(,f))
						     (memq fun funs))))))))
		      cases funs)
		   ,(epairify loc
		     `(else
		       ,@(if elseclause
			     (map (lambda (f) `(,f)) (memq elsefun funs))
			     '((js-undefined)))))))))

      (define (eval-switch)
	 (let ((elseclause #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return))
		    (%acc (js-undefined))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode acc-return))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((nil? expr)
					   (set! elseclause expr)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp ,(j2s-scheme expr mode return))
						,@(map (lambda (f) `(,f))
						     (memq fun funs))))))))
		      cases funs)
		   ,(epairify loc
		     `(else
		       ,@(if elseclause
			     (map (lambda (f) `(,f)) (memq elsefun funs))
			     '((js-undefined)))
		       %acc))))))
      
      (let ((switch (if (in-eval? return) (eval-switch) (comp-switch))))
	 (epairify-deep loc
	    (if need-bind-exit-break
		`(bind-exit (,(escape-name '%break id)) ,switch)
		switch)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCall mode return)
   
   (define (read-only-function ref::J2SRef)
      (with-access::J2SRef ref (decl)
	 (when (isa? decl J2SDeclInit)
	    (with-access::J2SDeclInit decl (ronly val id)
	       (when (and (eq? ronly #t) (isa? val J2SFun))
		  decl)))))
   
   (define (call-method fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field)
	 (if (isa? obj J2SRef)
	     (call-unknown-function fun
		`(js-toobject %this
		    ,(j2s-scheme obj mode return)) args)
	     (let ((tmp (gensym)))
		`(let ((,tmp ,(j2s-scheme obj mode return)))
		    ,(call-unknown-function
			(duplicate::J2SAccess fun
			   (obj (instantiate::J2SPragma
				   (loc loc)
				   (expr tmp))))
			`(js-toobject %this ,tmp) args))))))
   
   (define (call-hop-function fun::J2SHopRef args)
      `(,(j2s-scheme fun mode return) ,@(j2s-scheme args mode return)))

   (define (call-fun-function fun::J2SFun f args)
      (with-access::J2SFun fun (params vararg)
	 (if vararg
	     `(,f (js-undefined) ,@(j2s-scheme args mode return))
	     (let ((lenf (length params))
		   (lena (length args)))
		(cond
		   ((=fx lenf lena)
		    ;; matching arity
		    `(,f (js-undefined) ,@(j2s-scheme args mode return)))
		   ((>fx lena lenf)
		    ;; too many arguments ignore the extra values
		    `(,f (js-undefined) ,@(j2s-scheme (take args lenf) mode return)))
		   (else
		    ;; argument missing
		    `(,f
			(js-undefined)
			,@(j2s-scheme args mode return)
			,@(make-list (-fx lenf lena) '(js-undefined)))))))))

   (define (call-with-function fun::J2SWithRef args)
      (with-access::J2SWithRef fun (id withs)
	 (let loop ((withs withs))
	    (if (null? withs)
		(call-unknown-function fun '(js-undefined) args)
		`(if (js-in? ',id ,(car withs) %this)
		     ,(call-unknown-function `(js-get ,(car withs) ',id %this)
			(car withs) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return))))

   (define (call-known-function fun::J2SDeclInit args)
      (with-access::J2SDeclInit fun (val id)
	 (call-fun-function val (j2s-fast-id id) args)))

   (define (call-unknown-function fun thisarg args)
      (let* ((len (length args))
	     (call (if (>=fx len 9)
		       'js-calln
		       (string->symbol (format "js-call~a" (length args))))))
	 (if (> (bigloo-debug) 0)
	     (with-access::J2SCall this (loc)
		`(,(symbol-append call '/debug) %this ',loc
		  ,(j2s-scheme fun mode return) ,thisarg
		  ,@(j2s-scheme args mode return)))
	     `(,call %this ,(j2s-scheme fun mode return) ,thisarg
		 ,@(j2s-scheme args mode return)))))
   
   (with-access::J2SCall this (loc fun args)
      (epairify loc
	 (cond
	    ((isa? fun J2SAccess)
	     (call-method fun args))
	    ((isa? fun J2SHopRef)
	     (call-hop-function fun args))
	    ((isa? fun J2SFun)
	     (with-access::J2SFun fun (mode)
		(call-fun-function fun (jsfun->scheme fun mode return) args)))
	    ((isa? fun J2SUnresolvedRef)
	     (call-unknown-function fun '(js-undefined) args))
	    ((isa? fun J2SWithRef)
	     (call-with-function fun args))
	    ((isa? fun J2SPragma)
	     (call-pragma fun args))
	    ((not (isa? fun J2SRef))
	     (call-unknown-function fun '(js-undefined) args))
	    ((read-only-function fun)
	     =>
	     (lambda (fun) (call-known-function fun args)))
	    (else
	     (call-unknown-function fun '(js-undefined) args))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SXml ...                                          */
;*---------------------------------------------------------------------*/
;* (define-method (j2s-scheme this::J2SXml mode return)                */
;*                                                                     */
;*    (define (call-tag tag)                                           */
;*       (cond                                                         */
;* 	 ((isa? tag J2SPragma)                                         */
;* 	  (with-access::J2SPragma tag (expr)                           */
;* 	     (list expr)))                                             */
;* 	 ((isa? tag J2SAccess)                                         */
;* 	  (with-access::J2SAccess tag (loc obj field)                  */
;* 	     (if (isa? obj J2SRef)                                     */
;* 		 (let ((thisarg (j2s-scheme obj mode return)))         */
;* 		    `(js-calln %this ,(j2s-scheme tag mode return)     */
;* 			(js-toobject %this ,thisarg)))                 */
;* 		 (let ((tmp (gensym)))                                 */
;* 		    `(let ((,tmp ,(j2s-scheme obj mode return)))       */
;* 			(js-calln %this `(js-toobject %this ,tmp) ,tmp)))))) */
;* 	 (else                                                         */
;* 	  (j2s-error "j2sscheme" "Illegal tag expression" this))))     */
;*                                                                     */
;*    (with-access::J2SXml this (loc tag attrs body)                   */
;*       (epairify loc                                                 */
;* 	 `(,@(call-tag tag)                                            */
;* 	     ,@(append-map (lambda (p::J2SDataPropertyInit)            */
;* 			      (with-access::J2SDataPropertyInit p (name val) */
;* 				 (with-access::J2SString name ((s val)) */
;* 				    (list (string->keyword s)          */
;* 				       (j2s-scheme val mode return))))) */
;* 		  attrs)                                               */
;* 	     ,@(cond                                                   */
;* 		  ((isa? body J2SBool)                                 */
;* 		   '())                                                */
;* 		  ((isa? body J2SSequence)                             */
;* 		   (with-access::J2SSequence body (exprs)              */
;* 		      (j2s-scheme exprs mode return)))                 */
;* 		  (else                                                */
;* 		   (j2s-error "j2sscheme" "Illegal tag expression" this))))))) */

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssig mode return)
   (with-access::J2SAssig this (loc lhs rhs)
      (let loop ((lhs lhs))
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field cache)
		(epairify loc
		   (j2s-put! (j2s-scheme obj mode return)
		      (j2s-scheme field mode return)
		      (j2s-scheme rhs mode return)
		      (eq? mode 'strict)
		      cache))))
	    ((isa? lhs J2SRef)
	     (let ((assig (j2s-scheme-set! lhs
			     (j2s-scheme rhs mode return)
			     (j2s-scheme lhs mode return)
			     mode return)))
		(if (pair? assig)
		    (epairify loc assig)
		    assig)))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(epairify loc
		   (j2s-unresolved-put! '%this `',id
		      (j2s-scheme rhs mode return) #f mode))))
	    ((isa? lhs J2SWithRef)
	     (with-access::J2SWithRef lhs (id withs expr loc)
		(epairify loc
		   (let liip ((withs withs))
		      (if (null? withs)
			  (loop expr)
			  `(if (js-in? ',id ,(car withs) %this)
			       ,(j2s-put! (car withs) (symbol->string id)
				   (j2s-scheme rhs mode return) #f #f)
			       ,(liip (cdr withs))))))))
	    ((isa? lhs J2SUndefined)
	     (j2s-scheme rhs mode return))
	    (else
	     (j2s-error "assignment" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SInit mode return)
   (with-access::J2SAssig this (loc lhs rhs)
      (if (isa? lhs J2SRef)
	  (epairify-deep loc
	     `(begin
		 ,(j2s-scheme-set! lhs (j2s-scheme rhs mode return) #f mode return)
		 (js-undefined)))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAssigOp mode return)
   (with-access::J2SAssigOp this (loc lhs rhs op)
      (epairify-deep loc
	 (cond
	    ((isa? lhs J2SAccess)
	     (with-access::J2SAccess lhs (obj field)
		(let ((tobj (gensym 'obj))
		      (pro (gensym 'pro))
		      (prov (j2s-scheme field mode return)))
		   `(let ((,tobj ,(j2s-scheme obj mode return))
			  ,@(if (string? prov) '() (list `(,pro ,prov))))
		       (js-put! ,tobj ,(if (string? prov) prov prov)
			  ,(js-binop op
			      `(js-get ,tobj ,(if (string? prov) prov pro) %this)
			      (j2s-scheme rhs mode return))
			  ,(eq? mode 'strict)
			  %this)))))
	    ((isa? lhs J2SRef)
	     (j2s-scheme-set! lhs
		(js-binop op (j2s-scheme lhs mode return) (j2s-scheme rhs mode return))
		(j2s-scheme lhs mode return)
		mode return))
	    ((isa? lhs J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef lhs (id)
		(j2s-unresolved-put! '%this `',id
		   (js-binop op
		      (j2s-scheme lhs mode return)
		      (j2s-scheme rhs mode return))
		   #t mode)))
	    (else
	     (j2s-error "j2sscheme" "Illegal assignment" this))))))

;*---------------------------------------------------------------------*/
;*    j2s-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-get obj prop cache)
   (if cache
       (if (string? prop)
	   `(js-get-name/cache ,obj ',(string->symbol prop) ,(pcache cache) %this)
	   `(js-get/cache ,obj ,prop ,(pcache cache) %this))
       `(js-get ,obj ,prop %this)))

;*---------------------------------------------------------------------*/
;*    j2s-put! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-put! obj prop val mode cache)
   (if cache
       (cond
	  ((string? prop)
	   `(js-put-name/cache! ,obj ',(string->symbol prop) ,val ,mode
	       ,(pcache cache) %this))
	  (else
	   `(js-put/cache! ,obj ,prop ,val ,mode ,(pcache cache) %this)))
       (cond
	  ((string? prop)
	   `(js-put! ,obj ',(string->symbol prop) ,val ,mode %this))
	  (else
	   `(js-put! ,obj ,prop ,val ,mode %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SAccess mode return)
   (with-access::J2SAccess this (loc obj field cache)
      (epairify-deep loc
	 `(begin
	     ,(format "~s" loc)
	     ,(j2s-get (j2s-scheme obj mode return) (j2s-scheme field mode return) cache)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SObjInit mode return)
   
   (define (j2s-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (let ((str (string-for-read val)))
		(if (string=? str val)
		    `(quote ,(string->symbol val))
		    `(string->symbol ,val)))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 `(quote ,(string->symbol (number->string val)))
		 `(js-toname ,(j2s-scheme val mode return) %this))))
	 (else
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return) %this)))))
   
   (with-access::J2SObjInit this (loc inits)
      (let ((tmp (gensym)))
	 (epairify loc
	    `(with-access::JsGlobalObject %this (js-object)
		(let ((,tmp ,(j2s-new 'js-object '())))
		   ,@(map (lambda (i)
			     (if (isa? i J2SDataPropertyInit)
				 (with-access::J2SDataPropertyInit i (loc name val)
				    (epairify loc
				       `(js-bind! %this ,tmp
					   ,(j2s-propname name)
					   :value ,(j2s-scheme val mode return)
					   :writable #t
					   :enumerable #t
					   :configurable #t)))
				 (with-access::J2SAccessorPropertyInit i (loc name get set)
				    (epairify loc
				       `(js-bind! %this ,tmp
					   ,(j2s-propname name)
					   :get ,(j2s-scheme get mode return)
					   :set ,(j2s-scheme set mode return)
					   :writable #t
					   :enumerable #t
					   :configurable #t)))))
			inits)
		   ,tmp))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDataPropertyInit mode return)
   (with-access::J2SDataPropertyInit this (loc name val)
      (epairify loc
	 `(,(j2s-scheme name mode return) ,(j2s-scheme val mode return)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SNew mode return)
   (with-access::J2SNew this (loc clazz args)
      (epairify loc
	 (j2s-new (j2s-scheme clazz mode return)
	    (j2s-scheme args mode return)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2STilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2STilde mode return)
   
   (define (concat lst)
      (cond
	 ((null? lst)
	  '())
	 ((not (string? (car lst)))
	  (cons (car lst) (concat (cdr lst))))
	 (else
	  (let loop ((prev lst)
		     (cursor (cdr lst)))
	     (cond
		((null? cursor)
		 (list (apply string-append lst)))
		((string? (car cursor))
		 (loop cursor (cdr cursor)))
		(else
		 (set-cdr! prev '())
		 (cons* (apply string-append lst)
		    (car cursor)
		    (concat (cdr cursor)))))))))
   
   (with-access::J2STilde this (loc stmt)
      (let ((js-stmt (concat (j2s-js stmt #f #f mode return))))
	 (epairify loc
	    `(instantiate::xml-tilde
		(%js-statement ,(cond
				   ((null? js-stmt)
				    "")
				   ((null? (cdr js-stmt))
				    (car js-stmt))
				   (else
				    `(string-append ,@js-stmt))))
		(%js-expression "NOT IMPLEMENTED (j2sscheme/scheme.scm)")
		(%js-return "NOT IMPLEMENTED (j2sscheme/scheme.scm)")
		(loc ',loc)
		(body #unspecified))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDollar ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDollar mode return)
   (with-access::J2SDollar this (loc expr)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location "hopscript" "Illegal $ expression" this
	     fname loc))
	 (else
	  (j2s-error "hopscript" "Illegal $ expression" this)))))

;*---------------------------------------------------------------------*/
;*    j2s-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-error proc msg obj)
   (with-access::J2SNode obj (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location proc msg (j2s->list obj) fname loc))
	 (else
	  (error proc msg obj)))))
