;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-call.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 25 07:00:50 2018                          */
;*    Last change :  Thu Mar 29 11:01:29 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript function calls              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-call

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-string
	   __js2scheme_scheme-regexp
	   __js2scheme_scheme-math
	   __js2scheme_scheme-date
	   __js2scheme_scheme-array
	   __js2scheme_scheme-class
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-arguments))

;*---------------------------------------------------------------------*/
;*    builtin-method ...                                               */
;*---------------------------------------------------------------------*/
(define-struct builtin-method jsname met ttype args %this)

;*---------------------------------------------------------------------*/
;*    j2s-builtin-methods ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-builtin-methods
   ;; jsname, scmname|procedure, (this.types), [optional-args] %this
   (map (lambda (e)
	   (apply builtin-method e))
      `(;; string methods
	("fromCharCode" js-jsstring-fromcharcode String (any) %this)
	("charAt" js-jsstring-charat string (any) %this)
	("charAt" js-jsstring-maybe-charat any (any) %this)
	("charCodeAt" ,j2s-jsstring-charcodeat string (any) %this)
	("charCodeAt" js-jsstring-maybe-charcodeat any (any) %this)
	("indexOf" js-jsstring-indexof string (string (any 0)) %this)
	("indexOf" js-jsstring-maybe-indexof any (any (any 0)) %this)
	("lastIndexOf" js-jsstring-lastindexof string (string (any +nan.0)) %this)
	("lastIndexOf" js-jsstring-maybe-lastindexof string (any (any +nan.0)) %this)
	("substring" js-jsstring-substring string (any any) %this)
	("substring" js-jsstring-maybe-substring any (any any) %this)
	("substr" js-jsstring-substr string (any any) %this)
	("substr" js-jsstring-maybe-substr any (any any) %this)
	("toUpperCase" js-jsstring-touppercase string () #f)
	("toUpperCase" js-jsstring-maybe-touppercase any () %this)
	("toLocaleUpperCase" js-jsstring-tolocaleuppercase string () #f)
	("toLocaleUpperCase" js-jsstring-maybe-tolocaleuppercase any () %this)
	("toLowerCase" js-jsstring-tolowercase string () #f)
	("toLowerCase" js-jsstring-maybe-tolowercase any () %this)
	("toLocaleLowerCase" js-jsstring-tolocalelowercase string () #f)
	("toLocaleLowerCase" js-jsstring-maybe-tolocalelowercase any () %this)
	("split" js-jsstring-split string (string (any (js-undefined))) %this)
	("split" js-jsstring-maybe-split any (any (any (js-undefined))) %this)
	("replace" ,j2s-jsstring-replace-regexp string (regexp any) %this)
	("replace" ,j2s-jsstring-replace-string string (string any) %this)
	("replace" ,j2s-jsstring-replace string (any any) %this)
	("replace" js-jsstring-maybe-replace any (any any) %this)
	("match" js-jsstring-match string (any) %this)
	("match" js-jsstring-maybe-match any (any) %this)
	("naturalCompare" js-jsstring-naturalcompare string (string) %this)
	("naturalCompare" js-jsstring-maybe-naturalcompare any (any) %this)
	("localeCompare" js-jsstring-localecompare string (string) %this)
	("localeCompare" js-jsstring-maybe-localecompare any (any) %this)
	("trim" js-jsstring-trim string () #f)
	("trim" js-jsstring-maybe-trim any () %this)
	("slice" js-jsstring-slice string (any any) %this)
	("slice" js-jsstring-maybe-slice any (any any) %this)
	;; regexp
	("test" ,j2s-regexp-test regexp (any) %this)
	;; array methods
;* 	("concat" js-array-concat array (any) %this)                   */
;* 	("concat" js-array-maybe-concat any (any) %this)               */
	("push" js-array-push array (any) %this)
	("push" js-array-maybe-push any (any) %this)
	("pop" js-array-pop array () %this)
	("pop" js-array-maybe-pop any () %this)
;* 	("slice" js-array-slice array () %this)                        */
;* 	("slice" js-array-maybe-slice any () %this)                    */
	("fill" js-array-fill array (any (any 0) (any #unspecified)) %this)
	("fill" js-array-maybe-fill any (any (any 0) (any #unspecified)) %this))))

;*---------------------------------------------------------------------*/
;*    read-only-function ...                                           */
;*---------------------------------------------------------------------*/
(define (read-only-function ref::J2SRef)
   (with-access::J2SRef ref (decl usage)
      (cond
	 ((isa? decl J2SDeclSvc)
	  #f)
	 ((isa? decl J2SDeclFun)
	  (with-access::J2SDecl decl (ronly)
	     (when ronly decl)))
	 ((j2s-let-opt? decl)
	  (with-access::J2SDeclInit decl (val ronly)
	     (when (and (isa? val J2SFun) ronly)
		decl)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCall mode return conf hint)
   
   (define (array-push obj arg)
      (let ((scmobj (j2s-scheme obj mode return conf hint))
	    (scmarg (j2s-scheme arg mode return conf hint)))
	 (if (isa? obj J2SAref)
	     (with-access::J2SAref obj (array alen)
		(let ((scmarr (j2s-decl-scheme-id array))
		      (scmalen (j2s-decl-scheme-id alen)))
		   `(let ((%l:len (js-array-length ,scmobj))
			  (%o:item ,scmarg))
		       (if (<u32 %l:len (fixnum->uint32 ,scmalen))
			   (let ((%t:tmp (uint32->fixnum %l:len)))
			      (vector-set-ur! ,scmarr %t:tmp %o:item)
			      (js-array-update-length! ,scmobj (+fx 1 %t:tmp))
			      %o:item)
			   (js-array-push ,scmobj ,scmarg %this)))))
	     `(js-array-push ,scmobj ,scmarg %this))))

   (define (find-builtin-method obj field args)
      
      (define (is-type-or-class? ty obj tyobj)
	 (cond
	    ((eq? ty 'any)
	     #t)
	    ((eq? ty tyobj)
	     #t)
	    ((isa? obj J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef obj (id)
		(eq? ty id)))))
      
      (define (j2s-type-sans-cast expr)
	 (if (isa? expr J2SCast)
	     (with-access::J2SCast expr (expr)
		(j2s-type-sans-cast expr))
	     (j2s-vtype expr)))
      
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((tyobj (j2s-type obj)))
	       (find (lambda (m)
			(when (string=? (builtin-method-jsname m) val)
			   (let ((ty (builtin-method-ttype m)))
			      (when (is-type-or-class? ty obj tyobj)
				 (let loop ((args args)
					    (params (builtin-method-args m)))
				    (cond
				       ((null? args)
					(or (null? params)
					    (every pair? params)))
				       ((null? params)
					#f)
				       (else
					(let ((tya (j2s-type-sans-cast
						      (car args)))
					      (tyf (if (pair? (car params))
						       (caar params)
						       (car params))))
					   (when (or (eq? tyf 'any)
						     (eq? tyf tya))
					      (loop (cdr args)
						 (cdr params)))))))))))
		  j2s-builtin-methods)))))


   (define (call-builtin-method obj::J2SExpr field::J2SExpr args)
      (let ((m (find-builtin-method obj field args)))
	 (when m
	    (let* ((met (builtin-method-met m))
		   (opt (builtin-method-args m))
		   (arity (length opt))
		   (len (length args)))
	       (cond
		  ((symbol? met)
		   ;; builtin simple
		   `(,met ,(j2s-scheme obj mode return conf hint)
		       ,@(map (lambda (arg)
				 (j2s-scheme arg mode return conf hint))
			    args)
		       ,@(if (=fx len arity)
			     '()
			     (let* ((lopt (length opt))
				    (nopt (-fx arity len)))
				(map cadr (list-tail opt (-fx lopt nopt)))))
		       ,@(if (builtin-method-%this m)
			     '(%this)
			     '())))
		  ((procedure? met)
		   ;; builtin procedure
		   (met obj
		      (append args
			 (if (=fx len arity)
			     '()
			     (let* ((lopt (length opt))
				    (nopt (-fx arity len)))
				(map cadr (list-tail opt (-fx lopt nopt)))))
			 (if (builtin-method-%this m)
			     '(%this)
			     '()))
		      mode return conf hint))
		  (else
		   (error "js2scheme" "illegal builtin method" m)))))))

	 
   (define (call-super-method fun args)
      (call-unknown-function fun '(this) args))

   (define (call-ref-method self ccache ocache ccspecs fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SAccess fun (loc field (ocspecs cspecs))
	 (cond
	    ((isa? self J2SSuper)
	     (call-super-method fun args))
	    ((and ccache (= (bigloo-debug) 0))
	     (cond
		((isa? field J2SString)
		 (let ((call (if (eq? (j2s-vtype obj) 'object)
				 'js-object-method-call-name/cache
				 'js-method-call-name/cache)))
		    (with-access::J2SString field (val)
		       `(,call
			   ,j2s-unresolved-call-workspace
			   ,(j2s-scheme obj mode return conf hint)
			   ',(string->symbol val)
			   ,(js-pcache ccache)
			   ,(js-pcache ocache)
			   ,(loc->point loc)
			   ',ccspecs
			   ',ocspecs
			   ,@(map (lambda (arg)
				     (j2s-scheme arg mode return conf hint))
				args)))))
		(else
		 (call-unknown-function fun
		    (list (j2s-scheme obj mode return conf hint))
		    args))))
	    (else
	     (call-unknown-function fun
		(list (j2s-scheme obj mode return conf hint))
		args)))))

   (define (call-globalref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SGlobalRef self (id decl)
	 (with-access::J2SDecl decl (usage)
	    (unless (memq 'assig usage)
	       (case id
		  ((Math)
		   (j2s-math-builtin-method fun args
		      mode return conf hint))
		  (else
		   #f))))))

   (define (call-new-method obj::J2SNew field args mode return conf hint)
      (with-access::J2SNew obj (clazz)
	 (when (isa? clazz J2SGlobalRef)
	    (with-access::J2SGlobalRef clazz (id decl)
	       (with-access::J2SDecl decl (usage)
		  (unless (memq 'assig usage)
		     (case id
			((Date)
			 (j2s-date-new-method obj field args mode return
			    conf hint))
			(else
			 #f))))))))
   
   (define (call-method ccache ccspecs fun::J2SAccess args)
      (with-access::J2SAccess fun (loc obj field (acache cache) (acspecs cspecs))
	 (let loop ((obj obj))
	    (cond
	       ((call-builtin-method obj field args)
		=>
		(lambda (sexp) sexp))
	       ((isa? obj J2SRef)
		(call-ref-method obj ccache acache ccspecs fun obj args))
	       ((isa? obj J2SGlobalRef)
		(or (call-globalref-method obj ccache acache fun obj args)
		    (let* ((tmp (gensym '%obj))
			   (fun (J2SAccess/cache (J2SHopRef tmp) field
				   acache acspecs)))
		       `(let ((,tmp ,(j2s-scheme obj mode return conf hint)))
			   ,(call-ref-method obj ccache acache
			      ccspecs fun (J2SHopRef tmp) args)))))
	       ((isa? obj J2SParen)
		(with-access::J2SParen obj (expr)
		   (loop expr)))
	       ((and (isa? obj J2SNew)
		     (call-new-method obj field args mode return conf hint))
		=>
		(lambda (sexp) sexp))
	       (else
		(let* ((tmp (gensym 'obj))
		       (ttmp (type-ident tmp (j2s-vtype obj))))
		   `(let ((,ttmp ,(j2s-scheme obj mode return conf hint)))
		       ,(call-ref-method obj
			   ccache acache ccspecs
			   (duplicate::J2SAccess fun
			      (obj (instantiate::J2SPragma
				      (loc loc)
				      (expr tmp))))
			   (instantiate::J2SHopRef
			      (type (j2s-vtype obj))
			      (loc loc)
			      (id tmp))
			   args))))))))
   
   (define (call-hop-function fun::J2SHopRef thisarg args)
      `(,(j2s-scheme fun mode return conf hint)
	,@(j2s-scheme args mode return conf hint)))

   (define (j2s-self thisarg)
      (map (lambda (t) (j2s-scheme t mode return conf hint)) thisarg))

   (define (call-rest-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a rest argument
      (with-access::J2SFun fun (params vararg)
	 (let loop ((params params)
		    (args args)
		    (actuals '()))
	    (cond
	       ((null? (cdr params))
		;; the rest argument
		`(,f ,@%gen ,@(j2s-self thisarg) ,@(reverse! actuals)
		    (js-vector->jsarray
		       (vector ,@(j2s-scheme args mode return conf hint))
		       %this)))
	       ((null? args)
		(with-access::J2SDecl (car params) (loc)
		   (loop (cdr params) '()
		      (cons '(js-undefined) actuals))))
	       (else
		(loop (cdr params) (cdr args)
		   (cons (j2s-scheme (car args) mode return conf hint)
		      actuals)))))))

   (define (call-fix-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a fix number of arguments
      (with-access::J2SFun fun (params vararg thisp)
	 (let ((lenf (length params))
	       (lena (length args)))
	    (cond
	       ((=fx lenf lena)
		;; matching arity
		`(,f ,@%gen
		    ,@(j2s-self thisarg)
		    ,@(map (lambda (a p)
			      (with-access::J2SDecl p (utype)
				 (j2s-scheme a mode return conf hint)))
			 args params)))
	       ((>fx lena lenf)
		;; too many arguments ignore the extra values,
		;; but still evaluate extra expressions
		(let ((temps (map (lambda (i)
				     (string->symbol
					(string-append "%a"
					   (integer->string i))))
				(iota lena))))
		   `(let* ,(map (lambda (t a)
				   `(,t ,(j2s-scheme a mode return conf hint))) temps args)
		       (,f ,@%gen ,@(j2s-self thisarg)
			  ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-self thisarg)
		    ,@(j2s-scheme args mode return conf hint)
		    ,@(make-list (-fx lenf lena) '(js-undefined))))))))

   (define (check-hopscript-fun-arity val::J2SFun id args)
      (with-access::J2SFun val (params vararg loc name mode)
	 (when (eq? mode 'hopscript)
	    (let ((lp (length params))
		  (la (length args)))
	       (unless (=fx lp la)
		  (case vararg
		     ((rest)
		      (unless (>=fx la (-fx (j2s-minlen val)  1))
			 (j2s-error id
			    (format "wrong number of arguments, minimum expected: ~a" (j2s-minlen val) la)
			    this (format "~a provided" la))))
		     ((arguments)
		      #t)
		     (else
		      (unless (and (>=fx la (j2s-minlen val)) (<=fx la lp))
			 (j2s-error id
			    (format "wrong number of arguments, minimum expected: ~a" (j2s-minlen val))
			    this
			    (format "~a provided" la))))))))))

   (define (call-fun-function fun::J2SFun thisarg::pair-nil protocol f %gen::pair-nil args::pair-nil)
      (with-access::J2SFun fun (params vararg idthis)
	 (case (if (eq? protocol 'bounce) 'bounce vararg)
	    ((arguments)
	     `(,f ,@%gen ,@(if idthis (j2s-self thisarg) '())
		 ,@(j2s-scheme args mode return conf hint)))
	    ((rest)
	     (call-rest-function fun (if idthis thisarg '()) f %gen args))
	    (else
	     (call-fix-function fun (if idthis thisarg '()) f %gen args)))))

   (define (call-with-function fun::J2SWithRef args)
      (with-access::J2SWithRef fun (id withs loc)
	 (let loop ((withs withs))
	    (if (null? withs)
		(call-unknown-function fun '((js-undefined)) args)
		`(if ,(j2s-in? loc `',id (car withs))
		     ,(call-unknown-function
			 (j2s-get loc (car withs) 'object `',id 'string 'any conf #f)
			(list (car withs)) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return conf hint))))

   (define (typed-generator? decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (parent)
	 (when (isa? parent J2SDeclFun)
	    (with-access::J2SDeclFun parent (val)
	       (with-access::J2SFun val (generator)
		  generator)))))

   (define (call-known-function protocol fun::J2SDecl thisarg::pair-nil args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (with-access::J2SDeclFun fun (id usage)
	     (let ((val (j2sdeclinit-val-fun fun)))
		(check-hopscript-fun-arity val id args)
		(let ((%gen (if (typed-generator? fun) '(%gen) '())))
		   (call-fun-function val thisarg protocol
		      (j2s-fast-id id) %gen args)))))
	 ((j2s-let-opt? fun)
	  (with-access::J2SDeclInit fun (id val)
	     (call-fun-function val thisarg protocol
		(j2s-fast-id id) '() args)))
	 (else
	  (error "js-scheme" "Should not be here" (j2s->list fun)))))

   (define (call-unknown-function fun self::pair-nil args)
      (let* ((len (length args))
	     (call (if (>=fx len 11)
		       'js-calln
		       (string->symbol (format "js-call~a" len)))))
	 (with-access::J2SCall this (loc cache)
	    (cond
	       ((> (bigloo-debug) 0)
		`(,(symbol-append call '/debug)
		  ,j2s-unresolved-call-workspace
		  ',loc
		  ,(j2s-scheme fun mode return conf hint)
		  ,@self
		  ,@(j2s-scheme args mode return conf hint)))
	       (cache
		`(js-call/cache
		    ,j2s-unresolved-call-workspace
		    ,(js-pcache cache)
		    ,(j2s-scheme fun mode return conf hint)
		    ,@self
		    ,@(j2s-scheme args mode return conf hint)))
	       (else
		`(,call ,j2s-unresolved-call-workspace
		    ,(j2s-scheme fun mode return conf hint)
		    ,@self
		    ,@(j2s-scheme args mode return conf hint)))))))

   (define (call-eval-function fun args)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1.1
      `(%js-direct-eval 
	  ,(if (null? args)
	       '(js-undefined)
	       (j2s-scheme (car args) mode return conf hint))
	  ,(strict-mode? mode)
	  %this this %scope))

   (define (is-eval? fun)
      (with-access::J2SUnresolvedRef fun (id)
	 (eq? id 'eval)))

   (define (call-unresolved-function fun thisarg args)
      (if (is-eval? fun)
	  (call-eval-function fun args)
	  (call-unknown-function fun
	     (j2s-scheme thisarg mode return conf hint) args)))

   (with-access::J2SCall this (loc fun thisarg args protocol cache cspecs)
      (let loop ((fun fun))
	 (epairify loc
	    (cond
	       ((isa? fun J2SAccess)
		(call-method cache cspecs fun args))
	       ((isa? fun J2SParen)
		(with-access::J2SParen fun (expr)
		   (loop expr)))
	       ((isa? fun J2SHopRef)
		(call-hop-function fun thisarg args))
	       ((isa? fun J2SSuper)
		(j2s-scheme-super this mode return conf hint))
	       ((and (isa? fun J2SFun) (not (j2sfun-id fun)))
		(call-fun-function fun thisarg protocol
		   (jsfun->lambda fun mode return conf (j2s-fun-prototype fun) #f)
		   '()
		   args))
	       ((isa? fun J2SUnresolvedRef)
		(call-unresolved-function fun thisarg args))
	       ((isa? fun J2SWithRef)
		(call-with-function fun args))
	       ((isa? fun J2SPragma)
		(call-pragma fun args))
	       ((not (isa? fun J2SRef))
		(call-unknown-function fun
		   (j2s-scheme thisarg mode return conf hint) args))
	       ((read-only-function fun)
		=>
		(lambda (fun)
		   (call-known-function protocol fun thisarg args)))
	       (else
		(call-unknown-function fun
		   (j2s-scheme thisarg mode return conf hint) args)))))))

