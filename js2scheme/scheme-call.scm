;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-call.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 25 07:00:50 2018                          */
;*    Last change :  Thu Jan  9 18:44:55 2020 (serrano)                */
;*    Copyright   :  2018-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript function calls              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-call

   (include "ast.sch"
	    "usage.sch")
   
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
	   __js2scheme_scheme-json
	   __js2scheme_scheme-date
	   __js2scheme_scheme-array
	   __js2scheme_scheme-class
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-arguments
	   __js2scheme_scheme-spread))

;*---------------------------------------------------------------------*/
;*    builtin-method ...                                               */
;*---------------------------------------------------------------------*/
(define-struct builtin-method jsname met ttype args %this cache predicate)
(define-struct builtin-function id scmid args %this)

;*---------------------------------------------------------------------*/
;*    j2s-builtin-methods ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-builtin-methods
   ;; jsname, scmname|procedure, (this.types), [opt-args], %this, [opt-cache], [opt-predicate]
   (map (lambda (e)
	   (match-case e
	      ((?jsname ?met ?ttype ?args ?%this)
	       (builtin-method jsname met ttype args %this #f #f))
	      ((?jsname ?met ?ttype ?args ?%this ?cache)
	       (builtin-method jsname met ttype args %this cache #f))
	      ((?jsname ?met ?ttype ?args ?%this ?cache ?pred)
	       (builtin-method jsname met ttype args %this cache pred))
	      (else
	       (error "hopc" "bad builtin method specification" e))))
      `(;; string and array methods
	("indexOf" js-array-indexof array (any (any 0)) %this #t)
	("indexOf" js-jsstring-indexof string (string (any 0)) %this #f)
	;; string methods
	("fromCharCode" ,js-jsstring-fromcharcode String (any) %this)
	("charAt" js-jsstring-charat string (any) %this)
	("charAt" js-jsstring-maybe-charat any (any) %this #t)
	("charCodeAt" ,j2s-jsstring-charcodeat string (any) %this)
	("charCodeAt" js-jsstring-maybe-charcodeat any (any) %this #t)
	("indexOf" js-jsstring-maybe-indexof any (any (any 0)) %this #t)
	("lastIndexOf" js-jsstring-lastindexof string (string (any +nan.0)) %this)
	("lastIndexOf" js-jsstring-maybe-lastindexof string (any (any +nan.0)) %this #t)
	("substring" js-jsstring-substring string (any any) %this)
	("substring" js-jsstring-maybe-substring any (any any) %this #t)
	("substr" js-jsstring-substr string (any any) %this)
	("substr" ,j2s-jsstring-substr string (any) %this)
	("substr" js-jsstring-maybe-substr any (any any) %this #t)
	("substr" ,j2s-jsstring-maybe-substr any (any) %this #t)
	("toUpperCase" js-jsstring-touppercase string () #f)
	("toUpperCase" js-jsstring-maybe-touppercase any () %this #t)
	("toLocaleUpperCase" js-jsstring-tolocaleuppercase string () #f)
	("toLocaleUpperCase" js-jsstring-maybe-tolocaleuppercase any () %this #t)
	("toLowerCase" js-jsstring-tolowercase string () #f)
	("toLowerCase" js-jsstring-maybe-tolowercase any () %this #t)
	("toLocaleLowerCase" js-jsstring-tolocalelowercase string () #f)
	("toLocaleLowerCase" js-jsstring-maybe-tolocalelowercase any () %this #t)
	("split" js-jsstring-split string (string (any (js-undefined))) %this)
	("split" js-jsstring-maybe-split any (any (any (js-undefined))) %this #t)
	("replace" ,j2s-jsstring-replace-regexp string (regexp any) %this)
	("replace" ,j2s-jsstring-replace-string string (string any) %this)
	("replace" ,j2s-jsstring-replace string (any any) %this)
	("replace" ,j2s-jsstring-maybe-replace any (any any) %this #t)
	("match" js-jsstring-match string (any) %this)
	("match" ,j2s-jsstring-match-string any (string) %this ,j2s-regexp-plain?)
	("match" ,j2s-jsstring-match-string string (string) %this ,j2s-regexp-plain?)
	("match" ,j2s-jsstring-match-regexp any (regexp) %this ,j2s-regexp-plain?)
	("match" js-jsstring-maybe-match any (any) %this #t)
	("naturalCompare" js-jsstring-naturalcompare string (string) %this)
	("naturalCompare" js-jsstring-maybe-naturalcompare any (any) %this #t)
	("localeCompare" js-jsstring-localecompare string (string) %this)
	("localeCompare" js-jsstring-maybe-localecompare any (any) %this #t)
	("trim" js-jsstring-trim string () #f)
	("trim" js-jsstring-maybe-trim any () %this #t)
	("slice" js-jsstring-slice string (any any) %this)
	("slice" js-jsstring-maybe-slice1 any (any) %this #t)
	("slice" js-jsstring-maybe-slice any (any any) %this #t)
	;; regexp
	("test" ,j2s-regexp-test regexp (any) %this)
	("exec" js-regexp-prototype-exec regexp (any) %this #f ,j2s-regexp-plain?)
	("exec" js-regexp-prototype-maybe-exec any (any) %this #t ,j2s-regexp-plain?)
	;; array methods
	("concat" js-array-concat1 array (array) %this #t ,j2s-array-plain?)
	("concat" js-array-maybe-concat1 any (any) %this #t ,j2s-array-plain?)
	("sort" js-array-sort array (any) %this #t ,j2s-array-plain?)
	("sort" js-array-maybe-sort any (any) %this #t ,j2s-array-plain?)
	("fill" js-array-fill array (any (any 0) (any #unspecified)) %this #t ,j2s-array-plain?)
	("fill" js-array-maybe-fill any (any (any 0) (any #unspecified)) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-foreach array (function) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-foreach array (function any) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-maybe-foreach any (function) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-maybe-foreach any (function any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (function) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (function any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (function) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (function any) %this #t ,j2s-array-plain?)
	("join" js-array-join array (any) %this #t ,j2s-array-plain?)
	("join" ,j2s-array-maybe-join any (any) %this #t ,j2s-array-plain?)
	("push" js-array-push array (any) %this #t ,j2s-array-plain?)
	("push" js-array-maybe-push any (any) %this #t ,j2s-array-plain?)
	("pop" js-array-pop array () %this #t ,j2s-array-plain?)
	("pop" js-array-maybe-pop any () %this #t ,j2s-array-plain?)
	("slice" js-array-maybe-slice0 any () %this #t)
	;; functions
	("apply",j2s-apply any (any any) %this #t)
	("call" ,j2s-call0 any (any) %this #t)
	("call" ,j2s-call1 any (any any) %this #t)
	("call" ,j2s-call2 any (any any any) %this #t)
	("call" ,j2s-call3 any (any any any any) %this #t)
	;; math
	("toFixed" js-maybe-tofixed any (any) %this #t)
	)))

;*---------------------------------------------------------------------*/
;*    j2s-builtin-functions ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-builtin-functions
   (map (lambda (e)
	   (apply builtin-function e))
      `((parseInt js-parseint-string-uint32 (string uint32) #f)
	(parseInt js-parseint-string (string) #f)
	(parseInt js-parseint-any (any) %this)
	(parseInt js-parseint (any any) %this)
	(parseFloat js-parsefloat (any) %this)
	(Number js-tonumber (any) %this)
	(isNaN nanfl? (real) #f)
	(isNaN js-number-isnan? (number) #f)
	(isNaN js-isnan? (any) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-apply ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-apply obj args mode return conf)

   (define (def obj args mode return conf)
      `(js-function-maybe-apply ,(caddr args)
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(j2s-scheme (cadr args) mode return conf)
	  ,(cadddr args)))

   (define (def-arguments obj args mode return conf)
      `(js-function-maybe-apply-arguments ,(caddr args)
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(j2s-ref-arguments-argid (cadr args))
	  ,(j2s-scheme (cadr args) mode return conf)
	  ,(cadddr args)))
   
   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (loc decl)
	  (if (and (or (isa? decl J2SDeclFun)
		       (and (isa? decl J2SDeclInit)
			    (with-access::J2SDeclInit decl (val)
			       (and (isa? val J2SFun) (decl-ronly? decl)))))
		   (decl-only-call? decl)
		   (and (pair? args) (=fx (length args) 4)))
	      (if (and (isa? (cadr args) J2SRef)
		       (j2s-ref-arguments-lazy? (cadr args)))
		  `(js-function-apply-arguments ,(caddr args)
		      ,(j2s-scheme obj mode return conf)
		      ,(j2s-scheme (car args) mode return conf)
		      ,(j2s-ref-arguments-argid (cadr args))
		      ,(j2s-scheme (cadr args) mode return conf)
		      ,(cadddr args))
		  `(js-function-apply ,(caddr args)
		      ,(j2s-scheme obj mode return conf)
		      ,(j2s-scheme (car args) mode return conf)
		      ,(j2s-scheme (cadr args) mode return conf)
		      ,(cadddr args)))
	      (if (and (isa? (cadr args) J2SRef)
		       (j2s-ref-arguments-lazy? (cadr args)))
		  (def-arguments obj args mode return conf)
		  (def obj args mode return conf)))))
      ((and (isa? (cadr args) J2SRef) (j2s-ref-arguments-lazy? (cadr args)))
       (def-arguments obj args mode return conf))
      (else
       (def obj args mode return conf))))

;*---------------------------------------------------------------------*/
;*    j2s-call0 ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-call0 obj args mode return conf)

   (define (def obj args mode return conf)
      `(js-function-maybe-call0 ,(cadr args)
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(cadr args)))

   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (loc decl)
	  (cond
	     ((and (or (isa? decl J2SDeclFun)
		       (and (isa? decl J2SDeclInit)
			    (with-access::J2SDeclInit decl (val)
			       (and (isa? val J2SFun) (decl-ronly? decl)))))
		   (decl-only-call? decl)
		   (and (pair? args) (<=fx (length args) 2)))
	      (j2s-scheme (J2SMethodCall* obj
			     (list (car args))
			     (if (pair? (cdr args))
				 (cdr args)
				 (list (J2SUndefined))))
		 mode return conf))
	     ((pair? (cddr args))
	      (def obj args mode return conf))
	     (else
	      #f))))
      ((pair? (cddr args))
       (def obj args mode return conf))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-call1 ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-call1 obj args mode return conf)

   (define (def obj args mode return conf)
      `(js-function-maybe-call1 ,(caddr args)
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(j2s-scheme (cadr args) mode return conf)
	  ,(caddr args)))
      
   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (loc decl)
	  (cond
	     ((and (or (isa? decl J2SDeclFun)
		       (and (isa? decl J2SDeclInit)
			    (with-access::J2SDeclInit decl (val)
			       (and (isa? val J2SFun) (decl-ronly? decl)))))
		   (decl-only-call? decl)
		   (and (pair? args) (<=fx (length args) 2)))
	      (j2s-scheme (J2SMethodCall* obj
			     (list (car args))
			     (if (pair? (cdr args))
				 (cdr args)
				 (list (J2SUndefined))))
		 mode return conf))
	     ((pair? (cdddr args))
	      (def obj args mode return conf))
	     (else
	      #f))))
      ((pair? (cdddr args))
       (def obj args mode return conf))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-call2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-call2 obj args mode return conf)
   
   (define (def obj args mode return conf)
      `(js-function-maybe-call2 ,(cadddr args)
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(j2s-scheme (cadr args) mode return conf)
	  ,(j2s-scheme (caddr args) mode return conf)
	  ,(cadddr args)))

   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (loc decl)
	  (cond
	     ((and (or (isa? decl J2SDeclFun)
		       (and (isa? decl J2SDeclInit)
			    (with-access::J2SDeclInit decl (val)
			       (and (isa? val J2SFun) (decl-ronly? decl)))))
		   (decl-only-call? decl))
	      (j2s-scheme (J2SMethodCall* obj
			     (list (car args))
			     (list (cadr args) (caddr args)))
		 mode return conf))
	     ((pair? (cddddr args))
	      (def obj args mode return conf))
	     (else
	      #f))))
      ((pair? (cddddr args))
       (def obj args mode return conf))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-call3 ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-call3 obj args mode return conf)
   
   (define (def obj args mode return conf)
      `(js-function-maybe-call3 ,(car (cddddr args))
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(j2s-scheme (cadr args) mode return conf)
	  ,(j2s-scheme (caddr args) mode return conf)
	  ,(j2s-scheme (cadddr args) mode return conf)
	  ,(car (cddddr args))))

   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (loc decl)
	  (cond
	     ((and (or (isa? decl J2SDeclFun)
		       (and (isa? decl J2SDeclInit)
			    (with-access::J2SDeclInit decl (val)
			       (and (isa? val J2SFun) (decl-ronly? decl)))))
		   (decl-only-call? decl))
	      (j2s-scheme (J2SMethodCall* obj
			     (list (car args))
			     (list (cadr args) (caddr args) (cadddr args)))
		 mode return conf))
	     ((pair? (cdr (cddddr args)))
	      (def obj args mode return conf))
	     (else
	      #f))))
      ((pair? (cdr (cddddr args)))
       (def obj args mode return conf))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-fromcharcode ...                                     */
;*---------------------------------------------------------------------*/
(define (js-jsstring-fromcharcode obj args mode return conf)
   `(js-jsstring-fromcharcode
       ,@(map (lambda (arg) (j2s-scheme arg mode return conf)) args)))

;*---------------------------------------------------------------------*/
;*    read-only-function ...                                           */
;*---------------------------------------------------------------------*/
(define (read-only-function ref::J2SRef)
   (with-access::J2SRef ref (decl)
      (cond
	 ((isa? decl J2SDeclSvc)
	  #f)
	 ((isa? decl J2SDeclFun)
	  (when (decl-ronly? decl) decl))
	 ((j2s-let-opt? decl)
	  (with-access::J2SDeclInit decl (val)
	     (when (and (isa? val J2SFun) (decl-ronly? decl))
		decl)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-array-plain? ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-array-plain? mode return conf)
   (let ((array (config-get conf :array)))
      (if (isa? array J2SDeclExtern)
	  (decl-only-call? array)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-string-plain? ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-string-plain? mode return conf)
   (let ((string (config-get conf :string)))
      (if (isa? string J2SDeclExtern)
	  (decl-only-call? string)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-regexp-plain? ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-regexp-plain? mode return conf)
   (let ((regexp (config-get conf :regexp)))
      (if (isa? regexp J2SDeclExtern)
	  (decl-only-call? regexp)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-math-plain? ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-math-plain? mode return conf)
   (let ((math (config-get conf :math)))
      (if (isa? math J2SDeclExtern)
	  (decl-only-call? math)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCall mode return conf)
   
   (define (call-profile profid call)
      (if (and (config-get conf :profile-call #f) (>=fx profid 0))
	  `(begin
	      (js-profile-log-call %call-log ,profid)
	      ,call)
	  call))
   
   (define (funcall-profile profid fun call)
      `(begin
	  (js-profile-log-funcall %call-log ,profid ,fun %source)
	  ,call))

   (define (cmap-profile profid obj)
      `(js-profile-log-cmap %cmap-log ,profid ,obj))
   
   (define (array-push obj arg)
      (let ((scmobj (j2s-scheme obj mode return conf))
	    (scmarg (j2s-scheme arg mode return conf)))
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
			   (js-array-push ,scmobj ,scmarg %this #f)))))
	     `(js-array-push ,scmobj ,scmarg %this #f))))
   
   (define (j2s-type-sans-cast expr)
      (if (isa? expr J2SCast)
	  (with-access::J2SCast expr (expr)
	     (j2s-type-sans-cast expr))
	  (j2s-vtype expr)))

   (define (find-builtin-method obj field args)
      
      (define (is-type-or-class? ty obj tyobj)
	 (cond
	    ((eq? ty 'any)
	     #t)
	    ((eq? ty tyobj)
	     #t)
	    ((isa? obj J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef obj (id)
		(eq? ty id)))
	    ((isa? obj J2SRef)
	     (with-access::J2SRef obj (decl)
		(when (isa? decl J2SDeclExtern)
		   (with-access::J2SDeclExtern decl (id)
		      (when (eq? id ty)
			 (not (decl-usage-has? decl '(assig))))))))))

      (when (and (isa? field J2SString) (= (config-get conf debug: 0) 0))
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
					(when (or (null? params)
						  (every pair? params))
					   (let ((p (builtin-method-predicate m)))
					      (or (not p) (p mode return conf)))))
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
   
   (define (find-builtin-function id args)
      (find (lambda (f)
	       (when (eq? (builtin-function-id f) id)
		  (let loop ((args args)
			     (params (builtin-function-args f)))
		     (cond
			((null? args)
			 (null? params))
			((null? params)
			 #f)
			(else
			 (let ((tya (j2s-type-sans-cast (car args)))
			       (tyf (car params)))
			    (when (or (eq? tyf 'any) (eq? tyf tya))
			       (loop (cdr args) (cdr params)))))))))
	 j2s-builtin-functions))

   (define (call-builtin-method obj::J2SExpr field::J2SExpr args cache cspecs)
      (let ((m (find-builtin-method obj field args)))
	 (when m
	    (let* ((met (builtin-method-met m))
		   (opt (builtin-method-args m))
		   (arity (length opt))
		   (len (length args)))
	       (cond
		  ((symbol? met)
		   ;; builtin simple
		   `(,met ,(j2s-scheme obj mode return conf)
		       ,@(map (lambda (arg)
				 (j2s-scheme arg mode return conf))
			    args)
		       ,@(if (=fx len arity)
			     '()
			     (let* ((lopt (length opt))
				    (nopt (-fx arity len)))
				(map cadr (list-tail opt (-fx lopt nopt)))))
		       ,@(if (builtin-method-%this m)
			     '(%this)
			     '())
		       ,@(if (builtin-method-cache m)
			     (if cache
				 `((js-pcache-ref %pcache ,cache))
				 '(#f))
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
			     '())
			 (if (builtin-method-cache m)
			     (if cache
				 `((js-pcache-ref %pcache ,cache))
				 '(#f))
			     '()))
		      mode return conf))
		  (else
		   (error "js2scheme" "illegal builtin method" m)))))))

	 
   (define (call-super-method fun args)
      (call-unknown-function 'direct fun '(this) args))

   (define (Array? self)
      (is-builtin-ref? self 'Array))

   (define (Math? self)
      (is-builtin-ref? self 'Math))
   
   (define (Json? self)
      (is-builtin-ref? self 'JSON))

   (define (mincspecs x y)
      (filter (lambda (c) (memq c y)) x))
   
   (define (call-ref-method self ccache ocache ccspecs fun::J2SAccess obj::J2SExpr args)

      (with-access::J2SAccess fun (loc field (ocspecs cspecs))
	 (cond
	    ((isa? self J2SSuper)
	     (call-super-method fun args))
	    ((and (Array? self)
		  (j2s-array-builtin-method fun args this mode return conf))
	     =>
	     (lambda (expr) expr))
	    ((and (Math? self)
		  (j2s-math-builtin-method fun args this mode return conf))
	     =>
	     (lambda (expr) expr))
	    ((and (Json? self)
		  (j2s-json-builtin-method fun args this mode return conf))
	     =>
	     (lambda (expr) expr))
	    ((and ccache (= (config-get conf :debug 0) 0) ccspecs)
	     (cond
		((isa? field J2SString)
		 (with-access::J2SString field (val)
		    (or (and (string=? val "toString")
			     (j2s-tostring this mode return conf))
			(let ((call (if (eq? (j2s-vtype obj) 'object)
					'js-object-method-call-name/cache
					'js-method-call-name/cache)))
			   `(,call
			       ,j2s-unresolved-call-workspace
			       ,(box (j2s-scheme obj mode return conf)
				   (j2s-type obj) conf)
			       (& ,val)
			       ,(js-pcache ccache)
			       ,(js-pcache ocache)
			       ,(loc->point loc)
			       ',ccspecs
			       ',(cond
				    ((>=fx (config-get conf :optim 0) 4)
				     (mincspecs ocspecs '(imap amap amap+ vtable)))
				    ((>=fx (config-get conf :optim 0) 3)
				     (mincspecs ocspecs '(imap amap+)))
				    (else
				     (mincspecs ocspecs '(imap+))))
			       ,@(map (lambda (arg)
					 (j2s-scheme arg mode return conf))
				    args))))))
		(else
		 (call-unknown-function 'direct fun
		    (list (box (j2s-scheme obj mode return conf)
			     (j2s-type obj) conf))
		    args))))
	    (else
	     (call-unknown-function 'direct fun
		(list
		   (box (j2s-scheme obj mode return conf) (j2s-type obj) conf))
		args)))))

   (define (call-globalref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SGlobalRef self (id decl)
	 (unless (decl-usage-has? decl '(assig))
	    (case id
	       ((Math)
		(j2s-math-builtin-method fun args
		   this mode return conf))
	       ((Array)
		;; This branch is currently never used
		;; because Array is defined as an external
		;; %scoped object (see header.scm).
		;; Array builtin methods are then
		;; handled in the call-ref-method above
		(j2s-array-builtin-method fun args
		   this mode return conf))
	       (else
		#f)))))

   (define (call-new-method obj::J2SNew field args mode return conf)
      (with-access::J2SNew obj (clazz)
	 (when (isa? clazz J2SGlobalRef)
	    (with-access::J2SGlobalRef clazz (id decl)
	       (unless (decl-usage-has? decl '(assig))
		  (case id
		     ((Date)
		      (j2s-date-new-method obj field args mode return
			 conf))
		     (else
		      #f)))))))
   
   (define (call-method this ccache ccspecs fun::J2SAccess args)
      (with-access::J2SCall this (profid cache)
	 (with-access::J2SAccess fun (loc obj field (acache cache) (acspecs cspecs))
	    (let loop ((obj obj))
	       (cond
		  ((call-builtin-method obj field args ccache ccspecs)
		   =>
		   (lambda (sexp) sexp))
		  ((isa? obj J2SGlobalRef)
		   (or (call-globalref-method obj ccache acache fun obj args)
		       (let* ((tmp (gensym '%obj))
			      (fun (J2SAccess/cache (J2SHopRef tmp) field
				      acache acspecs)))
			  `(let ((,tmp ,(j2s-scheme obj mode return conf)))
			      ,(call-ref-method obj ccache acache
				  ccspecs fun (J2SHopRef tmp) args)))))
		  ((and (config-get conf :profile-call #f) (>=fx profid 0))
		   (with-access::J2SAccess fun (obj loc)
		      ;; when profile method call, inverse the call cache
		      ;; and object cache to improve profiling precision
		      (if (isa? obj J2SSuper)
			  (let* ((self (j2s-scheme obj mode return conf))
				 (s (gensym '%obj-profile))
				 (f (duplicate::J2SAccess fun
				       (cspecs '(pmap vtable))
				       (cache cache)
				       (obj (J2SHopRef s)))))
			     `(let ((,s ,self))
				 ,(call-unknown-function 'direct
				     f (list 'this) args)))
			  (let* ((self (j2s-scheme obj mode return conf))
				 (s (gensym '%obj-profile))
				 (f (duplicate::J2SAccess fun
				       (cspecs '(pmap-dummy-profile vtable-dummy-profile))
				       (cache cache)
				       (obj (J2SHopRef s)))))
			     `(let ((,s ,self))
				 ,(call-unknown-function 'direct
				     f (list s) args))))))
		  ((isa? obj J2SRef)
		   (call-ref-method obj ccache acache ccspecs fun obj args))
		  ((isa? obj J2SParen)
		   (with-access::J2SParen obj (expr)
		      (loop expr)))
		  ((and (isa? obj J2SNew)
			(call-new-method obj field args mode return conf))
		   =>
		   (lambda (sexp) sexp))
		  (else
		   (let ((tmp (gensym 'obj)))
		      `(let ((,tmp ,(box (j2s-scheme obj mode return conf)
				       (j2s-vtype obj) conf)))
			  ,(call-ref-method obj
			      ccache acache ccspecs
			      (duplicate::J2SAccess fun
				 (obj (instantiate::J2SPragma
					 (loc loc)
					 (expr tmp))))
			      (instantiate::J2SHopRef
				 (type 'any)
				 (loc loc)
				 (id tmp))
			      args)))))))))
   
   (define (call-hop-function fun::J2SHopRef thisarg args)
      `(,(j2s-scheme fun mode return conf)
	,@(j2s-scheme args mode return conf)))

   (define (j2s-self thisarg)
      (map (lambda (t) (j2s-scheme t mode return conf)) thisarg))

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
		       (vector ,@(j2s-scheme args mode return conf))
		       %this)))
	       ((null? args)
		(with-access::J2SDecl (car params) (loc)
		   (loop (cdr params) '()
		      (cons '(js-undefined) actuals))))
	       (else
		(loop (cdr params) (cdr args)
		   (cons (j2s-scheme (car args) mode return conf)
		      actuals)))))))

   (define (call-fix-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a fix number of arguments
      (with-access::J2SFun fun (params vararg thisp id)
	 (let ((lenf (length params))
	       (lena (length args)))
	    (cond
	       ((=fx lenf lena)
		;; matching arity
		`(,f ,@%gen
		    ,@(j2s-self thisarg)
		    ,@(map (lambda (a p)
			      (with-access::J2SDecl p (utype)
				 (j2s-scheme a mode return conf)))
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
				   `(,t ,(j2s-scheme a mode return conf)))
			      temps args)
		       (,f ,@%gen ,@(j2s-self thisarg)
			  ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-self thisarg)
		    ,@(j2s-scheme args mode return conf)
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

   (define (call-fun-function profid fun::J2SFun thisarg::pair-nil protocol f %gen::pair-nil args::pair-nil)
      (with-access::J2SFun fun (params vararg idthis loc argumentsp)
	 (case (if (eq? protocol 'bounce) 'bounce vararg)
	    ((arguments)
	     (let ((self (if idthis (j2s-self thisarg) '())))
		(if (config-get conf :optim-arguments)
		    (with-access::J2SDeclArguments argumentsp (alloc-policy)
		       (if (eq? alloc-policy 'lazy)
			   (let ((v (gensym 'vec)))
			      (call-profile profid
				 `(js-call-with-stack-vector
				     (vector ,@(j2s-scheme args mode return conf))
				     (lambda (,v)
					(,f ,@%gen ,@self ,v)))))
			   (call-profile profid
			      `(,f ,@%gen ,@self
				  (vector ,@(j2s-scheme args mode return conf))))))
		    (call-profile profid
		       `(,f ,@%gen ,@self
			   ,@(j2s-scheme args mode return conf))))))
	    ((rest)
	     (call-profile profid
		(call-rest-function fun (if idthis thisarg '()) f %gen args)))
	    (else
	     (call-profile profid
		(call-fix-function fun (if idthis thisarg '()) f %gen args))))))

   (define (call-with-function fun::J2SWithRef args)
      (with-access::J2SWithRef fun (id withs loc)
	 (let loop ((withs withs))
	    (if (null? withs)
		(call-unknown-function 'direct fun '((js-undefined)) args)
		`(if ,(j2s-in? loc `(& ,(symbol->string id)) (car withs) conf)
		     ,(call-unknown-function 'direct
			 (j2s-get loc (car withs) #f 'object
			    `(& ,(symbol->string id)) 'string 'any conf #f #f)
			(list (car withs)) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return conf))))

   (define (typed-generator? decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (parent)
	 (when (isa? parent J2SDeclFun)
	    (let ((val (j2sdeclinit-val-fun parent)))
	       (with-access::J2SFun val (generator)
		  generator)))))

   (define (call-known-function protocol profid fun::J2SDecl thisarg::pair-nil args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (with-access::J2SDeclFun fun (id)
	     (let ((val (j2sdeclinit-val-fun fun)))
		(check-hopscript-fun-arity val id args)
		(let ((%gen (if (typed-generator? fun) '(%gen) '())))
		   (call-fun-function profid val thisarg protocol
		      (j2s-fast-id id) %gen args)))))
	 ((j2s-let-opt? fun)
	  (with-access::J2SDeclInit fun (id val)
	     (call-fun-function profid val thisarg protocol
		(j2s-fast-id id) '() args)))
	 (else
	  (error "js-scheme" "Should not be here" (j2s->list fun)))))

   (define (call-unknown-function protocol fun self::pair-nil args)
      (with-access::J2SCall this (loc cache profid)
	 (let* ((len (length args))
		(call (if (>=fx len 11)
			  'js-calln
			  (string->symbol (format "js-call~a" len)))))
	    (case protocol
	       ((function)
		(cond
		   ((> (config-get conf :debug 0) 0)
		    `(,(symbol-append call '/debug)
		      ,j2s-unresolved-call-workspace
		      ',loc
		      ,(j2s-scheme fun mode return conf)
		      ,@self
		      ,@(j2s-scheme args mode return conf)))
		   ((and (config-get conf :profile-call #f) (>=fx profid 0))
		    (let* ((f (gensym '%fun-profile))
			   (call `(,call ,j2s-unresolved-call-workspace
				     ,f
				     ,@self
				     ,@(j2s-scheme args mode return conf))))
		       `(let ((,f ,(j2s-scheme fun mode return conf)))
			   ,(when (and (isa? fun J2SAccess)
				       (config-get conf :profile-cmap #f))
			       (with-access::J2SAccess fun (obj)
				  (let ((o (j2s-scheme obj mode return conf)))
				     (cmap-profile profid o))))
			   ,(funcall-profile profid f call))))
		   (else
		    `(with-access::JsFunction ,(j2s-scheme fun mode return conf) (procedure)
			(procedure ,@self ,@(j2s-scheme args mode return conf))))))
	       (else
		(cond
		   ((> (config-get conf :debug 0) 0)
		    `(,(symbol-append call '/debug)
		      ,j2s-unresolved-call-workspace
		      ',loc
		      ,(j2s-scheme fun mode return conf)
		      ,@self
		      ,@(j2s-scheme args mode return conf)))
		   ((and (config-get conf :profile-call #f) (>=fx profid 0))
		    (let* ((f (gensym '%fun-profile))
			   (call `(,call ,j2s-unresolved-call-workspace
				     ,f
				     ,@self
				     ,@(j2s-scheme args mode return conf))))
		       `(let ((,f ,(j2s-scheme fun mode return conf)))
			   ,(when (and (isa? fun J2SAccess)
				       (config-get conf :profile-cmap #f))
			       (with-access::J2SAccess fun (obj)
				  (let ((o (j2s-scheme obj mode return conf)))
				     (cmap-profile profid o))))
			   ,(funcall-profile profid f call))))
		   (cache
		    `(js-call/cache
			,j2s-unresolved-call-workspace
			,(js-pcache cache)
			,(j2s-scheme fun mode return conf)
			,@self
			,@(j2s-scheme args mode return conf)))
		   ((eq? (j2s-type fun) 'function)
		    `(,(symbol-append call '/function)
		      ,j2s-unresolved-call-workspace
		      ,(j2s-scheme fun mode return conf)
		      ,@self
		      ,@(j2s-scheme args mode return conf)))
		   (else
		    `(,call ,j2s-unresolved-call-workspace
			,(j2s-scheme fun mode return conf)
			,@self
			,@(j2s-scheme args mode return conf)))))))))

   (define (call-eval-function fun args)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1.1
      `(%js-direct-eval 
	  ,(if (null? args)
	       '(js-undefined)
	       (j2s-scheme (car args) mode return conf))
	  ,(strict-mode? mode)
	  %this this %scope))

   (define (is-eval? fun)
      (with-access::J2SUnresolvedRef fun (id)
	 (eq? id 'eval)))

   (define (call-unresolved-function fun thisarg args)
      (if (is-eval? fun)
	  (call-eval-function fun args)
	  (call-unknown-function 'direct fun
	     (j2s-scheme thisarg mode return conf) args)))

   (define (call-scheme this fun args)
      `(,(j2s-scheme fun mode return conf)
	,@(map (lambda (a) (j2s-scheme a mode return conf)) args)))

   (define (call-scheme-nothis this fun args)
      (if (isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (with-access::J2SDecl decl (id)
		`(,(j2s-fast-id id)
		  ,@(map (lambda (a) (j2s-scheme a mode return conf)) args))))
	  (call-scheme this fun args)))

   (define (call-scheme-this this fun thisarg args)
      `(,(j2s-scheme fun mode return conf)
	,@(j2s-scheme thisarg mode return conf)
	,@(map (lambda (a) (j2s-scheme a mode return conf)) args)))

   (define (call-scheme-this-arity this fun thisarg args)
      (let ((len (length args)))
	 `(,(if (>=fx len 11)
		'js-calln/procedure
		(string->symbol (format "js-call~a/procedure" len)))
	   ,(j2s-scheme fun mode return conf)
	   ,@(j2s-scheme thisarg mode return conf)
	   ,@(map (lambda (a) (j2s-scheme a mode return conf)) args))))

   (with-access::J2SCall this (loc profid fun thisarg args protocol cache cspecs)
      (let loop ((fun fun))
	 (epairify loc
	    (cond
	       ((eq? (j2s-vtype fun) 'procedure)
		(case protocol
		   ((procedure-this)
		    (call-scheme-this this fun thisarg args))
		   ((procedure-this-arity)
		    (call-scheme-this-arity this fun thisarg args))
		   ((procedure-nothis)
		    (call-scheme-nothis this fun args))
		   (else
		    (call-scheme this fun args))))
	       ((eq? protocol 'spread)
		(j2s-scheme-call-spread this mode return conf))
	       ((isa? fun J2SAccess)
		(call-method this cache cspecs fun args))
	       ((isa? fun J2SParen)
		(with-access::J2SParen fun (expr)
		   (loop expr)))
	       ((isa? fun J2SHopRef)
		(call-hop-function fun thisarg args))
	       ((isa? fun J2SSuper)
		(j2s-scheme-super this mode return conf))
	       ((and (isa? fun J2SFun) (not (j2sfun-id fun)))
		(call-fun-function profid fun thisarg protocol
		   (jsfun->lambda fun mode return conf (j2s-fun-prototype fun) #f)
		   '()
		   args))
	       ((isa? fun J2SGlobalRef)
		(with-access::J2SGlobalRef fun (decl)
		   (with-access::J2SDecl decl (id scope)
		      (cond
			 ((not (decl-ronly? decl))
			  (call-unresolved-function fun thisarg args))
			 ((find-builtin-function id args)
			  =>
			  (lambda (f)
			     `(,(builtin-function-scmid f)
				 ,@(map (lambda (arg)
					   (j2s-scheme arg mode return conf))
				      args)
				 ,@(if (builtin-function-%this f)
				       '(%this)
				       '()))))
			 ((eq? scope '%hop)
			  'TODO)
			 (else
			  (call-unresolved-function fun thisarg args))))))
	       ((isa? fun J2SUnresolvedRef)
		(call-unresolved-function fun thisarg args))
	       ((isa? fun J2SWithRef)
		(call-with-function fun args))
	       ((isa? fun J2SPragma)
		(call-pragma fun args))
	       ((not (isa? fun J2SRef))
		(call-unknown-function protocol fun
		   (j2s-scheme thisarg mode return conf) args))
	       ((read-only-function fun)
		=>
		(lambda (fun)
		   (call-known-function protocol profid fun thisarg args)))
	       (else
		(call-unknown-function protocol fun
		   (j2s-scheme thisarg mode return conf) args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-tostring ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-tostring this mode return conf)
   (with-access::J2SCall this (fun args)
      (with-access::J2SAccess fun (obj)
	 (case (j2s-type obj)
	    ((number)
	     `(js-jsnumber-tostring
		 ,(j2s-scheme obj mode return conf)
		 ,(if (pair? args)
		      (j2s-scheme (car args) mode return conf)
		      10) %this))
	    ((int53 bint)
	     (if (pair? args)
		 `(js-string->jsstring
		     (fixnum->string ,(j2s-scheme obj mode return conf)
			,(j2s-scheme (car args) mode return conf)))
		 `(js-integer->jsstring
		     ,(j2s-scheme obj mode return conf))))
	    ((int32)
	     `(js-string->jsstring
		 (number->string ,(j2s-scheme obj mode return conf)
		    ,(if (pair? args)
			 (j2s-scheme (car args) mode return conf)
			 10))))
	    ((uint32)
	     `(js-string->jsstring
		 (number->string ,(j2s-scheme obj mode return conf)
		    ,(if (pair? args)
			 (j2s-scheme (car args) mode return conf)
			 10))))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-spread ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-call-spread this mode return conf)
   (with-access::J2SCall this (loc profid fun thisarg args)
      (let ((expr (spread->array-expr loc args #f)))
	 (cond
	    ((and (isa? fun J2SRef)
		  (with-access::J2SRef fun (decl)
		     (decl-only-call? decl)))
	     (if (isa? expr J2SArray)
		 ;; fun(...[x,y,z])
		 (with-access::J2SArray expr (exprs)
		    (let ((ncall (instantiate::J2SCall
				    (loc loc)
				    (fun fun)
				    (thisarg (list (J2SNull)))
				    (args exprs))))
		       (j2s-scheme ncall mode return conf)))
		 (epairify loc
		    `(js-apply %this ,(j2s-scheme fun mode return conf)
			,@(map (lambda (a) (j2s-scheme a mode return conf))
			     thisarg)
			,(j2s-spread->expr-list args mode return conf)))))
	    ((isa? fun J2SAccess)
	     (with-access::J2SAccess fun (obj field)
		(let* ((o (gensym 'o))
		       (axs (duplicate::J2SAccess fun
			       (obj (J2SHopRef o))))
		       (ncall (instantiate::J2SCall
				 (loc loc)
				 (fun (instantiate::J2SAccess
					 (loc loc)
					 (obj axs)
					 (field (instantiate::J2SString
						   (loc loc)
						   (val "apply")))))
				 (thisarg (list (J2SUndefined)))
				 (args (list (J2SHopRef o) expr)))))
		   `(let ((,o ,(j2s-scheme obj mode return conf)))
		       ,(j2s-scheme ncall mode return conf)))))
	    (else
	     (let ((ncall (instantiate::J2SCall
			     (loc loc)
			     (fun (instantiate::J2SAccess
				     (loc loc)
				     (obj fun)
				     (field (instantiate::J2SString
					       (loc loc)
					       (val "apply")))))
			     (thisarg (list (J2SUndefined)))
			     (args (list (J2SNull) expr)))))
		(j2s-scheme ncall mode return conf)))))))
						      
;*---------------------------------------------------------------------*/
;*    decl-only-call? ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-only-call? decl::J2SDecl)
   (and (decl-usage-has? decl '(get call new init instanceof))
	(not (decl-usage-has? decl '(assig ref set uninit rest eval)))))
