;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-call.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 25 07:00:50 2018                          */
;*    Last change :  Sun Jun 14 06:56:47 2020 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript function calls              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-call

   (include "ast.sch"
	    "usage.sch"
	    "context.sch")
   
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
	   __js2scheme_scheme-object
	   __js2scheme_scheme-json
	   __js2scheme_scheme-date
	   __js2scheme_scheme-process
	   __js2scheme_scheme-array
	   __js2scheme_scheme-class
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-arguments
	   __js2scheme_scheme-spread
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-constant))

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
	("indexOf" js-array-indexof0 array (any) %this #t)
	("indexOf" js-jsstring-indexof0 string (string) %this #f)
	("indexOf" js-array-indexof array (any (any 0)) %this #t)
	("indexOf" js-jsstring-indexof string (string (any 0)) %this #f)
	;; string methods
	("fromCharCode" ,js-jsstring-fromcharcode String (any) %this)
	("charAt" ,j2s-jsstring-charat string (any) %this)
	("charAt" js-jsstring-maybe-charat any (any) %this #t)
	("charCodeAt" ,j2s-jsstring-charcodeat string (any) %this)
	("charCodeAt" js-jsstring-maybe-charcodeat any (any) %this #t)
	("codePointAt" ,j2s-jsstring-codepointat string (any) %this)
	("codePointAt" js-jsstring-maybe-codepointat any (any) %this #t)
	("indexOf" js-jsstring-maybe-indexof any (any (any 0)) %this #t)
	("indexOf" js-jsstring-maybe-indexof0 any (any) %this #t)
	("lastIndexOf" js-jsstring-lastindexof string (string (any +nan.0)) %this)
	("lastIndexOf" js-jsstring-maybe-lastindexof string (any (any +nan.0)) %this #t)
	("substring" ,j2s-jsstring-substring string (any) %this #f)
	("substring" ,j2s-jsstring-substring string (any any) %this #f)
	("substring" js-jsstring-maybe-substring1 any (any) %this #t)
	("substring" js-jsstring-maybe-substring any (any any) %this #t)
	("substr" ,j2s-jsstring-substr string (any any) %this)
	("substr" ,j2s-jsstring-substr string (any (any (js-undefined))) %this)
	("substr" js-jsstring-maybe-substr any (any any) %this #t)
	("substr" ,j2s-jsstring-maybe-substr any (any) %this #t)
	("toUpperCase" ,j2s-jsstring-touppercase string () #f)
	("toUpperCase" js-jsstring-maybe-touppercase any () %this #t)
	("toLocaleUpperCase" js-jsstring-tolocaleuppercase string () #f)
	("toLocaleUpperCase" js-jsstring-maybe-tolocaleuppercase any () %this #t)
	("toLowerCase" ,j2s-jsstring-tolowercase string () #f)
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
	("match" #f object (any) %this #t)
	("match" js-jsstring-maybe-match any (any) %this #t)
	("naturalCompare" js-jsstring-naturalcompare string (string) %this)
	("naturalCompare" js-jsstring-maybe-naturalcompare any (any) %this #t)
	("localeCompare" js-jsstring-localecompare string (string) %this)
	("localeCompare" js-jsstring-maybe-localecompare any (any) %this #t)
	("trim" js-jsstring-trim string () %this #f)
	("trim" js-jsstring-maybe-trim any () %this #t)
	("slice" ,j2s-jsstring-slice string (any any) %this)
	("padStart" ,j2s-jsstring-maybe-padstart any (any any) #t)
	("padStart" ,j2s-jsstring-padstart string (any any) #t)
	("padEnd" ,j2s-jsstring-maybe-padend any (any any) #t)
	("padEnd" ,j2s-jsstring-padend string (any any) #t)
	;; regexp
	("test" ,j2s-regexp-test regexp (any) %this #f ,j2s-regexp-plain?)
	("exec" js-regexp-prototype-exec regexp (any) %this #f ,j2s-regexp-plain?)
	("exec" js-regexp-prototype-maybe-exec any (any) %this #t ,j2s-regexp-plain?)
	;; array methods
	("concat" ,j2s-array-concat1 array (array) %this #t ,j2s-array-plain?)
	("concat" ,j2s-array-maybe-concat1 any (any) %this #t ,j2s-array-plain?)
	("concat" js-array-concat array (array . any) %this #f ,j2s-array-plain?)
	("concat" js-array-maybe-concat any (any . any) %this #t ,j2s-array-plain?)
	("sort" js-array-sort array (any) %this #t ,j2s-array-plain?)
	("sort" js-array-maybe-sort any (any) %this #t ,j2s-array-plain?)
	("fill" js-array-fill1 array (any) %this #t ,j2s-array-plain?)
	("fill" js-array-maybe-fill1 any (any) %this #t ,j2s-array-plain?)
	("fill" js-array-fill array (any (any 0) (any #unspecified)) %this #t ,j2s-array-plain?)
	("fill" js-array-maybe-fill any (any (any 0) (any #unspecified)) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-foreach array (function) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-foreach array (function any) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-maybe-foreach any (function) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-maybe-foreach any (function any) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-foreach array (arrow) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-foreach array (arrow any) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-maybe-foreach any (arrow) %this #t ,j2s-array-plain?)
	("forEach" ,j2s-array-maybe-foreach any (arrow any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (function) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (function any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (function) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (function any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (arrow) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (arrow any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (arrow) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (arrow any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-map array (any any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (any) %this #t ,j2s-array-plain?)
	("map" ,j2s-array-maybe-map any (any any) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-filter array (function) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-filter array (function any) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-maybe-filter any (function) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-maybe-filter any (any) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-maybe-filter any (function any) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-filter array (arrow) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-filter array (arrow any) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-maybe-filter any (arrow) %this #t ,j2s-array-plain?)
	("filter" ,j2s-array-maybe-filter any (arrow any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-flatmap array (function) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-flatmap array (function any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-maybe-flatmap any (function) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-maybe-flatmap any (function any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-flatmap array (arrow) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-flatmap array (arrow any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-maybe-flatmap any (arrow) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-maybe-flatmap any (arrow any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-flatmap array (any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-flatmap array (any any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-maybe-flatmap any (any) %this #t ,j2s-array-plain?)
	("flatMap" ,j2s-array-maybe-flatmap any (any any) %this #t ,j2s-array-plain?)
	("join" js-array-join array (any) %this #t ,j2s-array-plain?)
	("join" ,j2s-array-maybe-join any (any) %this #t ,j2s-array-plain?)
	("push" js-array-push array (any) %this #t ,j2s-array-plain?)
	("push" js-array-maybe-push any (any) %this #t ,j2s-array-plain?)
	("pop" js-array-pop array () %this #t ,j2s-array-plain?)
	("pop" js-array-maybe-pop any () %this #t ,j2s-array-plain?)
	("slice" ,j2s-jsstring-slice1 string (any) %this #t)
	("slice" js-jsstring-maybe-slice1 (:hint string) (any) %this #t)
	("slice" js-jsstring-maybe-slice2 (:hint string) (any any) %this #t)
	("slice" js-array-maybe-slice0 any () %this #t)
	("slice" js-array-maybe-slice1 any (any) %this #t)
	("slice" js-array-maybe-slice2 any (any any) %this #t)
	("shift" js-array-maybe-shift0 any () %this #t)
	("reverse" js-array-reverse array () %this #f ,j2s-array-plain?)
	("reverse" js-array-maybe-reverse any () %this #t)
	("some" js-array-maybe-some any (any (any #unspecified)) %this #t ,j2s-array-plain?)
	;; functions
	("apply",j2s-apply any (any any) %this #t)
	("call" ,j2s-call0 any (any) %this #t)
	("call" ,j2s-call1 any (any any) %this #t)
	("call" ,j2s-call2 any (any any any) %this #t)
	("call" ,j2s-call3 any (any any any any) %this #t)
	;; math
	("toFixed" js-maybe-tofixed any (any) %this #t)
	;; date
	("getTime" js-date-gettime date () #f #f)
	("getTime" js-date-maybe-gettime any () %this #t)
	("getFullYear" js-date-getfullyear date () #f #f)
	("getFullYear" js-date-maybe-getfullyear any () %this #t)
	("getMonth" js-date-getmonth date () #f #f)
	("getMonth" js-date-maybe-getmonth any () %this #t)
	("getDate" js-date-getdate date () #f #f)
	("getDate" js-date-maybe-getdate any () %this #t)
	("getUTCDate" js-date-getutcdate date () #f #f)
	("getUTCDate" js-date-maybe-getutcdate any () %this #t)
	("getHours" js-date-gethours date () #f #f)
	("getHours" js-date-maybe-gethours any () %this #t)
	("getMinutes" js-date-getminutes date () #f #f)
	("getMinutes" js-date-maybe-getminutes any () %this #t)
	("getUTCMinutes" js-date-getutcminutes date () #f #f)
	("getUTCMinutes" js-date-maybe-getutcminutes any () %this #t)
	("getSeconds" js-date-getseconds date () #f #f)
	("getSeconds" js-date-maybe-getseconds any () %this #t)
	("getMilliseconds" js-date-getmilliseconds date () #f #f)
	("getMilliseconds" js-date-maybe-getmilliseconds any () %this #t)
	("setMinutes" ,j2s-date-maybe-setminutes any (any) #t #t)
	("setMinutes" ,j2s-date-maybe-setminutes any (any any) #t #t)
	("setMinutes" ,j2s-date-maybe-setminutes any (any any any) #t #t)
	("setMinutes" ,j2s-date-setminutes date (any) #f #f)
	("setMinutes" ,j2s-date-setminutes date (any any) #f #f)
	("setMinutes" ,j2s-date-setminutes date (any any any) #f #f)
	("setUTCMinutes" ,j2s-date-maybe-setminutes any (any) #t #t)
	("setUTCMinutes" ,j2s-date-maybe-setminutes any (any any) #t #t)
	("setUTCMinutes" ,j2s-date-maybe-setminutes any (any any any) #t #t)
	("setUTCMinutes" ,j2s-date-setminutes date (any) #f #f)
	("setUTCMinutes" ,j2s-date-setutcminutes date (any any) #f #f)
	("setUTCMinutes" ,j2s-date-setutcminutes date (any any any) #t #t)
	;; object
	("hasOwnProperty" js-has-own-property any (any) %this #f ,j2s-object-plain?)
	("isFrozen" ,j2s-object-isfrozen any (any) #f #f ,j2s-object-plain?)
	)))

;*---------------------------------------------------------------------*/
;*    j2s-builtin-functions ...                                        */
;*---------------------------------------------------------------------*/
(define j2s-builtin-functions
   (map (lambda (e)
	   (apply builtin-function e))
      '((parseInt js-parseint-string-uint32 (string uint32) #f)
	(parseInt js-parseint-string (string) #f)
	(parseInt js-parseint-any (any) %this)
	(parseInt js-parseint (any any) %this)
	(parseFloat js-parsefloat (any) %this)
	(Number js-tonumber (any) %this)
	(isNaN nanfl? (real) #f)
	(isNaN js-number-isnan? (number) #f)
	(isNaN js-isnan? (any) %this)
	(encodeURI js-jsstring-encodeuri (string) #t)
	(encodeURI js-jsstring-maybe-encodeuri (any) #t)
	(encodeURIComponent js-jsstring-encodeuricomponent (string) #t)
	(encodeURIComponent js-jsstring-maybe-encodeuricomponent (any) #t)
	(unescape js-jsstring-unescape (string) #t)
	(unescape js-jsstring-maybe-unescape (any) #t)
	(TypeError js-type-error1 (any) #t)
	(TypeError js-type-error2 (any any) #t)
	(TypeError js-type-error (any any any) #t))))

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
;*    is-object-prototype? ...                                         */
;*---------------------------------------------------------------------*/
(define (is-object-prototype? obj)
   (when (isa? obj J2SAccess)
      (with-access::J2SAccess obj (obj field)
	 (when (isa? field J2SString)
	    (with-access::J2SString field (val)
	       (when (string=? val "prototype")
		  (is-builtin-ref? obj 'Object)))))))

;*---------------------------------------------------------------------*/
;*    is-array-prototype? ...                                          */
;*---------------------------------------------------------------------*/
(define (is-array-prototype? obj)
   (when (isa? obj J2SAccess)
      (with-access::J2SAccess obj (obj field)
	 (when (isa? field J2SString)
	    (with-access::J2SString field (val)
	       (when (string=? val "prototype")
		  (is-builtin-ref? obj 'Array)))))))

;*---------------------------------------------------------------------*/
;*    j2s-call0 ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-call0 obj args mode return conf)

   (define (def obj args mode return conf)
      `(js-function-maybe-call0 ,(cadr args)
	  ,(j2s-scheme obj mode return conf)
	  ,(j2s-scheme (car args) mode return conf)
	  ,(cadr args)))

   (define (is-object-prototype-tostring? obj args)
      (when (=fx (length args) 3)
	 ;; a method called with exactly 1 argument
	 ;; (%this and cache have been added)
	 (when (isa? obj J2SAccess)
	    (with-access::J2SAccess obj (obj field)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (when (string=? val "toString")
			(is-object-prototype? obj))))))))

   (cond
      ((is-object-prototype-tostring? obj args)
       `(js-object-prototype-tostring 
	   ,(j2s-scheme (car args) mode return conf)
	   ,(cadr args)))
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

   (define (is-object-prototype-has-own-property? obj args)
      (when (=fx (length args) 4)
	 ;; a method called with exactly two arguments
	 ;; (%this and cache have been added)
	 (when (isa? obj J2SAccess)
	    (with-access::J2SAccess obj (obj field)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (when (string=? val "hasOwnProperty")
			(is-object-prototype? obj))))))))

   (define (is-array-prototype-slice? obj args)
      (when (=fx (length args) 4)
	 ;; a method called with exactly two arguments
	 ;; (%this and cache have been added)
	 (when (isa? obj J2SAccess)
	    (with-access::J2SAccess obj (obj field)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (when (string=? val "slice")
			(is-array-prototype? obj))))))))
   
   (cond
      ((is-object-prototype-has-own-property? obj args)
       `(js-has-own-property
	   ,(j2s-scheme (car args) mode return conf)
	   ,(j2s-scheme (cadr args) mode return conf)
	   ,(caddr args)))
      ((is-array-prototype-slice? obj args)
       (case (j2s-type (car args))
	  ((string)
	   (let ((o (gensym '%o)))
	      `(let ((,o ,(j2s-scheme (car args) mode return conf)))
		  (js-jsstring-slice ,o
		     ,(j2s-scheme (cadr args) mode return conf)
		     (js-jsstring-lengthfx ,o)
		     ,(caddr args)))))
	  ((arguments)
	   (let ((a (gensym '%a)))
	      `(let ((,a ,(j2s-scheme (car args) mode return conf)))
		  (js-arguments-slice ,a
		     ,(j2s-scheme (cadr args) mode return conf)
		     (js-arguments-length ,a %this)
		     ,(caddr args)))))
	  (else
	   `(js-array-prototype-maybe-slice1
	       ,(j2s-scheme (car args) mode return conf)
	       ,(j2s-scheme (cadr args) mode return conf)
	       ,(caddr args)))))
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

   (define (is-array-prototype-slice? obj args)
      (when (=fx (length args) 5)
	 ;; a method called with exactly two arguments
	 ;; (%this and cache have been added)
	 (when (isa? obj J2SAccess)
	    (with-access::J2SAccess obj (obj field)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (when (string=? val "slice")
			(is-array-prototype? obj))))))))

   (cond
      ((is-array-prototype-slice? obj args)
       (case (j2s-type (car args))
	  ((string)
	   `(js-string-slice
	       ,(j2s-scheme (car args) mode return conf)
	       ,(j2s-scheme (cadr args) mode return conf)
	       ,(j2s-scheme (caddr args) mode return conf)
	       ,(cadddr args)))
	  ((arguments)
	   `(js-arguments-slice
	       ,(j2s-scheme (car args) mode return conf)
	       ,(j2s-scheme (cadr args) mode return conf)
	       ,(j2s-scheme (caddr args) mode return conf)
	       ,(cadddr args)))
	  (else
	   `(js-array-maybe-slice2
	       ,(j2s-scheme (car args) mode return conf)
	       ,(j2s-scheme (cadr args) mode return conf)
	       ,(j2s-scheme (caddr args) mode return conf)
	       ,(cadddr args)
	       ,(car (cddddr args))))))
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
(define (j2s-array-plain? mode return ctx)
   (let ((array (context-array ctx)))
      (if (isa? array J2SDeclExtern)
	  (decl-only-call? array)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-object-plain? ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-object-plain? mode return ctx)
   (let ((object (context-object ctx)))
      (if (isa? object J2SDeclExtern)
	  (decl-only-call? object)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-string-plain? ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-string-plain? mode return ctx)
   (let ((string (context-string ctx)))
      (if (isa? string J2SDeclExtern)
	  (decl-only-call? string)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-regexp-plain? ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-regexp-plain? mode return ctx)
   (let ((regexp (context-regexp ctx)))
      (if (isa? regexp J2SDeclExtern)
	  (decl-only-call? regexp)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-math-plain? ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-math-plain? mode return ctx)
   (let ((math (context-math ctx)))
      (if (isa? math J2SDeclExtern)
	  (decl-only-call? math)
	  #t)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SCall mode return ctx)

   (define (call-profile profid call)
      (if (and (context-get ctx :profile-call #f) (>=fx profid 0))
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
      (let ((scmobj (j2s-scheme obj mode return ctx))
	    (scmarg (j2s-scheme arg mode return ctx)))
	 (if (isa? obj J2SAref)
	     (with-access::J2SAref obj (array alen)
		(let ((scmarr (j2s-decl-scm-id array ctx))
		      (scmalen (j2s-decl-scm-id alen ctx)))
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
	  (j2s-type expr)))

   (define (find-builtin-method obj field args)
      
      (define (is-type-or-class? ty obj tyobj)
	 (cond
	    ((eq? ty 'any)
	     #t)
	    ((eq? ty tyobj)
	     #t)
	    ((and (pair? ty) (eq? (car ty) :hint))
	     (is-hint? obj (cadr ty)))
	    ((isa? obj J2SUnresolvedRef)
	     (with-access::J2SUnresolvedRef obj (id)
		(eq? ty id)))
	    ((isa? obj J2SRef)
	     (with-access::J2SRef obj (decl)
		(when (isa? decl J2SDeclExtern)
		   (with-access::J2SDeclExtern decl (id)
		      (when (eq? id ty)
			 (not (decl-usage-has? decl '(assig))))))))))
      
      (when (and (isa? field J2SString) (= (context-get ctx debug: 0) 0))
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
						  (and (list? params)
						       (every pair? params)))
					   (let ((p (builtin-method-predicate m)))
					      (or (not p) (p mode return ctx)))))
				       ((null? params)
					#f)
				       ((symbol? params)
					#t)
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

   (define (arity args)
      (let loop ((arity 0)
		 (args args))
	 (cond
	    ((null? args) arity)
	    ((pair? args) (loop (+fx arity 1) (cdr args)))
	    (else (negfx (+fx arity 1))))))
   
   (define (call-builtin-method obj::J2SExpr field::J2SExpr args cache cspecs)
      (let ((m (find-builtin-method obj field args)))
	 (when m
	    (let* ((met (builtin-method-met m))
		   (opt (builtin-method-args m))
		   (arity (arity opt))
		   (len (length args)))
	       (cond
		  ((not met)
		   #f)
		  ((symbol? met)
		   ;; builtin simple
		   (with-tmp-args args mode return ctx
		      (lambda (args)
			 (cond
			    ((>=fx arity 0)
			     `(,met ,(j2s-scheme obj mode return ctx)
				 ,@args
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
			    ((=fx arity -2)
			     (let ((o (gensym '%o)))
				`(let ((,o ,(j2s-scheme obj mode return ctx)))
				    (js-call-with-stack-list
				       (list ,@args)
				       (lambda (l)
					  (,met ,o
					     l
					     ,@(if (builtin-method-%this m)
						   '(%this)
						   '())
					     ,@(if (builtin-method-cache m)
						   (if cache
						       `((js-pcache-ref %pcache ,cache))
						       '(#f))
						   '())))))))
			    (else
			     ;; var args
			     (let* ((o (gensym '%o))
				    (fix (take args (negfx (+fx arity 1))))
				    (tmps (map (lambda (_) (gensym '%a)) fix)))
				`(let* ((,o ,(j2s-scheme obj mode return ctx))
					,@(map list fix tmps))
				    (js-call-with-stack-list
				       (list ,@(list-tail args (negfx (+fx arity 1))))
				       (lambda (l)
					  (,met ,o
					     ,@tmps
					     l
					     ,@(if (builtin-method-%this m)
						   '(%this)
						   '())
					     ,@(if (builtin-method-cache m)
						   (if cache
						       `((js-pcache-ref %pcache ,cache))
						       '(#f))
						   '())))))))))))
		  ((procedure? met)
		   ;; builtin procedure
		   (cond
		      ((>=fx arity 0)
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
			  mode return ctx))
		      ((=fx arity -2)
		       ;; untested
		       (met obj '() args
			  (if (builtin-method-%this m)
			      '(%this)
			      '())
			  (if (builtin-method-cache m)
			      (if cache
				  `((js-pcache-ref %pcache ,cache))
				  '(#f))
			      '())
			  mode return ctx))
		      (else
		       ;; untested
		       (met obj 
			  (take args (negfx (+fx arity 1)))
			  (list-tail args (negfx (+fx arity 1)))
			  (if (builtin-method-%this m)
			      '(%this)
			      '())
			  (if (builtin-method-cache m)
			      (if cache
				  `((js-pcache-ref %pcache ,cache))
				  '(#f))
			      '())
			  mode return ctx))))
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

   (define (Date? self)
      (is-builtin-ref? self 'Date))

   (define (Object? self)
      (is-builtin-ref? self 'Object))

   (define (Process? self)
      (is-builtin-ref? self 'process))

   (define (mincspecs x y)
      (filter (lambda (c) (memq c y)) x))
   
   (define (call-ref-method self ccache ocache ccspecs fun::J2SAccess obj::J2SExpr args)

      (with-access::J2SAccess fun (loc field (ocspecs cspecs))
	 (cond
	    ((isa? self J2SSuper)
	     (call-super-method fun args))
	    ((and (Array? self)
		  (j2s-array-builtin-method fun args this mode return ctx))
	     =>
	     (lambda (expr) expr))
	    ((and (Math? self)
		  (j2s-math-builtin-method fun args this mode return ctx))
	     =>
	     (lambda (expr) expr))
	    ((and (Json? self)
		  (j2s-json-builtin-method fun args this mode return ctx))
	     =>
	     (lambda (expr) expr))
	    ((and (Date? self)
		  (j2s-date-builtin-method fun args this mode return ctx))
	     =>
	     (lambda (expr) expr))
	    ((and (Object? self)
		  (j2s-object-builtin-method fun args this mode return ctx))
	     =>
	     (lambda (expr) expr))
	    ((and (Process? self)
		  (j2s-process-builtin-method fun args this mode return ctx))
	     =>
	     (lambda (expr) expr))
	    ((and ccache (= (context-get ctx :debug 0) 0) ccspecs)
	     (cond
		((isa? field J2SString)
		 (with-access::J2SString field (val)
		    (or (and (string=? val "toString")
			     (j2s-tostring this mode return ctx))
			(let ((call (if (eq? (j2s-type obj) 'object)
					'js-method-jsobject-call-name/cache
					'js-method-call-name/cache)))
			   `(,call
			       ,j2s-unresolved-call-workspace
			       ,(j2s-as (j2s-scheme obj mode return ctx)
				   obj (j2s-type obj) 'any ctx)
			       ,(& val (context-program ctx))
			       ,(js-pcache ccache)
			       ,(js-pcache ocache)
			       ,(loc->point loc)
			       ',ccspecs
			       ',(cond
				    ((>=fx (context-get ctx :optim 0) 4)
				     (mincspecs ocspecs '(imap amap amap+ vtable)))
				    ((>=fx (context-get ctx :optim 0) 3)
				     (mincspecs ocspecs '(imap amap+)))
				    (else
				     (mincspecs ocspecs '(imap+))))
			       ,@(map (lambda (arg)
					 (j2s-scheme arg mode return ctx))
				    args))))))
		(else
		 (call-unknown-function 'direct fun
		    (list (box (j2s-scheme obj mode return ctx)
			     (j2s-type obj) ctx))
		    args))))
	    (else
	     (call-unknown-function 'direct fun
		(list
		   (box (j2s-scheme obj mode return ctx) (j2s-type obj) ctx))
		args)))))

   (define (call-globalref-method self ccache ocache fun::J2SAccess obj::J2SExpr args)
      (with-access::J2SGlobalRef self (id decl)
	 (unless (decl-usage-has? decl '(assig))
	    (case id
	       ((Math)
		(j2s-math-builtin-method fun args
		   this mode return ctx))
	       ((Array)
		;; This branch is currently never used
		;; because Array is defined as an external
		;; %scoped object (see header.scm).
		;; Array builtin methods are then
		;; handled in the call-ref-method above
		(j2s-array-builtin-method fun args
		   this mode return ctx))
	       ((Date)
		(j2s-date-builtin-method fun args
		   this mode return ctx))
	       (else
		#f)))))

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
			  `(let ((,tmp ,(j2s-scheme obj mode return ctx)))
			      ,(call-ref-method obj ccache acache
				  ccspecs fun (J2SHopRef tmp) args)))))
		  ((and (context-get ctx :profile-call #f) (>=fx profid 0))
		   (with-access::J2SAccess fun (obj loc)
		      ;; when profile method call, inverse the call cache
		      ;; and object cache to improve profiling precision
		      (if (isa? obj J2SSuper)
			  (let* ((self (j2s-scheme obj mode return ctx))
				 (s (gensym '%obj-profile))
				 (f (duplicate::J2SAccess fun
				       (cspecs '(pmap vtable))
				       (cache cache)
				       (obj (J2SHopRef s)))))
			     `(let ((,s ,self))
				 ,(call-unknown-function 'direct
				     f (list 'this) args)))
			  (let* ((self (j2s-scheme obj mode return ctx))
				 (s (gensym '%obj-profile))
				 (f (duplicate::J2SAccess fun
				       (cspecs '(pmap-dummy-profile vtable-dummy-profile))
				       (cache cache)
				       (obj (J2SCast 'any
					       (J2SHopRef/type s
						  (j2s-type obj)))))))
			     `(let ((,s ,self))
				 ,(call-unknown-function 'direct
				     f (list s) args))))))
		  ((isa? obj J2SRef)
		   (call-ref-method obj ccache acache ccspecs fun obj args))
		  ((isa? obj J2SParen)
		   (with-access::J2SParen obj (expr)
		      (loop expr)))
		  (else
		   (let ((tmp (gensym 'obj)))
		      `(let ((,tmp ,(box (j2s-scheme obj mode return ctx)
				       (j2s-type obj) ctx)))
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
      `(,(j2s-scheme fun mode return ctx)
	,@(j2s-scheme args mode return ctx)))

   (define (j2s-self thisarg)
      (map (lambda (t) (j2s-scheme t mode return ctx)) thisarg))


   (define (call-arguments-function fun::J2SFun thisarg::pair-nil f %gen args)
      (with-access::J2SFun fun (params vararg idthis loc argumentsp)
	 (let ((self (if idthis (j2s-self thisarg) '())))
	    (if (context-get ctx :optim-arguments)
		(with-access::J2SDeclArguments argumentsp (alloc-policy)
		   (if (and (eq? alloc-policy 'lazy)
			    (context-get ctx :optim-stack-alloc))
		       (let ((v (gensym 'vec)))
			  `(js-call-with-stack-vector
			      (vector ,@(j2s-scheme args mode return ctx))
			      (lambda (,v)
				 (,f ,@%gen ,@self ,v))))
		       `(,f ,@%gen ,@self
			   (vector ,@(j2s-scheme args mode return ctx)))))
		`(,f ,@%gen ,@self
		    ,@(j2s-scheme args mode return ctx))))))
   
   (define (call-rest-function fun::J2SFun thisarg::pair-nil f %gen args)
      ;; call a function that accepts a rest argument
      (with-access::J2SFun fun (params vararg)
	 (let loop ((params params)
		    (args args)
		    (actuals '()))
	    (cond
	       ((null? (cdr params))
		;; the rest argument
		(with-access::J2SDeclRest (car params) (alloc-policy)
		   (if (and (eq? alloc-policy 'lazy)
			    (context-get ctx :optim-arguments)
			    (context-get ctx :optim-stack-alloc))
		       (let ((v (gensym 'vec)))
			  `(js-call-with-stack-vector
			      (vector ,@(j2s-scheme args mode return ctx))
			      (lambda (,v)
				 (,f ,@%gen
				    ,@(j2s-self thisarg)
				    ,@(reverse! actuals)
				    ,v))))
		       `(,f ,@%gen
			   ,@(j2s-self thisarg)
			   ,@(reverse! actuals)
			   (js-vector->jsarray
			      (vector ,@(j2s-scheme args mode return ctx))
			      %this)))))
	       ((null? args)
		(with-access::J2SDecl (car params) (loc)
		   (loop (cdr params) '()
		      (cons '(js-undefined) actuals))))
	       (else
		(loop (cdr params) (cdr args)
		   (cons (j2s-scheme (car args) mode return ctx)
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
				 (j2s-scheme a mode return ctx)))
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
				   `(,t ,(j2s-scheme a mode return ctx)))
			      temps args)
		       (,f ,@%gen ,@(j2s-self thisarg)
			  ,@(take temps lenf)))))
	       (else
		;; argument missing
		`(,f ,@(j2s-self thisarg)
		    ,@(j2s-scheme args mode return ctx)
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
			    (format "wrong number of arguments, minimum expected: ~a"
			       (j2s-minlen val))
			    this (format "~a provided" la))))
		     ((arguments)
		      #t)
		     (else
		      (unless (and (>=fx la (j2s-minlen val)) (<=fx la lp))
			 (j2s-error id
			    (format "wrong number of arguments, minimum expected: ~a"
			       (j2s-minlen val))
			    this (format "~a provided" la))))))))))

   (define (call-fun-function profid fun::J2SFun thisarg::pair-nil protocol f %gen::pair-nil args::pair-nil)
      (with-access::J2SFun fun (params vararg idthis loc argumentsp)
	 (case (if (eq? protocol 'bounce) 'bounce vararg)
	    ((arguments)
	     (call-profile profid
		(call-arguments-function fun thisarg f %gen args)))
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
		`(if ,(j2s-in? loc (& id (context-program ctx))
			 (car withs) ctx)
		     ,(call-unknown-function 'direct
			 (j2s-get loc (car withs) #f 'object
			    (& id (context-program ctx)) 'string 'any ctx #f #f)
			(list (car withs)) args)
		     ,(loop (cdr withs)))))))

   (define (call-pragma fun::J2SPragma args)
      (with-access::J2SPragma fun (expr)
	 `(,expr %this ,@(j2s-scheme args mode return ctx))))

   (define (typed-generator? decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (parent)
	 (when (isa? parent J2SDeclFun)
	    (let ((val (j2sdeclinit-val-fun parent)))
	       (with-access::J2SFun val (generator)
		  generator)))))

   (define (call-known-function protocol profid fun::J2SDecl thisarg::pair-nil args)
      (cond
	 ((isa? fun J2SDeclFun)
	  (let ((val (j2sdeclinit-val-fun fun))
		(id (j2s-decl-fast-id fun ctx)))
	     (check-hopscript-fun-arity val id args)
	     (let ((%gen (if (typed-generator? fun) '(%gen) '())))
		(call-fun-function profid val thisarg protocol id %gen args))))
	 ((j2s-let-opt? fun)
	  (with-access::J2SDeclInit fun (val)
	     (call-fun-function profid val thisarg protocol
		(j2s-decl-fast-id fun ctx) '() args)))
	 (else
	  (error "js-scheme" "Should not be here" (j2s->list fun)))))

   (define (call-unknown-function protocol fun self::pair-nil args)
      (with-access::J2SCall this (loc cache profid)
	 (let* ((len (length args))
		(call (cond
			 ((>=fx len 11)
			  'js-calln)
			 ((and (context-get ctx :optim-size)
			       (= (context-get ctx :debug 0) 0))
			  (string->symbol (format "js-call~a-obj" len)))
			 (else
			  (string->symbol (format "js-call~a" len)))))
		(packargs (if (>=fx len 11)
			      (lambda (expr) `((list ,@expr)))
			      (lambda (expr) expr))))
	    (case protocol
	       ((function)
		(cond
		   ((> (context-get ctx :debug 0) 0)
		    (with-tmp-args args mode return ctx
		       (lambda (args)
			  `(,(symbol-append call '/debug)
			    ,j2s-unresolved-call-workspace
			    ',loc
			    ,(j2s-as (j2s-scheme fun mode return ctx)
				fun (j2s-type fun) 'any ctx)
			    ,@self
			    ,@(packargs args)))))
		   ((and (context-get ctx :profile-call #f) (>=fx profid 0))
		    (let* ((f (gensym '%fun-profile))
			   (call `(,call ,j2s-unresolved-call-workspace
				     ,f
				     ,@self
				     ,@(packargs
					  (j2s-scheme args mode return ctx)))))
		       `(let ((,f ,(j2s-as (j2s-scheme fun mode return ctx)
				      fun (j2s-type fun) 'any ctx)))
			   ,(when (and (isa? fun J2SAccess)
				       (context-get ctx :profile-cmap #f))
			       (with-access::J2SAccess fun (obj)
				  (let ((o (j2s-scheme obj mode return ctx)))
				     (cmap-profile profid o))))
			   ,(funcall-profile profid f call))))
		   (else
		    `(with-access::JsFunction ,(j2s-scheme fun mode return ctx) (procedure)
			(procedure ,@self
			   ,@(packargs (j2s-scheme args mode return ctx)))))))
	       (else
		(cond
		   ((> (context-get ctx :debug 0) 0)
		    (with-tmp-args args mode return ctx
		       (lambda (args)
			  `(,(symbol-append call '/debug)
			    ,j2s-unresolved-call-workspace
			    ',loc
			    ,(j2s-as (j2s-scheme fun mode return ctx)
				fun (j2s-type fun) 'any ctx)
			    ,@self
			    ,@(packargs args)))))
		   ((and (context-get ctx :profile-call #f) (>=fx profid 0))
		    (let* ((f (gensym '%fun-profile))
			   (call `(,call ,j2s-unresolved-call-workspace
				     ,f
				     ,@self
				     ,@(packargs
					  (j2s-scheme args mode return ctx)))))
		       `(let ((,f ,(j2s-as (j2s-scheme fun mode return ctx)
				      fun (j2s-type fun) 'any ctx)))
			   ,(when (and (isa? fun J2SAccess)
				       (context-get ctx :profile-cmap #f))
			       (with-access::J2SAccess fun (obj)
				  (let ((o (j2s-scheme obj mode return ctx)))
				     (cmap-profile profid o))))
			   ,(funcall-profile profid f call))))
		   (cache
		    `(js-call/cache
			,j2s-unresolved-call-workspace
			,(js-pcache cache)
			,(j2s-as (j2s-scheme fun mode return ctx)
			    fun (j2s-type fun) 'any ctx)
			,@self
			,@(packargs (j2s-scheme args mode return ctx))))
		   ((and (memq (j2s-type fun) '(arrow function))
			 (not (context-get ctx :optim-size)))
		    `(,(symbol-append call '-jsprocedure)
		      ,j2s-unresolved-call-workspace
		      ,(j2s-as (j2s-scheme fun mode return ctx)
			    fun (j2s-type fun) 'any ctx)
		      ,@self
		      ,@(packargs (j2s-scheme args mode return ctx))))
		   (else
		    `(,call ,j2s-unresolved-call-workspace
			,(j2s-as (j2s-scheme fun mode return ctx)
			    fun (j2s-type fun) 'any ctx)
			,@self
			,@(packargs (j2s-scheme args mode return ctx))))))))))

   (define (call-eval-function fun args)
      ;; http://www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.1.1
      `(%js-direct-eval 
	  ,(if (null? args)
	       '(js-undefined)
	       (j2s-scheme (car args) mode return ctx))
	  ,(strict-mode? mode)
	  %this this %scope))

   (define (is-eval? fun)
      (with-access::J2SUnresolvedRef fun (id)
	 (eq? id 'eval)))

   (define (call-unresolved-function fun thisarg args)
      (if (is-eval? fun)
	  (call-eval-function fun args)
	  (call-unknown-function 'direct fun
	     (j2s-scheme thisarg mode return ctx) args)))

   (define (call-scheme this fun args)
      `(,(j2s-scheme fun mode return ctx)
	,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))

   (define (call-scheme-nothis this fun args)
      (if (isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     `(,(j2s-decl-fast-id decl ctx)
	       ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))
	  (call-scheme this fun args)))

   (define (call-scheme-this this fun thisarg args)
      `(,(j2s-scheme fun mode return ctx)
	,@(j2s-scheme thisarg mode return ctx)
	,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))

   (define (call-scheme-this-arity this fun thisarg args)
      (let ((len (length args)))
	 `(js-calln-procedure
	     ,(j2s-scheme fun mode return ctx)
	     ,@(j2s-scheme thisarg mode return ctx)
	     (list ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))
	 `(,(string->symbol (format "js-call~a-procedure" len))
	   ,(j2s-scheme fun mode return ctx)
	   ,@(j2s-scheme thisarg mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args))))

   (with-access::J2SCall this (loc profid fun thisarg args protocol cache cspecs)
      (let loop ((fun fun))
	 (epairify loc
	    (cond
	       ((eq? protocol 'spread)
		(j2s-scheme-call-spread this mode return ctx))
	       ((eq? (j2s-type fun) 'procedure)
		(case protocol
		   ((procedure-this)
		    (call-scheme-this this fun thisarg args))
		   ((procedure-this-arity)
		    (call-scheme-this-arity this fun thisarg args))
		   ((procedure-nothis)
		    (call-scheme-nothis this fun args))
		   (else
		    (call-scheme this fun args))))
	       ((isa? fun J2SAccess)
		(call-method this cache cspecs fun args))
	       ((isa? fun J2SParen)
		(with-access::J2SParen fun (expr)
		   (loop expr)))
	       ((isa? fun J2SHopRef)
		(call-hop-function fun thisarg args))
	       ((isa? fun J2SSuper)
		(j2s-scheme-super this mode return ctx))
	       ((and (isa? fun J2SFun)
		     (with-access::J2SFun fun (decl)
			(not decl)))
		(call-fun-function profid fun thisarg protocol
		   (jsfun->lambda fun mode return ctx (j2s-fun-prototype fun) #f)
		   '()
		   args))
	       ((Array? fun)
		(j2s-scheme (J2SNew* fun args) mode return ctx))
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
			       ,@(map (lambda (arg param)
					 (j2s-as 
					    (j2s-scheme arg mode return ctx)
					    arg
					    (j2s-type arg)
					    param ctx))
				    args
				    (builtin-function-args f))
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
		   (j2s-scheme thisarg mode return ctx) args))
	       ((read-only-function fun)
		=>
		(lambda (fun)
		   (call-known-function protocol profid fun thisarg args)))
	       (else
		(call-unknown-function protocol fun
		   (j2s-scheme thisarg mode return ctx) args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-tostring ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-tostring this mode return ctx)
   (with-access::J2SCall this (fun args)
      (with-access::J2SAccess fun (obj)
	 (case (j2s-type obj)
	    ((number)
	     `(js-jsnumber-tostring
		 ,(j2s-scheme obj mode return ctx)
		 ,(if (pair? args)
		      (j2s-scheme (car args) mode return ctx)
		      10)
		 %this))
	    ((int53 bint)
	     (if (pair? args)
		 `(js-string->jsstring
		     (fixnum->string ,(j2s-scheme obj mode return ctx)
			,(j2s-scheme (car args) mode return ctx)))
		 `(js-integer->jsstring
		     ,(j2s-scheme obj mode return ctx))))
	    ((int32)
	     `(js-string->jsstring
		 (number->string ,(j2s-scheme obj mode return ctx)
		    ,(if (pair? args)
			 (j2s-scheme (car args) mode return ctx)
			 10))))
	    ((uint32)
	     `(js-string->jsstring
		 (number->string ,(j2s-scheme obj mode return ctx)
		    ,(if (pair? args)
			 (j2s-scheme (car args) mode return ctx)
			 10))))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-spread ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-call-spread this mode return ctx)
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
		       (j2s-scheme ncall mode return ctx)))
		 (epairify loc
		    `(js-apply %this ,(j2s-scheme fun mode return ctx)
			,@(map (lambda (a) (j2s-scheme a mode return ctx))
			     thisarg)
			,(j2s-spread->expr-list args mode return ctx)))))
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
		   `(let ((,o ,(j2s-scheme obj mode return ctx)))
		       ,(j2s-scheme ncall mode return ctx)))))
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
		(j2s-scheme ncall mode return ctx)))))))
						      
;*---------------------------------------------------------------------*/
;*    decl-only-call? ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-only-call? decl::J2SDecl)
   (and (decl-usage-has? decl '(get call new init instanceof))
	(not (decl-usage-has? decl '(assig ref set uninit rest eval)))))
