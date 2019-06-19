;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-string.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Wed Jun 19 07:40:42 2019 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript string functions.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-string

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
	   __js2scheme_scheme-fun)

   (export (j2s-string-ref ::J2SAccess mode return conf)
	   (j2s-jsstring-replace-regexp obj args mode return conf)
	   (j2s-jsstring-replace-string obj args mode return conf)
	   (j2s-jsstring-replace obj args mode return conf)
	   (j2s-jsstring-maybe-replace obj args mode return conf)
	   (j2s-jsstring-charcodeat obj args mode return conf)
	   (j2s-jsstring-match-string obj args mode return conf)
	   (j2s-jsstring-match-regexp obj args mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-string-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-string-ref this::J2SAccess mode return conf)

   (define (literal-ascii? obj)
      (cond
	 ((isa? obj J2SLiteralCnst)
	  (with-access::J2SLiteralCnst obj (val)
	     (when (isa? val J2SString)
		(literal-ascii? val))))
	 ((isa? obj J2SString)
	  (with-access::J2SString obj (val)
	     (eq? (string-minimal-charset val) 'ascii)))))
   
   (define (jsstring-ref type obj index mode return conf)
      (let ((str (j2s-scheme obj mode return conf)))
	 (if (literal-ascii? obj)
	     `(js-ascii-ref ,str ,index %this)
	     `(js-jsstring-ref ,str ,index %this))))
   
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((eq? (j2s-vtype field) 'uint32)
	  (jsstring-ref type obj
	     (j2s-scheme field mode return conf)
	     mode return conf))
	 ((eq? (j2s-vtype field) 'int32)
	  (jsstring-ref type obj
	     `(int32->uint32 ,(j2s-scheme field mode return conf))
	     mode return conf))
	 ((memq (j2s-vtype field) '(integer bint))
	  (jsstring-ref type obj
	     `(fixnum->uint32 ,(j2s-scheme field mode return conf))
	     mode return conf))
	 ((j2s-field-length? field)
	  (let ((x `(js-jsstring-codeunit-length
		       ,(j2s-scheme obj mode return conf))))
	     (if (eq? type 'uint32)
		 x
		 (js-uint32-tointeger x conf))))
	 ((maybe-number? field)
	  `(js-string-ref ,(j2s-scheme obj mode return conf)
	      ,(j2s-scheme field mode return conf)
	      %this))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-string-replace-regexp ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-replace-regexp obj args mode return conf)
   
   (define (literal-regexp obj)
      (when (isa? obj J2SLiteralCnst)
	 (with-access::J2SLiteralCnst obj (val index)
	    (when (isa? val J2SRegExp)
	       index))))
   
   (define (mark-inline! obj)
      (with-access::J2SLiteralCnst obj (val)
	 (with-access::J2SRegExp val (inline)
	    (set! inline #t))))
   
   (define (tmp obj kont)
      (if (isa? obj J2SRef)
	  (kont (j2s-scheme obj mode return conf))
	  (let ((tmp (gensym 'obj)))
	     `(let ((,tmp ,(j2s-scheme obj mode return conf)))
		 ,(kont tmp)))))
   
   (define (fun1? obj)
      (when (isa? obj J2SFun)
	 (with-access::J2SFun obj (vararg params)
	    (unless vararg
	       (=fx (length params) 1)))))
   
   (define (replace tmp rx global)
      (let ((replacevalue (cadr args)))
	 (cond
	    ((fun1? replacevalue)
	     `(js-jsstring-replace-regexp-fun1 ,tmp
		 ,rx 0 ,global
		 ,(jsfun->lambda replacevalue mode return conf #f #f)
		 ,@(map (lambda (arg)
			   (j2s-scheme arg
			      mode return conf))
		      (cddr args))))
	    ((eq? (j2s-type replacevalue) 'string)
	     `(js-jsstring-replace-regexp-string ,tmp
		 ,rx 0 ,global
		 ,(j2s-scheme replacevalue mode return conf)
		 ,@(map (lambda (arg)
			   (j2s-scheme arg
			      mode return conf))
		      (cddr args))))
	    (else
	     `(js-jsstring-replace-regexp ,tmp
		 ,rx 0 ,global
		 ,(j2s-scheme replacevalue mode return conf)
		 ,@(map (lambda (arg)
			   (j2s-scheme arg
			      mode return conf))
		      (cddr args)))))))

   (if (literal-regexp (uncast (car args)))
       (with-access::J2SLiteralCnst (uncast (car args)) (val index)
	  (with-access::J2SRegExp val (inline flags)
	     (set! inline #t)
	     (let ((global (when (string? flags)
			      (integer? (string-index flags #\g)))))
		(tmp obj
		   (lambda (tmp)
		      (replace tmp `(js-cnst-table-ref ,index) global))))))
       (let ((regexp (j2s-scheme (uncast (car args)) mode return conf)))
	  (tmp obj
	     (lambda (tmp)
		`(with-access::JsRegExp ,regexp (rx flags)
		    ,(replace tmp 'rx (list 'js-regexp-flags-global? 'flags))))))))

;*---------------------------------------------------------------------*/
;*    string-replace-need22 ...                                        */
;*---------------------------------------------------------------------*/
(define (string-replace-need22 arg)
   (cond
      ((isa? arg J2SLiteralValue)
       (with-access::J2SLiteralValue arg (val)
	  (cond
	     ((not (string? val)) #f)
	     ((string-index val #\$) #t)
	     (else #f))))
      ((isa? arg J2SLiteralCnst)
       (with-access::J2SLiteralCnst arg (val)
	  (string-replace-need22 val)))
      ((isa? arg J2SString)
       (with-access::J2SString arg (val)
	  (string-index val #\$)))
      (else
       #t)))

;*---------------------------------------------------------------------*/
;*    j2s-string-replace-string ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-replace-string obj args mode return conf)
   `(js-jsstring-replace-string
       ,(j2s-scheme obj mode return conf)
       ,(string-replace-need22 (cadr args))
       ,@(map (lambda (arg)
		 (j2s-scheme arg mode return conf))
	    args)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-string-replace ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-replace obj args mode return conf)
   `(js-jsstring-replace
       ,(j2s-scheme obj mode return conf)
       ,(string-replace-need22 (cadr args))
       ,@(map (lambda (arg)
		 (j2s-scheme arg mode return conf))
	    args)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-string-maybe-replace ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-maybe-replace obj args mode return conf)
   `(js-jsstring-maybe-replace
       ,(j2s-scheme obj mode return conf)
       ,(string-replace-need22 (cadr args))
       ,@(map (lambda (arg)
		 (j2s-scheme arg mode return conf))
	    args)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-jsstring-charcodeat ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-charcodeat obj args mode return conf)
   (match-case args
      (((and ?pos (? expr-asuint32)) ?%this)
       (let* ((expr (expr-asuint32 pos))
	      (sexp (j2s-scheme expr mode return conf)))
	  `(js-jsstring-charcodeatu32
	      ,(j2s-scheme obj mode return conf)
	      ,(if (eq? (j2s-vtype expr) 'uint32)
		   sexp
		   `(fixnum->uint32 ,sexp))
	      ,(j2s-scheme %this mode return conf))))
      (else
       `(js-jsstring-charcodeat
	   ,(j2s-scheme obj mode return conf)
	   ,@(map (lambda (arg)
		     (j2s-scheme arg mode return conf))
		args)))))
       
;*---------------------------------------------------------------------*/
;*    j2s-jsstring-match-string ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-match-string obj args mode return conf)
   (let ((str (if (isa? (car args) J2SLiteralCnst)
		  (with-access::J2SLiteralCnst (car args) (val) val)
		  (car args))))
      (if (not (isa? str J2SString))
	  `(js-jsstring-maybe-match
	      ,(j2s-scheme obj mode return conf)
	      ,(j2s-scheme (car args) mode return conf)
	      %this
	      #f)
	  (with-access::J2SProgram (config-get conf :program) (cnsts)
	     (with-access::J2SString str (loc val)
		(let* ((len (length cnsts))
		       (rx (instantiate::J2SRegExp
			      (loc loc)
			      (flags "")
			      (inline #f)
			      (val val)))
		       (cnst (instantiate::J2SLiteralCnst
				(loc loc)
				(type 'regexp)
				(index len)
				(val rx))))
		   (set-cdr! (last-pair cnsts)
		      (list rx))
		   ;; update j2s-totest if the name of the library function
		   ;; JS-JSSTRING-MATCH-REGEXP-FROM-STRING changes
		   `(js-jsstring-match-regexp-from-string
		       ,(j2s-scheme obj mode return conf)
		       ,(j2s-scheme (car args) mode return conf)
		       ,(j2s-scheme cnst mode return conf)
		       %this)))))))

;*---------------------------------------------------------------------*/
;*    j2s-jsstring-match-regexp ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-jsstring-match-regexp obj args mode return conf)
   (let ((rx (if (isa? (car args) J2SLiteralCnst)
		 (with-access::J2SLiteralCnst (car args) (val) val)
		 (car args))))
      (if (not (isa? rx J2SRegExp))
	  `(js-jsstring-maybe-match
	      ,(j2s-scheme obj mode return conf)
	      ,(j2s-scheme (car args) mode return conf)
	      %this
	      #f)
	  (with-access::J2SRegExp rx (flags)
	     (if (string-index flags #\g)
		 `(js-jsstring-maybe-match
		     ,(j2s-scheme obj mode return conf)
		     ,(j2s-scheme (car args) mode return conf)
		     %this
		     #f)
		 `(js-regexp-prototype-exec %this
		     ,(j2s-scheme (car args) mode return conf)
		     ,(j2s-scheme obj mode return conf)))))))
