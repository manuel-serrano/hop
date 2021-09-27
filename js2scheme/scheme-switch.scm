;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-switch.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 25 07:00:50 2018                          */
;*    Last change :  Mon Sep 27 12:36:03 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript switch                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-switch

   (include "ast.sch"
	    "usage.sch"
	    "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_classutils
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
	   __js2scheme_scheme-record
	   __js2scheme_scheme-ops
	   __js2scheme_scheme-arguments
	   __js2scheme_scheme-spread
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-constant))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SSwitch mode return ctx)
   (with-access::J2SSwitch this (loc key cases id need-bind-exit-break)

      (define (comp-literal expr type)
	 (unless (equal? (j2s-scheme expr mode return ctx)
		    (j2s-cast (j2s-scheme expr mode return ctx)
		       expr (j2s-type expr) type ctx))
	    (error "j2s-scheme::J2SSwitch" "bad literal compilation"
	       (j2s->list expr)))
	 (j2s-cast (j2s-scheme expr mode return ctx)
	    expr (j2s-type expr) type ctx))
      
      (define (comp-char-literal expr type)
	 (with-access::J2SString expr (val)
	    (string-ref val 0)))
      
      (define (test-switch tleft tright)
	 (if (and (memq tleft '(number integer))
		  (memq tright '(number integer)))
	     '=
	     'js-strict-equal?))
      
      (define (comp-switch-cond-clause case tmp ttmp body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(epairify loc
		   `(,(js-binop2 loc '=== 'bool
			 (instantiate::J2SHopRef
			    (loc loc)
			    (id tmp)
			    (type ttmp))
			 expr
			 mode return ctx)
		     ,body)))))
      
      (define (comp-switch-cond-string-clause case tmp body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(epairify loc
		   `((eq? ,tmp ,(comp-literal expr tleft)) ,body)))))

      (define (comp-switch-case-char-clause case tmp body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(epairify loc
		   `((,(comp-char-literal expr tleft)) ,body)))))

      (define (comp-switch-case-clause case body tleft)
	 (with-access::J2SCase case (loc expr)
	    (if (isa? case J2SDefault)
		(epairify loc `(else ,body))
		(let ((literal (comp-literal expr tleft)))
		   (epairify loc `((,literal) ,body))))))

      (define (empty? seq::J2SSeq)
	 (with-access::J2SSeq seq (nodes)
	    (null? nodes)))
      
      (define (comp-switch-clause-body case funs)
	 (with-access::J2SCase case (loc body cascade)
	    (epairify loc
	       (if (empty? body)
		   (if (and cascade (pair? (cdr funs)))
		       ;; must check null if default is not the last clause
		       `(,(cadr funs))
		       '(js-undefined))
		   `(begin
		       ,(j2s-scheme body mode return ctx)
		       ,@(if (and cascade (pair? (cdr funs)))
			     ;; must check null if default is not the last clause
			     `((,(cadr funs)))
			     '()))))))

      (define (comp-switch-clause-bodies cases funs)
	 (let loop ((cases cases)
		    (funs funs)
		    (in-cascade #f)
		    (bindings '())
		    (bodies '()))
	    (if (null? cases)
		(values bindings (reverse! bodies))
		(with-access::J2SCase (car cases) (loc cascade)
		   (if in-cascade
		       (let* ((body (epairify loc `(,(car funs))))
			      (fun `(lambda ()
				       ,(comp-switch-clause-body (car cases)
					   funs)))
			      (binding (list (car funs) fun)))
			  (loop (cdr cases) (cdr funs)
			     cascade
			     (cons binding bindings)
			     (cons body bodies)))
		       (let ((body (epairify loc
				      (comp-switch-clause-body (car cases)
					 funs))))
			  (loop (cdr cases) (cdr funs)
			     cascade
			     bindings
			     (cons body bodies))))))))

      (define (mapc proc cases bodies)
	 (let loop ((cases cases)
		    (bodies bodies)
		    (default #f)
		    (res '()))
	    (cond
	       ((null? cases)
		(if default
		    (reverse! (cons default res))
		    (reverse! res)))
	       ((isa? (car cases) J2SDefault)
		(loop (cdr cases) (cdr bodies)
		   (proc (car cases) (car bodies))
		   res))
	       (else
		(loop (cdr cases) (cdr bodies)
		   default
		   (cons (proc (car cases) (car bodies)) res))))))

      (define (comp-switch-cond key cases)
	 (let ((tmp (gensym 'tmp))
	       (ttmp (j2s-type key))
	       (funs (map (lambda (c) (gensym 'fun)) cases))
	       (tleft (j2s-type key)))
	    (multiple-value-bind (bindings bodies)
	       (comp-switch-clause-bodies cases funs)
	       `(let* ((,tmp ,(j2s-scheme key mode return ctx))
		       ,@bindings)
		   (cond
		      ,@(mapc (lambda (c body)
				 (comp-switch-cond-clause c tmp ttmp body tleft))
			 cases bodies))))))

      (define (comp-switch-case key cases)
	 (let ((funs (map (lambda (c) (gensym 'fun)) cases))
	       (tleft (j2s-type key)))
	    (multiple-value-bind (bindings bodies)
	       (comp-switch-clause-bodies cases funs)
	       `(let* ,bindings
		   (case ,(j2s-scheme key mode return ctx)
		      ,@(mapc (lambda (c body)
				 (comp-switch-case-clause c body tleft))
			 cases bodies))))))

      (define (comp-char-case key cases)
	 (if (<=fx (length cases) 3)
	     (comp-switch-cond key cases)
	     (let ((val (gensym 'val))
		   (tmp (gensym 'tmp))
		   (funs (map (lambda (c) (gensym 'fun)) cases))
		   (tleft (j2s-type key)))
		(multiple-value-bind (bindings bodies)
		   (comp-switch-clause-bodies cases funs)
		   `(let* ((,val ,(j2s-scheme key mode return ctx))
			   ,@bindings)
		       ,(if (eq? (j2s-type key) 'string)
			    `(let ((,tmp (js-jsstring->string ,val)))
				(case (if (>fx (string-length ,tmp) 0)
				       (string-ref ,tmp 0)
				       #a128)
				   ,@(mapc (lambda (c body)
					      (comp-switch-case-char-clause
						 c tmp body tleft))
				      cases bodies) ))
			    `(let ((,tmp (js-tostring ,val %this)))
				(case (if (>fx (string-length ,tmp) 0)
				       (string-ref ,tmp 0)
				       #a128)
				   ,@(mapc (lambda (c body)
					      (comp-switch-case-char-clause
						 c tmp body tleft))
				      cases bodies) ))))))))

      (define (comp-string-case key cases)
	 (if (<=fx (length cases) 3)
	     (comp-switch-cond key cases)
	     (let ((val (gensym 'val))
		   (tmp (gensym 'tmp))
		   (funs (map (lambda (c) (gensym 'fun)) cases))
		   (tleft (j2s-type key)))
		(multiple-value-bind (bindings bodies)
		   (comp-switch-clause-bodies cases funs)
		   `(let* ((,val ,(j2s-scheme key mode return ctx))
			   (,tmp ,(if (eq? (j2s-type key) 'string)
				      `(js-jsstring-toname-unsafe ,val)
				      `(if (js-jsstring? ,val)
					   (js-jsstring-toname-unsafe ,val)
					   #f)))
			   ,@bindings)
		       (cond
			  ,@(mapc (lambda (c body)
				     (comp-switch-cond-string-clause c tmp body tleft))
			     cases bodies)))))))
      
      (define (scheme-case? key cases)
	 (let ((t (j2s-type key)))
	    (when (or (memq t '(integer index uint32 int32))
		      (and (eq? t 'int53) (m64? (context-conf ctx))))
	       (every (lambda (c)
			 (or (isa? c J2SDefault)
			     (with-access::J2SCase c (expr)
				(cond
				   ((isa? expr J2SNumber)
				   (with-access::J2SNumber expr (val)
				      (fixnum? val)))
				   ((isa? expr J2SCast)
				    (with-access::J2SCast expr (type expr)
				       (when (and (eq? type t)
						  (isa? expr J2SNumber))
					  (with-access::J2SNumber expr (val)
					     (fixnum? val)))))
				   (else
				    #f)))))
		  cases))))

      (define (string-case? key cases)
	 (every (lambda (c)
		   (or (isa? c J2SDefault)
		       (with-access::J2SCase c (expr)
			  (isa? expr J2SString))))
	    cases))
      
      (define (char-case? key cases)
	 (every (lambda (c)
		   (or (isa? c J2SDefault)
		       (with-access::J2SCase c (expr)
			  (when (isa? expr J2SString)
			     (with-access::J2SString expr (val)
				(when (=fx (string-length val) 1)
				   (<fx (char->integer (string-ref val 0)) 128)))))))
	    cases))
      
      (define (comp-switch)
	 (cond
	    ((scheme-case? key cases)
	     (comp-switch-case key cases))
	    ((char-case? key cases)
	     (comp-char-case key cases))
	    ((string-case? key cases)
	     (comp-string-case key cases))
	    (else
	     (comp-switch-cond key cases))))

      (define (switch-sans-break this)
	 (switch-unbreak! this)
	 (comp-switch))
      
      (define (eval-switch)
	 (let ((elsebody #f)
	       (elsefun #f)
	       (tmp (gensym 'tmp))
	       (funs (map (lambda (c) (gensym 'fun)) cases)))
	    `(let* ((,tmp ,(j2s-scheme key mode return ctx))
		    (%acc (js-undefined))
		    ,@(map (lambda (case fun)
			      (with-access::J2SCase case (loc body)
				 (epairify loc
				    `(,fun
					(lambda ()
					   ,(j2s-scheme body mode acc-return ctx))))))
			 cases funs))
		(cond
		   ,@(filter-map (lambda (case::J2SCase fun)
				    (with-access::J2SCase case (loc expr body)
				       (cond
					  ((isa? case J2SDefault)
					   (set! elsebody body)
					   (set! elsefun fun)
					   #f)
					  (else
					   (epairify loc
					      `((js-strict-equal? ,tmp
						   ,(j2s-scheme expr mode return ctx))
						,@(map (lambda (f) `(,f))
						     (memq fun funs))))))))
		      cases funs)
		   ,(epairify loc
		     `(else
		       ,@(if elsebody
			     (map (lambda (f) `(,f)) (memq elsefun funs))
			     '((js-undefined)))
		       %acc))))))

      (epairify-deep loc
	 (if (and need-bind-exit-break (not (in-eval? return)) (always-break? this)
		  (memq (caddr loc) '(26006 26555 27215 27591 29673 30320
		  32023 36350 26006 26555 27215 27591 29673 30320 32023)))
	     (switch-sans-break this)
	     (let ((switch (if (in-eval? return) (eval-switch) (comp-switch))))
		(if need-bind-exit-break
		    `(bind-exit (,(j2s-escape-id '%break id)) ,switch)
		    switch))))))

;*---------------------------------------------------------------------*/
;*    always-break? ...                                                */
;*---------------------------------------------------------------------*/
(define (always-break? this::J2SSwitch)
   (with-access::J2SSwitch this (cases)
      (every (lambda (n)
		(with-access::J2SCase n (body)
		   (break? body this (isa? n J2SDefault))))
	 cases)))

;*---------------------------------------------------------------------*/
;*    break? ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (break? this::J2SNode switch default::bool)
   default)

;*---------------------------------------------------------------------*/
;*    break? ::J2SStmt ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (break? this::J2SStmt switch default)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    break? ::J2SSeq ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (break? this::J2SSeq switch default)
   (with-access::J2SSeq this (nodes)
      (if (null? nodes)
	  default
	  (let loop ((nodes nodes))
	     (cond
		((null? (cdr nodes))
		 (break? (car nodes) switch default))
		((not (node-use-break? (car nodes) switch))
		 (loop (cdr nodes)))
		(else
		 #f))))))

;*---------------------------------------------------------------------*/
;*    break? ::J2SBreak ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (break? this::J2SBreak switch default)
   (with-access::J2SBreak this (target)
      (eq? target switch)))

;*---------------------------------------------------------------------*/
;*    break? ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (break? this::J2SIf switch default)
   (with-access::J2SIf this (then else)
      (and (break? then switch default) (break? else switch default))))

;*---------------------------------------------------------------------*/
;*    switch-unbreak! ...                                              */
;*---------------------------------------------------------------------*/
(define (switch-unbreak! this::J2SSwitch)
   (with-access::J2SSwitch this (cases)
      (for-each (lambda (c)
		   (with-access::J2SCase c (cascade body)
		      (set! cascade #f)
		      (unbreak! body this)))
	 cases)
      this))

;*---------------------------------------------------------------------*/
;*    unbreak! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unbreak! this::J2SNode switch)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unbreak! ::J2SBreak ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unbreak! this::J2SBreak switch)
   (with-access::J2SBreak this (target loc)
      (if (eq? target switch)
	  (J2SNop)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    node-use-break? ...                                              */
;*---------------------------------------------------------------------*/
(define (node-use-break? this::J2SNode switch::J2SSwitch)
   (let ((cell (make-cell #f)))
      (use-break? this switch cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    use-break? ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (use-break? this::J2SNode switch::J2SSwitch cell::cell)
   (or (cell-ref cell)
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    use-break? ::J2SBreak ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (use-break? this::J2SBreak switch::J2SSwitch cell::cell)
   (with-access::J2SBreak this (target)
      (when (eq? target switch)
	 (cell-set! cell #t)
	 #t)))


