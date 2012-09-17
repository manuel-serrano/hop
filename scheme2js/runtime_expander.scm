;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module runtime-expander
   (import config
	   error
	   verbose
	   (runtime-ref pobject-conv))
   (export (runtime-expand! prog)))

;; inlines/expands for-each, map, filter, ....

(define (runtime-expand! prog)
   (if (and (config 'runtime-is-constant)
	    (config 'inline-runtime))
       (begin
	  (verbose "inlining runtime-functions")
	  (let ((global-env (add-global-ids prog '())))
	     (rt-expand! prog global-env))
	  prog)
       prog))

(define (add-global-ids exprs env)
   (cond
      ((null? exprs)
       env)
      ((not (pair? (car exprs)))
       (add-global-ids (cdr exprs) env))
      ((eq? (caar exprs) 'define)
       (let ((id (cadr (car exprs))))
	  (add-global-ids (cdr exprs)
			  (if (and (has-rt-expander? id)
				   (not (memq id env)))
			      (cons id env)
			      env))))
      ((eq? (caar exprs) 'begin)
       (add-global-ids (cdr exprs)
		       (add-global-ids (cdr (car exprs))
				       env)))
      (else
       (add-global-ids (cdr exprs) env))))
      
(define (add-head-define-ids body env)
   (define (inner exprs extended-env finish-fun)
      (let loop ((exprs exprs)
		 (extended-env extended-env))
	 (cond
	    ((null? exprs)
	     extended-env)
	    ((not (pair? (car exprs))) ;; just a symbol... inside a body
	     (finish-fun extended-env))
	    ((eq? (caar exprs) 'begin)
	     (loop (cdr exprs)
		   (inner (cdr (car exprs))
			  extended-env
			  finish-fun)))
	    ((eq? (caar exprs) 'define)
	     ;; this is after the expansion-pass -> we know there is a symbol
	     (let ((id (cadr (car exprs))))
		(loop (cdr exprs)
		      (if (and (has-rt-expander? id)
			       (not (memq id extended-env)))
			  (cons id extended-env)
			  extended-env))))
	    (else
	     (finish-fun extended-env)))))
   (bind-exit (finish-fun)
      (inner body env finish-fun)))

(define (rt-expand-list! l env)
   (for-each (lambda (el)
		(rt-expand! el env))
	     l))

(define (rt-expand-lambda! formals body env)
   (define (add-formals formals env)
      (let loop ((formals formals)
		 (extended env))
	 (cond
	    ((null? formals)
	     extended)
	    ((symbol? formals) ;; vaarg
	     (if (and (has-rt-expander? formals)
		      (not (memq formals extended)))
		 (cons formals extended)
		 extended))
	    (else
	     (loop (cdr formals)
		   (if (and (has-rt-expander? (car formals))
			    (not (memq (car formals) extended)))
		       (cons (car formals) extended)
		       extended))))))

   (let* ((formals-env (add-formals formals env))
	  (extended-env (add-head-define-ids body formals-env)))
      (rt-expand-list! body extended-env)))

(define (rt-expand-clauses! clauses env)
   (for-each (lambda (clause)
		(rt-expand-list! (cdr clause) env))
	     clauses))

(define (rt-expand-let-form! bindings body type env)
   (let* ((ids (map car bindings))
	  (filtered-ids (filter! (lambda (id)
				    (and (has-rt-expander? id)
					 (not (memq id env))))
				 ids))
	  (extended-env (append! filtered-ids env)))
      (for-each (lambda (binding)
		   (rt-expand! (cadr binding) (if (eq? type 'let)
						  env
						  extended-env)))
		bindings)
      (rt-expand-list! body extended-env)))
      
(define (rt-expand! exp env)
   (if (pair? exp)
       (match-case exp
	  ((quote ?datum) 'do-nothing)
	  ((lambda ?formals . ?body)
	   (rt-expand-lambda! formals body env))
	  ((if ?test ?then)
	   (rt-expand! test env)
	   (rt-expand! then env))
	  ((if ?test ?then ?else)
	   (rt-expand! test env)
	   (rt-expand! then env)
	   (rt-expand! else env))
	  ((if . ?L) exp) ;; will yield an error later.
	  ((case ?key . ?clauses)
	   (rt-expand! key env)
	   (rt-expand-clauses! clauses env))
	  ((set! ?var ?expr)
	   (rt-expand! expr env))
	  ((set! . ?L) exp) ;; will yield an error later.
	  ((let ?bindings . ?body)
	   (rt-expand-let-form! bindings body 'let env))
	  ((letrec ?bindings . ?body)
	   (rt-expand-let-form! bindings body 'letrec env))
	  ((begin . ?body)
	   (rt-expand-list! body env))
	  ((define ?var ?expr)
	   (rt-expand! expr env))
	  ((pragma ?str . ?args)
	   (rt-expand! args env))
	  ((runtime-ref ?id (? procedure?))
	   'do-nothing)
	  ((?operator . ?operands)
	   (rt-expand-list! operands env)
	   (if (and (not (memq operator env))
		    (has-rt-expander? operator))
	       (let* ((expander (rt-expander operator))
		      (rt-expanded (expander exp)))
		  (unless (eq? rt-expanded exp)
		     ;; physically modify exp to replace it with expanded.
		     (set-car! exp 'begin)
		     (set-cdr! exp (list rt-expanded))))
	       (rt-expand! operator env))))
       'do-nothing))

(define & runtime-ref)

(define (emap1 f orig-L)
   (let loop ((L orig-L)
	      (rev-res '()))
      (cond
	 ((null? L)
	  (reverse! rev-res))
	 ((epair? L)
	  (loop (cdr L)
		(econs (f (car L))
		       rev-res
		       (cer L))))
	 ((pair? L)
	  (loop (cdr L)
		(cons (f (car L))
		      rev-res)))
	 (else
	  (scheme2js-error  "expander"
			    "not a list"
			    orig-L
			    orig-L)))))

(define (emap2 f orig-L)
   (let loop ((L orig-L)
	      (rev-res '()))
      (cond
	 ((null? L)
	  (reverse! rev-res))
	 ((epair? L)
	  (loop (cdr L)
		(econs (f (car L) (cer L))
		       rev-res
		       (cer L))))
	 ((pair? L)
	  (loop (cdr L)
		(cons (f (car L) #f)
		      rev-res)))
	 (else
	  (scheme2js-error "expander"
			   "not a list"
			   orig-L
			   orig-L)))))

;; be careful: expansion is already finished. So do not add bad constructs that
;; should be macro-expanded...
(define *rt-expanders*
   `((for-each
      ,(lambda (x)
	  (match-case x
	     ((?- ?proc ?L1 . ?Lrest)
	      (let* ((Ls (cons L1 Lrest))
		     (L-ids (emap1 (lambda (ignored)
				      (gensym 'L))
				   Ls))
		     (tmp-f (gensym 'tmpF))
		     (loop (gensym 'loop))
		     (proc-loc (if (epair? (cdr x)) (cer (cdr x)) #f))
		     (for-each-loc (if (epair? x) (cer x) #f)))
		 (econs
		  'let
		  `(#;let (,(econs tmp-f  ;; (,tmp-f ,proc)
				   (econs proc '() proc-loc)
				   proc-loc))
		     (letrec ((,loop
			       (lambda ,L-ids
				  (if (,(& 'null?) ,(car L-ids))
				      #unspecified
				      (begin
					 (,tmp-f ,@(emap2
						    (lambda (L pos)
						       ;; `(,&car ,L)
						       (cons (& 'car)
							     (econs L '()
								    pos)))
						    L-ids))
					 (,loop  ,@(emap2
						    (lambda (L pos)
						       ;; `(,&cdr ,L)
						       (cons (& 'cdr)
							     (econs L '()
								    pos)))
						    L-ids)))))))
			(,loop ,@Ls)))
		  for-each-loc)))
	     (else
	      x))))
     (map
      ,(lambda (x)
	  (match-case x
	     ((?- ?proc ?L1 . ?Lrest)
	      (let* ((Ls (cons L1 Lrest))
		     (L-ids (emap1 (lambda (ignored)
				      (gensym 'L))
				   Ls))
		     (tmp-f (gensym 'tmpF))
		     (loop (gensym 'loop))
		     (false-head (gensym 'falseHead))
		     (tail (gensym 'tail))
		     (proc-loc (if (epair? (cdr x)) (cer (cdr x)) #f))
		     (map-loc (if (epair? x) (cer x) #f)))
		 (econs
		  'let
		  `(#;let (,(econs tmp-f ;; (,tmp-f ,proc)
				   (econs proc '() proc-loc)
				   proc-loc)
			   (,false-head (,(& 'cons) '() '())))
		     (letrec ((,loop
			       (lambda ,(cons tail L-ids)
				  (if (,(& 'null?) ,(car L-ids))
				      (,(& 'cdr) ,false-head)
				      (begin
					 (,(& 'set-cdr!)
					  ,tail
					  (,(& 'cons)
					   (,tmp-f ,@(emap2
						      (lambda (L pos)
							 ;; `(,&car ,L)
							 (cons (& 'car)
							       (econs L '()
								      pos)))
						      L-ids))
					   '()))
					 (,loop  (,(& 'cdr) ,tail)
						 ,@(emap2
						    (lambda (L pos)
						       ;; `(,&cdr ,L)
						       (cons (& 'cdr)
							     (econs L '()
								    pos)))
						    L-ids)))))))
			(,loop ,false-head ,@Ls)))
		  map-loc)))
	     (else
	      x))))

     (map!
      ,(lambda (x)
	  (match-case x
	     ((?- ?proc ?L1 . ?Lrest)
	      (let* ((Ls (cons L1 Lrest))
		     (L-ids (emap1 (lambda (ignored)
				      (gensym 'L))
				   Ls))
		     (loop (gensym 'loop))
		     (tmp-f (gensym 'tmpF))
		     (first-L (gensym 'firstL))
		     (proc-loc (if (epair? (cdr x)) (cer (cdr x)) #f))
		     (map!-loc (if (epair? x) (cer x) #f)))
		 (econs
		  'let
		 `(#;let (,(econs tmp-f ;; (,tmp-f ,proc)
				  (econs proc '() proc-loc)
				  proc-loc)
			  (,first-L ,L1))
		     (letrec ((,loop
			       (lambda ,L-ids
				  (if (,(& 'null?) ,(car L-ids))
				      ,first-L
				      (begin
					 (,(& 'set-car!)
					  ,(car L-ids)
					  (,tmp-f ,@(emap2
						     (lambda (L pos)
							;; `(,&car ,L)
							(cons (& 'car)
							      (econs L '()
								     pos)))
						     L-ids)))
					 (,loop ,@(emap2
						   (lambda (L pos)
						      ;; `(,&cdr ,L)
						      (cons (& 'cdr)
							    (econs L '()
								   pos)))
						   L-ids)))))))
			(,loop ,first-L ,@(cdr Ls))))
		 map!-loc)))
	     (else
	      x))))
     (filter
      ,(lambda (x)
	  (match-case x
	     ((?- ?proc ?L)
	      (let* ((L-id (gensym 'L))
		     (tmp-f (gensym 'tmpF))
		     (loop (gensym 'loop))
		     (false-head (gensym 'falseHead))
		     (tail (gensym 'tail))
		     (proc-loc (if (epair? (cdr x)) (cer (cdr x)) #f))
		     (filter-loc (if (epair? x) (cer x) #f)))
		 (econs
		  'let
		 `(#;let (,(econs tmp-f ;; (,tmp-f ,proc)
				  (econs proc '() proc-loc)
				  proc-loc)
			  (,false-head (,(& 'cons) '() '())))
		     (letrec ((,loop
			       (lambda (,tail ,L-id)
				  (if (,(& 'null?) ,L-id)
				      (,(& 'cdr) ,false-head)
				      (if (,tmp-f (,(& 'car) ,L-id))
					  (begin
					     (,(& 'set-cdr!)
					      ,tail
					      (,(& 'cons)
					       (,(& 'car) ,L-id)
					       '()))
					     (,loop (,(& 'cdr) ,tail)
						    (,(& 'cdr) ,L-id)))
					  (,loop  ,tail
						  (,(& 'cdr) ,L-id)))))))
			(,loop ,false-head ,L)))
		 filter-loc)))
	     (else
	      x))))
     (filter!
      ,(lambda (x)
	  (match-case x
	     ((?- ?proc ?L)
	      (let* ((loop (gensym 'loop))
		     (tmp-f (gensym 'tmpF))
		     (L-id (gensym 'L))
		     (tail (gensym 'tail))
		     (false-head (gensym 'falseHead))
		     (proc-loc (if (epair? (cdr x)) (cer (cdr x)) #f))
		     (filter!-loc (if (epair? x) (cer x) #f)))
		 (econs
		  'let
		  `(#;let (,(econs tmp-f ;; (,tmp-f ,proc)
				   (econs proc '() proc-loc)
				   proc-loc)
			(,false-head (,(& 'cons) '() '())))
		     (letrec ((,loop
			       (lambda (,tail ,L-id)
				  (if (,(& 'null?) ,L-id)
				      (begin
					 (,(& 'set-cdr!) ,tail '())
					 (,(& 'cdr) ,false-head))
				      (if (,tmp-f (,(& 'car) ,L-id))
					  (begin
					     (,(& 'set-cdr!) ,tail ,L-id)
					     (,loop ,L-id (,(& 'cdr) ,L-id)))
					  (,loop ,tail (,(& 'cdr) ,L-id)))))))
			(,loop ,false-head ,L)))
		  filter!-loc)))
	     (else
	      x))))))


(define (has-rt-expander? id)
   (and (assq id *rt-expanders*) #t))

(define (rt-expander id)
   (cadr (assq id *rt-expanders*)))
