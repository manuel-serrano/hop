;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/range.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Sun Dec  4 18:49:34 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Integer Range analysis (fixnum detection)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_range

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-range-stage))

;*---------------------------------------------------------------------*/
;*    j2s-range-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-range-stage
   (instantiate::J2SStageProc
      (name "range")
      (comment "Integer range analysis inference")
      (proc j2s-range!)))

;*---------------------------------------------------------------------*/
;*    j2s-range! ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-range! this args)
   ;; debug mode
   (let ((env (or (getenv "HOPTRACE") "")))
      (when (string-contains env "j2s:dump")
	 (let ((i (string-contains env "j2s:dump")))
	    (when i
	       (call-with-input-string (substring env (+fx i 9))
		  (lambda (ip)
		     (set! *dump-env* (port->sexp-list ip)))))))
      (when (string-contains env "j2s:unfix")
	 (set! *dump-unfix* #t))
      (when (string-contains env "j2s:stop")
	 (set! *dump-stop* #t))
      (let ((i (string-contains env "j2s:stop")))
	 (when i
	    (call-with-input-string (substring env (+fx i 9))
	       (lambda (ip)
		  (set! *dump-stop* (read ip)))))))
   (when (isa? this J2SProgram)
      (when (>=fx (config-get args :optim 0) 4)
	 ;; compute the integer value ranges
	 (j2s-range-program! this args)
	 ;; allocate precise types according to the ranges
	 (when (>=fx (config-get args :optim 0) 5)
	    (j2s-range-type-program! this args)))
      this))

;*---------------------------------------------------------------------*/
;*    debug control                                                    */
;*---------------------------------------------------------------------*/
(define *dump-stop* -1)
(define *dump-env* '())
(define *dump-unfix* #f)

;*---------------------------------------------------------------------*/
;*    interval ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct interval min max)

;*---------------------------------------------------------------------*/
;*    interval constructor ...                                         */
;*---------------------------------------------------------------------*/
(define-expander interval
   (lambda (x e)
      (match-case x
	 ((?- ?min ?max)
	  `(interval ,(e min e) ,(e max e)))
	 ((?- ?min ?max ?wstamp)
	  `(interval ,(e min e) ,(e max e)))
	 (else
	  (error "interval" "wrong syntax" x)))))

;*---------------------------------------------------------------------*/
;*    interval-ok? ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-ok? i)
   (<= (interval-min i) (interval-max i)))

;*---------------------------------------------------------------------*/
;*    interval-sealed? ...                                             */
;*---------------------------------------------------------------------*/
;* (define (interval-sealed? i)                                        */
;*    (or (interval-min-sealed i) (interval-max-sealed i)))            */

;*---------------------------------------------------------------------*/
;*    fix ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct fix stamp wstamp)

;*---------------------------------------------------------------------*/
;*    j2s-range-program! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-range-program! this::J2SProgram args)
   (let* ((msize (config-get args :long-size (bigloo-config 'int-size)))
	  (lsize (min 53 msize)))
      (set! *minfix* (- (expt 2. (fixnum->flonum (-fx lsize 1)))))
      (set! *maxfix* (-fl (expt 2. (fixnum->flonum (-fx lsize 1))) 1.0))
      (set! *maxidx* (-fl (expt 2. (fixnum->flonum 32)) 2.0))
      (set! *index-intv* (interval 0.0 *maxidx*))
      (set! *infinity-intv* (interval -inf.0 +inf.0)))
   (with-access::J2SProgram this (headers decls nodes)
      (let ((fix (fix 0 0)))
	 (let loop ((i 0))
	    (let ((ostamp (fix-stamp fix)))
	       (when (>=fx *dump-stop* 0)
		  (tprint "================================= " ostamp))
	       (for-each (lambda (n) (range n (empty-env) fix)) decls)
	       (for-each (lambda (n) (range n (empty-env) fix)) nodes)
	       (if (or (=fx (fix-stamp fix) ostamp)
		       (and (>fx *dump-stop* 0)
			    (>=fx (fix-stamp fix) *dump-stop*)))
		   (when (>=fx (bigloo-debug) 4)
		      (fprintf (current-error-port) "~a." i)
		      (flush-output-port (current-error-port)))
		   (begin
		      (fix-wstamp-set! fix (+fx 1 (fix-wstamp fix)))
		      (loop (+fx i 1))))))
	 this)))

;*---------------------------------------------------------------------*/
;*    j2s-range-type-program! ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-range-type-program! this::J2SProgram args)
   (let* ((lsize (config-get args :long-size (bigloo-config 'int-size)))
	  (minfix (- (expt 2. (fixnum->flonum (-fx lsize 1)))))
	  (maxfix (-fl (expt 2. (fixnum->flonum (-fx lsize 1))) 1.0))
	  (fixnum (interval (+fl minfix 1.0) (-fl maxfix 1.0))))
      (with-access::J2SProgram this (headers decls nodes)
	 (for-each (lambda (n) (type-range! n fixnum *index-intv*)) decls)
	 (for-each (lambda (n) (type-range! n fixnum *index-intv*)) nodes)
	 this)))

;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix::struct reason)
   (when *dump-unfix*
      (tprint "--- UNFIX reason=" reason))
   (fix-stamp-set! fix (+fx 1 (fix-stamp fix))))

;*---------------------------------------------------------------------*/
;*    type-integer? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (type-integer? type::symbol)
   (or (eq? type 'integer) (eq? type 'index)))
   
;*---------------------------------------------------------------------*/
;*    type-number? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (type-number? type::symbol)
   (or (type-integer? type) (eq? type 'number)))
   
;*---------------------------------------------------------------------*/
;*    integer bounds                                                   */
;*---------------------------------------------------------------------*/
(define *max-length* (-fl (exptfl 2. 32.) 1.))
(define *max-index* (-fl *max-length* 1.))
(define *minfix* +nan.0)
(define *maxfix* +nan.0)
(define *maxidx* +nan.0)
(define *index-intv* #f)
(define *infinity-intv* #f)

;*---------------------------------------------------------------------*/
;*    make-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-env::pair-nil decl::J2SDecl intv::struct)
   (list (cons decl intv)))

;*---------------------------------------------------------------------*/
;*    empty-env ...                                                    */
;*---------------------------------------------------------------------*/
(define (empty-env::pair-nil)
   '())

;*---------------------------------------------------------------------*/
;*    extend-env ...                                                   */
;*---------------------------------------------------------------------*/
(define (extend-env::pair-nil env::pair-nil decl::J2SDecl intv)
   (cond
      ((not intv)
       (filter (lambda (c) (not (eq? (car c) decl))) env))
      ((=fx (bigloo-debug) 0)
       (cons (cons decl intv) env))
      (else
       (cons (cons decl intv)
	  (filter (lambda (c) (not (eq? (car c) decl))) env)))))

;*---------------------------------------------------------------------*/
;*    append-env ...                                                   */
;*---------------------------------------------------------------------*/
(define (append-env left right)
   (if (>=fx (bigloo-debug) 1)
       (append left (filter (lambda (c) (not (assq (car c) left))) right))
       (append left right)))

;*---------------------------------------------------------------------*/
;*    env-merge ...                                                    */
;*---------------------------------------------------------------------*/
(define (env-merge::pair-nil left::pair-nil right::pair-nil)
   
   (define (merge2 env1 env2)
      (filter-map (lambda (entry)
		     (let* ((decl (car entry))
			    (intvl (cdr entry))
			    (intvf (env-lookup env2 decl))
			    (intvm (interval-merge intvl intvf)))
			(cons decl intvm)))
	 env1))

   (merge2 right (merge2 left right)))

;*---------------------------------------------------------------------*/
;*    env-lookup ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-lookup::obj env::pair-nil decl::J2SDecl)
   (let ((c (assq decl env)))
      (when (pair? c)
	 (cdr c))))

;*---------------------------------------------------------------------*/
;*    interval-equal? ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-equal? left right)
   (and (= (interval-min left) (interval-min right))
	(= (interval-max left) (interval-max right))))

;*---------------------------------------------------------------------*/
;*    interval-in? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Is interval LEFT included in interval RIGHT?                     */
;*---------------------------------------------------------------------*/
(define (interval-in? left right)
   (when (and (interval? left) (interval? right))
      (and (>= (interval-min left) (interval-min right))
	   (<= (interval-max left) (interval-max right)))))

;*---------------------------------------------------------------------*/
;*    interval-merge ...                                               */
;*---------------------------------------------------------------------*/
(define (interval-merge left right)
   (cond
      ((not (interval? left))
       right)
      ((not (interval? right))
       left)
      (else
       (interval
	  (min (interval-min left) (interval-min right))
	  (max (interval-max left) (interval-max right))))))

;*---------------------------------------------------------------------*/
;*    interval-lts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-lts left::struct right::struct shift::int)
   (let ((ra (- (interval-max right) shift)))
      (if (< ra (interval-max left))
	  (if (>= ra (interval-min left))
	      (interval (min (interval-min left) ra) ra)
	      (interval (interval-min left) (interval-min left)))
	  left)))

(define (interval-lt left right)
   (let ((i (interval-lts left right 1)))
      [assert (i left right) (and (interval-ok? i) (interval-in? i left))]
      i))

(define (interval-lte left right)
   (let ((i (interval-lts left right 0)))
      [assert (i left right) (and (interval-ok? i) (interval-in? i left))]
      i))

;*---------------------------------------------------------------------*/
;*    interval-gts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-gts left::struct right::struct shift::int)
   (let ((ri (+ (interval-min right) shift)))
      (if (> ri (interval-min left))
	  (if (<= ri (interval-max left))
	      (interval ri (max (interval-max left) ri))
	      (interval (interval-max left) (interval-max left)))
	  left)))

(define (interval-gt left right)
   (let ((i (interval-gts left right 1)))
      [assert (i left right) (and (interval-ok? i) (interval-in? i left))]
      i))

(define (interval-gte left right)
   (let ((i (interval-gts left right 0)))
      [assert (i left right) (and (interval-ok? i) (interval-in? i left))]
      i))

;*---------------------------------------------------------------------*/
;*    interval-eq ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval-eq left::struct right::struct)
   (let* ((ri (interval-min right))
	  (ra (interval-max right))
	  (li (interval-min left))
	  (la (interval-max left))
	  (oi (max ri li))
	  (oa (min ra la)))
      (if (<= oi oa)
	  (interval oi oa)
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    interval-neq ...                                                 */
;*    -------------------------------------------------------------    */
;*    The intervals NEQ rules are:                                     */
;*                                                                     */
;*        L:  ||       [.............]       ||                        */
;*                                                                     */
;*    1:  R:  || [...] |             |       ||   =>  L                */
;*    2:  R:  ||     [...]           |       ||   =>  [Ra+1,La]        */
;*    3:  R:  ||       |    [...]    |       ||   =>  L                */
;*    4:  R:  ||       |           [...]     ||   =>  [Li,Ri-1]        */
;*    5:  R:  ||       |             | [...] ||   =>  L                */
;*    6:  R:  ||     [.|.............|.]     ||   =>  [0,0]            */
;*---------------------------------------------------------------------*/
(define (interval-neq left right)
   (let ((ri (interval-min right))
	 (ra (interval-max right))
	 (li (interval-min left))
	 (la (interval-max left)))
      (cond
	 ((< ra li)
	  ;; rule 1
	  left)
	 ((< ra la)
	  (if (<= ri li)
	      ;; rule 2
	      (interval (+ 1 ra) la)
	      ;; rule 3
	      left))
	 ((<= ri li)
	  ;; rule 6
	  (interval 0 0))
	 ((<= ri la)
	  ;; rule 4
	  (interval li (- ri 1)))
	 (else
	  ;; rule 5
	  left))))

;*---------------------------------------------------------------------*/
;*    interval-neq-test ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (interval-neq-test)
   (when (>= (bigloo-debug) 0)
      `(let ((L (interval 10 20)))
	  ;; case 1
	  [assert () (interval-equal? (interval-neq L (interval 0 0)) L)]
	  [assert () (interval-equal? (interval-neq L (interval 0 1)) L)]
	  ;; case 2
	  [assert () (interval-equal? (interval-neq L (interval 0 10)) (interval 11 20))]
	  [assert () (interval-equal? (interval-neq L (interval 0 11)) (interval 12 20))]
	  [assert () (interval-equal? (interval-neq L (interval 10 11)) (interval 12 20))]
	  [assert () (interval-equal? (interval-neq L (interval 10 12)) (interval 13 20))]
	  [assert () (interval-equal? (interval-neq L (interval 10 10)) (interval 11 20))]
	  ;; case 3
	  [assert () (interval-equal? (interval-neq L (interval 15 15)) L)]
	  [assert () (interval-equal? (interval-neq L (interval 15 16)) L)]
	  ;; case 4
	  [assert () (interval-equal? (interval-neq L (interval 15 20)) (interval 10 14))] 
	  [assert () (interval-equal? (interval-neq L (interval 20 30)) (interval 10 19))] 
	  [assert () (interval-equal? (interval-neq L (interval 15 22)) (interval 10 14))] 
	  [assert () (interval-equal? (interval-neq L (interval 20 25)) (interval 10 19))]
	  ;; case 5
	  [assert () (interval-equal? (interval-neq L (interval 21 22)) L)]
	  ;; case 6
	  [assert () (interval-equal? (interval-neq L L) (interval 0 0))] 
	  [assert () (interval-equal? (interval-neq L (interval 8 22)) (interval 0 0))]
	  #t)))

(interval-neq-test)

;*---------------------------------------------------------------------*/
;*    interval-binary ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-binary binary::procedure left right)
   (when (and (interval? left) (interval? right))
      (interval
	 (min (binary (interval-min left) (interval-min right)))
	 (max (binary (interval-max left) (interval-max right))))))

;*---------------------------------------------------------------------*/
;*    widening ...                                                     */
;*    -------------------------------------------------------------    */
;*    widening operator for the interval approximation.                */
;*---------------------------------------------------------------------*/
(define (widening left right o)
   (let ((li (interval-min left))
	 (la (interval-max left))
	 (ri (interval-min right))
	 (ra (interval-max right))
	 (oi (interval-min o))
	 (oa (interval-max o)))
      (cond
	 ((> la oa)
	  (cond
	     ((> oi 0)
	      (let ((min 0))
		 (interval min (max oa min))))
	     ((> oi -10)
	      (let ((min -10))
		 (interval min (max oa min))))
	     ((> oi (- *max-length*))
	      (let ((min (- *max-length*)))
		 (interval min (max oa min))))
	     (else
	      (interval -inf.0 oa))))
	 ((< la oa)
	  (cond
	     ((> ra 0)
	      (cond
		 ((< oa 8192)
		  (let ((max 8192))
		     (interval (min oi max) max)))
		 ((< oa (- *max-index* 10))
		  (let ((max (- *max-index* 10)))
		     (interval (min oi max) max)))
		 ((<= oa *max-index*)
		  (let ((max *max-index*))
		     (interval (min oi max) max)))
		 ((<= oa *max-length*)
		  (let ((max *max-length*))
		     (interval (min oi max) max)))
		 (else
		  (interval oi +inf.0))))
	     (else
	      (interval -inf.0 oa))))
	 (else
	  o))))
   
;*---------------------------------------------------------------------*/
;*    interval-add ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-add left right)
   (when (and (interval? left) (interval? right))
      (let ((intr (interval
		     (+ (interval-min left) (interval-min right))
		     (+ (interval-max left) (interval-max right)))))
	 (widening left right intr))))
   
;*---------------------------------------------------------------------*/
;*    interval-sub ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-sub left right)
   (when (and (interval? left) (interval? right))
      (let ((intr (interval
		     (- (interval-min left) (interval-max right))
		     (- (interval-max left) (interval-min right)))))
	 (widening left right intr))))
   
;*---------------------------------------------------------------------*/
;*    interval-mul ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-mul left right)
   (when (and (interval? left) (interval? right))
      (let ((intr (interval
		     (if (or (>= (interval-min left) *maxfix*)
			     (>= (interval-min right) *maxfix*))
			 *maxfix*
			 (* (interval-min left) (interval-min right)))
		     (if (or (>= (interval-max left) *maxfix*)
			     (>= (interval-max right) *maxfix*))
			 *maxfix*
			 (min *maxfix*
			    (* (interval-max left) (interval-max right)))))))
	 (widening left right intr))))
   
;*---------------------------------------------------------------------*/
;*    interval-div ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-div left right)
   (when (and (interval? left) (interval? right))
      (let ((min (/ (interval-min left) (interval-max right)))
	    (max (/ (interval-max left) (interval-min right))))
	 (when (and (integer? min) (integer? max))
	    (let ((intr (interval min max)))
	       (widening left right intr))))))
   
;*---------------------------------------------------------------------*/
;*    interval-shiftl ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-shiftl left right)
   
   (define (lsh n s)
      (llong->flonum (bit-lshllong (flonum->llong n) (fixnum->llong s))))
   
   (let ((intr (interval-binary lsh left right)))
      (widening left right intr)))
   
;*---------------------------------------------------------------------*/
;*    interval-shiftr ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-shiftr left right)
   
   (define (rsh n s)
      (llong->flonum (bit-rshllong (flonum->llong n) (fixnum->llong s))))
   
   (let ((intr (interval-binary rsh left right)))
      (widening left right intr)))
   
;*---------------------------------------------------------------------*/
;*    node-interval-set! ...                                           */
;*    -------------------------------------------------------------    */
;*    Set the expression interval and if needed update the fix stamp.  */
;*---------------------------------------------------------------------*/
(define (node-interval-set! this::J2SNode env::pair-nil fix::struct intv)
   (with-access::J2SNode this (%info)
      (when (interval? intv)
	 (cond
	    ((not (interval? %info))
	     (when (interval? intv)
		(unfix! fix
		   (format "node-interval-set.1! ~a -> ~a"
		      (j2s->list this) intv))
		(set! %info intv)))
	    ((not (interval-in? intv %info))
	     (unfix! fix
		(format "node-interval-set.2! ~a -> ~a => ~a"
		   (j2s->list this) intv (interval-merge intv %info)))
	     (set! %info (interval-merge intv %info)))))
      (return %info env)))
   
;*---------------------------------------------------------------------*/
;*    dump-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-env env . ids)
   (filter-map (lambda (e)
		  (with-access::J2SDecl (car e) (id key)
		     (let ((keys (if (pair? ids) ids *dump-env*)))
			(when (or (symbol? keys)
				  (memq id keys)
				  (memq key keys))
			   (cons (format "~a:~a" id key) (cdr e))))))
      env))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define (return intv::obj env::pair-nil)
   (values intv env))

;*---------------------------------------------------------------------*/
;*    range* ...                                                       */
;*---------------------------------------------------------------------*/
(define (range* nodes::pair-nil env::pair-nil fix::struct)
   (let loop ((nodes nodes)
	      (intv #f)
	      (env env))
      (if (null? nodes)
	  (return intv env)
	  (multiple-value-bind (int env)
	     (range (car nodes) env fix)
	     (begin
		(if (not (list? env))
		    (tprint "PAS BON: " (j2s->list (car nodes))))
		(loop (cdr nodes) int env))))))

;*---------------------------------------------------------------------*/
;*    range-seq ...                                                    */
;*---------------------------------------------------------------------*/
(define (range-seq nodes::pair-nil env::pair-nil fix::struct)
   (let loop ((nodes nodes)
	      (env env)
	      (envr #f))
      (if (null? nodes)
	  (return #f (or envr env))
	  (multiple-value-bind (_ envn)
	     (range (car nodes) env fix)
	     (loop (cdr nodes) envn envr)))))

;*---------------------------------------------------------------------*/
;*    range ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SNode env::pair-nil fix::struct)
   (error "range" "not implemented" (typeof this)))

;*---------------------------------------------------------------------*/
;*    range ::J2SExpr ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SExpr env::pair-nil fix::struct)
   (call-default-walker)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    range ::J2SStmt ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SStmt env::pair-nil fix::struct)
   (unless (or (isa? this J2SDecl)
	       (isa? this J2SReturn)
	       (isa? this J2SBreak)
	       (isa? this J2SLabel))
      (tprint "not implemented " (typeof this)))
   (call-default-walker)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    range ::J2SDataPropertyInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SDataPropertyInit env::pair-nil fix::struct)
   (with-access::J2SDataPropertyInit this (val)
      (multiple-value-bind (_ env)
	 (range val env fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    range ::J2SNumber ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SNumber env::pair-nil fix::struct)
   (with-access::J2SNumber this (val type)
      (if (type-number? type)
	  (let ((intv (interval val val)))
	     (node-interval-set! this env fix intv))
	  (return #f env))))

;*---------------------------------------------------------------------*/
;*    range ::J2SRef ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SRef env::pair-nil fix::struct)
   (with-access::J2SRef this (decl type)
      (if (type-number? type)
	  (with-access::J2SDecl decl (%info)
	     (let ((intv (env-lookup env decl)))
		(let ((denv (dump-env env)))
		   (with-access::J2SDecl decl (key)
		      (when (eq? key 10)
			 (when (pair? denv)
			    (tprint ">>> j2sref " (j2s->list this) " denv=" denv)))))
		(if intv
		    (node-interval-set! this env fix intv)
		    (return %info env))))
	  (return #f env))))

;*---------------------------------------------------------------------*/
;*    test-envs ...                                                    */
;*    -------------------------------------------------------------    */
;*    Returns two new environments for the positive and negative       */
;*    values of the test.                                              */
;*---------------------------------------------------------------------*/
(define (test-envs test::J2SExpr env fix::struct)
   
   (define (test-envs-ref left::J2SRef right op)
      (multiple-value-bind (intl envl)
	 (range left env fix)
	 (multiple-value-bind (intr envr)
	    (range right envl fix)
	    (with-access::J2SRef left (decl)
	       (if (or (not (interval? intl)) (not (interval? intr)))
		   (values (empty-env) (empty-env))
		   (case op
		      ((<)
		       (let ((intrt (interval-lt intl intr))
			     (intro (interval-gte intl intr)))
			  (values (make-env decl intrt)
			     (make-env decl intro))))
		      ((<=)
		       (let ((intrt (interval-lte intl intr))
			     (intro (interval-gt intl intr)))
			  (values (make-env decl intrt)
			     (make-env decl intro))))
		      ((>)
		       (let ((intrt (interval-gt intl intr))
			     (intro (interval-lte intl intr)))
			  (values (make-env decl intrt)
			     (make-env decl intro))))
		      ((>=)
		       (let ((intrt (interval-gte intl intr))
			     (intro (interval-lt intl intr)))
			  (values (make-env decl intrt)
			     (make-env decl intro))))
		      ((== ===)
		       (let ((ieq (interval-eq intl intr)))
			  (values (if (interval? ieq)
				      (make-env decl ieq)
				      (empty-env))
			     (empty-env))))
		      ((!= !==)
		       (let ((ieq (interval-eq intl intr)))
			  (values ;;(make-env decl (interval-neq intl intr))
			     (empty-env)
			     (if (interval? ieq)
				 (make-env decl ieq)
				 (empty-env)))))
		      (else
		       (values (empty-env) (empty-env)))))))))
   
   (define (inv-op op)
      (case op
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 ((== ===) op)
	 ((!= !==) op)
	 (else op)))

   (define (is-js-index test)
      ;; if test === (js-index? (ref decl) ) return decl
      ;; see __js2scheme_ast
      (when (isa? test J2SCall)
	 (with-access::J2SCall test (fun args)
	    (when (isa? fun J2SHopRef)
	       (with-access::J2SHopRef fun (id)
		  (when (eq? id 'js-index?)
		     (when (and (pair? args) (null? (cdr args)))
			(when (isa? (car args) J2SRef)
			   (with-access::J2SRef (car args) (decl)
			      decl)))))))))

   (cond
      ((isa? test J2SBinary)
       (with-access::J2SBinary test (op lhs rhs)
	  (cond
	     ((eq? op '&&)
	      (multiple-value-bind (lenvt lenvo)
		 (test-envs lhs env fix)
		 (multiple-value-bind (intl envl)
		    (range lhs env fix)
		    (multiple-value-bind (renvt renvo)
		       (test-envs rhs envl fix)
		       (values (append-env lenvt renvt)
			  (append-env lenvo renvo))))))
	     ((not (type-number? (j2s-type lhs)))
	      (values (empty-env) (empty-env)))
	     ((not (type-number? (j2s-type rhs)))
	      (values (empty-env) (empty-env)))
	     ((not (eq? (j2s-type test) 'bool))
	      (values (empty-env) (empty-env)))
	     ((isa? lhs J2SRef)
	      (if (isa? rhs J2SRef)
		  (multiple-value-bind (lenvt lenvo)
		     (test-envs-ref lhs rhs op)
		     (multiple-value-bind (renvt renvo)
			(test-envs-ref rhs lhs (inv-op op))
			(values (append-env lenvt renvt)
			   (append-env lenvo renvo))))
		  (test-envs-ref lhs rhs op)))
	     ((isa? rhs J2SRef)
	      (test-envs-ref rhs lhs (inv-op op)))
	     (else
	      (values (empty-env) (empty-env))))))
      ((is-js-index test)
       =>
       (lambda (decl)
	  (values (make-env decl *index-intv*) (empty-env))))
      (else
       (values (empty-env) (empty-env)))))

;*---------------------------------------------------------------------*/
;*    range ::J2SUnary ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SUnary env::pair-nil fix::struct)
   (with-access::J2SUnary this (op expr type)
      (if (type-number? type)
	  (case op
	     ((+)
	      (range expr env fix))
	     ((-)
	      (multiple-value-bind (intv env)
		 (range expr env fix)
		 (if (interval? intv)
		     (node-interval-set! this env fix
			(interval
			   (- (interval-max intv)) (- (interval-min intv))))
		     (return *infinity-intv* env))))
	     ((~)
	      (multiple-value-bind (intv env)
		 (range expr env fix)
		 (if (interval? intv)
		     (let ((i (interval-min env))
			   (a (interval-max env)))
			(if (and (integer? i) (integer? a))
			    (let ((ni (bit-not i))
				  (na (bit-not a)))
			       (node-interval-set! this env fix
				  (interval (min ni na) (max ni na))))
			    (return *infinity-intv* env)))
		     (return *infinity-intv* env))))
	     (else
	      (call-default-walker)
	      (return *infinity-intv* env)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    range ::J2SBinary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SBinary env::pair-nil fix::struct)
   (with-access::J2SBinary this (op lhs rhs type %info %%wstamp)
      (if (type-number? type)
	  (multiple-value-bind (intl envl)
	     (range lhs env fix)
	     (multiple-value-bind (intr envr)
		(range rhs envl fix)
		(if (< %%wstamp (fix-wstamp fix))
		    (begin
		       (set! %%wstamp (fix-wstamp fix))
		       (case op
			  ((+)
			   (node-interval-set! this env fix
			      (interval-add intl intr)))
			  ((-)
			   (node-interval-set! this env fix
			      (interval-sub intl intr)))
			  ((*)
			   (node-interval-set! this env fix
			      (interval-mul intl intr)))
			  ((/)
			   (node-interval-set! this env fix
			      (interval-div intl intr)))
			  ((<<)
			   (node-interval-set! this env fix
			      (interval-shiftl intl intr)))
			  ((>>)
			   (node-interval-set! this env fix
			      (interval-shiftr intl intr)))
			  (else
			   (return *infinity-intv* env))))
		    %info)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    range ::J2SAssig ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SAssig env::pair-nil fix::struct)
   (with-access::J2SAssig this (lhs rhs type)
      (if (type-number? type)
	  (multiple-value-bind (intv env)
	     (range lhs env fix)
	     (cond
		((isa? lhs J2SRef)
		 ;; a variable assignment
		 (multiple-value-bind (ir env)
		    (range rhs env fix)
		    (with-access::J2SRef lhs (decl)
		       (let ((nenv (extend-env env decl ir)))
			  (node-interval-set! this nenv fix ir)))))
		(else
		 ;; a non variable assinment
		 (multiple-value-bind (intv nenv)
		    (range rhs env fix)
		    (node-interval-set! this nenv fix intv)))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    range ::J2SPostfix ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SPostfix env::pair-nil fix::struct)
   (with-access::J2SPostfix this (lhs %info)
      (with-access::J2SExpr lhs ((linfo %info))
	 (let ((intv linfo))
	    (multiple-value-bind (_ nenv)
	       (call-next-method)
	       (return intv nenv))))))

;*---------------------------------------------------------------------*/
;*    range ::J2SPrefix ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SPrefix env::pair-nil fix::struct)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    range ::J2SCond ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SCond env::pair-nil fix::struct)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (_ env)
	 (range test env fix)
	 (multiple-value-bind (envt envo)
	    (test-envs test env fix)
	    (multiple-value-bind (intvt envt)
	       (range then (append-env envt env) fix)
	       (multiple-value-bind (intvo envo)
		  (range else (append-env envo env) fix)
		  (return (interval-merge intvt intvo)
		     (env-merge envt envo))))))))

;*---------------------------------------------------------------------*/
;*    range ::J2SFun ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SFun env::pair-nil fix::struct)
   (with-access::J2SFun this (body params rtype)
      (let ((envp (filter-map (lambda (p)
				 (with-access::J2SDecl p (itype %info)
				    (when (and (type-number? itype)
					       (interval? %info))
				       (cons p %info))))
		     params)))
	 (range body envp fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    range ::J2SCall ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SCall env::pair-nil fix::struct)
   
   (define (range-fun callee args env)
      (with-access::J2SFun callee (rtype params)
	 (let loop ((params params)
		    (args args))
	    (when (and (pair? params) (pair? args))
	       (with-access::J2SDecl (car params) (itype %info)
		  (when (type-number? itype)
		     (with-access::J2SExpr (car args) ((ainfo %info))
			(let ((ni (interval-merge %info ainfo)))
			   (unless (equal? ni %info)
			      (unfix! fix "j2scall")
			      (set! %info ni)))))
		  (loop (cdr params) (cdr args)))))))

   (call-default-walker)
   
   (with-access::J2SCall this ((callee fun) args)
      (cond
	 ((isa? callee J2SFun)
	  (range-fun callee args env))
	 ((isa? callee J2SRef)
	  (with-access::J2SRef callee (decl)
	     (cond
		((isa? decl J2SDeclFunCnst)
		 ;;(typing callee env fun fix)
		 (with-access::J2SDeclFunCnst decl (val)
		    (range-fun val args env)))
		((isa? decl J2SDeclFun)
		 ;;(typing callee env fun fix)
		 (with-access::J2SDeclFun decl (ronly val)
		    (when ronly
		       (range-fun val args env))))))))))

;*---------------------------------------------------------------------*/
;*    range ::J2SAccess ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SAccess env::pair-nil fix::struct)
   (with-access::J2SAccess this (obj field type)
      (if (type-number? type)
	  (with-access::J2SExpr obj (type)
	     (if (and (memq type '(string array)) (j2s-field-length? field))
		 (node-interval-set! this env fix (interval 0.0 *max-length*))
		 (return #f env)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    range ::J2SNop ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SNop env::pair-nil fix::struct)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    range ::J2SDeclInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SDeclInit env::pair-nil fix::struct)
   (with-access::J2SDeclInit this (val itype %info)
      (if (type-number? itype)
	  (multiple-value-bind (intv env)
	     (range val env fix)
	     (node-interval-set! this env fix intv)
	     (return #f (extend-env env this intv)))
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    range ::J2SStmtExpr ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SStmtExpr env::pair-nil fix::struct)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (intv env)
	 (range expr env fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    range ::J2SSeq ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SSeq env::pair-nil fix::struct)
   (with-access::J2SSeq this (nodes)
      (range-seq nodes env fix)))

;*---------------------------------------------------------------------*/
;*    range ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SLetBlock env::pair-nil fix::struct)
   (with-access::J2SLetBlock this (decls nodes)
      (multiple-value-bind (_ denv)
	 (range* decls env fix)
	 (multiple-value-bind (_ benv)
	    (range-seq nodes denv fix)
	    (let ((nenv (filter (lambda (d) (not (memq (car d) decls))) benv)))
	       (return #f nenv))))))

;*---------------------------------------------------------------------*/
;*    range ::J2SIf ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SIf env::pair-nil fix::struct)
   (with-access::J2SIf this (test then else)
      (multiple-value-bind (_ env)
	 (range test env fix)
	 (multiple-value-bind (envt envo)
	    (test-envs test env fix)
	    (multiple-value-bind (_ nenvt)
	       (range then (append-env envt env) fix)
	       (multiple-value-bind (_ nenvo)
		  (range else (append-env envo env) fix)
		  (return #f (env-merge nenvt nenvo))))))))
   
;*---------------------------------------------------------------------*/
;*    range ::J2SFor ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SFor env::pair-nil fix::struct)
   (with-access::J2SFor this (init test incr body)
      (let ((denv (dump-env env))
	    (ffix (fix-stamp fix)))
	 (when (pair? denv)
	    (tprint ">>> for [" ffix "] test=" (j2s->list test))
	    (tprint ">>> env=" denv))
	 (multiple-value-bind (initi inite)
	    (range init env fix)
	    (let loop ((env inite))
	       (let ((ostamp (fix-stamp fix)))
		  (when (pair? denv)
		     (tprint "--- for [" ffix "] / " ostamp))
		  (multiple-value-bind (testi teste)
		     (range test env fix)
		     (when (pair? denv)
			(tprint "    [" ffix "] test=" (j2s->list test))
			(tprint "    [" ffix "] teste=" (dump-env teste)))
		     (multiple-value-bind (testet testef)
			(test-envs test env fix)
			(when (pair? denv)
			   (when (pair? (dump-env testet))
			      (tprint "    [" ffix "] testet="
				 (dump-env testet))
			      (tprint "    [" ffix "] testef="
				 (dump-env testef))))
			(multiple-value-bind (bodyi bodye)
			   (range-seq (list body incr) (append-env testet teste) fix)
			   (if (=fx ostamp (fix-stamp fix))
			       (begin
				  (when (pair? denv)
				     (tprint "<<< for [" ffix "] "
					(dump-env (append-env testef bodye)))
				     (return #f (append-env testef bodye))))
			       (loop (env-merge bodye env))))))))))))

;*---------------------------------------------------------------------*/
;*    range ::J2SDo ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SDo env::pair-nil fix::struct)
   (with-access::J2SDo this (body test)
      (let ((denv (dump-env env))
	    (ffix (fix-stamp fix)))
	 (multiple-value-bind (bodyi bodye)
	    (range body env fix)
	    (let loop ((env env))
	       (let ((ostamp (fix-stamp fix)))
		  (multiple-value-bind (testi teste)
		     (range test bodye fix)
		     (multiple-value-bind (testet testef)
			(test-envs test bodye fix)
			(multiple-value-bind (bodyi bodye)
			   (range body teste fix)
			   (if (=fx ostamp (fix-stamp fix))
			       (return #f (append-env testef bodye))
			       (loop (env-merge bodye env))))))))))))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SNode fixnum::struct index::struct)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SDecl fixnum::struct index::struct)
   (call-default-walker)
   (with-access::J2SDecl this (%info itype)
      (when (interval? %info)
	 (cond
	    ((interval-in? %info index) (set! itype 'index))
	    ((interval-in? %info fixnum) (set! itype 'fixnum)))))
   this)

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SExpr fixnum::struct index::struct)
   (call-default-walker)
   (with-access::J2SExpr this (%info type)
      (when (interval? %info)
	 (cond
	    ((interval-in? %info index) (set! type 'index))
	    ((interval-in? %info fixnum) (set! type 'fixnum)))))
   this)


