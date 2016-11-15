;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/range.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Sun Nov 13 15:19:11 2016 (serrano)                */
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
   (when (isa? this J2SProgram)
      (when (>=fx (config-get args :optim 0) 4)
	 ;; compute the integer value ranges
	 (j2s-range-program! this args)
	 ;; allocate precise types according to the ranges
	 (when (>=fx (config-get args :optim 0) 5)
	    (j2s-range-type-program! this args)))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-range-program! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-range-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (let ((fix (make-cell 0)))
	 (let loop ((i 0))
	    (let ((ofix (cell-ref fix)))
	       (for-each (lambda (n) (range n (empty-env) fix)) decls)
	       (for-each (lambda (n) (range n (empty-env) fix)) nodes)
	       (if (=fx (cell-ref fix) ofix)
		   (when (>=fx (bigloo-debug) 4)
		      (fprintf (current-error-port) "~a." i)
		      (flush-output-port (current-error-port)))
		   (loop (+fx i 1)))))
	 this)))

;*---------------------------------------------------------------------*/
;*    j2s-range-type-program! ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-range-type-program! this::J2SProgram args)
   (let* ((lsize (config-get args :long-size (bigloo-config 'elong-size)))
	  (fixnum (interval (- (expt 2. (fixnum->flonum (-fx lsize 1))))
		     (-fl (expt 2. (fixnum->flonum (-fx lsize 1))) 1.0)))
	  (fxsize (-fx lsize 2))
	  (index (interval 0
		    (-fl (expt 2. (fixnum->flonum (min 32 fxsize))) 2.0))))
      (with-access::J2SProgram this (headers decls nodes)
	 (for-each (lambda (n) (type-range! n fixnum index)) decls)
	 (for-each (lambda (n) (type-range! n fixnum index)) nodes)
	 this)))

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
       env)
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
;*    interval ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct interval min max)

;*---------------------------------------------------------------------*/
;*    integer bounds                                                   */
;*---------------------------------------------------------------------*/
(define *length-int* (-fl (exptfl 2. 32.) 1.))
(define *min-int* (negfl (-fl (expt 2. 53.) 1.0)))
(define *max-int* (-fl (expt 2. 53.) 1.0))
(define *widening-threshold* 4)

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
(define (interval-lts left right shift)
   (if (not (interval? right))
       left
       (let ((max (- (interval-max right) shift)))
	  (interval (min (interval-min left) max) max))))

(define (interval-lt left right) (interval-lts left right 1))
(define (interval-lte left right) (interval-lts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-gts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-gts left right shift)
   (if (not (interval? right))
       left
       (let ((min (+ (interval-min right) shift)))
	  (interval min (max (interval-max left) min)))))

(define (interval-gt left right) (interval-gts left right 1))
(define (interval-gte left right) (interval-gts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-neq ...                                                 */
;*    -------------------------------------------------------------    */
;*    The intervals NEQ rules are:                                     */
;*                                                                     */
;*        L:  ||       [............]       ||                         */
;*                                                                     */
;*    1:  R:  || [...] |            |       ||   =>  L                 */
;*    2:  R:  ||     [...]          |       ||   =>  [Ra+1,La]         */
;*    3:  R:  ||       |    [...]   |       ||   =>  L                 */
;*    4:  R:  ||       |          [...]     ||   =>  [Li,Ri-1]         */
;*    5:  R:  ||       |            | [...] ||   =>  L                 */
;*    6:  R:  ||     [.|............|.]     ||   =>  [0,0]             */
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
	  [assert () (equal? (interval-neq L (interval 0 0)) L)]
	  [assert () (equal? (interval-neq L (interval 0 1)) L)]
	  ;; case 2
	  [assert () (equal? (interval-neq L (interval 0 10)) (interval 11 20))]
	  [assert () (equal? (interval-neq L (interval 0 11)) (interval 12 20))]
	  [assert () (equal? (interval-neq L (interval 10 11)) (interval 12 20))]
	  [assert () (equal? (interval-neq L (interval 10 12)) (interval 13 20))]
	  [assert () (equal? (interval-neq L (interval 10 10)) (interval 11 20))]
	  ;; case 3
	  [assert () (equal? (interval-neq L (interval 15 15)) L)]
	  [assert () (equal? (interval-neq L (interval 15 16)) L)]
	  ;; case 4
	  [assert () (equal? (interval-neq L (interval 15 20)) (interval 10 14))] 
	  [assert () (equal? (interval-neq L (interval 20 30)) (interval 10 19))] 
	  [assert () (equal? (interval-neq L (interval 15 22)) (interval 10 14))] 
	  [assert () (equal? (interval-neq L (interval 20 25)) (interval 10 19))]
	  ;; case 5
	  [assert () (equal? (interval-neq L (interval 21 22)) L)]
	  ;; case 6
	  [assert () (equal? (interval-neq L L) (interval 0 0))] 
	  [assert () (equal? (interval-neq L (interval 8 22)) (interval 0 0))]
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
   (let ((ri (interval-min right))
	 (ra (interval-max right))
	 (li (interval-min left))
	 (la (interval-max left))
	 (oi (interval-min o))
	 (oa (interval-max o)))
      (cond
	 ((> la oa)
	  (cond
	     ((and (> ra 0) (> oa ra))
	      (interval (min oi ra) ra))
	     ((> oa 0)
	      (interval (min oi 0) 0))
	     (else
	      (interval *min-int* oa))))
	 ((< la oa)
	  (cond
	     ((> ra 0)
	      (cond
		 ((< oa (- *length-int* ra))
		  (let ((max (- *length-int* ra)))
		     (interval (min oi max) max)))
		 ((< oa *length-int*)
		  (let ((max *length-int*))
		     (interval (min oi max) max)))
		 ((< oa (+ 1 *length-int*))
		  (let ((max (+ 1 *length-int*)))
		     (interval (min oi max) max)))
		 (else
		  (interval oi *max-int*))))
	     (else
	      (interval *min-int* oa))))
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
		     (* (interval-min left) (interval-max right))
		     (* (interval-max left) (interval-min right)))))
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
   
   (interval-binary lsh left right))
   
;*---------------------------------------------------------------------*/
;*    interval-shiftr ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-shiftr left right)
   
   (define (rsh n s)
      (llong->flonum (bit-rshllong (flonum->llong n) (fixnum->llong s))))
   
   (interval-binary rsh left right))
   
;*---------------------------------------------------------------------*/
;*    node-interval-set! ...                                           */
;*    -------------------------------------------------------------    */
;*    Set the expression interval and if needed update the fix stamp.  */
;*---------------------------------------------------------------------*/
(define (node-interval-set! this::J2SNode env::pair-nil fix::cell intv)
   (with-access::J2SNode this (%info)
      (cond
	 ((not (interval? %info))
	  (when (interval? intv)
	     (unfix! fix
		(format "node-interval-set! ~a -> ~a" (j2s->list this) intv))
	     (set! %info intv)))
	 ((not (interval-in? intv %info))
	  (unfix! fix
	     (format "node-interval-set! ~a -> ~a" (j2s->list this) intv))
	  (set! %info (interval-merge intv %info))))
      (return %info env)))

;*---------------------------------------------------------------------*/
;*    dump-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-env env)
   (map (lambda (e)
	   (cons (with-access::J2SDecl (car e) (id key)
		    (format "~a:~a" id key))
	      (cdr e)))
      env))

;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix reason)
   ;;(tprint "--- UNFIX reason=" reason)
   (cell-set! fix (+fx 1 (cell-ref fix))))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define (return intv::obj env::pair-nil)
   (values intv env))

;*---------------------------------------------------------------------*/
;*    range* ...                                                       */
;*---------------------------------------------------------------------*/
(define (range* nodes::pair-nil env::pair-nil fix::cell)
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
(define (range-seq nodes::pair-nil env::pair-nil fix::cell)
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
(define-walk-method (range this::J2SNode env::pair-nil fix::cell)
   (error "range" "not implemented" (typeof this)))

;*---------------------------------------------------------------------*/
;*    range ::J2SExpr ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SExpr env::pair-nil fix::cell)
   (call-default-walker)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    range ::J2SStmt ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SStmt env::pair-nil fix::cell)
   (unless (or (isa? this J2SDecl))
      (tprint "not implemented " (typeof this)))
   (call-default-walker)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    range ::J2SNumber ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SNumber env::pair-nil fix::cell)
   (with-access::J2SNumber this (val type)
      (if (eq? type 'integer)
	  (let ((intv (interval val val)))
	     (node-interval-set! this env fix intv))
	  (return #f env))))

;*---------------------------------------------------------------------*/
;*    range ::J2SRef ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SRef env::pair-nil fix::cell)
   (with-access::J2SRef this (decl type)
      (if (eq? type 'integer)
	  (with-access::J2SDecl decl (%info)
	     (let ((intv (env-lookup env decl)))
		(if intv
		    (node-interval-set! this env fix (interval-merge intv %info))
		    (return %info env))))
	  (return #f env))))

;*---------------------------------------------------------------------*/
;*    test-envs ...                                                    */
;*    -------------------------------------------------------------    */
;*    Returns two new environments for the positive and negative       */
;*    values of the test.                                              */
;*---------------------------------------------------------------------*/
(define (test-envs test::J2SExpr env fix::cell)
   
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
		       (values (make-env decl intr)
			  (make-env decl (interval-neq intl intr))))
		      ((!= !==)
		       (values (interval-neq intl intr)
			  (make-env decl intr)))
		      (else
		       (values (empty-env) (empty-env)))))))))
   
   (define (inv-op op)
      (case op
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 ((== ===) '!=)
	 ((!= !==) '==)
	 (else op)))

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
	     ((not (eq? (j2s-type lhs) 'integer))
	      (values (empty-env) (empty-env)))
	     ((not (eq? (j2s-type rhs) 'integer))
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
      (else
       (values (empty-env) (empty-env)))))

;*---------------------------------------------------------------------*/
;*    range ::J2SBinary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SBinary env::pair-nil fix::cell)
   (with-access::J2SBinary this (op lhs rhs type)
      (if (eq? type 'integer)
	  (multiple-value-bind (intl envl)
	     (range lhs env fix)
	     (multiple-value-bind (intr envr)
		(range rhs envl fix)
		(case op
		   ((+)
		    (node-interval-set! this env fix (interval-add intl intr)))
		   ((-)
		    (node-interval-set! this env fix (interval-sub intl intr)))
		   ((*)
		    (node-interval-set! this env fix (interval-mul intl intr)))
		   ((/)
		    (node-interval-set! this env fix (interval-div intl intr)))
		   ((<<)
		    (node-interval-set! this env fix (interval-shiftl intl intr)))
		   ((>>)
		    (node-interval-set! this env fix (interval-shiftr intl intr)))
		   (else
		    (return #f env)))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    range ::J2SAssig ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SAssig env::pair-nil fix::cell)
   (with-access::J2SAssig this (lhs rhs type)
      (if (eq? type 'integer)
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
(define-walk-method (range this::J2SPostfix env::pair-nil fix::cell)
   (with-access::J2SPostfix this (lhs type op)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    range ::J2SPrefix ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SPrefix env::pair-nil fix::cell)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    range ::J2SCond ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SCond env::pair-nil fix::cell)
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
(define-walk-method (range this::J2SFun env::pair-nil fix::cell)
   (with-access::J2SFun this (body params rtype)
      (range body (empty-env) fix)
      (return #f env)))

;*---------------------------------------------------------------------*/
;*    range ::J2SAccess ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SAccess env::pair-nil fix::cell)
   (with-access::J2SAccess this (obj field type)
      (if (eq? type 'integer)
	  (with-access::J2SExpr obj (type)
	     (if (and (memq type '(string array)) (j2s-field-length? field))
		 (node-interval-set! this env fix (interval 0.0 *length-int*))
		 (return #f env)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    range ::J2SNop ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SNop env::pair-nil fix::cell)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    range ::J2SDeclInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SDeclInit env::pair-nil fix::cell)
   (with-access::J2SDeclInit this (val itype %info)
      (if (eq? itype 'integer)
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
(define-walk-method (range this::J2SStmtExpr env::pair-nil fix::cell)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (intv env)
	 (range expr env fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    range ::J2SSeq ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SSeq env::pair-nil fix::cell)
   (with-access::J2SSeq this (nodes)
      (range-seq nodes env fix)))

;*---------------------------------------------------------------------*/
;*    range ::J2SReturn ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SReturn env::pair-nil fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    range ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SLetBlock env::pair-nil fix::cell)
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
(define-walk-method (range this::J2SIf env::pair-nil fix::cell)
   (with-access::J2SIf this (test then else)
      (multiple-value-bind (_ env)
	 (range test env fix)
	 (multiple-value-bind (envt envo)
	    (test-envs test env fix)
	    (if (not (list? envt)) (tprint "PAS GLOP envt=" envt))
	    (if (not (list? envo)) (tprint "PAS GLOP envo=" envo))
	    (multiple-value-bind (_ envt)
	       (range then (append-env envt env) fix)
	       (multiple-value-bind (_ envo)
		  (range else (append-env envo env) fix)
		  (return #f (env-merge envt envo))))))))
   
;*---------------------------------------------------------------------*/
;*    range ::J2SFor ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (range this::J2SFor env::pair-nil fix::cell)
   (with-access::J2SFor this (init test incr body)
;*       (tprint "--------------------- j2sfor env=" (dump-env env))   */
      (multiple-value-bind (initi inite)
	 (range init env fix)
;* 	 (tprint "inite=" (dump-env inite))                            */
	 (let loop ((env inite))
	    (let ((ofix (cell-ref fix)))
	       (multiple-value-bind (testi teste)
		  (range test env fix)
;* 		  (tprint "teste=" (dump-env teste))                   */
		  (multiple-value-bind (testet testeo)
		     (test-envs test inite fix)
;* 		     (tprint "testet=" (dump-env testet))              */
		     (multiple-value-bind (bodyi bodye)
			(range-seq (list body incr) (append-env testet teste) fix)
;* 			(tprint "bodye=" (dump-env bodye))             */
			(if (=fx ofix (cell-ref fix))
			    (return #f (append-env testet bodye))
			    (begin
;* 			       (tprint "merge=" (dump-env (env-merge bodye env))) */
			       (loop (env-merge bodye env))))))))))))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SNode fixnum::struct index::struct)
   (call-default-walker))

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

