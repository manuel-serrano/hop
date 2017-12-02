;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/range.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Sat Dec  2 11:41:10 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Integer Range analysis (fixnum detection)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_range

   (include "ast.sch")
   
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
;*    -------------------------------------------------------------    */
;*    Use as:                                                          */
;*      HOPTRACE="j2s:info j2s:key j2s:dump 25" hopc -Ox foo.js -g     */
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
      (when (config-get args :optim-cast #f)
	 ;; compute the integer value ranges, same condition as the CAST stage
	 (j2s-range-program! this args)
	 ;; allocate precise types according to the ranges
	 (when (config-get args :optim-range #f)
	    (j2s-range-type-program! this args))
	 ;; optimize operators according to ranges
	 (when (>=fx (config-get args :optim 0) 2)
	    (j2s-range-opt-program! this args)))
      this))

;*---------------------------------------------------------------------*/
;*    debug control                                                    */
;*---------------------------------------------------------------------*/
(define *dump-stop* -1)
(define *dump-env* '())
(define *dump-unfix* #f)

;*---------------------------------------------------------------------*/
;*    interval constructor ...                                         */
;*---------------------------------------------------------------------*/
(define-expander interval
   (lambda (x e)
      (match-case x
	 ((?- ?min ?max)
	  `(let ((%min ,(e min e))
		 (%max ,(e max e)))
	      ,(e `(cond
		       ((fixnum? %min) (set! %min (fixnum->llong %min)))
		       ((bignum? %min) (set! %min *-inf.0* )))
		  e)
	      ,(e `(cond
		     ((fixnum? %max) (set! %max (fixnum->llong %max)))
		     ((bignum? %max) (set! %max *+inf.0*)))
		  e)
	      (interval %min %max)))
	 (else
	  (error "interval" "wrong syntax" x)))))

;*---------------------------------------------------------------------*/
;*    interval-ok? ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-ok? i)
   (<= (interval-min i) (interval-max i)))

;*---------------------------------------------------------------------*/
;*    fix ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct fix stamp wstamp)

;*---------------------------------------------------------------------*/
;*    exptllong ...                                                    */
;*---------------------------------------------------------------------*/
(define (exptllong n exp::long)
   (if (=llong n #l2)
       (bit-lshllong #l1 exp)
       (error "exptllong" "wrong number" n)))

;*---------------------------------------------------------------------*/
;*    j2s-range-program! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-range-program! this::J2SProgram args)
   (set! *uint29-intv* (interval #l0 *max-uint29*))
   (set! *index-intv* (interval #l0 *max-index*))
   (set! *indexof-intv* (interval #l-1 *max-index*))
   (set! *length-intv* (interval #l0 *max-length*))
   (set! *int30-intv* (interval *min-int30* *max-int30*))
   (set! *int32-intv* (interval *min-int32* *max-int32*))
   (set! *int53-intv* (interval *min-int53* *max-int53*))
   (set! *integer* (interval *min-integer* *max-integer*))
   (set! *infinity-intv* (interval *-inf.0* *+inf.0*))
   (when (>=fx (config-get args :verbose 0) 4)
      (display " " (current-error-port)))
   (with-access::J2SProgram this (decls nodes)
      (let ((fix (fix 0 0)))
	 (let loop ((i 1))
	    (when (>=fx (config-get args :verbose 0) 4)
	       (fprintf (current-error-port) "~a." i)
	       (flush-output-port (current-error-port)))
	    (let ((ostamp (fix-stamp fix)))
	       (when (>=fx *dump-stop* 0)
		  (tprint "================================= " ostamp))
	       (let ((env (empty-env)))
		  (multiple-value-bind (_ env)
		     (node-range* decls env #f fix)
		     (node-range* nodes env #f fix)))
	       (unless (or (=fx (fix-stamp fix) ostamp)
			   (and (>fx *dump-stop* 0)
				(>=fx (fix-stamp fix) *dump-stop*)))
		  (fix-wstamp-set! fix (+fx 1 (fix-wstamp fix)))
		  (loop (+fx i 1)))))
	 this)))

;*---------------------------------------------------------------------*/
;*    j2s-range-type-program! ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-range-type-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (n) (type-range! n)) decls)
      (for-each (lambda (n) (type-range! n)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix::struct reason)
   (when *dump-unfix*
      (tprint "--- UNFIX reason=" reason))
   (fix-stamp-set! fix (+fx 1 (fix-stamp fix))))

;*---------------------------------------------------------------------*/
;*    integer bounds                                                   */
;*---------------------------------------------------------------------*/
(define *max-length* (-llong (exptllong #l2 32) #l1))
(define *max-index* (-llong *max-length* #l1))
(define *max-uint29* (-llong (exptllong #l2 29) #l1))
(define *max-int30* (-llong (exptllong #l2 29) #l1))
(define *min-int30* (negllong (exptllong #l2 29)))
(define *max-int32* (-llong (exptllong #l2 31) #l1))
(define *min-int32* (negllong (exptllong #l2 31)))
(define *max-int53* (exptllong #l2 53))
(define *min-int53* (negllong (exptllong #l2 53)))
(define *max-integer* (exptllong #l2 53))
(define *min-integer* (negllong (exptllong #l2 53)))

(define *uint29-intv* #f)
(define *index-intv* #f)
(define *indexof-intv* #f)
(define *length-intv* #f)
(define *int30-intv* #f)
(define *int32-intv* #f)
(define *int53-intv* #f)
(define *integer* #f)
(define *infinity-intv* #f)

(define *+inf.0* (exptllong #l2 54))
(define *-inf.0* (negllong (exptllong #l2 54)))

;*---------------------------------------------------------------------*/
;*    string-method-range ...                                          */
;*---------------------------------------------------------------------*/
(define (string-method-range name)
   (case (string->symbol name)
      ((charCodeAt) *uint29-intv*)
      ((indexOf lastIndexOf) *indexof-intv*)
      ((naturalCompare) *int30-intv*)
      ((localeCompare) *int30-intv*)))

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
	      (interval ra ra))
	  left)))

(define (interval-lt left right)
   (interval-lts left right 1))

(define (interval-lte left right)
   (interval-lts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-gts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-gts left::struct right::struct shift::int)
   (let ((ri (+ (interval-min right) shift)))
      (if (> ri (interval-min left))
	  (if (<= ri (interval-max left))
	      (interval ri (max (interval-max left) ri))
	      (interval ri ri))
	  left)))

(define (interval-gt left right)
   (interval-gts left right 1))

(define (interval-gte left right)
   (interval-gts left right 0))

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
;* (define-macro (interval-neq-test)                                   */
;*    (when (>= (bigloo-debug) 0)                                      */
;*       `(let ((L (interval 10 20)))                                  */
;* 	  ;; case 1                                                    */
;* 	  [assert () (interval-equal? (interval-neq L (interval 0 0)) L)] */
;* 	  [assert () (interval-equal? (interval-neq L (interval 0 1)) L)] */
;* 	  ;; case 2                                                    */
;* 	  [assert () (interval-equal? (interval-neq L (interval 0 10)) (interval 11 20))] */
;* 	  [assert () (interval-equal? (interval-neq L (interval 0 11)) (interval 12 20))] */
;* 	  [assert () (interval-equal? (interval-neq L (interval 10 11)) (interval 12 20))] */
;* 	  [assert () (interval-equal? (interval-neq L (interval 10 12)) (interval 13 20))] */
;* 	  [assert () (interval-equal? (interval-neq L (interval 10 10)) (interval 11 20))] */
;* 	  ;; case 3                                                    */
;* 	  [assert () (interval-equal? (interval-neq L (interval 15 15)) L)] */
;* 	  [assert () (interval-equal? (interval-neq L (interval 15 16)) L)] */
;* 	  ;; case 4                                                    */
;* 	  [assert () (interval-equal? (interval-neq L (interval 15 20)) (interval 10 14))]  */
;* 	  [assert () (interval-equal? (interval-neq L (interval 20 30)) (interval 10 19))]  */
;* 	  [assert () (interval-equal? (interval-neq L (interval 15 22)) (interval 10 14))]  */
;* 	  [assert () (interval-equal? (interval-neq L (interval 20 25)) (interval 10 19))] */
;* 	  ;; case 5                                                    */
;* 	  [assert () (interval-equal? (interval-neq L (interval 21 22)) L)] */
;* 	  ;; case 6                                                    */
;* 	  [assert () (interval-equal? (interval-neq L L) (interval 0 0))]  */
;* 	  [assert () (interval-equal? (interval-neq L (interval 8 22)) (interval 0 0))] */
;* 	  #t)))                                                        */
;*                                                                     */
;* (interval-neq-test)                                                 */
;*                                                                     */
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

   (define (inrange val)
      (cond
	 ((>llong val *+inf.0*) *+inf.0*)
	 ((<llong val *-inf.0*) *-inf.0*)
	 (else val)))
   
   (let ((li (interval-min left))
	 (la (interval-max left))
	 (ri (interval-min right))
	 (ra (interval-max right))
	 (oi (inrange (interval-min o)))
	 (oa (inrange (interval-max o))))
      (cond
	 ((> la oa)
	  (cond
	     ((>= oi #l2)
	      (let ((min #l2))
		 (interval min (max oa min))))
	     ((>= oi #l1)
	      (let ((min #l1))
		 (interval min (max oa min))))
	     ((>= oi #l0)
	      (let ((min #l0))
		 (interval min (max oa min))))
	     ((> oi #l-1)
	      (let ((min #l-1))
		 (interval min (max oa min))))
	     ((> oi #l-2)
	      (let ((min #l-2))
		 (interval min (max oa min))))
	     ((> oi #l-10)
	      (let ((min #l-10))
		 (interval min (max oa min))))
	     ((> oi (- *max-length*))
	      (let ((min (- *max-length*)))
		 (interval min (max oa min))))
	     ((> oi *min-int30*)
	      (let ((min *min-int30*))
		 (interval min (max oa min))))
	     ((> oi *min-int32*)
	      (let ((min *min-int32*))
		 (interval min (max oa min))))
	     ((> oi *min-integer*)
	      (let ((min *min-integer*))
		 (interval min (max oa min))))
	     (else
	      (interval *-inf.0* oa))))
	 ((and (< la oa) (< oa *+inf.0*))
	  (cond
	     ((> ra 0)
	      (cond
		 ((< oa #l8192)
		  (let ((max #l8192))
		     (interval (min oi max) max)))
		 ((< oa *max-int30*)
		  (let ((max *max-int30*))
		     (interval (min oi max) max)))
		 ((< oa (- *max-index* #l10))
		  (let ((max (- *max-index* #l10)))
		     (interval (min oi max) max)))
		 ((<= oa *max-index*)
		  (let ((max *max-index*))
		     (interval (min oi max) max)))
		 ((<= oa *max-length*)
		  (let ((max *max-length*))
		     (interval (min oi max) max)))
		 ((<= oa *max-integer*)
		  (let ((max *max-integer*))
		     (interval (min oi max) max)))
		 ((<= oa *max-int32*)
		  (let ((max *max-int32*))
		     (interval (min oi max) max)))
		 (else
		  (interval oi *+inf.0*))))
	     (else
	      (interval *-inf.0* oa))))
	 ((> li oi)
	  (cond
	     ((>= oi #l2)
	      (let ((min #l2))
		 (interval min (max oa min))))
	     ((>= oi #l1)
	      (let ((min #l1))
		 (interval min (max oa min))))
	     ((>= oi #l0)
	      (let ((min #l0))
		 (interval min (max oa min))))
	     ((> oi #l-1)
	      (let ((min #l-1))
		 (interval min (max oa min))))
	     ((> oi #l-2)
	      (let ((min #l-2))
		 (interval min (max oa min))))
	     ((> oi #l-10)
	      (let ((min #l-10))
		 (interval min (max oa min))))
	     (else
	      (interval *-inf.0* oa))))
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
      (let* ((v0 (* (interval-min left) (interval-min right)))
	     (v1 (* (interval-min left) (interval-max right)))
	     (v2 (* (interval-max left) (interval-max right)))
	     (v3 (* (interval-max left) (interval-min right)))
	     (mi0 (min v0 v1))
	     (mi1 (min v2 v3))
	     (ma0 (max v0 v1))
	     (ma1 (max v2 v3))
	     (intr (interval (min mi0 mi1) (max ma0 ma1))))
	 (widening left right intr))))
   
;*---------------------------------------------------------------------*/
;*    interval-div ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-div left right)
   (when (and (interval? left) (interval? right))
      (unless (or (= (interval-max right) 0) (= (interval-min right) 0))
	 ;; MS: 22 May 2017, don't know how to ensure that the result is
	 ;; an integer
	 #f)))
;* 	 (let ((min (truncate (/ (interval-min left) (interval-max right)))) */
;* 	       (max (ceiling (/ (interval-max left) (interval-min right))))) */
;* 	    (when (and (integer? min) (integer? max))                  */
;* 	       (let ((intr (interval                                   */
;* 			      (fixnum->llong (inexact->exact min))     */
;* 			      (fixnum->llong (inexact->exact max)))))  */
;* 		  (widening left right intr)))))))                     */
   
;*---------------------------------------------------------------------*/
;*    interval-bitop ...                                               */
;*---------------------------------------------------------------------*/
(define (interval-bitop op::procedure left right)
   
   (define (compiler-high::long val)
      (cond-expand
	 ((or bit61 bit63) (bit-lsh val 32))
	 (else val)))

   (define (compiler-low::long val)
      (cond-expand
	 ((or bit61 bit63) (bit-rsh val 32))
	 (else val)))

   (define (bitop x y def)
      (compiler-low
	 (op (compiler-high x) (compiler-high y) (compiler-high def))))

   (define (int32 n)
      (cond
	 ((<llong n *min-int32*) *min-int32*)
	 ((>llong n *max-int32*) *max-int32*)
	 (else n)))
   
   (if (and (interval? left) (interval? right))
       (let ((u (int32
		   (max (interval-max left)
		      (bitop (interval-max left) (interval-max right)
			 *max-int32*))))
	     (l (int32
		   (min (interval-min left)
		      (bitop (interval-min left) (interval-min right)
			 *min-int32*)))))
	  (interval (min u l) (max u l)))
       *int32-intv*))

;*---------------------------------------------------------------------*/
;*    interval-shiftl ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-shiftl left right)
   
   (define (lsh n s def)
      (if (and (llong? s)
	       (=llong (elong->llong (llong->elong s)) s)
	       (<llong s #l32))
	  (elong->llong (bit-lshelong (llong->elong n) (llong->fixnum s)))
	  def))
   
   (interval-bitop lsh left right))
   
;*---------------------------------------------------------------------*/
;*    interval-shiftr ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-shiftr left right)

   (define (rsh n s def)
      (if (and (llong? s)
	       (=llong (elong->llong (llong->elong s)) s)
	       (<llong s #l33))
	  (elong->llong (bit-rshelong (llong->elong n) (llong->fixnum s)))
	  def))
   
   (interval-bitop rsh left right))
   
;*---------------------------------------------------------------------*/
;*    interval-ushiftr ...                                             */
;*---------------------------------------------------------------------*/
(define (interval-ushiftr left right)

   (define (ursh n s def)
      (if (and (llong? s)
	       (=llong (elong->llong (llong->elong s)) s)
	       (<llong s #l33))
	  (elong->llong (bit-urshelong (llong->elong n) (llong->fixnum s)))
	  def))
   
   (interval-bitop ursh left right))
   
;*---------------------------------------------------------------------*/
;*    interval-bitand ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-bitand left right)

   (define (shrink32 intv)
      (interval
	 (max (interval-min intv) *min-int32*)
	 (min (interval-max intv) *max-int32*)))
	 
   (when (and (interval? left) (interval? right))
      (if (or (> (interval-max left) 0) (> (interval-max right) 0))
	  (let* ((mi (min (interval-min left) (interval-min right)))
		 (ma (min (interval-max left) (interval-max right)))
		 (intr (interval mi ma)))
	     (shrink32 (widening left right intr)))
	  *int32-intv*)))
   
;*---------------------------------------------------------------------*/
;*    node-interval-set! ...                                           */
;*    -------------------------------------------------------------    */
;*    Set the expression interval and if needed update the fix stamp.  */
;*---------------------------------------------------------------------*/
(define (node-interval-set! this::J2SNode env::pair-nil fix::struct intv)
   
   (define (range-get this)
      (cond
	 ((isa? this J2SExpr)
	  (with-access::J2SExpr this (range) range))
	 ((isa? this J2SDecl)
	  (with-access::J2SDecl this (range) range))))

   (define (range-set! this r)
      (cond
	 ((isa? this J2SExpr)
	  (with-access::J2SExpr this (range) (set! range r)))
	 ((isa? this J2SDecl)
	  (with-access::J2SDecl this (range) (set! range r)))))

   (let ((range (range-get this)))
      (when (interval? intv)
	 (cond
	    ((not (interval? range))
	     (when (interval? intv)
		(unfix! fix
		   (format "node-interval-set.1! ~a -> ~a"
		      (j2s->list this) intv))
		(range-set! this intv)))
	    ((not (interval-in? intv range))
	     (unfix! fix
		(format "node-interval-set.2! ~a -> ~a/~a => ~a"
		   (j2s->list this) intv range (interval-merge intv range)))
	     (range-set! this (interval-merge intv range)))))
      (return (range-get this) env)))

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
;*    node-range* ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-range* nodes::pair-nil env::pair-nil fun fix::struct)
   (let loop ((nodes nodes)
	      (intv #f)
	      (env env))
      (if (null? nodes)
	  (return intv env)
	  (multiple-value-bind (int env)
	     (node-range (car nodes) env fun fix)
	     (loop (cdr nodes) int env)))))

;*---------------------------------------------------------------------*/
;*    node-range-seq ...                                               */
;*---------------------------------------------------------------------*/
(define (node-range-seq nodes::pair-nil env::pair-nil fun fix::struct)
   (let loop ((nodes nodes)
	      (rng *infinity-intv*)
	      (env env)
	      (envr #f))
      (if (null? nodes)
	  (return rng (or envr env))
	  (multiple-value-bind (ir envn)
	     (node-range (car nodes) env fun fix)
	     (loop (cdr nodes) rng envn envr)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNode env::pair-nil fun fix::struct)
   (error "range" "not implemented" (typeof this)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SMeta ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SMeta env::pair-nil fun fix::struct)
   (with-access::J2SMeta this (optim)
      (if (=fx optim 0)
	  (return #f env)
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SExpr env::pair-nil fun fix::struct)
   (multiple-value-bind (ints envs)
      (call-default-walker)
      (with-access::J2SExpr this (type)
	 (if (type-number? type)
	     (return ints envs)
	     (return #f envs)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SParen env::pair-nil fun fix::struct)
   (with-access::J2SParen this (expr)
      (node-range expr env fun fix)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDataPropertyInit env::pair-nil fun fix::struct)
   (with-access::J2SDataPropertyInit this (val)
      (node-range val env fun fix)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAccessorPropertyInit ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAccessorPropertyInit env::pair-nil fun fix::struct)
   (with-access::J2SDataPropertyInit this (get set)
      (call-default-walker)
      (return #f env)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SStmt ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SStmt env::pair-nil fun fix::struct)
   (unless (or (isa? this J2SDecl)
	       (isa? this J2SBreak)
	       (isa? this J2SLabel)
	       (isa? this J2SThrow))
      (tprint "not implemented " (typeof this)))
   (call-default-walker)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SReturn env::pair-nil fun fix::struct)
   (with-access::J2SReturn this (expr)
      (multiple-value-bind (intv env)
	 (node-range expr env fun fix)
	 (when (isa? fun J2SFun)
	    (node-interval-set! fun '() fix intv))
	 (return intv env))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDataPropertyInit env::pair-nil fun fix::struct)
   (with-access::J2SDataPropertyInit this (val)
      (multiple-value-bind (_ env)
	 (node-range val env fun fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNumber env::pair-nil fun fix::struct)

   (define (integer->llong num)
      (if (fixnum? num) (fixnum->llong num) (flonum->llong num)))
	  
   (with-access::J2SNumber this (val type)
      (if (and (type-number? type) (integer? val))
	  (let ((intv (interval (integer->llong val) (integer->llong val))))
	     (node-interval-set! this env fix intv))
	  (return #f env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SRef env::pair-nil fun fix::struct)
   (with-access::J2SRef this (decl type)
      (if (type-number? type)
	  (with-access::J2SDecl decl (range ronly)
	     (let ((intv (env-lookup env decl)))
		(when ronly
		   (with-access::J2SDecl decl (id range)
		      (when (and (interval? range) (not (interval? intv)))
			 (set! intv range))))
		(if intv
		    (node-interval-set! this env fix intv)
		    (return range env))))
	  (return #f env))))

;*---------------------------------------------------------------------*/
;*    test-envs ...                                                    */
;*    -------------------------------------------------------------    */
;*    Returns two new environments for the positive and negative       */
;*    values of the test.                                              */
;*---------------------------------------------------------------------*/
(define (test-envs test::J2SExpr env fun fix::struct)
   
   (define (test-envs-ref left::J2SRef right op)
      (multiple-value-bind (intl envl)
	 (node-range left env fun fix)
	 (multiple-value-bind (intr envr)
	    (node-range right envl fun fix)
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
		      ((== === eq?)
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
	 ((== === eq?) op)
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
		 (test-envs lhs env fun fix)
		 (multiple-value-bind (intl envl)
		    (node-range lhs env fun fix)
		    (multiple-value-bind (renvt renvo)
		       (test-envs rhs envl fun fix)
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
;*    node-range ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SUnary env::pair-nil fun fix::struct)
   (with-access::J2SUnary this (op expr type)
      (if (type-number? type)
	  (case op
	     ((+)
	      (node-range expr env fun fix))
	     ((-)
	      (multiple-value-bind (intv env)
		 (node-range expr env fun fix)
		 (cond
		    ((not (interval? intv))
		     (return *infinity-intv* env))
		    ((and (= (interval-max intv) 0) (= (interval-min intv) 0))
		     ;; -0 is the flonum -0.0
		     (set! type 'number)
		     (return #f env))
		    (else
		     (node-interval-set! this env fix
			(interval
			   (- (interval-max intv)) (- (interval-min intv))))))))
	     ((~)
	      (multiple-value-bind (intv env)
		 (node-range expr env fun fix)
		 (node-interval-set! this env fix *int32-intv*)))
	     (else
	      (call-default-walker)
	      (return *infinity-intv* env)))
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    range-binary ...                                                 */
;*---------------------------------------------------------------------*/
(define (node-range-binary this op lhs rhs env::pair-nil fun fix::struct)
   (with-access::J2SExpr this (range %%wstamp)
      (multiple-value-bind (intl envl)
	 (node-range lhs env fun fix)
	 (multiple-value-bind (intr envr)
	    (node-range rhs envl fun fix)
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
		      ((%)
		       (with-access::J2SExpr rhs (type)
			  (if (and (eq? type 'integer)
				   (> (interval-min intr) 0))
			      (node-interval-set! this env fix intl)
			      (return *infinity-intv* env))))
		      ((<<)
		       (node-interval-set! this env fix
			  (interval-shiftl intl intr)))
		      ((>>)
		       (node-interval-set! this env fix
			  (interval-shiftr intl intr)))
		      ((>>>)
		       (node-interval-set! this env fix
			  (interval-ushiftr intl intr)))
		      ((^ BIT_OR ~)
		       (node-interval-set! this env fix
			  *int32-intv*))
		      ((&)
		       (node-interval-set! this env fix
			  (interval-bitand intl intr)))
		      (else
		       (return *infinity-intv* env))))
		(return range env))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SBinary env::pair-nil fun fix::struct)
   (with-access::J2SBinary this (op lhs rhs type range)
      (if (type-number? type)
	  (node-range-binary this op lhs rhs env fun fix)
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAssig env::pair-nil fun fix::struct)
   (with-access::J2SAssig this (lhs rhs type)
      (if (type-number? type)
	  (multiple-value-bind (_ env)
	     (node-range lhs env fun fix)
	     (cond
		((isa? lhs J2SRef)
		 ;; a variable assignment
		 (multiple-value-bind (ir env)
		    (node-range rhs env fun fix)
		    (with-access::J2SRef lhs (decl)
		       (let ((nenv (extend-env env decl ir)))
			  (node-interval-set! this nenv fix ir)))))
		(else
		 ;; a non variable assignment
		 (multiple-value-bind (intv nenv)
		    (node-range rhs env fun fix)
		    (node-interval-set! this nenv fix intv)))))
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAssigOp env::pair-nil fun fix::struct)
   (with-access::J2SAssigOp this (op lhs rhs type)
      (if (type-number? type)
	  (multiple-value-bind (intv nenv)
	     (node-range-binary this op lhs rhs env fun fix)
	     (cond
		((isa? lhs J2SRef)
		 ;; a variable assignment
		 (with-access::J2SRef lhs (decl)
		    (let ((nenv (extend-env env decl intv)))
		       (node-interval-set! lhs nenv fix intv))))
		(else
		 ;; a non variable assignment
		 (node-interval-set! this nenv fix intv))))
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SPostfix ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SPostfix env::pair-nil fun fix::struct)
   (with-access::J2SPostfix this (lhs range)
      (with-access::J2SExpr lhs ((linfo range))
	 (let ((intv linfo))
	    (multiple-value-bind (_ nenv)
	       (call-next-method)
	       (return intv nenv))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SPrefix ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SPrefix env::pair-nil fun fix::struct)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCond env::pair-nil fun fix::struct)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (_ env)
	 (node-range test env fun fix)
	 (multiple-value-bind (envt envo)
	    (test-envs test env fun fix)
	    (multiple-value-bind (intvt envt)
	       (node-range then (append-env envt env) fun fix)
	       (multiple-value-bind (intvo envo)
		  (node-range else (append-env envo env) fun fix)
		  (return (interval-merge intvt intvo)
		     (env-merge envt envo))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SFun env::pair-nil fun fix::struct)
   (with-access::J2SFun this (body params rtype)
      (let ((envp (filter-map (lambda (p)
				 (with-access::J2SDecl p (itype range)
				    (when (and (type-number? itype)
					       (interval? range))
				       (cons p range))))
		     params)))
	 (multiple-value-bind (intv env)
	    (node-range body envp this fix)
	    (return #f env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCall env::pair-nil fun fix::struct)
   
   (define (node-range-fun callee args env)
      (with-access::J2SFun callee (rtype params range)
	 (let loop ((params params)
		    (args args))
	    (if (and (pair? params) (pair? args))
		(with-access::J2SDecl (car params) (itype range)
		   (when (type-number? itype)
		      (with-access::J2SExpr (car args) ((ainfo range))
			 (let ((ni (interval-merge range ainfo)))
			    (unless (equal? ni range)
			       (unfix! fix "j2scall")
			       (set! range ni)))))
		   (loop (cdr params) (cdr args)))
		(node-interval-set! this env fix range)))))
   
   (call-default-walker)
   
   (with-access::J2SCall this ((callee fun) args type)
      (cond
	 ((isa? callee J2SFun)
	  (node-range-fun callee args env))
	 ((isa? callee J2SRef)
	  (with-access::J2SRef callee (decl)
	     (cond
		((isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (ronly val id)
		    (if ronly
			(node-range-fun val args env)
			(return *infinity-intv* env))))
		(else
		 (return *infinity-intv* env)))))
	 ((and (isa? callee J2SAccess) (eq? type 'integer))
	  (with-access::J2SAccess callee (obj field)
	     (let ((fn (j2s-field-name field)))
		(if (string? fn)
		    (case (j2s-type obj)
		       ((string)
			(let ((intv (string-method-range fn)))
			   (with-access::J2SCall this (range)
			      (unless (equal? intv range)
				 (unfix! fix "j2scall")
				 (set! range intv)))
			   (return intv env)))
		       (else
			(return *infinity-intv* env)))
		    (return *infinity-intv* env)))))
	 (else
	  (return *infinity-intv* env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAccess env::pair-nil fun fix::struct)
   (with-access::J2SAccess this (obj field type)
      (if (type-number? type)
	  (with-access::J2SExpr obj (type)
	     (if (and (memq type '(string array)) (j2s-field-length? field))
		 (node-interval-set! this env fix (interval #l0 *max-length*))
		 (return #f env)))
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNop ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNop env::pair-nil fun fix::struct)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDeclInit env::pair-nil fun fix::struct)
   (with-access::J2SDeclInit this (val itype range)
      (if (type-number? itype)
	  (multiple-value-bind (intv env)
	     (node-range val env fun fix)
	     (node-interval-set! this env fix intv)
	     (return #f (extend-env env this intv)))
	  (begin
	     (call-default-walker)
	     (return #f env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SStmtExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SStmtExpr env::pair-nil fun fix::struct)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (intv env)
	 (node-range expr env fun fix)
	 (return intv env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SSeq ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SSeq env::pair-nil fun fix::struct)
   (with-access::J2SSeq this (nodes)
      (node-range-seq nodes env fun fix)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SLetBlock env::pair-nil fun fix::struct)
   (with-access::J2SLetBlock this (decls nodes)
      (multiple-value-bind (_ denv)
	 (node-range* decls env fun fix)
	 (multiple-value-bind (intv benv)
	    (node-range-seq nodes denv fun fix)
	    (let ((nenv (filter (lambda (d) (not (memq (car d) decls))) benv)))
	       (return intv nenv))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SBindExit env::pair-nil fun fix::struct)
   (with-access::J2SBindExit this (stmt)
      (node-range stmt env fun fix)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SIf ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SIf env::pair-nil fun fix::struct)
   (with-access::J2SIf this (test then else)
      (multiple-value-bind (_ env)
	 (node-range test env fun fix)
	 (multiple-value-bind (envt envo)
	    (test-envs test env fun fix)
	    (multiple-value-bind (_ nenvt)
	       (node-range then (append-env envt env) fun fix)
	       (multiple-value-bind (_ nenvo)
		  (node-range else (append-env envo env) fun fix)
		  (return #f (env-merge nenvt nenvo))))))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SFor ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SFor env::pair-nil fun fix::struct)
   (with-access::J2SFor this (init test incr body)
      (let ((denv (dump-env env))
	    (ffix (fix-stamp fix)))
	 (when (pair? denv)
	    (tprint ">>> for [" ffix "] test=" (j2s->list test))
	    (tprint ">>> env=" denv))
	 (multiple-value-bind (initi inite)
	    (node-range init env fun fix)
	    (let loop ((env inite))
	       (let ((ostamp (fix-stamp fix)))
		  (when (pair? denv)
		     (tprint "--- for [" ffix "] / " ostamp))
		  (multiple-value-bind (testi teste)
		     (node-range test env fun fix)
		     (when (pair? denv)
			(tprint "    [" ffix "] test=" (j2s->list test))
			(tprint "    [" ffix "] teste=" (dump-env teste)))
		     (multiple-value-bind (testet testef)
			(test-envs test env fun fix)
			(when (pair? denv)
			   (when (pair? (dump-env testet))
			      (tprint "    [" ffix "] testet="
				 (dump-env testet))
			      (tprint "    [" ffix "] testef="
				 (dump-env testef))))
			(multiple-value-bind (bodyi bodye)
			   (node-range-seq (list body incr)
			      (append-env testet teste) fun fix)
			   (if (=fx ostamp (fix-stamp fix))
			       (begin
				  (when (pair? denv)
				     (tprint "<<< for [" ffix "] "
					(dump-env (append-env testef bodye)))
				     (return #f (append-env testef bodye))))
			       (loop (env-merge bodye env))))))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDo ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDo env::pair-nil fun fix::struct)
   (with-access::J2SDo this (body test)
      (let ((denv (dump-env env))
	    (ffix (fix-stamp fix)))
	 (multiple-value-bind (bodyi bodye)
	    (node-range body env fun fix)
	    (let loop ((env env))
	       (let ((ostamp (fix-stamp fix)))
		  (multiple-value-bind (testi teste)
		     (node-range test bodye fun fix)
		     (multiple-value-bind (testet testef)
			(test-envs test bodye fun fix)
			(multiple-value-bind (bodyi bodye)
			   (node-range body teste fun fix)
			   (if (=fx ostamp (fix-stamp fix))
			       (return #f (append-env testef bodye))
			       (loop (env-merge bodye env))))))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SWhile ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SWhile env::pair-nil fun fix::struct)
   (with-access::J2SWhile this (test body)
      (let ((denv (dump-env env))
	    (ffix (fix-stamp fix)))
	 (when (pair? denv)
	    (tprint ">>> while [" ffix "] test=" (j2s->list test))
	    (tprint ">>> env=" denv))
	 (let loop ((env env))
	    (let ((ostamp (fix-stamp fix)))
	       (when (pair? denv)
		  (tprint "--- while [" ffix "] / " ostamp))
	       (multiple-value-bind (testi teste)
		  (node-range test env fun fix)
		  (when (pair? denv)
		     (tprint "    [" ffix "] test=" (j2s->list test))
		     (tprint "    [" ffix "] teste=" (dump-env teste)))
		  (multiple-value-bind (testet testef)
		     (test-envs test env fun fix)
		     (when (pair? denv)
			(when (pair? (dump-env testet))
			   (tprint "    [" ffix "] testet="
			      (dump-env testet))
			   (tprint "    [" ffix "] testef="
			      (dump-env testef))))
		     (multiple-value-bind (bodyi bodye)
			(node-range  body (append-env testet teste) fun fix)
			(if (=fx ostamp (fix-stamp fix))
			    (begin
			       (when (pair? denv)
				  (tprint "<<< while [" ffix "] "
				     (dump-env (append-env testef bodye)))
				  (return #f (append-env testef bodye))))
			    (loop (env-merge bodye env)))))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SSwitch env::pair-nil fun fix::struct)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (_ env)
	 (node-range key env fun fix)
	 (let loop ((cases cases)
		    (env env))
	    (if (null? cases)
		(return #f env)
		(multiple-value-bind (_ envc)
		   (node-range (car cases) env fun fix)
		   (loop (cdr cases) (env-merge envc env))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCase ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCase env::pair-nil fun fix::struct)
   (with-access::J2SCase this (expr body)
      (multiple-value-bind (_ enve)
	 (node-range expr env fun fix)
	 (node-range body (env-merge enve env) fun fix))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SClass ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SClass env::pair-nil fun fix::struct)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SClassElement ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SClassElement env::pair-nil fun fix::struct)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    interval->type ...                                               */
;*---------------------------------------------------------------------*/
(define (interval->type intv::struct)
   (cond
      ((interval-in? intv *uint29-intv*) 'uint29)
      ((interval-in? intv *int30-intv*) 'int30)
      ((interval-in? intv *index-intv*) 'index)
      ((interval-in? intv *length-intv*) 'length)
      ((interval-in? intv *int32-intv*) 'int32)
      ((interval-in? intv *int53-intv*) 'int53)
      ((interval-in? intv *integer*) 'integer)
      (else 'number)))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SDecl)
   (call-default-walker)
   (with-access::J2SDecl this (range itype vtype)
      (when (interval? range)
	 (let ((ty (interval->type range)))
	    (when ty
	       (set! itype ty)
	       (set! vtype ty)))))
   this)

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SExpr)
   (call-default-walker)
   (with-access::J2SExpr this (range type)
      (when (interval? range)
	 (set! type (interval->type range))))
   this)

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SFun)
   (call-default-walker)
   (with-access::J2SFun this (range rtype)
      (when (interval? range)
	 (set! rtype (interval->type range))))
   this)

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SParen ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SParen)
   (call-default-walker)
   (with-access::J2SParen this (expr type)
      (with-access::J2SExpr expr ((etype type))
	 (set! type etype)))
   this)

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SRef)
   (call-next-method)
   (with-access::J2SRef this (decl type)
      (with-access::J2SDecl decl (vtype)
	 (set! vtype (minimal-type vtype type))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-range-opt-program! ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-range-opt-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (n) (opt-range! n args)) decls)
      (for-each (lambda (n) (opt-range! n args)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    opt-range! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (opt-range! this::J2SNode args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    opt-range! ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (opt-range! this::J2SBinary args)

   (define (fixnum? e::J2SExpr)
      (with-access::J2SExpr e (type)
	 (type-fixnum? type)))
      
   (define (int53? e::J2SExpr)
      (with-access::J2SExpr e (type)
	 (type-int53? type)))
      
   (define (positive-fixnum? e::J2SExpr)
      (when (fixnum? e)
	 (with-access::J2SExpr e (range)
	    (and (interval? range) (> (interval-min range) 0)))))
   
   (define (m64? conf)
      (=fx (config-get conf :long-size 0) 64))

   (with-access::J2SBinary this (op lhs rhs)
      (case op
	 ((%)
	  (cond
	     ((and (fixnum? lhs) (positive-fixnum? rhs))
	      (set! op 'remainderfx))
	     ((and (int53? lhs) (positive-fixnum? rhs))
	      (if (m64? args)
		  (set! op 'remainderfx)
		  (set! op 'remainder)))))))
   
   (call-default-walker))
