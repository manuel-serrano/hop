;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/range.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Sat Jan  4 05:46:44 2020 (serrano)                */
;*    Copyright   :  2016-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Integer Range analysis (fixnum detection)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_range

   (include "ast.sch"
	    "usage.sch")
   
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
;*    debug control                                                    */
;*---------------------------------------------------------------------*/
(define *debug-env* (or (getenv "HOPTRACE") ""))
(define *debug-range* (string-contains *debug-env* "j2s:range"))
(define *debug-range-if* (string-contains *debug-env* "j2s:range-if"))
(define *debug-range-call* (string-contains *debug-env* "j2s:range-call"))
(define *debug-range-for* (string-contains *debug-env* "j2s:range-for"))
(define *debug-range-while* (string-contains *debug-env* "j2s:range-while"))
(define *debug-range-fix* (string-contains *debug-env* "j2s:range-fix"))
(define *debug-range-test* (string-contains *debug-env* "j2s:range-test"))
(define *debug-range-binary* (string-contains *debug-env* "j2s:range-binary"))
(define *debug-range-function* (string-contains *debug-env* "j2s:range-function"))
(define *debug-range-key* (string-contains *debug-env* "j2s:key-function"))

(define *indebug* #f)

(define *dump-stop* -1)
(define *dump-env* '())
(define *dump-unfix* #f)

;*---------------------------------------------------------------------*/
;*    debugging                                                        */
;*---------------------------------------------------------------------*/
(define-macro (with-debug pred lbl . args)
   (if (>=fx (bigloo-debug) 0)
       `(let* ((__thunk (lambda () ,(car (last-pair args))))
	       (__l ,lbl)
	       (__lbl (if (or (string? __l) (pair? __l))
			  __l
			  (j2s->list __l))))
	   (if ,pred
	       (begin
		  (range-debug 1 __lbl)
		  ,(when (pair? (cdr args))
		      `(debug ,pred ,@(reverse (cdr (reverse args)))))
		  (let ((__r (__thunk)))
		     (range-debug -1 "")
		     __r))
	       (__thunk)))
       `(begin ,@body)))

(define-macro (debug pred . args)
   (when (>=fx (bigloo-debug) 0)
      `(when ,pred (range-debug 0 "" ,@args))))

(define (range-debug shift lbl . args)
   (when *debug-range*
      (when (< shift 0)
	 (set! *range-debug-level* (+ *range-debug-level* shift)))
      (apply fprint (current-error-port)
	 (range-debug-format
	    (cond ((> shift 0) "> ") ((< shift 0) "< ") (else "- ")))
	 lbl
	 args)
      (when (> shift 0)
	 (set! *range-debug-level* (+ *range-debug-level* shift)))))

(define *range-debug-level* 0)

(define (range-debug-format mark)
   (case *range-debug-level*
      ((0) mark)
      ((1) (string-append " " mark))
      ((2) (string-append "  " mark))
      ((3) (string-append "   " mark))
      ((4) (string-append "    " mark))
      ((5) (string-append "     " mark))
      ((6) (string-append "      " mark))
      ((7) (string-append "       " mark))
      ((8) (string-append "        " mark))
      ((9) (string-append "        " mark))
      ((10) (string-append "          " mark))
      ((11) (string-append "           " mark))
      ((12) (string-append "            " mark))
      ((13) (string-append "             " mark))
      ((14) (string-append "              " mark))
      ((15) (string-append "               " mark))
      ((16) (string-append "                " mark))
      ((17) (string-append "                 " mark))
      ((18) (string-append "                  " mark))
      ((19) (string-append "                   " mark))
      ((20) (string-append "                    " mark))
      (else (string-append "                   ~" mark))))

;*---------------------------------------------------------------------*/
;*    j2s-range! ::obj ...                                             */
;*    -------------------------------------------------------------    */
;*    Use as:                                                          */
;*      HOPTRACE="j2s:key j2s:range-env[25 iii]" hopc -Ox foo.js       */
;*    NOTE!!! the range.scm file has be compiled in debug mode         */
;*---------------------------------------------------------------------*/
(define (j2s-range! this conf)
   ;; debug mode
   (let ((env *debug-env*))
      (when (string-contains env "j2s:range-env")
	 (let ((keys (pregexp-match "j2s:range-env\\[([^]]*)\\]" env)))
	    (when keys
	       (call-with-input-string (cadr keys)
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
      ;; init range intervals
      (j2s-range-init!)
      ;; mark local captured variables
      (program-capture! this)
      (let ((tymap (if (>=fx (config-get conf :int-size 0) 53)
		       typemap53 typemap32)))
	 (if (config-get conf :optim-range #f)
	     (begin
		;; compute the integer value ranges,
		(j2s-range-program! this conf)
		;; allocate precise types according to the ranges
		(if (config-get conf :optim-integer #f)
		    (begin
		       ;; allocate precise variable types
		       (type-range! this tymap)
		       (map-types this tymap))
		    (map-types this defmap)))
	     (map-types this defmap)))
      (j2s-call-hint! this #t conf)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-range-program! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-range-program! this::J2SProgram conf)
   (when (>=fx (config-get conf :verbose 0) 4)
      (display " " (current-error-port)))
   (with-access::J2SProgram this (decls nodes)
      (let ((fix (make-cell 0)))
	 (let loop ((i 1)
		    (mode 'slow))
	    (when (>=fx (config-get conf :verbose 0) 4)
	       (fprintf (current-error-port) "~a." i)
	       (flush-output-port (current-error-port)))
	    (let ((ostamp (cell-ref fix)))
	       (when (>=fx *dump-stop* 0)
		  (tprint "================================= " ostamp))
	       (let ((env (empty-env)))
		  (multiple-value-bind (_ env)
		     (node-range* decls env conf mode fix)
		     (node-range* nodes env conf mode fix)))
	       (cond
		  ((not (=fx (cell-ref fix) ostamp))
		   (loop (+fx i 1) mode))
		  ((eq? mode 'slow)
		   (reset-loop-range! this #f)
		   (loop (+fx i 1) 'fast))
		  ((force-int32 this)
		   (loop (+fx i 1) mode)))))
	 this)))

;*---------------------------------------------------------------------*/
;*    interval constructor ...                                         */
;*---------------------------------------------------------------------*/
(define-expander interval
   (lambda (x e)
      (match-case x
	 ((?- ?min ?max . ?type)
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
	      (interval %min %max ,(if (pair? type) (car type) ''integer))))
	 (else
	  (error "interval" "wrong syntax" x)))))

;*---------------------------------------------------------------------*/
;*    dump-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-env env . ids)
   (when *debug-range*
      (filter-map (lambda (e)
		     (with-access::J2SDecl (car e) (id key)
			(let ((keys (if (pair? ids) ids *dump-env*)))
			   (when (and (or (symbol? keys)
					  (memq id keys)
					  (memq key keys)
					  (null? keys))
				      (interval? (cdr e)))
			      (cons
				 (if *debug-range-key*
				     (format "~a:~a" id key)
				     id)
				 (j2s-dump-range (cdr e)))))))
	 env)))

;*---------------------------------------------------------------------*/
;*    exptllong ...                                                    */
;*---------------------------------------------------------------------*/
(define (exptllong n exp::long)
   (if (=llong n #l2)
       (bit-lshllong #l1 exp)
       (error "exptllong" "wrong number" n)))

;*---------------------------------------------------------------------*/
;*    integer bounds                                                   */
;*---------------------------------------------------------------------*/
(define *max-length* (-llong (exptllong #l2 32) #l1))
(define *max-index* (-llong *max-length* #l1))
(define *max-int30* (-llong (exptllong #l2 29) #l1))
(define *min-int30* (negllong (exptllong #l2 29)))
(define *max-uint30* (-llong (exptllong #l2 29) #l1))
(define *max-int32* (-llong (exptllong #l2 31) #l1))
(define *min-int32* (negllong (exptllong #l2 31)))
(define *max-uint32* (-llong (exptllong #l2 32) #l1))
(define *max-int53* (exptllong #l2 53))
(define *min-int53* (negllong (exptllong #l2 53)))
(define *max-integer* (exptllong #l2 53))
(define *min-integer* (negllong (exptllong #l2 53)))

(define *+inf.0* (maxvalllong))
(define *-inf.0* (minvalllong))

(define *index-intv* #f)
(define *index30-intv* #f)
(define *indexof-intv* #f)
(define *length-intv* #f)
(define *int30-intv* #f)
(define *int32-intv* #f)
(define *uint32-intv* #f)
(define *int53-intv* #f)
(define *integer* #f)

(define *real1-intv* #f)
(define *ureal1-intv* #f)
(define *real4-intv* #f)

(define *infinity-intv* #f)

;*---------------------------------------------------------------------*/
;*    j2s-range-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-range-init!)
   (set! *index-intv* (interval #l0 *max-index*))
   (set! *index30-intv* (interval #l0 *max-uint30*))
   (set! *indexof-intv* (interval #l-1 *max-index*))
   (set! *length-intv* (interval #l0 *max-length*))
   (set! *int30-intv* (interval *min-int30* *max-int30*))
   (set! *int32-intv* (interval *min-int32* *max-int32*))
   (set! *uint32-intv* (interval #l0 *max-uint32*))
   (set! *int53-intv* (interval *min-int53* *max-int53*))
   (set! *integer* (interval *min-integer* *max-integer*))

   (set! *real1-intv* (interval #l-1 #l1 'real))
   (set! *ureal1-intv* (interval #l0 #l1 'real))
   (set! *real4-intv* (interval #l-4 #l4 'real))
      
   (set! *infinity-intv* (interval *-inf.0* *+inf.0* 'real)))
   
;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix::cell reason)
   (tprint "--- UNFIX reason=" reason)
   (cell-set! fix (+fx 1 (cell-ref fix))))

(define-macro (unfix! fix reason)
   `(cell-set! ,fix (+fx 1 (cell-ref ,fix))))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define (return intv::obj env::pair-nil)
   (values intv env))

;*---------------------------------------------------------------------*/
;*    decl-vrange-add! ...                                             */
;*---------------------------------------------------------------------*/
(define (decl-vrange-add! decl::J2SDecl rng::obj fix::cell)
   (when rng
      (with-access::J2SDecl decl (vrange id loc)
	 (let ((nr (interval-merge vrange rng)))
	    (unless (equal? nr vrange)
	       (unfix! fix
		  (format "J2SDecl.add(~a, ~a) range=~a/~a" id loc vrange nr))
	       (set! vrange nr))))))

;*---------------------------------------------------------------------*/
;*    decl-irange-add! ...                                             */
;*---------------------------------------------------------------------*/
(define (decl-irange-add! decl::J2SDecl rng::obj fix::cell)
   (when rng
      (with-access::J2SDecl decl (irange id loc)
	 (let ((nr (interval-merge irange rng)))
	    (unless (equal? nr irange)
	       (unfix! fix
		  (format "J2SDecl.add(~a, ~a) range=~a/~a" id loc irange nr))
	       (set! irange nr))))))

;*---------------------------------------------------------------------*/
;*    expr-range-add! ...                                              */
;*---------------------------------------------------------------------*/
(define (expr-range-add! this::J2SExpr env::pair-nil fix::cell rng)
   (with-access::J2SExpr this (range loc)
      (if rng
	  (let ((nr (interval-merge range rng)))
	     (unless (equal? nr range)
		(unfix! fix
		   (format "J2SExpr.add(~a) range=~a/~a" loc range nr))
		(set! range nr))
	     (return nr env))
	  (return rng env))))

;*---------------------------------------------------------------------*/
;*    type->range ...                                                  */
;*    -------------------------------------------------------------    */
;*    Type names mapped to their speicifying JS intervals.             */
;*---------------------------------------------------------------------*/
(define (type->range type)
   (case type
      ((index) *index-intv*)
      ((indexof) *indexof-intv*)
      ((length) *length-intv*)
      ((integer) *int53-intv*)
      ((int32) *int32-intv*)
      ((uint32) *uint32-intv*)
      ((real1) *real1-intv*)
      ((ureal1) *ureal1-intv*)
      ((real4) *real4-intv*)
      (else *infinity-intv*)))

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
;*    env? ...                                                         */
;*---------------------------------------------------------------------*/
(define (env? o)
   ;; heuristic check (not very precise but should be enough)
   (or (null? o)
       (and (pair? o)
	    (isa? (caar o) J2SDecl)
	    (interval? (cdar o)))))

;*---------------------------------------------------------------------*/
;*    extend-env ...                                                   */
;*---------------------------------------------------------------------*/
(define (extend-env::pair-nil env::pair-nil decl::J2SDecl intv)
   (if *debug-range*
       (cons (cons decl intv)
	  (filter (lambda (c) (not (eq? (car c) decl))) env))
       (cons (cons decl intv) env)))

;*---------------------------------------------------------------------*/
;*    append-env ...                                                   */
;*---------------------------------------------------------------------*/
(define (append-env left right)
   (if (>=fx (bigloo-debug) 1)
       (append left (filter (lambda (c) (not (assq (car c) left))) right))
       (append left right)))

;*---------------------------------------------------------------------*/
;*    delete-env ...                                                   */
;*---------------------------------------------------------------------*/
(define (delete-env env::pair-nil decl::J2SDecl)
   (with-access::J2SDecl decl (scope range)
      (filter (lambda (c) (not (eq? (car c) decl))) env)))

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
;*    env-override ...                                                 */
;*---------------------------------------------------------------------*/
(define (env-override left::pair-nil right::pair-nil)
   (append right
      (filter (lambda (l)
		 (not (assq (car l) right)))
	 left)))

;*---------------------------------------------------------------------*/
;*    env-lookup ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-lookup::obj env::pair-nil decl::J2SDecl)
   (let ((c (assq decl env)))
      (if (pair? c)
	  (cdr c)
	  (with-access::J2SDecl decl (vrange) vrange))))

;*---------------------------------------------------------------------*/
;*    env-remove ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-remove env::pair-nil decls::pair-nil)
   (filter (lambda (l)
	      (not (memq (car l) decls)))
      env))
   
;*---------------------------------------------------------------------*/
;*    env-filter ...                                                   */
;*---------------------------------------------------------------------*/
(define (env-filter proc::procedure env::pair-nil)
   (filter (lambda (l) (proc (car l))) env))
   
;*---------------------------------------------------------------------*/
;*    env-nocapture ...                                                */
;*---------------------------------------------------------------------*/
(define (env-nocapture env::pair-nil)
   (map (lambda (c)
	   (with-access::J2SDecl (car c) (%info)
	      (if (eq? %info 'capture)
		  (cons (car c) 'any)
		  c)))
      env))
   
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
	  (max (interval-max left) (interval-max right))
	  (interval-merge-types left right)))))

;*---------------------------------------------------------------------*/
;*    interval-merge-types ...                                         */
;*---------------------------------------------------------------------*/
(define (interval-merge-types x y)
   (if (and (eq? (interval-type x) 'integer) (eq? (interval-type y) 'integer))
       'integer
       'real))

;*---------------------------------------------------------------------*/
;*    interval-lts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-lts left::struct right::struct shift::int)
   (let ((ra (- (interval-max right) shift)))
      (cond
	 ((< ra (interval-min left))
	  (interval (interval-min left) (interval-min left)
	     (interval-merge-types left right)))
	 ((>= ra (interval-max left))
	  left)
	 (else
	  (interval (interval-min left) ra
	     (interval-merge-types left right))))))

(define (interval-lt left right)
   (interval-lts left right 1))

(define (interval-lte left right)
   (interval-lts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-gts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-gts left::struct right::struct shift::int)
   (let ((ri (+ (interval-min right) shift)))
      (cond
	 ((> ri (interval-max right))
	  (interval (interval-max left) (interval-max left)
	     (interval-merge-types left right)))
	 ((<= ri (interval-min left))
	  left)
	 (else
	  (interval ri (interval-max left)
	     (interval-merge-types left right))))))

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

   (define (widen)
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
		((>= oi #l-1)
		 (let ((min #l-1))
		    (interval min (max oa min))))
		((>= oi #l-2)
		 (let ((min #l-2))
		    (interval min (max oa min))))
		((>= oi #l-10)
		 (let ((min #l-10))
		    (interval min (max oa min))))
		((>= oi (- *max-length*))
		 (let ((min (- *max-length*)))
		    (interval min (max oa min))))
		((>= oi *min-int30*)
		 (let ((min *min-int30*))
		    (interval min (max oa min))))
		((>= oi *min-int32*)
		 (let ((min *min-int32*))
		    (interval min (max oa min))))
		((>= oi *min-int53*)
		 (let ((min *min-integer*))
		    (interval min (max oa min))))
		((>= oi *min-integer*)
		 (let ((min *min-integer*))
		    (interval min (max oa min))))
		(else
		 (interval *-inf.0* oa))))
	    ((and (< la oa) (< oa *+inf.0*))
	     (cond
		((> ra 0)
		 (cond
		    ((<= oa #l8192)
		     (let ((max #l8192))
			(interval (min oi max) max)))
		    ((<= oa *max-int30*)
		     (let ((max *max-int30*))
			(interval (min oi max) max)))
		    ((<= oa (- *max-index* #l10))
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
		    ((<= oa *max-int53*)
		     (let ((max *max-int53*))
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
		((>= oi #l-1)
		 (let ((min #l-1))
		    (interval min (max oa min))))
		((>= oi #l-2)
		 (let ((min #l-2))
		    (interval min (max oa min))))
		((>= oi #l-10)
		 (let ((min #l-10))
		    (interval min (max oa min))))
		(else
		 (interval *-inf.0* oa))))
	    (else
	     o))))

   (let ((intv (widen)))
      (when (interval? intv)
	 (interval-type-set! intv (interval-type o)))
      intv))

;*---------------------------------------------------------------------*/
;*    interval-add ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-add left right conf)

   (define (+safe53 x y)
      (cond
	 ((and (=llong x #l1) (=llong y *max-int53*)) *max-int53*)
	 ((and (=llong y #l1) (=llong x *max-int53*)) *max-int53*)
	 (else (+llong x y))))
   
   (define (interval-add64 left right)
      (let ((intr (interval
		     (+safe53 (interval-min left) (interval-min right))
		     (+safe53 (interval-max left) (interval-max right))
		     (interval-merge-types left right))))
	 (widening left right intr)))
      
   (define (interval-add32 left right)
      (let ((intr (interval
		     (+ (interval-min left) (interval-min right))
		     (+ (interval-max left) (interval-max right))
		     (interval-merge-types left right))))
	 (widening left right intr)))
   
   (when (and (interval? left) (interval? right))
      (if (m64? conf)
	  (interval-add64 left right)
	  (interval-add32 left right))))

;*---------------------------------------------------------------------*/
;*    interval-sub ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-sub left right conf)
   
   (define (-safe53 x y)
      (cond
	 ((and (=llong x *min-int53*) (=llong y #l1)) *min-int53*)
	 (else (-llong x y))))
   
   (define (interval-sub64 left right)
      (when (and (interval? left) (interval? right))
	 (let ((intr (interval
			(-safe53 (interval-min left) (interval-max right))
			(-safe53 (interval-max left) (interval-min right))
			(interval-merge-types left right))))
	    (widening left right intr))))

   (define (interval-sub32 left right)
      (when (and (interval? left) (interval? right))
	 (let ((intr (interval
			(- (interval-min left) (interval-max right))
			(- (interval-max left) (interval-min right))
			(interval-merge-types left right))))
	    (widening left right intr))))
   
   (when (and (interval? left) (interval? right))
      (if (m64? conf)
	  (interval-sub64 left right)
	  (interval-sub32 left right))))
   
;*---------------------------------------------------------------------*/
;*    interval-mul ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-mul left right)
   (when (and (interval? left) (interval? right))
      (let* ((v0 (*llong (interval-min left) (interval-min right)))
	     (v1 (*llong (interval-min left) (interval-max right)))
	     (v2 (*llong (interval-max left) (interval-max right)))
	     (v3 (*llong (interval-max left) (interval-min right)))
	     (mi0 (min v0 v1))
	     (mi1 (min v2 v3))
	     (ma0 (max v0 v1))
	     (ma1 (max v2 v3))
	     (intr (interval
		      (min mi0 mi1 (interval-min left) (interval-min right))
		      (max ma0 ma1 (interval-max left) (interval-max right))
		      (interval-merge-types left right))))
	 (widening left right intr))))
   
;*---------------------------------------------------------------------*/
;*    interval-exptt ...                                               */
;*---------------------------------------------------------------------*/
(define (interval-expt left right)
   (when (and (interval? left) (interval? right))
      (let* ((i0 (expt (llong->flonum (interval-min left)) (llong->flonum (interval-min right))))
	     (i1 (expt (llong->flonum (interval-min left)) (llong->flonum (interval-max right))))
	     (a0 (expt (llong->flonum (interval-max left)) (llong->flonum (interval-max right))))
	     (a1 (expt (llong->flonum (interval-max left)) (llong->flonum (interval-min right))))
	     (i (min i0 i1))
	     (a (max a0 a1))
	     (intv (when (and (integer? i) (integer? a))
		      (interval
			 (flonum->llong (min i a))
			 (flonum->llong (max i a))))))
	 (if intv
	     (widening left right intv)
	     *infinity-intv*))))
   
;*---------------------------------------------------------------------*/
;*    interval-div ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-div left right)
   (when (and (interval? left) (interval? right))
      ;; MS: 22 May 2017, don't know how to ensure that the result is
      ;; an integer
      (with-handler
	 ;; ignore floating point exceptions
	 (lambda (e) #f)
	 (let ((min (/ (interval-min left) (interval-max right)))
	       (max (/ (interval-max left) (interval-min right))))
	    (when (and (integer? min) (integer? max))
	       (let ((intr (interval
			      (if (flonum? min)
				  (flonum->llong (truncate min))
				  min)
			      (if (flonum? max)
				  (flonum->llong (ceiling max))
				  max)
			      'real)))
		  (widening left right intr)))))))
   
;*---------------------------------------------------------------------*/
;*    interval-bitop ...                                               */
;*---------------------------------------------------------------------*/
(define (interval-bitop op::procedure left right)

   (define (minov u l)
      (if (or (>llong u *max-int32*) (>llong l *max-int32*))
	  *min-int32*
	  (max (min u l) *min-int32*)))

   (define (maxov u l)
      (if (or (<llong u *min-int32*) (<llong l *min-int32*))
	  *max-int32*
	  (min (max u l) *max-int32*)))
   
   (when (and (interval? left) (interval? right))
      (let ((u (max (interval-max left)
		  (op (interval-max left) (interval-max right)
		     *max-int32*)))
	    (l (min (interval-min left)
		  (op (interval-min left) (interval-min right)
		     *min-int32*))))
	 (interval (minov u l) (maxov u l)))))

;*---------------------------------------------------------------------*/
;*    interval-bitopu32 ...                                            */
;*---------------------------------------------------------------------*/
(define (interval-bitopu32 op::procedure left right)
   
   (define (minov u l)
      (if (or (>llong u *max-uint32*) (>llong l *max-uint32*))
	  #l0
	  (max (min u l) #l0)))

   (define (maxov u l)
      (if (or (<llong u #l0) (<llong l #l0))
	  *max-uint32*
	  (min (max u l) *max-uint32*)))
   
   (when (and (interval? left) (interval? right))
      (let ((u (max (interval-max left)
		  (op (interval-max left) (interval-max right)
		     *max-uint32*)))
	    (l (min (interval-min left)
		  (op (interval-min left) (interval-min right)
		     #l0))))
	 (interval (minov u l) (maxov u l)))))

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
      (if (and (llong? s) (<llong s #l33))
	  (uint32->llong (bit-urshu32 (llong->uint32 n) (llong->fixnum s)))
	  def))
   
   (interval-bitopu32 ursh left right))
   
;*---------------------------------------------------------------------*/
;*    interval-bitand ...                                              */
;*---------------------------------------------------------------------*/
(define (interval-bitand left right)
   
   (define (shrink32 intv)
      (interval
	 (max (interval-min intv) *min-int32*)
	 (min (interval-max intv) *max-int32*)))
   
   (when (and (interval? left) (interval? right))
      (if (or (>= (interval-max left) 0) (>= (interval-max right) 0))
	  (let* ((mi (min (interval-min left) (interval-min right)))
		 (ma (max (interval-max left) (interval-max right)))
		 (min (if (or (>= (interval-min left) 0)
			      (>= (interval-min right) 0))
			  0
			  mi))
		 (intr (interval min ma)))
	     (shrink32 intr))
	  (shrink32 (interval-merge left right)))))

;*---------------------------------------------------------------------*/
;*    interval-abs ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-abs intv)
   (when (interval? intv)
      (interval #l0
	 (maxllong
	    (absllong (interval-min intv))
	    (absllong (interval-max intv)))
	 (interval-type intv))))

;*---------------------------------------------------------------------*/
;*    node-range* ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-range* nodes::pair-nil env::pair-nil conf::pair-nil mode::symbol fix::cell)
   (let loop ((nodes nodes)
	      (intv #f)
	      (env env))
      (if (null? nodes)
	  (return intv env)
	  (multiple-value-bind (int env)
	     (node-range (car nodes) env conf mode fix)
	     (loop (cdr nodes) int env)))))

;*---------------------------------------------------------------------*/
;*    node-range-args ...                                              */
;*---------------------------------------------------------------------*/
(define (node-range-args nodes::pair-nil env::pair-nil conf::pair-nil mode::symbol fix::cell)
   (let loop ((nodes nodes)
	      (intvs '())
	      (env env))
      (if (null? nodes)
	  (return (reverse! intvs) env)
	  (multiple-value-bind (int env)
	     (node-range (car nodes) env conf mode fix)
	     (loop (cdr nodes) (cons int intvs) env)))))

;*---------------------------------------------------------------------*/
;*    node-range-seq ...                                               */
;*---------------------------------------------------------------------*/
(define (node-range-seq nodes::pair-nil env::pair-nil conf::pair-nil mode::symbol fix::cell)
   (let loop ((nodes nodes)
	      (rng *infinity-intv*)
	      (env env)
	      (envr #f))
      (if (null? nodes)
	  (return rng (or envr env))
	  (with-access::J2SNode (car nodes) (loc)
	     (multiple-value-bind (ir envn)
		(node-range (car nodes) env conf mode fix)
		(loop (cdr nodes) rng envn envr))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNode env::pair-nil conf::pair-nil mode::symbol fix::cell)
   (error "range" "not implemented" (typeof this)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SExpr env::pair-nil conf mode::symbol fix::cell)
   (call-default-walker)
   (expr-range-add! this env fix *infinity-intv*))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNumber ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNumber env::pair-nil conf mode::symbol fix::cell)

   (define (integer->llong num)
      (if (fixnum? num) (fixnum->llong num) (flonum->llong num)))

   (with-access::J2SNumber this (val type)
      (cond
	 ((and (type-number? type) (not (flonum? val)))
	  (let ((intv (interval (integer->llong val) (integer->llong val))))
	     (expr-range-add! this env fix intv)))
	 ((and (type-number? type) (real? val))
	  (let* ((x (flonum->llong (floor val)))
		 (y (flonum->llong (ceiling val)))
		 (intv (interval (minllong x y) (maxllong x y) 'real)))
	     (expr-range-add! this env fix intv)))
	 (else
	  (expr-range-add! this env fix *infinity-intv*)))))

;*---------------------------------------------------------------------*/
;*    typing ::J2STemplate ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2STemplate env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2STemplate this (exprs)
      (multiple-value-bind (intv env)
	 (node-range* exprs env conf mode fix)
	 (expr-range-add! this env fix *infinity-intv*))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SArray env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SArray this (exprs)
      (multiple-value-bind (intv env)
	 (node-range* exprs env conf mode fix)
	 (expr-range-add! this env fix *infinity-intv*))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SPragma env::pair-nil conf mode::symbol fix::cell)
   (expr-range-add! this env fix *infinity-intv*))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDataPropertyInit env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SDataPropertyInit this (val)
      (multiple-value-bind (intv env)
	 (node-range val env conf mode fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAccessorPropertyInit ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAccessorPropertyInit env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SDataPropertyInit this (get set)
      (call-default-walker)
      (return *infinity-intv* env)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SObjInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SObjInit env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SObjInit this (inits)
      (multiple-value-bind (intv env)
	 (node-range* inits env conf mode fix)
	 (expr-range-add! this env fix *infinity-intv*))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SParen ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SParen env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SParen this (expr range)
      (multiple-value-bind (intv env)
	 (node-range expr env conf mode fix)
	 (expr-range-add! this env fix intv))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SSequence ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SSequence env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SSequence this (exprs)
      (multiple-value-bind (intv env)
	 (node-range* exprs env conf mode fix)
	 (expr-range-add! this env fix intv))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SBindExit env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SBindExit this (stmt range)
      (multiple-value-bind (intv env)
	 (node-range stmt env conf mode fix)
	 (return range env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SHopRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SHopRef env::pair-nil conf mode::symbol fix::cell)
   (return *infinity-intv* env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SUnresolvedRef ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SUnresolvedRef env::pair-nil conf mode::symbol fix::cell)
   (return *infinity-intv* env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SRef env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (id key utype)
	 (let ((nenv env))
	    (when (and (isa? decl J2SDeclFun)
		       (or (not (constructor-only? decl))
			   (not (constructor-no-return? decl))))
	       (set! nenv (env-nocapture env))
	       (with-access::J2SDeclFun decl (val)
		  (if (isa? val J2SMethod)
		      (escape-method val fix)
		      (escape-fun val fix #t))))
	    (with-access::J2SDecl decl (range key id)
	       (let ((intv (env-lookup nenv decl)))
		  (expr-range-add! this nenv fix intv)))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDecl env::pair-nil conf mode::symbol fix::cell)
   (decl-vrange-add! this *infinity-intv* fix)
   (return *infinity-intv* env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDeclInit env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SDeclInit this (val range vtype id)
      (when *indebug*
	 (tprint "\\\\\\ J2SDeclInit.1 " id " env=" (dump-env env)))
      (multiple-value-bind (intv env)
	 (node-range val env conf mode fix)
	 (when *indebug*
	    (tprint "\\\\\\ J2SDeclInit.2 " id " env=" (dump-env env)))
	 (cond
	    ((decl-usage-has? this '(eval))
	     (decl-vrange-add! this *infinity-intv* fix)
	     (return #f (extend-env env this intv)))
	    ((not intv)
	     (return *infinity-intv* env))
	    (else
	     (decl-vrange-add! this intv fix)
	     (return *infinity-intv* (extend-env env this intv)))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDeclFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDeclFun env::pair-nil conf mode::symbol fix::cell)
   
   (define (node-range-ctor-only val)
      (with-access::J2SFun val (generator rrange thisp)
	 (when generator
	    (set! rrange *infinity-intv*))
	 (when (isa? thisp J2SDecl)
	    (decl-irange-add! thisp *infinity-intv* fix))))
   
   (with-access::J2SDeclFun this (val vrange)
      (decl-vrange-add! this *infinity-intv* fix)
      (cond
	 ((isa? this J2SDeclSvc)
	  ;; services are as escaping function, the arguments are "any"
	  (if (isa? val J2SFun)
	      (escape-fun val fix #t)
	      (escape-method val fix)))
	 ((constructor-only? this)
	  ;; a mere constructor
	  (if (isa? val J2SFun)
	      (node-range-ctor-only val)
	      (with-access::J2SMethod val (function method)
		 (node-range-ctor-only function)
		 (node-range-ctor-only method)))))
      (multiple-value-bind (intvf env _)
	 (if (isa? val J2SMethod)
	     (node-range val env conf mode fix)
	     (node-range-fun val (node-range-fun-decl val env conf mode fix) conf mode fix))
	 (return *infinity-intv* (extend-env env this intvf)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDeclClass ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDeclClass env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SDeclClass this (val)
      (decl-vrange-add! this *infinity-intv* fix)
      (multiple-value-bind (intf env)
	 (node-range val env conf mode fix)
	 (return *infinity-intv* env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAssig env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SAssig this (lhs rhs type loc)
      (multiple-value-bind (intv env)
	 (node-range lhs env conf mode fix)
	 (multiple-value-bind (intv env)
	    (node-range rhs env conf mode fix)
	    (cond
	       ((isa? lhs J2SRef)
		;; a variable assignment
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SDecl decl (writable id)
		      (cond
			 ((and (not writable) (not (isa? this J2SInit)))
			  (return intv env))
			 (intv
			  (decl-vrange-add! decl intv fix)
			  (let ((nenv (extend-env env decl intv)))
			     (expr-range-add! this nenv fix intv)))
			 (else
			  (return #f env))))))
	       (else
		;; a non variable assignment
		(if intv
		    (expr-range-add! this env fix intv)
		    (return #f env))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAssigOp env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SAssigOp this (op lhs rhs type)
      (multiple-value-bind (intv nenv)
	 (node-range-binary this op lhs rhs env conf mode fix)
	 (cond
	    ((isa? lhs J2SRef)
	     ;; a variable assignment
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (writable id)
		   (cond
		      ((not writable)
		       (return intv env))
		      (intv
		       (decl-vrange-add! decl intv fix)
		       (let ((nenv (extend-env env decl intv)))
			  (expr-range-add! this nenv fix intv)))
		      (else
		       (return #f env))))))
	    ((not intv)
	     (return #f env))
	    (else
	     ;; a non variable assignment
	     (expr-range-add! this env fix intv))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SPostfix ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SPostfix env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SPostfix this (lhs rhs range type)
      (with-access::J2SExpr lhs ((lrange range))
	 (let ((intv lrange))
	    (multiple-value-bind (_ nenv)
	       (call-next-method)
	       (expr-range-add! this nenv fix intv))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SPrefix ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SPrefix env::pair-nil conf mode::symbol fix::cell)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-range-fun ...                                               */
;*---------------------------------------------------------------------*/
(define (node-range-fun this::J2SFun env::pair-nil conf mode::symbol fix::cell)
   (with-debug *debug-range-function*
      (with-access::J2SFun this (name loc)
	 `(J2SFun ,name ,loc ...))
      "env=" (dump-env env)
      (with-access::J2SFun this (body %info vararg argumentsp params thisp)
	 (let ((envp (map (lambda (p::J2SDecl)
			     (with-access::J2SDecl p (irange)
				(cond
				   ((decl-usage-has? p '(rest))
				    (decl-vrange-add! p *infinity-intv* fix)
				    (cons p *infinity-intv*))
				   (vararg
				    (decl-vrange-add! p *infinity-intv* fix)
				    (cons p *infinity-intv*))
				   (else
				    (cons p irange)))))
			params)))
	    (debug *debug-range-function* "envp=" (dump-env envp))
	    ;; transfer all itypes to vranges
	    (for-each (lambda (decl)
			 (with-access::J2SDecl decl (irange)
			    (decl-vrange-add! decl irange fix)))
	       params)
	    (let ((fenv (append envp env)))
	       (debug *debug-range-function* "fenv=" (dump-env fenv))
	       (when thisp
		  (with-access::J2SDecl thisp (loc)
		     (decl-vrange-add! thisp *infinity-intv* fix)
		     (set! fenv (extend-env fenv thisp *infinity-intv*))))
	       (when argumentsp
		  (decl-irange-add! argumentsp *infinity-intv* fix)
		  (decl-vrange-add! argumentsp *infinity-intv* fix)
		  (set! fenv (extend-env fenv argumentsp *infinity-intv*)))
	       (debug *debug-range-function* "fenve=" (dump-env fenv))
	       (multiple-value-bind (_ envf _)
		  (node-range body fenv conf mode fix)
		  (set! %info
		     (env-filter
			(lambda (d) (not (decl-ronly? d)))
			(env-remove envf params)))
		  (debug *debug-range-function* "%info=" (dump-env envf))))
	    (expr-range-add! this env fix #f)))))

;*---------------------------------------------------------------------*/
;*    node-range-fun-decl ::J2SFun ...                                 */
;*---------------------------------------------------------------------*/
(define (node-range-fun-decl this::J2SFun env::pair-nil conf mode fix)
   (with-access::J2SFun this (body rtype decl)
      (when (isa? decl J2SDecl)
	 (decl-vrange-add! decl *infinity-intv* fix))
      (filter-map (lambda (c)
		     (let ((d (car c))
			   (t (cdr c)))
			(with-access::J2SDecl d (vrange)
			   (if (decl-ronly? d)
			       c
			       (cons d vrange)))))
	 env)))

;*---------------------------------------------------------------------*/
;*    escape-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (escape-fun val::J2SFun fix::cell met::bool)
   (with-access::J2SFun val (params rrange thisp)
      (when (and (not met) thisp)
	 (decl-vrange-add! thisp *infinity-intv* fix))
      (set! rrange *infinity-intv*)
      (for-each (lambda (p::J2SDecl)
		   (decl-irange-add! p *infinity-intv* fix))
	 params)))

;*---------------------------------------------------------------------*/
;*    escape-method ...                                                */
;*---------------------------------------------------------------------*/
(define (escape-method fun::J2SMethod fix)
   (with-access::J2SMethod fun (method function)
      (escape-fun function fix #f)
      (escape-fun method fix #t)))

;*---------------------------------------------------------------------*/
;*    node-range-function-or-method ...                                */
;*---------------------------------------------------------------------*/
(define (node-range-function-or-method this::J2SFun env::pair-nil conf mode::symbol fix::cell met::bool)
   (escape-fun this fix met)
   (node-range-fun this (node-range-fun-decl this (env-nocapture env) conf mode fix) conf mode fix))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SFun env::pair-nil conf mode::symbol fix::cell)
   (node-range-function-or-method this env conf mode fix #f))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SMethod ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SMethod env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SMethod this (function method)
      (node-range-function-or-method function env conf mode fix #f)
      (node-range-function-or-method method env conf mode fix #t)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCall env::pair-nil conf mode::symbol fix::cell)
   (with-debug *debug-range-call* this
      "env=" (dump-env env)
      (with-access::J2SCall this ((callee fun) thisarg args)
	 (multiple-value-bind (tty env bkt)
	    (if (pair? thisarg)
		(node-range (car thisarg) env conf mode fix)
		(values #f env))
	    (multiple-value-bind (rrange env)
	       (node-range-call callee args env conf mode fix)
	       (multiple-value-bind (resrange resenv)
		  (expr-range-add! this env fix rrange)
		  (debug *debug-range-call* "resrange=" resrange)
		  (debug *debug-range-call* "resenv=" (dump-env resenv))
		  (return resrange resenv)))))))

;*---------------------------------------------------------------------*/
;*    node-range-call ...                                              */
;*---------------------------------------------------------------------*/
(define (node-range-call callee args env conf mode fix)
   
   (define (unknown-call-env env)
      ;; compute a new node-range environment where all mutated globals
      ;; and all mutated captured locals are removed
      (filter (lambda (e)
		 (with-access::J2SDecl (car e) (scope %info id)
		    (or (decl-ronly? (car e))
			(and (eq? scope 'local) (eq? %info 'nocapture)))))
	 env))
   
   (define (range-known-call-args fun::J2SFun iargs env)
      (with-access::J2SFun fun (params vararg mode thisp mode)
	 (when thisp (decl-irange-add! thisp *infinity-intv* fix))
	 (let loop ((params params)
		    (iargs iargs))
	    (when (pair? params)
	       (with-access::J2SDecl (car params) (id)
		  (cond
		     (vararg
		      (decl-irange-add! (car params) *infinity-intv* fix))
		     ((and (null? (cdr params)) (decl-usage-has? (car params) '(rest)))
		      (decl-irange-add! (car params) *infinity-intv* fix))
		     ((null? iargs)
		      (decl-irange-add! (car params) *infinity-intv* fix)
		      (loop (cdr params) '()))
		     (else
		      (decl-irange-add! (car params) (car iargs) fix)
		      (debug *debug-range-call*
			 "range-known-call-arg " id "=" (car iargs))
		      (loop (cdr params) (cdr iargs)))))))))
   
   (define (range-inline-call fun::J2SFun iargs env)
      ;; type a direct function call: ((function (...) { ... })( ... ))
      ;; side effects are automatically handled when
      ;; node-range the function body
      (range-known-call-args fun iargs env)
      (multiple-value-bind (_ envf)
	 (node-range-fun callee env conf mode fix)
	 (with-access::J2SFun fun (rrange mode %info)
	    (let ((nenv (if (env? %info) (env-override envf %info) env)))
	       (return rrange nenv)))))
   
   (define (range-known-call ref::J2SRef fun::J2SFun iargs env)
      ;; type a known constant function call: F( ... )
      ;; the new node-range environment is a merge of env and the environment
      ;; produced by the function
      (with-access::J2SRef ref (decl)
	 (expr-range-add! ref env fix *infinity-intv*)
	 (range-known-call-args fun iargs env)
	 (with-access::J2SDecl decl (scope)
	    (with-access::J2SFun fun (rrange %info)
	       (let ((nenv (if (env? %info) (env-override env %info) env)))
		  (debug *debug-range-call*
		     "results=" rrange)
		  (debug *debug-range-call*
		     "range-known-call nenv=" (dump-env nenv))
		  (return rrange nenv))))))
   
   (define (range-ref-call callee iargs env)
      ;; call a JS variable, check is it a known function
      (with-access::J2SRef callee (decl)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDeclFun decl (val)
		(if (decl-ronly? decl)
		    (if (isa? val J2SMethod)
			(with-access::J2SMethod val (function method)
			   (range-known-call callee function iargs env)
			   (range-known-call callee method iargs env))
			(range-known-call callee val iargs env))
		    (range-unknown-call callee env))))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val)
		(cond
		   ((and (decl-ronly? decl) (isa? val J2SFun))
		    (range-known-call callee val iargs env))
		   ((and (decl-ronly? decl) (isa? val J2SMethod))
		    (with-access::J2SMethod val (function method)
		       (range-known-call callee function iargs env)
		       (range-known-call callee method iargs env)))
		   (else
		    (range-unknown-call callee env)))))
	    (else
	     (range-unknown-call callee env)))))
   
   (define (is-global? obj ident)
      (when (isa? obj J2SGlobalRef)
	 (with-access::J2SGlobalRef obj (id decl)
	    (when (eq? id ident)
	       (decl-ronly? decl)))))
   
   (define (range-method-call callee iargs env)
      ;; type a method call: O.m( ... )
      (multiple-value-bind (_ env)
	 (node-range callee env conf mode fix)
	 (with-access::J2SAccess callee (obj field)
	    (let* ((fn (j2s-field-name field))
		   (intv (cond
			    ((not (string? fn))
			     *infinity-intv*)
			    ((is-global? obj 'Math) 
			     (cond
				((and (string=? fn "abs") (pair? iargs))
				 (interval-abs (car iargs)))
				((and (member fn '("floor" "ceil" "round"))
				      (pair? iargs))
				 (car iargs))
				(else
				 (type->range
				    (car (find-builtin-method-type obj fn))))))
			    (else
			     (type->range
				(car (find-builtin-method-type obj fn)))))))
	       (if (eq? intv *infinity-intv*)
		   ;; the method is unknown, filter out the node-range env
		   (return intv (unknown-call-env env))
		   (return intv env))))))
   
   (define (range-hop-call callee args env)
      ;; type a hop (foreign function) call: H( ... )
      ;; hop calls have no effect on the node-range env
      (with-access::J2SHopRef callee (loc)
	 (return #f env)))
   
   (define (range-global-call callee args env)
      (node-range callee env conf mode fix)
      (range-unknown-call callee env))
   
   (define (range-unknown-call callee env)
      ;; type a unknown function call: expr( ... )
      ;; filter out the node-range env
      (multiple-value-bind (_ env)
	 (node-range callee env conf mode fix)
	 (return *infinity-intv* (unknown-call-env env))))

   (with-debug *debug-range-call* `(node-range-call ,(j2s->list callee) ...)
      "env=" (dump-env env)
      (multiple-value-bind (iargs env)
	 (node-range-args args env conf mode fix)
	 (debug *debug-range-call* "args-env=" (dump-env env))
	 (cond
	    ((isa? callee J2SFun) (range-inline-call callee iargs env))
	    ((isa? callee J2SRef) (range-ref-call callee iargs env))
	    ((isa? callee J2SHopRef) (range-hop-call callee args env))
	    ((isa? callee J2SAccess) (range-method-call callee iargs env))
	    ((isa? callee J2SGlobalRef) (range-global-call callee args env))
	    (else (range-unknown-call callee env))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCond env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SCond this (test then else)
      (multiple-value-bind (_ env)
	 (node-range test env conf mode fix)
	 (multiple-value-bind (envt envo)
	    (test-envs test env conf mode fix)
	    (multiple-value-bind (intvt envt)
	       (node-range then (append-env envt env) conf mode fix)
	       (multiple-value-bind (intvo envo)
		  (node-range else (append-env envo env) conf mode fix)
		  (let ((envc (env-merge envt envo)))
		     (expr-range-add! this envc fix
			(interval-merge intvt intvo)))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNew ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNew env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SNew this (clazz args loc)
      (when *indebug*
	 (tprint "... J2SCall.1 env=" (dump-env env)))
      (multiple-value-bind (_ env)
	 (node-range clazz env conf mode fix)
	 (multiple-value-bind (_ env)
	    (node-range-call clazz args env conf mode fix)
	    (when *indebug*
	       (tprint "... J2SCall.2 env=" (dump-env env)))
	    (expr-range-add! this env fix *infinity-intv*)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SUnary env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SUnary this (op expr type)
      (case op
	 ((+)
	  (multiple-value-bind (intv env)
	     (node-range expr env conf mode fix)
	     (expr-range-add! this env fix intv)))
	 ((-)
	  (multiple-value-bind (intv env)
	     (node-range expr env conf mode fix)
	     (cond
		((not (interval? intv))
		 (return intv env))
		((and (<= (interval-min intv) 0) (>= (interval-max intv) 0))
		 (expr-range-add! this env fix
		    (interval
		       (- (interval-max intv)) (- (interval-min intv))
		       'real)))
		(else
		 (expr-range-add! this env fix
		    (interval
		       (- (interval-max intv)) (- (interval-min intv))
		       (interval-type intv)))))))
	 ((~)
	  (multiple-value-bind (intv env)
	     (node-range expr env conf mode fix)
	     (expr-range-add! this env fix *int32-intv*)))
	 (else
	  (call-default-walker)
	  (expr-range-add! this env fix *infinity-intv*)))))

;*---------------------------------------------------------------------*/
;*    range-binary ...                                                 */
;*---------------------------------------------------------------------*/
(define (node-range-binary this op lhs rhs env::pair-nil conf mode::symbol fix::cell)
   (debug *debug-range-binary* "env=" (dump-env env))
   (multiple-value-bind (intl envl)
      (node-range lhs env conf mode fix)
      (debug *debug-range-binary* "envl=" (dump-env envl))
      (multiple-value-bind (intr envr)
	 (node-range rhs envl conf mode fix)
	 (debug *debug-range-binary* "envr=" (dump-env envr))
	 (case op
	    ((+)
	     (expr-range-add! this env fix (interval-add intl intr conf)))
	    ((-)
	     (expr-range-add! this env fix (interval-sub intl intr conf)))
	    ((*)
	     (expr-range-add! this env fix (interval-mul intl intr)))
	    ((**)
	     (expr-range-add! this env fix (interval-expt intl intr)))
	    ((/)
	     (expr-range-add! this env fix (interval-div intl intr)))
	    ((%)
	     (if (and (interval? intl) (interval? intr))
		 (if (and (> (interval-min intr) 0)
			  (>= (interval-min intl) 0))
		     ;; a negative divider may produce -0.0
		     (expr-range-add! this env fix intr)
		     (expr-range-add! this env fix *infinity-intv*))))
	    ((<<)
	     (expr-range-add! this env fix (interval-shiftl intl intr)))
	    ((>>)
	     (expr-range-add! this env fix (interval-shiftr intl intr)))
	    ((>>>)
	     (expr-range-add! this env fix (interval-ushiftr intl intr)))
	    ((^ BIT_OR)
	     (expr-range-add! this env fix *int32-intv*))
	    ((&)
	     (expr-range-add! this env fix (interval-bitand intl intr)))
	    (else
	     (return #unspecified env))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SBinary env::pair-nil conf mode::symbol fix::cell)
   (with-debug *debug-range-binary* this
      (with-access::J2SBinary this (op lhs rhs)
	 (multiple-value-bind (intv env)
	    (node-range-binary this op lhs rhs env conf mode fix)
	    (debug *debug-range-binary* "return intv=" intv)
	    (debug *debug-range-binary* "env=" (dump-env env))
	    (return intv env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SAccess env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SAccess this (obj field type)
      (multiple-value-bind (into envo)
	 (node-range obj env conf mode fix)
	 (multiple-value-bind (intff envf)
	    (node-range field envo conf mode fix)
	    (with-access::J2SExpr obj (type)
	       (if (and (memq type '(string array)) (j2s-field-length? field))
		   (expr-range-add! this envf fix *length-intv*)
		   (expr-range-add! this envf fix *infinity-intv*)))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SYield ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SYield env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SYield this (expr from loc)
      (multiple-value-bind (intv env)
	 (node-range expr env conf mode fix)
	 (return #f env))))
   
;*---------------------------------------------------------------------*/
;*    test-envs ...                                                    */
;*    -------------------------------------------------------------    */
;*    Returns two new environments for the positive and negative       */
;*    values of the test.                                              */
;*---------------------------------------------------------------------*/
(define (test-envs test::J2SExpr env conf::pair-nil mode::symbol fix::cell)
   
   (define (test-envs-ref decl::J2SDecl left right op)
      (multiple-value-bind (intl envl)
	 (node-range left env conf mode fix)
	 (multiple-value-bind (intr envr)
	    (node-range right envl conf mode fix)
	    (when *debug-range-test*
	       (tprint "test-env-ref " (j2s->list test)))
	    (if (or (not (interval? intl)) (not (interval? intr)))
		(values (empty-env) (empty-env))
		(case op
		   ((<)
		    (let ((intrt (interval-lt intl intr))
			  (intro (interval-gte intl intr)))
		       (when *debug-range-test*
			  (tprint "test-env-ref intrt=" intrt " intro=" intro))
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
		    (values (empty-env) (empty-env))))))))
   
   (define (inv-op op)
      (case op
	 ((<) '>=)
	 ((<=) '>)
	 ((>) '<=)
	 ((>=) '<)
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
   
   (define (is-fixnum test)
      ;; if test === (fixnum? (ref decl) ) return decl
      ;; see __js2scheme_ast
      (when (isa? test J2SCall)
	 (with-access::J2SCall test (fun args)
	    (when (isa? fun J2SHopRef)
	       (with-access::J2SHopRef fun (id)
		  (when (eq? id 'fixnum?)
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
		 (test-envs lhs env conf mode fix)
		 (multiple-value-bind (intl envl)
		    (node-range lhs env conf mode fix)
		    (multiple-value-bind (renvt renvo)
		       (test-envs rhs envl conf mode fix)
		       (values (append-env lenvt renvt)
			  (append-env lenvo renvo))))))
	     ((not (type-number? (j2s-vtype lhs)))
	      (values (empty-env) (empty-env)))
	     ((not (type-number? (j2s-vtype rhs)))
	      (values (empty-env) (empty-env)))
	     ((not (eq? (j2s-vtype test) 'bool))
	      (values (empty-env) (empty-env)))
	     ((isa? lhs J2SRef)
	      (with-access::J2SRef lhs (decl)
		 (if (isa? rhs J2SRef)
		     (with-access::J2SRef rhs ((rdecl decl))
			(multiple-value-bind (lenvt lenvo)
			   (test-envs-ref decl lhs rhs op)
			   (multiple-value-bind (renvt renvo)
			      (test-envs-ref rdecl rhs lhs (inv-op op))
			      (values (append-env lenvt renvt)
				 (append-env lenvo renvo)))))
		     (test-envs-ref decl lhs rhs op))))
	     ((isa? rhs J2SRef)
	      (with-access::J2SRef rhs (decl)
		 (test-envs-ref decl rhs lhs (inv-op op))))
	     ((isa? lhs J2SAssig)
	      (with-access::J2SAssig lhs ((left lhs))
		 (if (isa? left J2SRef)
		     (with-access::J2SRef left (decl)
			(if (isa? rhs J2SRef)
			    (with-access::J2SRef rhs ((rdecl decl))
			       (multiple-value-bind (lenvt lenvo)
				  (test-envs-ref decl lhs rhs op)
				  (multiple-value-bind (renvt renvo)
				     (test-envs-ref rdecl rhs lhs (inv-op op))
				     (values (append-env lenvt renvt)
					(append-env lenvo renvo)))))
			    (test-envs-ref decl lhs rhs op)))
		     (values (empty-env) (empty-env)))))
	     ((isa? rhs J2SAssig)
	      (with-access::J2SAssig rhs ((right rhs))
		 (if (isa? right J2SRef)
		     (with-access::J2SRef right (decl)
			(test-envs-ref decl rhs lhs (inv-op op)))
		     (values (empty-env) (empty-env)))))
	     (else
	      (values (empty-env) (empty-env))))))
      ((is-js-index test)
       =>
       (lambda (decl)
	  (values (make-env decl *index-intv*) (empty-env))))
      ((is-fixnum test)
       =>
       (lambda (decl)
	  (cond
	     ((>=fx (config-get conf :int-size 0) 53)
	      (values (make-env decl *int53-intv*) (empty-env)))
	     ((>=fx (config-get conf :int-size 0) 32)
	      (values (make-env decl *int32-intv*) (empty-env)))
	     (else
	      (values (make-env decl *int30-intv*) (empty-env))))))
      (else
       (values (empty-env) (empty-env)))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SStmt ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SStmt env::pair-nil conf mode::symbol fix::cell)
   (unless (or (isa? this J2SDecl)
	       (isa? this J2SBreak)
	       (isa? this J2SLabel)
	       (isa? this J2SThrow)
	       (isa? this J2SMeta))
      (tprint "not implemented " (typeof this)))
   (call-default-walker)
   (return *infinity-intv* env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SNop ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SNop env::pair-nil conf mode::symbol fix::cell)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SStmtExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SStmtExpr env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SStmtExpr this (expr)
      (multiple-value-bind (intv env)
	 (node-range expr env conf mode fix)
	 (return intv env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SSeq ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SSeq env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SSeq this (nodes)
      (node-range-seq nodes env conf mode fix)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SLetBlock env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SLetBlock this (decls nodes loc)
      (let ((ienv (filter-map (lambda (d::J2SDecl)
				 (with-access::J2SDecl d (vrange)
				    (when vrange
				       (cons d vrange))))
		     decls)))
	 (when *indebug*
	    (tprint "/// J2SLetBlock.1 env=" (dump-env env)))
	 (multiple-value-bind (_ denv)
	    (node-range-seq decls (append ienv env) conf mode fix)
	    (when *indebug*
	       (tprint "/// J2SLetBlock.2 denv=" (dump-env denv)))
	    (multiple-value-bind (intv benv)
	       (node-range-seq nodes denv conf mode fix)
	       (let ((nenv (filter (lambda (d)
				      (not (memq (car d) decls)))
			      benv)))
		  (return intv nenv)))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SWith ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SWith env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SWith this (obj block)
      (multiple-value-bind (tye enve)
	 (node-range obj env conf mode fix)
	 (multiple-value-bind (tyb envb)
	    (node-range block enve conf mode fix)
	    (return #f envb)))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SReturn env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SReturn this (expr from loc)
      (multiple-value-bind (intve enve)
	 (node-range expr env conf mode fix)
	 (cond
	    ((not (interval? intve))
	     (return #f enve))
	    ((isa? from J2SFun)
	     (with-access::J2SFun from (rrange)
		(let ((tyr (interval-merge rrange intve)))
		   (unless (interval-in? tyr rrange)
		      (unfix! fix
			 (format "J2SReturn(~a) e=~a ~a/~a" loc intve tyr rrange))
		      (set! rrange tyr)))
		(values #f enve)))
	    ((isa? from J2SExpr)
	     (expr-range-add! from enve fix intve)
	     (return #f enve))
	    (else
	     (values #f enve))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SReturnYield ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SReturnYield env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SReturnYield this (expr from loc kont)
      (multiple-value-bind (intv env)
	 (node-range expr env conf mode fix)
	 (multiple-value-bind (kintv kenv)
	    (node-range kont env conf mode fix)
	    (return #f kenv)))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SKont ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SKont env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SKont this (body exn param)
      (decl-vrange-add! param *infinity-intv* fix)
      (decl-vrange-add! exn *infinity-intv* fix)
      (node-range body env conf mode fix)
      (expr-range-add! this env fix *infinity-intv*)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SIf ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SIf env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SIf this (test then else)
      (with-debug *debug-range-if* `(J2SIf ,(j2s->list test) ...)
	 "env=" (dump-env env)
	 (multiple-value-bind (_ env)
	    (node-range test env conf mode fix)
	    (debug *debug-range-if* "env-test" (dump-env env))
	    (multiple-value-bind (envt envo)
	       (test-envs test env conf mode fix)
	       (debug *debug-range-if* "envt=" (dump-env envt))
	       (multiple-value-bind (_ nenvt)
		  (node-range then (append-env envt env) conf mode fix)
		  (debug *debug-range-if* "envo=" (dump-env envo))
		  (multiple-value-bind (_ nenvo)
		     (node-range else (append-env envo env) conf mode fix)
		     (return #f (env-merge nenvt nenvo)))))))))
   
;*---------------------------------------------------------------------*/
;*    node-range ::J2SSwitch ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SSwitch env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SSwitch this (key cases)
      (multiple-value-bind (_ env)
	 (node-range key env conf mode fix)
	 (let loop ((cases cases)
		    (env env))
	    (if (null? cases)
		(return #f env)
		(multiple-value-bind (_ envc)
		   (node-range (car cases) env conf mode fix)
		   (loop (cdr cases) (env-merge envc env))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCase ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCase env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SCase this (expr body)
      (multiple-value-bind (_ enve)
	 (node-range expr env conf mode fix)
	 (node-range body (env-merge enve env) conf mode fix))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SWhile ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SWhile env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SWhile this (test body)
      (let ((denv (dump-env env))
	    (ffix (cell-ref fix)))
	 (when *debug-range-while*
	    (tprint ">>> while [" ffix "] test=" (j2s->list test))
	    (tprint ">>> env=" denv))
	 (let loop ((env env))
	    (let ((ostamp (cell-ref fix)))
	       (when *debug-range-while*
		  (tprint "--- while [" ffix "] / " ostamp)
		  (tprint "     env=" (dump-env env)))
	       (multiple-value-bind (testi teste)
		  (node-range test env conf mode fix)
		  (when *debug-range-while*
		     (tprint "    [" ffix "] test=" (j2s->list test))
		     (tprint "    [" ffix "] teste=" (dump-env teste)))
		  (multiple-value-bind (testet testef)
		     (test-envs test env conf mode fix)
		     (when (pair? denv)
			(when *debug-range-while*
			   (tprint "    [" ffix "] testet="
			      (dump-env testet))
			   (tprint "    [" ffix "] testef="
			      (dump-env testef))))
		     (multiple-value-bind (bodyi bodye)
			(node-range body (append-env testet env) conf mode fix)
			(if (or (=fx ostamp (cell-ref fix))
				(eq? mode 'slow))
			    (let ((wenv (append-env testef
					   (env-merge bodye env))))
			       (when *debug-range-while*
				  (tprint "<<< while [" ffix "] "
				     (dump-env wenv)))
			       (return #f wenv))
			    (loop (env-merge bodye env)))))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SDo ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SDo env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SDo this (body test)
      (let ((denv (dump-env env))
	    (ffix (cell-ref fix)))
	 (multiple-value-bind (bodyi bodye)
	    (node-range body env conf mode fix)
	    (let loop ((env env))
	       (let ((ostamp (cell-ref fix)))
		  (multiple-value-bind (testi teste)
		     (node-range test bodye conf mode fix)
		     (multiple-value-bind (testet testef)
			(test-envs test bodye conf mode fix)
			(multiple-value-bind (bodyi bodye)
			   (node-range body teste conf mode fix)
			   (if (or (=fx ostamp (cell-ref fix))
				   (eq? mode 'slow))
			       (return #f (append-env testef bodye))
			       (loop (env-merge bodye env))))))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SFor ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SFor env::pair-nil conf mode::symbol fix::cell)
   (define (debug-for loc)
      *debug-range-for*)

   (with-access::J2SFor this (init test incr body loc)
      (let ((ffix (cell-ref fix)))
	 ;; (set! *debug-range-for* (eq? (caddr loc) 1835))
	 (when (debug-for loc)
	    (tprint ">>> for " loc
	       " [" ffix "/" mode "] test=" (j2s->list test))
	       (tprint ">>> env=" (dump-env env)))
	 (multiple-value-bind (initi inite)
	    (node-range init env conf mode fix)
	    (let loop ((env inite))
	       (let ((ostamp (cell-ref fix)))
		  (when (debug-for loc)
		     (tprint "--- for " loc
			" [" ffix "/" mode "] / " ostamp)
		     (tprint "    [" ffix "] env=" (dump-env env)))
		  (multiple-value-bind (testi teste)
		     (node-range test env conf mode fix)
		     (when (debug-for loc)
			(tprint "    [" ffix "] test=" (j2s->list test))
			(tprint "    [" ffix "] testenv=" (dump-env teste)))
		     (multiple-value-bind (testet testef)
			(test-envs test env conf mode fix)
			(when (debug-for loc)
			   (tprint "    [" ffix "] testet=" (dump-env testet))
			   (tprint "    [" ffix "] testef=" (dump-env testef)))
			(multiple-value-bind (bodyi bodye)
			   (node-range-seq (list body incr)
			      (append-env testet env) conf mode fix)
			   (when (debug-for loc)
			      (tprint "    [" ffix "] bodye=" (dump-env bodye)))
			   (if (or (=fx ostamp (cell-ref fix))
				   (eq? mode 'slow))
			       (begin
				  (when (debug-for loc)
				     (tprint "<<< for " loc
					" [" ffix "/" mode "] "
					(dump-env (append-env testef bodye))))
				  (return #f (append-env testef bodye)))
			       (let ((menv (env-merge bodye env)))
				  (when (debug-for loc)
				     (tprint "~~~ [" ffix "] merge-env"
					(dump-env menv)))
				  (loop menv))))))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SForIn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SForIn env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SForIn this (lhs obj body op)
      (let ((decl (if (isa? lhs J2SRef)
		      (with-access::J2SRef lhs (decl) decl)
		      (with-access::J2SGlobalRef lhs (decl) decl))))
	 (decl-vrange-add! decl *infinity-intv* fix)
	 (let loop ((env (extend-env env decl *infinity-intv*)))
	    (let ((ofix (cell-ref fix)))
	       (multiple-value-bind (typ envb bk)
		  (node-range-seq (list obj body) env conf mode fix)
		  (if (=fx ofix (cell-ref fix))
		      (return typ envb)
		      (loop (env-merge env envb)))))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2STry ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2STry env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2STry this (body catch finally)
      (multiple-value-bind (_ envb)
	 (node-range body env conf mode fix)
	 (multiple-value-bind (_ envc)
	    (node-range catch env conf mode fix)
	    (multiple-value-bind (_ envf)
	       (node-range finally (env-merge envb envc) conf mode fix)
	       (return #f envf))))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SCatch ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SCatch env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SCatch this (body param)
      (decl-vrange-add! param *infinity-intv* fix)
      (node-range body env conf mode fix)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SThrow ...                                        */
;*--------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SThrow env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SThrow this (expr)
      (multiple-value-bind (_ env)
	 (node-range expr env conf mode fix)
	 (return #f env))))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SClass ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SClass env::pair-nil conf mode::symbol fix::cell)
   (with-access::J2SClass this (expr decl)
      (call-default-walker)
      (when decl (decl-vrange-add! decl *infinity-intv* fix))
      (expr-range-add! this env fix *infinity-intv*)))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SClassElement ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SClassElement env::pair-nil conf mode::symbol fix::cell)
   (call-default-walker)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SExportVars ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SExportVars env::pair-nil conf mode::symbol fix::cell)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    node-range ::J2SImport ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-range this::J2SImport env::pair-nil conf mode::symbol fix::cell)
   (return #f env))

;*---------------------------------------------------------------------*/
;*    interval->type ...                                               */
;*---------------------------------------------------------------------*/
(define (interval->type intv tymap default)
   (cond
      ((not (interval? intv)) default)
      ((not (eq? (interval-type intv) 'integer)) default)
      ((interval-in? intv *uint32-intv*) 'uint32)
      ((interval-in? intv *int32-intv*) 'int32)
      ((interval-in? intv *int30-intv*) 'int30)
      ((interval-in? intv *int53-intv*) (map-type 'int53 tymap))
      (else default)))

;*---------------------------------------------------------------------*/
;*    type->boxed-type ...                                             */
;*---------------------------------------------------------------------*/
(define (type->boxed-type ty)
   (case ty
      ((int32 uint32) 'integer)
      (else ty)))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SNode tymap)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-range! ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SProgram tymap)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (n) (type-range! n tymap)) decls)
      (for-each (lambda (n) (type-range! n tymap)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    range-type? ...                                                  */
;*---------------------------------------------------------------------*/
(define (range-type? ty)
   (memq ty '(integer number)))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SDecl tymap)
   (call-default-walker)
   (with-access::J2SDecl this (vrange vtype id scope escape)
      (when (range-type? vtype)
	 (let ((ity (interval->type vrange tymap vtype)))
	    (unless (eq? ity 'unknown)
	       (let ((rty (min-type vtype ity)))
		  (unless (eq? rty vtype)
		     (let ((aty (cond
				   ((memq scope '(%scope global))
				    (if (decl-ronly? this)
					rty
					(type->boxed-type rty)))
				   (escape
				    (type->boxed-type rty))
				   (else
				    rty))))
			(set! vtype aty)))))))
      this))

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SRef tymap)
   (with-access::J2SRef this (range type decl)
      (when (range-type? type)
	 (with-access::J2SDecl decl (escape)
	    (let* ((ity (interval->type range tymap 'number))
		   (mty (min-type type ity)))
	       (set! type (if escape (type->boxed-type mty) mty))))))
   this)

;*---------------------------------------------------------------------*/
;*    type-range! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (type-range! this::J2SExpr tymap)
   (call-default-walker)
   (with-access::J2SExpr this (range type)
      (when (range-type? type)
	 (let ((ity (interval->type range tymap 'number)))
	    (set! type (min-type type ity)))))
   this)

;*---------------------------------------------------------------------*/
;*    typemapXX ...                                                    */
;*---------------------------------------------------------------------*/
(define typemap32
   '((index uint32)
     (indexof number)
     (length uint32)
     (int53 number)
     (integer number)
     (number number)))

(define typemap53
   '((int29 int32)
     (uint29 uint32)
     (indexof int53)
     (index uint32)
     (length uint32)
     (integer int53)
     (number number)))

(define defmap
   '((index number)
     (indexof number)
     (length integer)
     (integer integer)
     (number number)))

;*---------------------------------------------------------------------*/
;*    map-type ...                                                     */
;*---------------------------------------------------------------------*/
(define (map-type type typemap)
   (let ((c (assq type typemap)))
      (if (pair? c) (cadr c) type)))

;*---------------------------------------------------------------------*/
;*    map-types  ...                                                   */
;*    -------------------------------------------------------------    */
;*    Map compiler types to actual target types (i.e., maps uint29     */
;*    to uint32, length to uint32, intege to bint, etc...).            */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SNode tmap)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SRef tmap)
   (with-access::J2SRef this (range type decl)
      (when (range-type? type)
	 (with-access::J2SDecl decl (escape)
	    (let ((ty (interval->type range tmap type)))
	       (set! type (if escape (type->boxed-type ty) ty)))))))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SExpr ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SExpr tmap)
   (with-access::J2SExpr this (range type)
      (when (range-type? type)
	 (set! type (interval->type range tmap type))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SCall ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SCall tmap)
   (call-next-method)
   (with-access::J2SCall this (fun type loc)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (cond
	       ((isa? decl J2SDeclFun)
		(with-access::J2SDeclFun decl (val)
		   (when (decl-ronly? decl)
		      (cond
			 ((isa? val J2SFun)
			  (with-access::J2SFun val (rtype %info)
			     (unless (eq? %info 'map-types)
				(map-types val tmap))
			     (set! type rtype)))
			 ((isa? val J2SMethod)
			  (with-access::J2SMethod val (function method)
			     (with-access::J2SFun function (rtype %info)
				(unless (eq? %info 'map-types)
				   (map-types function tmap))
				(set! type rtype))))))))
	       ((isa? decl J2SDeclInit)
		(with-access::J2SDeclInit decl (val)
		   (if (and (decl-ronly? decl) (isa? val J2SFun))
		       (with-access::J2SFun val (rtype %info)
			  (unless (eq? %info 'map-types)
			     (map-types val tmap))
			  (set! type rtype))))))))))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SFun tmap)
   (with-access::J2SFun this (rtype rrange thisp %info)
      (set! %info 'map-types)
      (when (isa? thisp J2SDecl) (map-types thisp tmap))
      (when (range-type? rtype)
	 (set! rtype (interval->type rrange tmap rtype))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SHopRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SHopRef tmap)
   (with-access::J2SHopRef this (type rtype)
      (set! type (map-type type tmap))
      (set! rtype (map-type rtype tmap)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SDecl ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SDecl tmap)
   (with-access::J2SDecl this (vtype id vrange escape)
      (when (range-type? vtype)
	 (let ((ty (interval->type vrange tmap 'number)))
	    (set! vtype (if escape (type->boxed-type ty) ty)))))
   (call-default-walker))
	 
;*---------------------------------------------------------------------*/
;*    map-types ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SDeclInit tmap)
   (with-access::J2SDeclInit this (val id)
      (call-next-method)
      (map-types val tmap)))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SBinary tmap)
   (with-access::J2SBinary this (op type range)
      (case op
	 ((>> << BIT_OT ^ &)
	  (set! type 'int32))
	 ((>>>)
	  (set! type 'uint32))
	 (else
	  (when (range-type? type)
	     (set! type (interval->type range tmap 'number))))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    map-types ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (map-types this::J2SUnary tmap)
   (with-access::J2SUnary this (op type range)
      (cond
	 ((eq? op '~)
	  (set! type 'int32))
	 ((range-type? type)
	  (set! type (interval->type range tmap 'number)))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-int32 ...                                                  */
;*---------------------------------------------------------------------*/
(define (force-int32::bool this::J2SNode)
   (let ((cell (make-cell #f)))
      (force-int32! this cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    force-int32! ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (force-int32! this::J2SNode cell::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-int32! ::J2SBinary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (force-int32! this::J2SBinary cell)
   (with-access::J2SBinary this (op range)
      (unless (interval? range)
	 (case op
	    ((<< >> ^ BIT_OR &)
	     (cell-set! cell #t)
	     (set! range *int32-intv*))
	    ((>>>)
	     (cell-set! cell #t)
	     (set! range *uint32-intv*))))
      this))
   
;*---------------------------------------------------------------------*/
;*    reset-loop-range! ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-loop-range! this::J2SNode resetp::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-loop-range! ::J2SExpr ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-loop-range! this::J2SExpr resetp)
   (when resetp
      (with-access::J2SExpr this (range)
	 (set! range #unspecified)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-loop-range! ::J2SLoop ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-loop-range! this::J2SLoop resetp)
   (set! resetp #t)
   (call-default-walker))
      
;*---------------------------------------------------------------------*/
;*    program-capture! ...                                             */
;*---------------------------------------------------------------------*/
(define (program-capture! this::J2SProgram)
   (with-access::J2SProgram this (decls nodes direct-eval)
      (unless direct-eval
	 (for-each (lambda (d) (mark-capture d '())) decls)
	 (for-each (lambda (n) (mark-capture n '())) nodes))))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SAssig env::pair-nil)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (scope ronly %info)
		(unless (memq decl env)
		   (set! %info 'capture))))
	  (mark-capture lhs env))
      (mark-capture rhs env)))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SFun env::pair-nil)
   (with-access::J2SFun this (params body)
      (for-each (lambda (p::J2SDecl)
		   (with-access::J2SDecl p (%info)
		      (set! %info 'nocapture)))
	 params)
      (mark-capture body params)))

;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SKont ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SKont env::pair-nil)
   (with-access::J2SKont this (param exn body)
      (with-access::J2SDecl param (%info)
	 (set! %info 'nocapture))
      (with-access::J2SDecl exn (%info)
	 (set! %info 'nocapture))
      (mark-capture body (list param exn))))
      
;*---------------------------------------------------------------------*/
;*    mark-capture ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-capture this::J2SLetBlock env::pair-nil)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (p::J2SDecl)
		   (with-access::J2SDecl p (%info)
		      (set! %info 'nocapture)))
	 decls)
      (let ((nenv (append decls env)))
	 (for-each (lambda (d) (mark-capture d nenv)) decls)
	 (for-each (lambda (n) (mark-capture n nenv)) nodes))))
   
