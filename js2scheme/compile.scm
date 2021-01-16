;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/compile.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:53:18 2013                          */
;*    Last change :  Thu Jun  4 10:41:41 2020 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The js2scheme compiler driver                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_compile
   
   (import __js2scheme_utils
	   __js2scheme_ast
	   __js2scheme_stage
	   __js2scheme_dump
	   __js2scheme_parser
	   __js2scheme_syntax
	   __js2scheme_symbol
	   __js2scheme_multivar
	   __js2scheme_header
	   __js2scheme_resolve
	   __js2scheme_return
	   __js2scheme_bestpractice
	   __js2scheme_this
	   __js2scheme_callapply
	   __js2scheme_loopexit
	   __js2scheme_ronly
	   __js2scheme_use
	   __js2scheme_propcache
	   __js2scheme_instanceof
	   __js2scheme_constrsize
	   __js2scheme_ctor
	   __js2scheme_propcce
	   __js2scheme_cspecs
	   __js2scheme_objinit
	   __js2scheme_constant
	   __js2scheme_tyflow
	   __js2scheme_range
	   __js2scheme_loopspec
	   __js2scheme_arguments
	   __js2scheme_cast
	   __js2scheme_vector
	   __js2scheme_array
	   __js2scheme_letfun
	   __js2scheme_letfusion
	   __js2scheme_letopt
	   __js2scheme_unletrec
	   __js2scheme_narrow
	   __js2scheme_scheme
	   __js2scheme_js
	   __js2scheme_debug
	   __js2scheme_stage
	   __js2scheme_ecmascript5
	   __js2scheme_cps
	   __js2scheme_sweep
	   __js2scheme_uninit
	   __js2scheme_globprop
	   __js2scheme_cse
	   __js2scheme_globvar
	   __js2scheme_varpreinit
	   __js2scheme_method
	   __js2scheme_inline
	   __js2scheme_unthis
	   __js2scheme_any
	   __js2scheme_sourcemap
	   __js2scheme_hintnum
	   __js2scheme_pce
	   __js2scheme_module
	   __js2scheme_newtarget
	   __js2scheme_procedure
	   __js2scheme_strbuffer
	   __js2scheme_cnstlift)

   (export (j2s-compile-options::pair-nil)
	   (j2s-compile-options-set! ::pair-nil)
	   (j2s-compile in #!key (driver (j2s-optim-driver)) tmp #!rest args)

	   (j2s-make-driver ::pair-nil)
	   (j2s-builtin-drivers-list::pair)
	   (j2s-driver-add-after ::pair-nil ::bstring ::J2SStage)
	   (j2s-optim-driver)
	   (j2s-plain-driver)
	   (j2s-debug-driver)
	   (j2s-eval-driver)
	   (j2s-javascript-driver)
	   (j2s-javascript-optim-driver)
	   (j2s-ecmascript5-driver)
	   (j2s-javascript-debug-driver)
	   (j2s-export-driver)))

;*---------------------------------------------------------------------*/
;*    *builtin-drivers* ...                                            */
;*---------------------------------------------------------------------*/
(define *builtin-drivers*
   `(("j2s-optim-driver" ,j2s-optim-driver)
     ("j2s-plain-driver" ,j2s-plain-driver)
     ("j2s-debug-driver" ,j2s-debug-driver)
     ("j2s-eval-driver" ,j2s-eval-driver)
     ("j2s-javascript-driver" ,j2s-javascript-driver)
     ("j2s-javascript-optim-driver" ,j2s-javascript-optim-driver)
     ("j2s-ecmascript5-driver" ,j2s-ecmascript5-driver)
     ("j2s-javascript-debug-driver" ,j2s-javascript-debug-driver)
     ("j2s-export-driver" ,j2s-export-driver)))

;*---------------------------------------------------------------------*/
;*    j2s-make-driver ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-make-driver names)
   
   (define (make-driver-stage name-or-proc)
      (cond
	 ((procedure? name-or-proc)
	  (instantiate::J2SStageProc
	     (name (format "~s" name-or-proc))
	     (comment "")
	     (proc name-or-proc)))
	 ((not (string? name-or-proc))
	  (error "j2s-make-driver" "Illegal stage" name-or-proc))
	 ((string-prefix? "http://" name-or-proc)
	  (instantiate::J2SStageUrl
	     (name name-or-proc)
	     (comment name-or-proc)
	     (url name-or-proc)))
	 ((file-exists? name-or-proc)
	  (instantiate::J2SStageFile
	     (name name-or-proc)
	     (comment name-or-proc)
	     (path name-or-proc)))
	 ((file-exists? (file-name-unix-canonicalize name-or-proc))
	  (instantiate::J2SStageFile
	     (name name-or-proc)
	     (comment name-or-proc)
	     (path (file-name-unix-canonicalize name-or-proc))))
	 ((find (lambda (s::J2SStage)
		   (with-access::J2SStage s ((sname name))
		      (string=? sname name-or-proc)))
	     (cons* j2s-javascript-stage j2s-debug-stage 
		(j2s-optim-driver)))
	  =>
	  (lambda (x) x))
	 (else
	  (error "j2s-make-driver" "Cannot find builtin stage" name-or-proc))))
   
   (if (and (pair? names) (null? (cdr names)))
       (let ((driver (assoc (car names) *builtin-drivers*)))
	  (if (pair? driver)
	      ((cadr driver))
	      (error "j2s-make-driver" "Cannot find builtin driver"
		 (car names))))
       (map make-driver-stage names)))

;*---------------------------------------------------------------------*/
;*    j2s-builtin-drivers-list ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-builtin-drivers-list)
   (map car *builtin-drivers*))

;*---------------------------------------------------------------------*/
;*    j2s-driver-add-after ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-driver-add-after driver name stage)
   (let loop ((driver driver))
      (if (null? driver)
	  (list stage)
	  (let ((next (car driver)))
	     (with-access::J2SStage next ((sname name))
		(if (string=? sname name)
		    (cons* next stage (cdr driver))
		    (loop (cdr driver)))))))) 

;*---------------------------------------------------------------------*/
;*    j2s-optim-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-optim-driver)
   (cond-expand
      ((or license-academic license-commercial)
       (list
	  j2s-syntax-stage
	  j2s-sourcemap-stage
	  j2s-hopscript-stage
	  j2s-loopexit-stage
	  j2s-bestpractice-stage
	  j2s-module-stage
	  j2s-symbol-stage
	  j2s-multivar-stage
	  j2s-narrow-stage
	  j2s-letfusion-stage
	  j2s-ronly-stage
	  j2s-letfun-stage
	  j2s-letopt-stage
	  j2s-unletrec-stage
	  j2s-this-stage
	  j2s-use-stage
	  j2s-callapply-stage
	  j2s-sweep-stage
	  j2s-ronly-stage
	  j2s-uninit-stage
	  j2s-globprop-stage
	  j2s-uninit-globprop-stage
	  j2s-globvar-stage
	  j2s-cspecs-stage
	  j2s-method-stage
	  j2s-return-stage
	  j2s-newtarget-stage
	  j2s-inline-stage
	  j2s-cps-stage
	  j2s-objinit-stage
	  j2s-constant-stage
	  j2s-varpreinit-stage
	  j2s-tyflow-stage
	  j2s-sweep-stage
	  j2s-cnstlift-stage
	  j2s-hintnum-stage
	  j2s-cse-stage
	  j2s-propcache-stage
	  j2s-instanceof-stage
	  j2s-propcce-stage
	  j2s-range-stage
	  j2s-loopspec-stage
	  j2s-sweep-stage
	  j2s-ctor-stage
	  j2s-pce-stage
	  j2s-cast-stage
	  j2s-arguments-stage
	  j2s-vector-stage
	  j2s-array-stage
	  j2s-dead-stage
	  j2s-constrsize-stage
	  j2s-unthis-stage
	  j2s-procedure-stage
	  j2s-strbuffer-stage
	  j2s-scheme-stage))
      (else
       (j2s-plain-driver))))

;*---------------------------------------------------------------------*/
;*    j2s-plain-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-plain-driver)
   (list
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-uninit-force-stage
      j2s-cspecs-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-any-stage
      j2s-constant-stage
      j2s-cast-stage
      j2s-newtarget-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-debug-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-debug-driver)
   (list
      j2s-syntax-stage
      j2s-cspecs-stage
      j2s-sourcemap-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-debug-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-uninit-force-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-any-stage
      j2s-cast-stage
      j2s-newtarget-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-eval-driver ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-eval-driver)
   (list
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-uninit-force-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-any-stage
      j2s-constant-stage
      j2s-scheme-eval-stage))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-optim-driver ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-javascript-optim-driver)
   (list
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-narrow-stage
      j2s-letfusion-stage
      j2s-letopt-stage
      j2s-unletrec-stage
      j2s-use-stage
      j2s-uninit-force-stage
      j2s-varpreinit-stage
      j2s-tyflow-stage
      j2s-range-stage
      j2s-cast-stage
      j2s-vector-stage
      j2s-array-stage
      j2s-dead-stage
      j2s-javascript-stage))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-driver ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-javascript-driver)
   (list
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-javascript-stage))

;*---------------------------------------------------------------------*/
;*    j2s-ecmascript5-driver ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-ecmascript5-driver)
   (list
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-ecmascript5-stage
      j2s-javascript-stage))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-debug-driver ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-javascript-debug-driver)
   (list
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-module-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-debug-stage
      j2s-javascript-stage))

;*---------------------------------------------------------------------*/
;*    j2s-export-driver ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-export-driver)
   (list 
      j2s-syntax-stage
      j2s-sourcemap-stage
      j2s-module-stage
      j2s-symbol-stage))

;*---------------------------------------------------------------------*/
;*    j2s-compile-options ...                                          */
;*---------------------------------------------------------------------*/
(define %j2s-compile-options '())

(define (j2s-compile-options)
   %j2s-compile-options)

(define (j2s-compile-options-set! v)
   (set! %j2s-compile-options v))

;*---------------------------------------------------------------------*/
;*    j2s-compile ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-compile in #!key (driver (j2s-optim-driver)) tmp #!rest args)
   (unless (and (list? driver) (every (lambda (s) (isa? s J2SStage))))
      (bigloo-type-error "j2s-compile" "driver list" driver))
   (let* ((filename (cond
		       ((input-port? in)
			(input-port-name in))
		       ((isa? in J2SProgram)
			(with-access::J2SProgram in (path)
			   path))
		       (else
			(bigloo-type-error "j2s-compile"
			   "port or J2SProgram" in))))
	  (tmp (or tmp
		   (make-file-path (os-tmp) 
		      (or (getenv "USER") "anonymous")
		      "J2S"
		      (if (string=? filename "[string]")
			  "stdin"
			  filename))))
	  (opts (compile-opts filename in args))
	  (conf (cons* :mmaps '() :tmp tmp opts)))
      (when (config-get opts :debug-stage)
	 (make-directories tmp))
      (when (>=fx (config-get opts :warning -1) 0)
	 (bigloo-warning-set! (config-get opts :warning)))
      (unwind-protect
	 (let ((ast (cond
		       ((input-port? in)
			(j2s-parser in conf))
		       ((isa? in J2SProgram)
			in))))
	    (if (eof-object? ast)
		'()
		(let loop ((ast ast)
			   (driver driver)
			   (count 1))
		   (if (null? driver)
		       ast
		       (multiple-value-bind (ast on)
			  (stage-exec (car driver) ast tmp count conf)
			  (loop ast (cdr driver) (+fx (if on 1 0) count)))))))
	 (let ((mmaps (config-get conf :mmaps)))
	    (for-each close-mmap mmaps)))))

;*---------------------------------------------------------------------*/
;*    compile-opts ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-opts filename in args)
   (let ((o (append args (j2s-compile-options)))
	 (l (config-get args :optim 0)))
      ;; misc
      (unless (memq :commonjs-export o)
	 (set! o (cons* :commonjs-export #t o)))
      ;; debugging
      (when (>= (bigloo-debug) 0)
	 (set! o (append o `(:debug ,(bigloo-debug))))
	 (when (or (>= (bigloo-debug) 2)
		   (string-contains (or (getenv "HOPTRACE") "") "j2s:"))
	    (unless (memq :debug-stage o)
	       (set! o (cons* :debug-stage #t o)))
	    (when (string-contains (or (getenv "HOPTRACE") "") "format:json")
	       (unless (memq :debug-stage-format o)
		  (set! o (cons* :debug-stage-format 'json o))))))
      ;; profiling
      (when (config-get args :profile #f)
	 (unless (memq :profile-call o)
	    (set! o (cons* :profile-call #t o)))
	 (unless (memq :profile-cache o)
	    (set! o (cons* :profile-cache #t o)))
	 (unless (memq :profile-hint o)
	    (set! o (cons* :profile-hint #t o)))
	 (unless (memq :profile-method o)
	    (set! o (cons* :profile-method #t o)))
	 (unless (memq :profile-cmap o)
	    (set! o (cons* :profile-cmap #t o))))
      ;; memory profile
      (when (config-get args :profile-mem #f)
	 (unless (memq :profile-ident o)
	    (set! o (cons* :profile-ident #t o))))
      ;; optimization
      (when (>=fx l 900)
	 (unless (memq :optim-integer o)
	    (set! o (cons* :optim-integer #t o)))
	 (unless (memq :optim-inline-method o)
	    (set! o (cons* :optim-inline-method #t o)))
	 (unless (memq :optim-globprop o)
	    (set! o (cons* :optim-globprop #t o)))
	 (unless (memq :optim-cse o)
	    (set! o (cons* :optim-cse #t o)))
	 (unless (memq :optim-loopspec o)
	    (set! o (cons* :optim-loopspec #t o)))
	 (unless (memq :optim-arguments o)
	    (set! o (cons* :optim-arguments #t o)))
	 (unless (memq :optim-stack-alloc o)
	    (set! o (cons* :optim-stack-alloc #t o)))
	 (unless (memq :optim-procedure o)
	    (set! o (cons* :optim-procedure #t o)))
	 (unless (memq :optim-cnstlift o)
	    (set! o (cons* :optim-cnstlift #t o)))
	 (unless (memq :optim-strbuffer o)
	    (set! o (cons* :optim-strbuffer #t o)))
	 )
      (when (>=fx l 3)
	 (unless (memq :optim-method o)
	    (set! o (cons* :optim-method #t o)))
	 (unless (memq :optim-literals o)
	    (set! o (cons* :optim-literals #t o)))
	 (unless (memq :optim-array o)
	    (set! o (cons* :optim-array #t o)))
	 (unless (memq :optim-hint o)
	    (set! o (cons* :optim-hint #t o)))
	 (unless (memq :optim-hintnum o)
	    (set! o (cons* :optim-hintnum #t o)))
	 (unless (memq :optim-hintfun o)
	    (set! o (cons* :optim-hintfun #t o)))
	 (unless (memq :optim-hintblock o)
	    (set! o (cons* :optim-hintblock #t o)))
	 (unless (memq :optim-range o)
	    (set! o (cons* :optim-range #t o)))
	 (unless (memq :optim-ctor o)
	    (set! o (cons* :optim-ctor #t o)))
	 (unless (memq :optim-vector o)
	    (set! o (cons* :optim-vector #t o)))
	 (unless (memq :optim-vector o)
	    (set! o (cons* :optim-vector #t o)))
;* 	 (unless (memq :optim-pce o)                                   */
;* 	    (set! o (cons* :optim-pce #t o)))                          */
	 )
      (when (>=fx l 2)
	 (unless (memq :optim-letopt o)
	    (set! o (cons* :optim-letopt #t o)))
	 (unless (memq :optim-unletrec o)
	    (set! o (cons* :optim-unletrec #t o)))
	 (unless (memq :optim-tyflow-resolve o)
	    (set! o (cons* :optim-tyflow-resolve #t o)))
	 (unless (memq :optim-cinstanceof o)
	    (set! o (cons* :optim-cinstanceof #t o)))
	 (unless (memq :optim-multivar o)
	    (set! o (cons* :optim-multivar #t o)))
	 (unless (memq :optim-ccall o)
	    (set! o (cons* :optim-ccall #t o)))
	 (unless (memq :optim-cspecs o)
	    (set! o (cons* :optim-cspecs #t o)))
	 (unless (memq :optim-callapply o)
	    (set! o (cons* :optim-callapply #t o)))
	 (unless (memq :optim-inline o)
	    (set! o (cons* :optim-inline #t o)))
	 (unless (memq :optim-uninit o)
	    (set! o (cons* :optim-uninit #t o)))
	 (unless (memq :optim-unthis o)
	    (set! o (cons* :optim-unthis #t o)))
	 (unless (memq :optim-varpreinit o)
	    (set! o (cons* :optim-varpreinit #t o)))
	 (unless (memq :optim-globvar o)
	    (set! o (cons* :optim-globvar #t o)))
	 (unless (memq :optim-letfun o)
	    (set! o (cons* :optim-letfun #t o)))
	 (unless (memq :optim-propcache o)
	    (set! o (cons* :optim-propcache #t o)))
 	 (unless (memq :optim-sweep o)
	    (set! o (cons* :optim-sweep #t o))))
      (when (>=fx l 1)
	 (unless (memq :optim-tyflow o)
	    (set! o (cons* :optim-tyflow #t o))))

      (let ((s (config-get args :optim-size 0)))
	 (when (>=fx s 1)
	    (set! o (cons* :fun-src #f o)))
	 (when (>=fx s 2)
	    (set! o (cons* :optim-hint #f
		       :optim-inline #f
		       :optim-method #f
		       :optim-loopspec #f
		       :optim-ctor #f
		       :optim-size #t
		       o))))

      (unless (memq :=fx-as-eq o)
	 (set! o (cons* :=fx-as-eq #t o)))
      (unless (memq :filename o)
	 (set! o (cons* :filename filename o)))

      (when (member (config-get args :language)
	       '("hopscript" "ecmascript6" "ecmascript2017"))
	 (for-each (lambda (k)
		      (unless (memq k o)
			 (set! o (cons* k #t o))))
	    '(:es6-let :es6-default-value :es6-arrow-function
	      :es6-rest-argument :es2017-async :es6-module)))
      
      (let ((v (getenv "HOPCFLAGS")))
	 (when (string? v)
	    (cond
	       ((string-contains v "j2s:this")
		(set! o (cons* :optim-this #t o)))
	       ((string-contains v "j2s:ccall")
		(set! o (cons* :optim-ccall #t o))))))
      o))
