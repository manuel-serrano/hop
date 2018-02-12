;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/compile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:53:18 2013                          */
;*    Last change :  Mon Feb 12 11:47:10 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
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
	   __js2scheme_header
	   __js2scheme_resolve
	   __js2scheme_return
	   __js2scheme_bestpractice
	   __js2scheme_this
	   __js2scheme_loopexit
	   __js2scheme_ronly
	   __js2scheme_use
	   __js2scheme_property
	   __js2scheme_instanceof
	   __js2scheme_constrsize
	   __js2scheme_ctor
	   __js2scheme_propcce
	   __js2scheme_clevel
	   __js2scheme_constant
	   __js2scheme_tyflow
	   __js2scheme_range
	   __js2scheme_cast
	   __js2scheme_vector
	   __js2scheme_array
	   __js2scheme_letfusion
	   __js2scheme_letopt
	   __js2scheme_narrow
	   __js2scheme_scheme
	   __js2scheme_js
	   __js2scheme_debug
	   __js2scheme_stage
	   __js2scheme_ecmascript5
	   __js2scheme_cps
	   __js2scheme_sweep
	   __js2scheme_globvar
	   __js2scheme_varpreinit
	   __js2scheme_method
	   __js2scheme_inline
	   __js2scheme_unthis
	   __js2scheme_any)

   (export (j2s-compile-options::pair-nil)
	   (j2s-compile-options-set! ::pair-nil)
	   (j2s-compile in::input-port
	      #!key
	      (driver (j2s-optim-driver))
	      tmp
	      #!rest args)

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
	   (j2s-javascript-debug-driver)))

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
     ("j2s-javascript-debug-driver" ,j2s-javascript-debug-driver)))

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
   (list
      j2s-syntax-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-narrow-stage
      j2s-letfusion-stage
      j2s-letopt-stage
      j2s-this-stage
      j2s-use-stage
      j2s-sweep-stage
      j2s-ronly-stage
      j2s-globvar-stage
      j2s-clevel-stage
      j2s-method-stage
      j2s-return-stage
      j2s-inline-stage
      j2s-cps-stage
      j2s-constant-stage
      j2s-varpreinit-stage
      j2s-tyflow-stage
      j2s-property-stage
      j2s-instanceof-stage
      j2s-propcce-stage
      j2s-range-stage
      j2s-ctor-stage
      j2s-cast-stage
      j2s-vector-stage
      j2s-array-stage
      j2s-dead-stage
      j2s-constrsize-stage
      j2s-unthis-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-plain-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-plain-driver)
   (list
      j2s-syntax-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-any-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-constant-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-debug-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-debug-driver)
   (list
      j2s-syntax-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-debug-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-any-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-eval-driver ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-eval-driver)
   (list
      j2s-syntax-stage
      j2s-hopscript-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-any-stage
      j2s-return-stage
      j2s-cps-stage
      j2s-constant-stage
      j2s-scheme-eval-stage))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-optim-driver ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-javascript-optim-driver)
   (list
      j2s-syntax-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-narrow-stage
      j2s-letfusion-stage
      j2s-letopt-stage
      j2s-use-stage
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
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-javascript-stage))

;*---------------------------------------------------------------------*/
;*    j2s-ecmascript5-driver ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-ecmascript5-driver)
   (list
      j2s-syntax-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
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
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-letfusion-stage
      j2s-debug-stage
      j2s-javascript-stage))

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
(define (j2s-compile in::input-port
	   #!key
	   (driver (j2s-optim-driver))
	   tmp
	   #!rest
	   args)
   (unless (and (list? driver) (every (lambda (s) (isa? s J2SStage))))
      (bigloo-type-error "j2s-compile" "driver list" driver))
   (let ((tmp (or tmp
		  (make-file-path (os-tmp) 
		     (or (getenv "USER") "anonymous")
		     "J2S"
		     (if (string=? (input-port-name in) "[string]")
			 "stdin"
			 (input-port-name in)))))
	 (opts (compile-opts in args)))
      (when (>=fx (bigloo-debug) 1) (make-directories tmp))
      (let ((ast (j2s-parser in opts)))
	 (if (eof-object? ast)
	     '()
	     (let loop ((ast ast)
			(driver driver)
			(count 1))
		(if (null? driver)
		    ast
		    (multiple-value-bind (ast on)
		       (stage-exec (car driver) ast tmp count opts)
		       (loop ast (cdr driver) (+fx (if on 1 0) count)))))))))

;*---------------------------------------------------------------------*/
;*    compile-opts ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-opts in args)
   (let ((o (append args (j2s-compile-options)))
	 (l (config-get args :optim 0)))
      (when (>=fx l 900)
	 (unless (memq :optim-integer o)
	    (set! o (cons* :optim-integer #t o)))
	 (unless (memq :optim-inline o)
	    (set! o (cons* :optim-inline #t o)))
	 (unless (memq :optim-hint o)
	    (set! o (cons* :optim-hint #t o)))
	 (unless (memq :optim-literals o)
	    (set! o (cons* :optim-literals #t o))))
      (when (>=fx l 4)
	 (unless (memq :optim-range o)
	    (set! o (cons* :optim-range #t o)))
	 (unless (memq :optim-ctor o)
	    (set! o (cons* :optim-ctor #t o)))
	 (unless (memq :optim-vector o)
	    (set! o (cons* :optim-vector #t o)))
	 (unless (memq :optim-vector o)
	    (set! o (cons* :optim-vector #t o))))
      (when (>=fx l 2)
	 (unless (memq :optim-tyflow-resolve o)
	    (set! o (cons* :optim-tyflow-resolve #t o)))
	 (unless (memq :optim-cinstanceof o)
	    (set! o (cons* :optim-cinstanceof #t o))))
      (when (>=fx l 1)
	 (unless (memq :optim-tyflow o)
	    (set! o (cons* :optim-tyflow #t o))))
      (when (and #f (>=fx l 2))
	 (unless (memq :optim-clevel o)
	    (set! o (cons* :optim-clevel #t o))))
;* 		     (unless (memq :optim-cce o)                       */
;* 			(set! o (cons* :optim-cce #t o)))              */
      (unless (memq :filename o)
	 (set! o (cons* :filename (input-port-name in) o)))
      (let ((v (getenv "HOPCFLAGS")))
	 (when (string? v)
	    (cond
	       ((string-contains v "j2s:this")
		(set! o (cons* :optim-this #t o)))
	       ((string-contains v "j2s:ccall")
		(set! o (cons* :optim-ccall #t o))))))
      o))
