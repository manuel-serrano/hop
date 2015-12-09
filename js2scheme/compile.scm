;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/compile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:53:18 2013                          */
;*    Last change :  Wed Dec  9 06:59:04 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The js2scheme compiler driver                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_compile
   
   (import __js2scheme_ast
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
	   __js2scheme_letopt
	   __js2scheme_scheme
	   __js2scheme_js
	   __js2scheme_debug
	   __js2scheme_stage)

   (export (j2s-compile-options::pair-nil)
	   (j2s-compile-options-set! ::pair-nil)
	   (j2s-compile in::input-port
	      #!key
	      (driver (j2s-optim-driver))
	      tmp
	      #!rest args)

	   (j2s-make-driver ::pair-nil)
	   (j2s-driver-add-after ::pair-nil ::bstring ::J2SStage)
	   (j2s-optim-driver)
	   (j2s-plain-driver)
	   (j2s-debug-driver)
	   (j2s-eval-driver)
	   (j2s-javascript-driver)
	   (j2s-ecmascript5-driver)
	   (j2s-javascript-debug-driver)))

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
	  (error "j2s-make-drive" "Illegal stage" name-or-proc))
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
   
   (map make-driver-stage names))

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
      j2s-letopt-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-return-stage
      j2s-property-stage
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
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-return-stage
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
      j2s-debug-stage
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-return-stage
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
      j2s-this-stage
      j2s-use-stage
      j2s-ronly-stage
      j2s-return-stage
      j2s-scheme-eval-stage))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-driver ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-javascript-driver)
   (list
      j2s-syntax-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
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
	 (opts (append args (j2s-compile-options))))
      (when (>=fx (bigloo-debug) 1) (make-directories tmp))
      (let ((ast (j2s-parser in opts)))
	 (if (eof-object? ast)
	     '()
	     (let loop ((ast ast)
			(driver driver)
			(count 0))
		(if (null? driver)
		    ast
		    (loop (stage-exec (car driver) ast  tmp count opts)
		       (cdr driver)
		       (+fx 1 count))))))))

