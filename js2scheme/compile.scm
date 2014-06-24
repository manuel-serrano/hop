;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/compile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 19 08:53:18 2013                          */
;*    Last change :  Fri Jun 20 08:01:26 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
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
	   __js2scheme_header
	   __js2scheme_symbol
	   __js2scheme_resolve
	   __js2scheme_return
	   __js2scheme_bestpractice
	   __js2scheme_this
	   __js2scheme_loopexit
	   __js2scheme_ronly
	   __js2scheme_property
	   __js2scheme_scheme
	   __js2scheme_js
	   __js2scheme_stage)

   (export (j2s-compile in::input-port
	      #!key
	      (driver (j2s-optim-driver))
	      tmp
	      #!rest args)

	   (j2s-make-driver ::pair-nil)
	   (j2s-driver-add-after ::pair-nil ::bstring ::J2SStage)
	   (j2s-optim-driver)
	   (j2s-plain-driver)
	   (j2s-eval-driver)
	   (j2s-javascript-driver)))

;*---------------------------------------------------------------------*/
;*    j2s-make-driver ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-make-driver names)

   (define (make-driver-stage name)
      (cond
	 ((string-prefix? "http://" name)
	  (instantiate::J2SStageUrl
	     (name name)
	     (comment name)
	     (url name)))
	 ((file-exists? name)
	  (instantiate::J2SStageFile
	     (name name)
	     (comment name)
	     (path name)))
	 ((file-exists? (file-name-unix-canonicalize name))
	  (instantiate::J2SStageFile
	     (name name)
	     (comment name)
	     (path (file-name-unix-canonicalize name))))
	 ((find (lambda (s::J2SStage)
		   (with-access::J2SStage s ((sname name))
		      (string=? sname name)))
	     (j2s-optim-driver))
	  =>
	  (lambda (x) x))
	 (else
	  (error "j2s-make-driver" "Cannot find builtin stage" name))))
   
   (map make-driver-stage names))

;*---------------------------------------------------------------------*/
;*    j2s-driver-add-after ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-driver-add-after driver name stage)
   (let loop ((driver driver)
	      (sname "first"))
      (cond
	 ((null? driver)
	  (list stage))
	 ((string=? sname name)
	  (cons stage driver))
	 (else
	  (with-access::J2SStage (car driver) (name)
	     (loop (cdr driver) name))))))

;*---------------------------------------------------------------------*/
;*    j2s-optim-driver ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-optim-driver)
   (list
      j2s-syntax-stage
      j2s-hopscript-header-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-this-stage
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
      j2s-hopscript-header-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-this-stage
      j2s-ronly-stage
      j2s-return-stage
      j2s-scheme-stage))

;*---------------------------------------------------------------------*/
;*    j2s-eval-driver ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-eval-driver)
   (list
      j2s-syntax-stage
      j2s-loopexit-stage
      j2s-bestpractice-stage
      j2s-symbol-stage
      j2s-this-stage
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
		  (make-file-path (os-tmp) "J2S"
		     (basename (input-port-name in))))))
      (when (>=fx (bigloo-debug) 1) (make-directories tmp))
      (let ((ast (j2s-parser in args)))
	 (if (eof-object? ast)
	     '()
	     (let loop ((ast ast)
			(driver driver)
			(count 0))
		(if (null? driver)
		    ast
		    (loop (stage-exec (car driver) ast  tmp count args)
		       (cdr driver)
		       (+fx 1 count))))))))
