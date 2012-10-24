;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/scheme2js/module_library.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 23 11:24:26 2011                          */
;*    Last change :  Wed Oct 24 18:12:59 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme2JS module library                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module-library
   
   (import module-system
	   export-desc
	   error
	   config
	   tools)

   (static (class Lib-Unit
	      (name read-only)
	      (macros::pair-nil read-only)
	      (imports read-only)))

   (export (module-read-libraries! ::WIP-Unit ::obj ::obj ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *libraries* ...                                                  */
;*---------------------------------------------------------------------*/
(define *libraries*
   (make-hashtable 16))

;*---------------------------------------------------------------------*/
;*    *library-mutex* ...                                              */
;*---------------------------------------------------------------------*/
(define *library-mutex*
   (make-mutex))

;*---------------------------------------------------------------------*/
;*    module-read-libraries! ...                                       */
;*---------------------------------------------------------------------*/
(define (module-read-libraries! m::WIP-Unit module-resolver reader lib-list)
   (when (pair? lib-list)
      (with-access::WIP-Unit m (header imports macros)
	 (unless (every symbol? lib-list)
	    (scheme2js-error "scheme2js-module"
	       "only symbols are allowed in library-list"
	       lib-list
	       header))
	 (for-each (lambda (lib)
		      (with-access::Lib-Unit (scheme2js-get-library m lib)
			    (macros imports name)
			 (with-access::WIP-Unit m ((wip-macros macros)
						   (wip-imports imports)
						   (wip-units import-units))
			    (set! wip-macros (append macros wip-macros))
			    (for-each (lambda (wim)
					 (with-access::Compilation-Unit wim
					       (exports name exported-macros
						  import-units)
					    (set! wip-macros
					       (cons exported-macros wip-macros))
					    (set! wip-imports
					       (cons (cons name exports)
						  wip-imports))
					    (set! wip-units
					       (append wip-units imports))))
			       imports))))
	    lib-list))))

;*---------------------------------------------------------------------*/
;*    scheme2js-get-library ...                                        */
;*---------------------------------------------------------------------*/
(define (scheme2js-get-library m lib)
   (with-lock *library-mutex*
      (lambda ()
	 (let ((old (hashtable-get *libraries* lib)))
	    (if old
		old
		(let ((lu (scheme2js-load-library m lib)))
		   (hashtable-put! *libraries* lib lu)
		   lu))))))
		   
;*---------------------------------------------------------------------*/
;*    scheme2js-load-library ...                                       */
;*---------------------------------------------------------------------*/
(define (scheme2js-load-library m lib)
   (let ((path (let ((venv (getenv "BIGLOOLIB")))
		  (if (not venv)
		      (config 'library-path)
		      (cons "." (unix-path->list venv))))))
      (let ((init (find-file/path (library-init-file lib) path)))
	 (if (string? init)
	     (let ((macros (scheme2js-load-library-init init m)))
		(let ((jsheap (string-append (prefix init) ".jsheap")))
		   (if (file-exists? jsheap)
		       (instantiate::Lib-Unit
			  (name lib)
			  (macros macros)
			  (imports (scheme2js-load-library-jsheap jsheap lib)))
		       (let ((jsheap (find-file/path (string-append (prefix (library-init-file lib)) ".jsheap") path)))
			  (if (and (string? jsheap) (file-exists? jsheap))
			      (instantiate::Lib-Unit
				 (name lib)
				 (macros macros)
				 (imports (scheme2js-load-library-jsheap jsheap lib)))
			      (scheme2js-error "schemejs-module"
				 "Cannot find library heap"
				 lib
				 lib))))))
	     (scheme2js-error "schemejs-module"
		"Cannot find library init"
		(library-init-file lib)
		lib)))))
   
;*---------------------------------------------------------------------*/
;*    scheme2js-load-library-init ...                                  */
;*---------------------------------------------------------------------*/
(define (scheme2js-load-library-init init m::WIP-Unit)
   (let ((evmod (eval-module))
	 (macros '()))
      (unwind-protect
	 (begin
	    (eval `(module ,(gensym) (static (declare-library! . l))))
	    (eval '(define (declare-library! . l) #f))
	    (eval `(install-eval-expander 'define-expander
		      ,(lambda (x e)
			  (set! macros (cons (list x) macros)))))
	    (loadq init))
	 (when (evmodule? evmod)
	    (eval-module-set! evmod)))
      macros))

;*---------------------------------------------------------------------*/
;*    scheme2js-load-library-jsheap ...                                */
;*---------------------------------------------------------------------*/
(define (scheme2js-load-library-jsheap jsheap lib)
   (let ((heap (with-input-from-file jsheap read)))
      (match-case heap
	 ((heap ?library ?modules ?imports)
	  ;; start building the import environment
	  (let loop ((modules modules)
		     (env '()))
	     (if (null? modules)
		 (map (lambda (i)
			 (let ((c (assq i env)))
			    (if (pair? c)
				(cdr c)
				(scheme2js-error "scheme2js-library"
				   "cannot find heap module" jsheap i))))
		    (cdr imports))
		 (let* ((id (cadr (car modules)))
			(im (parse-imported-module id (car modules)
			      (lambda l (call-with-input-string "" read))
			      #f :module-cache env :ignore-missing-modules #t)))
		    (loop (cdr modules)
		       (cons (cons id im) env))))))
	 (else
	  (scheme2js-error "scheme2js-library"
	     "illegal jsheap file" jsheap lib)))))


