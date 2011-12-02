;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/scheme2js/module_library.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 23 11:24:26 2011                          */
;*    Last change :  Fri Dec  2 18:49:04 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
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
	   tools)

   (export (module-read-libraries! ::WIP-Unit ::obj ::obj ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    module-read-libraries! ...                                       */
;*---------------------------------------------------------------------*/
(define (module-read-libraries! m::WIP-Unit module-resolver reader lib-list)
   (with-access::WIP-Unit m (header imports macros)
      (unless (every symbol? lib-list)
	 (scheme2js-error "scheme2js-module"
	    "only symbols are allowed in library-list"
	    lib-list
	    header))
      (let ((path (let ((venv (getenv "BIGLOOLIB")))
		     (if (not venv)
			 (bigloo-library-path)
			 (cons "." (unix-path->list venv))))))
	 (for-each (lambda (lib)
		      (let ((init (find-file/path (library-init-file lib) path)))
			 (when (string? init)
			    (scheme2js-load-library-init init m)
			    (let ((jsheap (string-append (prefix init) ".jsheap")))
			       (when (file-exists? jsheap)
				  (scheme2js-load-library-jsheap jsheap
				     m header))))))
	    lib-list))))

;*---------------------------------------------------------------------*/
;*    scheme2js-load-library-init ...                                  */
;*---------------------------------------------------------------------*/
(define (scheme2js-load-library-init init m::WIP-Unit)
   (let ((evmod (eval-module)))
      (unwind-protect
	 (begin
	    (eval `(module ,(gensym) (static (declare-library! . l))))
	    (eval '(define (declare-library! . l) #f))
	    (eval `(install-eval-expander 'define-expander
		      ,(lambda (x e)
			  (with-access::WIP-Unit m (macros)
			     (set! macros (cons (list x) macros))))))
	    (loadq init))
	 (when (evmodule? evmod)
	    (eval-module-set! evmod)))))

;*---------------------------------------------------------------------*/
;*    scheme2js-load-library-jsheap ...                                */
;*---------------------------------------------------------------------*/
(define (scheme2js-load-library-jsheap jsheap m::WIP-Unit header)
   (let ((heap (with-input-from-file jsheap read)))
      (match-case heap
	 ((heap ?- ?modules ?import)
	  'todo)
	 (else
	  (scheme2js-error "scheme2js-module"
	     "illegal jsheap file"
	     jsheap
	     header)))))


