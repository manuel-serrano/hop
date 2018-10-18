;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscheme/precompilation.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Tue Mar  9 05:13:01 2010                          */
;*    Last change :  Wed Oct 14 10:49:13 2015 (serrano)                */
;*    Copyright   :  2010-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Module pre-compilation                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_precompilation
   
   (library scheme2js)
   
   (import __hopscheme_config)
   
   (export (precompile-module m::symbol resolver::procedure base)
	   (precompile-headers headers::pair-nil ::obj)))

;*---------------------------------------------------------------------*/
;*    precompile-module ...                                            */
;*---------------------------------------------------------------------*/
(define (precompile-module m resolver base)
   (with-trace 'hopscheme "precompile-module"
      (trace-item "mod=" m)
      (trace-item "base=" base)
      (let* ((files (resolver m '() base))
	     (compiled (any (lambda (f)
			       (precompile-imported-module-file m f
				  *hop-reader*
				  (hopscheme-config #f)
				  :bigloo-modules? #t
				  :store-exports-in-ht? #t
				  :store-exported-macros-in-ht? #t))
			  files)))
	 (trace-item "files=" files)
	 (if compiled
	     compiled
	     (error "hopscheme" "Could not find module" m)))))

;*---------------------------------------------------------------------*/
;*    precompile-headers ...                                           */
;*---------------------------------------------------------------------*/
(define (precompile-headers headers base)
   (let loop ((headers headers)
	      (rev-imports '())
	      (rev-others '()))
      (if (null? headers)
	  `(merge-first ,@(reverse! rev-others)
	      (import ,@(reverse! rev-imports)))
	  (let ((header (car headers)))
	     (match-case header
		((import ?i1 . ?Lis)
		 (let liip ((import-names (cdr header))
			    (rev-imports rev-imports))
		    (cond
		       ((null? import-names)
			(loop (cdr headers)
			   rev-imports
			   rev-others))
		       ((and (pair? (car import-names))
			     (symbol? (caar import-names)))
			(unless (every string? (cdar import-names))
			   (error "hopscheme" "Illegal module clause" header))
			(module-add-access! (caar import-names)
			   (cdar import-names)
			   base)
			(liip (cons (caar import-names) (cdr import-names))
			   rev-imports))
		       ((not (symbol? (car import-names)))
			(error "hopscheme" "Illegal module clause" header))
		       (else
			(liip (cdr import-names)
			   (cons (precompile-module (car import-names)
				    (bigloo-module-resolver)
				    base)
			      rev-imports))))))
		(else
		 (loop (cdr headers)
		    rev-imports
		    (cons header rev-others))))))))
