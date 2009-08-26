(module __hopscheme
   (library scheme2js)
   (import __hopscheme_config
	   __hopscheme_scm-compil
	   __hopscheme_dollar-escape
	   __hopscheme_tilde-escape)
   (from __hopscheme_scm-compil)
   (export (compile-scheme-module clause)))

(define (precompile-module m)
   (let* ((files ((bigloo-module-resolver) m '*))
	  (compiled (any (lambda (f)
			    (precompile-imported-module-file
			     m
			     f
			     *hop-reader*
			     (hopscheme-config #f)
			     :bigloo-modules? #t
			     :store-exports-in-ht? #t
			     :store-exported-macros-in-ht? #t))
			 files)))
      (if compiled
	  compiled
	  (error 'hopscheme
		 "Could not find module"
		 m))))
   
(define (precompile-headers headers)
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
		    (if (null? import-names)
			(loop (cdr headers)
			      rev-imports
			      rev-others)
			(liip (cdr import-names)
			      (cons (precompile-module (car import-names))
				    rev-imports)))))
		(else
		 (loop (cdr headers)
		       rev-imports
		       (cons header rev-others))))))))

;; precompiles the given clauses, so they can be used as "module-header" for
;; expressions that are then compiled by compile-scheme-expression.
;; clauses should be something like '(import m1), etc.
(define (compile-scheme-module clauses)
   (list (precompile-headers clauses)))
