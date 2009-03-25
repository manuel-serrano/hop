(module __hopscheme
   (library scheme2js)
   (import __hopscheme_scm-compil
	   __hopscheme_dollar-escape
	   __hopscheme_tilde-escape)
   (from __hopscheme_scm-compil)
   (export (compile-scheme-module clause)))

(define (compile-scheme-module clause)
   (list 'merge-first clause))
