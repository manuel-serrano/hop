(module __hopscheme
   (library scheme2js)
   (import __hopscheme_config
	   __hopscheme_scm-compil
	   __hopscheme_dollar-escape
	   __hopscheme_tilde-escape
	   __hopscheme_precompilation)
   (from __hopscheme_scm-compil)
   (export (compile-scheme-module clauses)))

;; precompiles the given clauses, so they can be used as "module-header" for
;; expressions that are then compiled by compile-scheme-expression.
;; clauses should be something like '(import m1), etc.
(define (compile-scheme-module clauses)
   (list (precompile-headers clauses)))
