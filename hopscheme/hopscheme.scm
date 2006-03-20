(module hopscheme
   (library scheme2js)
   (import hop-scheme-head
	   runtime-files
	   dollar-escape
	   tilde-escape)
   (from hop-scheme-head))

;; unresolved symbols are considered to be JS variables
(set! *unresolved=JS* #t)
