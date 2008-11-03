(module __hopscheme_scm-compil
   
   (library scheme2js)
   
   (import __hopscheme_config
	   __dollar_scheme2js_module)

   (export (compile-scheme-file file::bstring)))
   
;*---------------------------------------------------------------------*/
(define *cached-config* #f)

(define (get-cached-config)
   ;; no need for locks. in the worst case we create more than one list.
   (when (not *cached-config*)
      (set! *cached-config*
	    (extend-config* `((dollar-eval . #t) ;; do an 'eval' on $s.
			      ;; allow $(import xyz) ...
			      (module-preprocessor . ,(dollar-modules-adder)))
			    (hopscheme-config #t))))
   *cached-config*)
       
;*---------------------------------------------------------------------*/
(define (compile-scheme-file file)
   (with-output-to-string
      (lambda ()
	 (scheme2js-compile-file file              ;; input-files
				 "-"               ;; output-file
				 '()               ;; additional module-headers
				 (get-cached-config)
				 :reader *hop-reader*))))
