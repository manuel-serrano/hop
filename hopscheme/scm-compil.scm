(module __hopscheme_scm-compil
   
   (library scheme2js)
   
   (import __hopscheme_config
	   __dollar_scheme2js_module
	   __hop_exports)

   (export (compile-scheme-file file::bstring ::obj)))
   
;*---------------------------------------------------------------------*/
(define *cached-config* #f)

(define (get-cached-config)
   ;; no need for locks. in the worst case we create more than one list.
   (when (not *cached-config*)
      (set! *cached-config*
	    (extend-config* (hopscheme-config #t)
			    `((dollar-eval . #t) ;; do an 'eval' on $s.
			      ;; allow $(import xyz) ...
			      (module-preprocessor . ,(dollar-modules-adder))))))
   *cached-config*)
       
;*---------------------------------------------------------------------*/
(define (compile-scheme-file file env)
   (with-output-to-string
      (lambda ()
	 (scheme2js-compile-file file              ;; input-files
				 "-"               ;; output-file
				 `(                ;; headers-overrides
				   (merge-first (import ,(hop-runtime-module)))
				   ,@env)
				 (extend-config (get-cached-config)
						'module-resolver
						(lambda (mod)
						   ((bigloo-module-resolver)
						    mod (module-abase))))
				 :reader *hop-reader*))))
