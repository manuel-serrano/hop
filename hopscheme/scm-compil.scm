(module __hopscheme_scm-compil
   
   (library scheme2js)
   
   (import __hopscheme_config
	   __dollar_scheme2js_module
	   __hopscheme_hop_runtime)

   (export (compile-scheme-file file::bstring ::obj)))
   
;*---------------------------------------------------------------------*/
(define *cached-config* #f)

(define (get-cached-config)
   ;; no need for locks. in the worst case we create more than one list.
   (when (not *cached-config*)
      (set! *cached-config*
	    (extend-config* (hopscheme-config #t)
			    ;; do an 'eval' on $s and 'eval' new module clause
			    `((hop-module-compilation . #t)))))
   *cached-config*)

;*---------------------------------------------------------------------*/
(define (compile-scheme-file file env)
   (set-abase! file)
   (with-output-to-string
      (lambda ()
	 (scheme2js-compile-file file              ;; input-files
				 "-"               ;; output-file
				 `(                ;; headers-overrides
				   (merge-first (import ,@(hop-runtime-modules)))
				   ,@env)
				 (extend-config (get-cached-config)
						'module-resolver
						(lambda (mod)
						   ((bigloo-module-resolver)
						    mod (module-abase))))
				 :reader *hop-reader*))))

(define (set-abase! file)
   (let loop ((dir (dirname file)))
      (if (file-exists? (make-file-name dir ".afile"))
	  (module-abase-set! dir)
	  (let ((ndir (dirname dir)))
	     (unless (string=? dir ndir)
		(loop ndir))))))
