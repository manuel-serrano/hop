(module __hopscheme_config
   (library scheme2js)
   (import __hop_exports)
   (export (hopscheme-config compile-file?)
	   (init-hopscheme! #!key reader share path verbose eval postprocess)
	   *hop-reader*
	   *hop-share-directory*
	   *hop-eval*
	   *hop-postprocess*))

(define *module-counter* -1)
(define *cached-config* #f)
(define *module-counter-lock* (make-mutex))

(define *hop-reader* (lambda (p v) (error 'hop-reader "not implemented yet" p)))
(define *hop-share-directory* "/")
(define *hop-verbose* 0)
(define *hop-eval* (lambda (e) (error 'hop-eval "not implemented yet" e)))
(define *hop-postprocess* (lambda (s) (error 'hop-postprocess "not implemented yet" s)))

(define (get-cached-config)
   (or *cached-config*
       (begin
	  ;; no need to worry about multi-threading. in the worst case we
	  ;; create several lists...
	  (set! *cached-config*
		(extend-config*
		 ;; on top of scheme2js's default config
		 (default-scheme2js-config)
		 `(;; unresolved symbols are considered to be JS variables
		   (unresolved=JS . module)
		   ;; procedures may use the 'this' variable. (This was actually
		   ;; already possible due to the 'unresolved=JS flag. But this
		   ;; is cleaner.)
		   (procedures-provide-js-this . #t)
		   ;; one can use the 'return!' form now. eg: (return! #t)
		   (return . #t)
		   ;; no indentation
		   (indent . #f)
		   ;; include-path
		   (include-paths . ,(list *hop-share-directory*))
		   ;; currently we are still using scheme2js-modules
		   (bigloo-modules . #t)
		   )))
	  *cached-config*)))

;; do not reuse hopscheme-configs!
(define (hopscheme-config compile-file?)

   (define (add-suffix-clause conf)
      ;; make sure static variables (not exported, but global) do not clash
      (if compile-file?
	  conf
	  (with-lock *module-counter-lock*
	     (lambda ()
		(set! *module-counter* (+ *module-counter* 1))
		(extend-config
		 conf
		 'statics-suffix
		 (string-append "_hopM"
				(number->string *module-counter*)))))))

   (let* ((config (get-cached-config))
	  (conf-indent (if (>fx *hop-verbose* 0)
			   (extend-config config 'indent 2)
			   config))
	  (conf-verbose (if (>fx *hop-verbose* 10)
			    (extend-config config 'verbose #t)
			    conf-indent))
	  (conf-module (add-suffix-clause conf-verbose)))
      conf-module))

;;
(define (init-hopscheme! #!key reader share path verbose eval postprocess)
   (set! *hop-reader* reader)
   (set! *hop-share-directory* share)
   (set! *hop-verbose* verbose)
   (set! *hop-eval* eval)
   (set! *hop-postprocess* postprocess))
