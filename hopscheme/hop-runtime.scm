(module __hopscheme_hop_runtime
   (library scheme2js)
   (import __hopscheme_precompilation
	   __hopscheme_config)
   (export (hop-runtime-modules::pair-nil)))

;; afile is supposed do be in share-directory (which don't yet know)
(define *hop-runtime-afile* ".afile")

(define *hop-runtime-modules* '(__hop __hop-canvas __hop-spage))

(define (runtime-resolver)
   (let* ((afile (make-file-path *hop-share-directory* *hop-runtime-afile*))
	  (a-list (with-input-from-file afile read)))
      (lambda (m _)
	 (let ((t (assq m a-list)))
	    (when (not t)
	       (error 'hop-runtime
		      "Internal error: runtime module not in alist"
		      m))
	    (map (lambda (f) (make-file-path *hop-share-directory* f))
		 (cdr t))))))

(define *hop-precompiled-modules* #f)

(define (precompile-runtime!)
   (let ((resolver (runtime-resolver)))
      (set! *hop-precompiled-modules* (map (lambda (m)
					      (precompile-module m resolver))
					   *hop-runtime-modules*))))

(define (hop-runtime-modules)
   (when (not *hop-precompiled-modules*)
      (precompile-runtime!))
   *hop-precompiled-modules*)
