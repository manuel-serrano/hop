(module scm-compil
   
   (option (set! *dlopen-init* "hopscheme_s"))
   
   (library hop
	    scheme2js)
   
   (import hopscheme-config)

   (export compile-scheme-file))
   
;*---------------------------------------------------------------------*/
(define (hop-file path file)
   (let ((p (find-file/path file path)))
      (if (string? p)
	  p
	  (make-file-name (hop-share-directory) file))))

;*---------------------------------------------------------------------*/
(define (compile-scheme-file file)
   (let ((config (hopscheme-config #t)))
      (hashtable-put! config 'dollar-eval #t)
      (with-output-to-string
	 (lambda ()
	    (scheme2js-compile-file file                      ;; input-files
				    "-"                       ;; output-file
				    ;; additional module-headers:
				    '((((import hop-runtime)) . merge-first))
				    config
				    :reader (lambda (p v)
					       (hop-read p)))))))


