(module __hopscheme_scm-compil
   
   (library hop
	    scheme2js)
   
   (import __hopscheme_config)

   (export (compile-scheme-file file)))
   
;*---------------------------------------------------------------------*/
(define (hop-file path file)
   (let ((p (find-file/path file path)))
      (if (string? p)
	  p
	  (make-file-name (hop-share-directory) file))))

(define *cached-config* #f)

(define (get-cached-config)
   ;; no need for locks. in the worst case we create more than one list.
   (when (not *cached-config*)
      (set! *cached-config* (cons '(dollar-eval . #t) (hopscheme-config #t))))
   *cached-config*)
       
;*---------------------------------------------------------------------*/
(define (compile-scheme-file file)
   (with-output-to-string
      (lambda ()
	 (scheme2js-compile-file file                      ;; input-files
				 "-"                       ;; output-file
				 ;; additional module-headers:
				 '((((import hop-runtime)) . merge-first))
				 (get-cached-config)
				 :reader (lambda (p v)
					    (hop-read p))))))


