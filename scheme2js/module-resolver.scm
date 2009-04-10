(module module-resolver
   (export (extension-resolver::procedure include-paths::pair-nil)))

;; currently scheme2js only supports a simple module-lookup based on
;; extensions.

(define *module-extensions* '("scm" "sch"))

(define (extension-resolver include-paths)
   (lambda (module abase)
      (let* ((module-str (symbol->string module))
	     (module-filenames (map (lambda (extension)
				       (string-append module-str
						      "."
						      extension))
				    *module-extensions*))
	     ;; for now just get the first hit.
	     ;; we can later add support for more possible results.
	     (module-file (any (lambda (file)
				  (find-file/path file include-paths))
			       module-filenames)))
	 (if module-file
	     (list module-file)
	     '()))))
