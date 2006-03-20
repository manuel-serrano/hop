(module runtime-files
   (library hop))

;; ===========================================================================
;; add hook, so we add always our runtime-files
;; ===========================================================================

(define (add-runtime-js-files req resp)
   (define (find-in-tree tree searched-markup)
      (and (xml-markup? tree)
	   (with-access::xml-markup tree (markup body)
	      (if (eq? markup searched-markup)
		  tree
		  (any (lambda (child)
			  (find-in-tree child searched-markup))
		       body)))))
   (if (http-response-hop? resp)
       (with-access::http-response-hop resp (xml)
	  (let ((head (find-in-tree xml 'head)))
	     (if head
		 (with-access::xml-markup head (body)
		    (set! body (cons (<HOP-HEAD> :jscript "runtime.js"
						 :dir (hop-share-directory))
				     (cons (<HOP-HEAD>
					      :jscript "runtime-interface.js"
					      :dir (hop-share-directory))
					   body))))
		 (let ((html (find-in-tree xml 'html)))
		    (when html
		       (with-access::xml-markup html (body)
			  (set! body (cons (<HEAD>) body)))
		       (add-runtime-js-files req resp)))))))
   resp)

(hop-http-response-local-hook-add! add-runtime-js-files)
