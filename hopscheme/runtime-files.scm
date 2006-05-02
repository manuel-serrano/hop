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
	  (let ((head (find-in-tree xml 'head))
		(jshead (<HEAD> :dir (hop-share-directory)
				:jscript "runtime.js"
				:jscript "runtime-interface.js")))
	     (if head
		 (with-access::xml-markup head (body)
		    (set! body (append (xml-markup-body jshead) body)))
		 (let ((html (find-in-tree xml 'html)))
		    (when html
		       (with-access::xml-markup html (body)
			  (set! body (cons jshead body)))))))))
   resp)

(hop-http-response-local-hook-add! add-runtime-js-files)
