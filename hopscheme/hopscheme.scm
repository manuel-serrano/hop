(module hopscheme
   (library scheme2js)
   (library hop)
   (eval (export-all)))

(set! *unresolved=JS* #t)

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


;; ===========================================================================
;; add hook for '~'-escaped expressions.
;; ===========================================================================

(define *rev-scheme-exprs* (make-hashtable))

(define (js->hop js-expr)
   (list (hop-read-javascript (open-input-string (string-append js-expr "}")))))

(define (new-scheme-expr p expr)
   (let* ((proxy (list 'begin #f))
	  (part (list 'part expr
		      (lambda (js-expr)
			 (set-cdr! proxy (js->hop js-expr))
			 ";"))))
      (hashtable-update! *rev-scheme-exprs*
			 p
			 (lambda (old-l)
			    (cons part old-l))
			 (list part))
      proxy))

(hop-make-escape-set! new-scheme-expr)


;; ===========================================================================
;; and one, once an expression has been read.
;; ===========================================================================

(define (post-compile p)
;   (print "post-compile")
   (let ((rev-scheme-exprs (hashtable-get *rev-scheme-exprs* p)))
      (when rev-scheme-exprs
	 (hashtable-remove! *rev-scheme-exprs* p)
;	 (print "compiling (reverse): " rev-scheme-exprs)
	 (with-output-to-string
	    (lambda ()
	       (scheme2js (reverse! rev-scheme-exprs) '()))))))

(hop-read-post-hook-set! post-compile)


;; ===========================================================================
;; modify scheme2js expander, so we recognize '$'escapes.
;; ===========================================================================

(define (starts-with-dollar? id)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\$ (string-ref str 0)))))

(define (strip-dollar s)
   (let ((str (symbol->string s)))
      (string->symbol (substring str 1 (string-length str)))))

(define (unhop x)
   (cond
      ; $var
      ((and (symbol? x)
	    (starts-with-dollar? x)
	    (not (eq? x '$)))
       `(pragma ,(symbol->string x)))
      ; '(...) `(...)
      ((and (pair? x)
	    (or (eq? (car x) 'quote)
		(eq? (car x) 'quasiquote)))
       x)
      ; (... $ (...) ....)
      ((pair? x)
       (let loop ((l x))
	  (cond
	     ((or (null? l)
		  (not (pair? l)))
	      x)
	     ((and (eq? (car l) '$)
		   (pair? (cdr l))
		   (pair? (cadr l)))
	      (set-car! l `(pragma ,(with-output-to-string
				       (lambda ()
					  (write '$)
					  (write (cadr l))))))
	      (set-cdr! l (cddr l))
	      (loop (cdr l)))
	     (else
	      (loop (cdr l))))))
      (else
       x)))

(let ((old-initial-expander *scheme2js-initial-expander*))
   (define (new-initial-expander x e::procedure)
      (let ((x-unhopped (unhop x)))
	 (old-initial-expander x-unhopped e)))
   (set! *scheme2js-initial-expander* new-initial-expander))
