(module walk
   (import nodes
	   export-desc)
   (export
    (generic walk0 n::Node env p::procedure)
    (generic walk1 n::Node env p::procedure arg0)
    (generic walk2 n::Node env p::procedure arg0 arg1)
    (generic walk3 n::Node env p::procedure arg0 arg1 arg2)
    (generic walk4 n::Node env p::procedure arg0 arg1 arg2 arg3)
    (generic walk0! n::Node env p::procedure)
    (generic walk1! n::Node env p::procedure arg0)
    (generic walk2! n::Node env p::procedure arg0 arg1)
    (generic walk3! n::Node env p::procedure arg0 arg1 arg2)
    (generic walk4! n::Node env p::procedure arg0 arg1 arg2 arg3)
    (macro define-nmethod)
    (macro ncall)))

;; (define-nmethod (While.optim! x y) BODY)
;; =>
;; (define-method (optim! this::While env x y)
;;   (define (default-walk! n x y)
;;      (walk2! n env optim! x y))
;;   (define (walk! n x y)
;;      (optim! n env x y))
;;   BODY)
(define-macro (define-nmethod args . body)
   (define (my-error msg val)
      (let ((loc (if (epair? args) (cer meta) #f)))
	 (match-case loc
	    ((at ?fname ?loc)
	     (error/location "walk" msg val fname loc))
	    (else
	     (error "walk" msg val)))))

   (define (without-type sym)
      (if (not (symbol? sym))
	  (my-error "bad define-nmethod (expected symbol)" args)
	  (let* ((str (symbol->string sym))
		 (pos (string-contains str "::")))
	     (if pos
		 (string->symbol (substring str 0 pos))
		 sym))))

   (let* ((Type.name (car args))
	  (method-args (cdr args))
	  (str-type.name (symbol->string Type.name))
	  (str-len (string-length str-type.name))
	  (dot-pos (string-index str-type.name #\.))
	  (dummy (when (not dot-pos)
		    (my-error "bad define-nmethod name" Type.name)))
	  (type (string->symbol (substring str-type.name 0 dot-pos)))
	  (name (string->symbol (substring str-type.name
					   (+fx dot-pos 1)
					   str-len)))
	  (bang? (char=? #\! (string-ref str-type.name (- str-len 1))))
	  (nb-method-args (length method-args))
	  (short-walk (if bang? 'walk! 'walk))
	  (long-walk (if bang?
			 (string->symbol (format "walk~a!" nb-method-args))
			 (string->symbol (format "walk~a" nb-method-args))))
	  (define-gen/met (if (eq? type 'Node)
			      'define-generic
			      'define-method))
	  (default-walk (symbol-append 'default- short-walk)))
      `(,define-gen/met	(,name ,(symbol-append 'this:: type) env ,@method-args)
	  (define (,default-walk node ,@method-args)
	     (,long-walk node env ,name ,@(map without-type method-args)))
	  (define (,short-walk node ,@method-args)
	     (,name node env ,@(map without-type method-args)))
	  ,@body)))

(define-macro (ncall method-name node . args)
   `(,method-name ,node env ,@args))
    
(define-generic (walk0 n::Node env p::procedure)
   (error "walk0"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1 n::Node env p::procedure arg0)
   (error "walk1"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2 n::Node env p::procedure arg0 arg1)
   (error "walk2"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3 n::Node env p::procedure arg0 arg1 arg2)
   (error "walk3"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4 n::Node env p::procedure arg0 arg1 arg2 arg3)
   (error "walk4"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0! n::Node env p::procedure)
   (error "walk0!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1! n::Node env p::procedure arg0)
   (error "walk1!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2! n::Node env p::procedure arg0 arg1)
   (error "walk2!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3! n::Node env p::procedure arg0 arg1 arg2)
   (error "walk3!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4! n::Node env p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!"
	  "Internal Error: forgot Node type"
	  (with-output-to-string (lambda () (write-circle n)))))

(define-macro (gen-walks class . fields)
   (define (field-name f)
      (if (pair? f)
	  (car f)
	  f))

   (define (visit f nb-args)
      (if (pair? f)
	  `(for-each (lambda (f)
			(p f env
			   ,@(map (lambda (i)
				     (string->symbol (format "arg~a" i)))
				  (iota nb-args))))
		     ,(car f))
	  `(p ,f env ,@(map (lambda (i)
			       (string->symbol (format "arg~a" i)))
			    (iota nb-args)))))

   (define (visit! f nb-args)
      (if (pair? f)
	  `(let loop ((fields ,(car f)))
	      (unless (null? fields)
		 (set-car! fields
			   (p (car fields) env
			      ,@(map (lambda (i)
					(string->symbol (format "arg~a" i)))
				     (iota nb-args))))
		 (loop (cdr fields))))
	  `(set! ,f (p ,f env ,@(map (lambda (i)
					(string->symbol (format "arg~a" i)))
				     (iota nb-args))))))
   
   (define (gen-method nb-args bang?)
      `(define-method (,(if bang?
			    (string->symbol (format "walk~a!" nb-args))
			    (string->symbol (format "walk~a" nb-args)))
		       ,(symbol-append 'n:: class)
		       env
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			      (iota nb-args)))
	  ,(if (null? fields)
	       #unspecified
	       `(,(symbol-append 'with-access:: class) n ,(map field-name fields)
	          ,@(map (lambda (f)
		      ((if bang? visit! visit) f nb-args))
		   fields)))
	  n))

   `(begin
       ,@(map (lambda (nb) (gen-method nb #f)) (iota 4))
       ,@(map (lambda (nb) (gen-method nb #t)) (iota 4))))

(gen-walks Const)
(gen-walks Ref)
(gen-walks Module body)
(gen-walks Lambda (formals) body)
(gen-walks If test then else)
(gen-walks Case key (clauses))
(gen-walks Clause (consts) expr)
(gen-walks Set! lvalue val)
(gen-walks Let (bindings) body)
(gen-walks Begin (exprs))
(gen-walks Call operator (operands))
(gen-walks Frame-alloc)
(gen-walks Frame-push body)
(gen-walks Return val)
(gen-walks Labeled body)
(gen-walks Break val)
(gen-walks Continue)
(gen-walks Pragma)
(gen-walks Tail-rec (inits) body)
(gen-walks Tail-rec-Call (updates))
(gen-walks While init test body)
(gen-walks Call/cc-Resume)

