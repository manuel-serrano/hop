(module protobject
   (include "protobject.sch")
   ;; HACK: see TODO below
   (include "tools.sch")
   (export
    (class pobject
       (type::pclass read-only)
       (props read-only)
       (proto::pobject read-only (default *no-proto*)))
    (class pclass::pobject
       (name::symbol read-only)
       (constr::procedure read-only (default pobject-id)))
    PObject::pclass
    PClass::pclass
    (pfield po::pobject field)
    (pfield-set! po::pobject field val)
    (empty-pobject::pobject pc::pclass)
    (no-proto)
    (pobject-id::pobject x::pobject)
    (pfield-delete! po::pobject field)
    (create-pclass::pclass class-name::symbol constr::procedure)
    (instance-of? var type)
    (pobject-dot-out po::pobject . Lfilter)
    (pclass-constructor var::pobject)
    (inherits-from? var type)
    (pobject-name po::pobject)
    (pobject-for-each proc po::pobject)
    (pobject-clone this::pobject . Lcopy-props?)
    (pobject-deep-clone this::pobject cloned-ht)))

(define (make-props-hashtable)
   (make-hashtable #unspecified #unspecified (lambda (x y)
						(or (eq? x y)
						    (and (string? x)
							 (string? y)
							 (string=? x y))))))
(define (pobject-id::pobject x::pobject) x)

(define (no-proto) *no-proto*)

(define *no-proto* (co-instantiate
			 ((no-proto (instantiate::pclass
				       (type no-proto)
				       (name '*no-proto*)
				       (props #f)
				       (proto no-proto))))
		      no-proto))

(define PObject (co-instantiate
		      ((PObject (instantiate::pclass
				   (type PClass)
				   (name 'PObject)
				   (props (make-props-hashtable))))
		       (PClass (instantiate::pclass
				  (type PObject)
				  (name 'PClass)
				  (props (make-props-hashtable)))))
		   PObject))
(define PClass (pobject-type PObject))


(define (pfield po::pobject field)
;   (let ((t
   (with-access::pobject po (props proto)
      (or (hashtable-get props field)
	  (and (not (eq? proto *no-proto*))
	       (pfield proto field)))))
;	 )
;      (print "class: " (pclass-name (pobject-type po)))
;      (print "pfield: " field " " (if t
;				      "found"
;				      "not found"))
;      t))

(define (pfield-set! po::pobject field val)
   (with-access::pobject po (props)
      (hashtable-put! props field val)))

(define (pfield-delete! po::pobject field)
   (with-access::pobject po (props)
      (hashtable-remove! props field)))

(define (empty-pobject::pobject pc::pclass)
   (instantiate::pobject
      (type pc)
      (proto (let ((proto-prop (pfield pc 'proto)))
		(if (pobject? proto-prop)
		    proto-prop
		    *no-proto*)))
      (props (make-props-hashtable))))

(define (create-pclass::pclass class-name::symbol constr::procedure)
   (let ((o (instantiate::pclass
	       (type PClass)
	       (name class-name)
	       (props (make-props-hashtable))
	       (proto (let ((proto-prop (pfield PClass 'proto)))
			 (if (pobject? proto-prop)
			     proto-prop
			     *no-proto*)))
	       (constr constr))))
      (pfield-set! o 'proto (new PObject))
      o))

(define (instance-of? var type)
   (and (pobject? var)
	(eq? (pobject-type var) type)))

(define (pclass-constructor var::pobject)
   (pobject-type var))

(define (inherits-from? var type)
   (and (pobject? var)
	(with-access::pobject var (proto)
	   (or (eq? (pclass-constructor var) type)
	       (and (not (eq? *no-proto* proto))
		    (inherits-from? proto type))))))

(define (pobject-name po::pobject)
   (with-access::pobject po (type)
      (pclass-name type)))

(define (pobject-for-each proc po::pobject)
   (with-access::pobject po (props)
      (hashtable-for-each props proc)))

(define-pmethod (pobject-clone . Lcopy-props?)
   (let ((copy-props? (or (null? Lcopy-props?)
			  (car Lcopy-props?)))
	 (new-node (empty-pobject (pobject-type this))))
      (if copy-props?
	  (pobject-for-each (lambda (prop val)
			       (pfield-set! new-node prop val))
			    this))
      new-node))

(define-pmethod (pobject-deep-clone cloned-ht)
   (define (deep-clone val)
      (cond
	 ((hashtable-get cloned-ht val)
	  =>
	  (lambda (cloned-val)
	     cloned-val))
	 ((and (pobject? val)
	       (pfield val 'deep-clone))
	  (pcall val (pfield val 'deep-clone) cloned-ht))
	 ((hashtable? val)
	  (let ((clone (make-eq-hashtable))) ;; TODO how to know ??
	     (hashtable-put! cloned-ht val clone)
	     (hashtable-for-each val
				 (lambda (key ht-val)
				    (hashtable-put! clone
						    (deep-clone key)
						    (deep-clone ht-val))))
	     clone))
	 ((list? val)
	  (map deep-clone val))
	 ((pair? val)
	  (cons (deep-clone (car val))
		(deep-clone (cdr val))))
	 (else
	  val)))

   (or (hashtable-get cloned-ht this)
       (let ((cloned-this (empty-pobject (pobject-type this))))
	  (hashtable-put! cloned-ht this cloned-this)
	  (pobject-for-each (lambda (prop val)
			       (pfield-set! cloned-this prop (deep-clone val)))
			    this)
	  cloned-this)))

(define-method (object-display po::pobject . port)
   (if (eq? po *no-proto*)
       (display "NO-PROTO")
       (begin
	  (display "{Object ")
	  (with-access::pobject po (props proto type)
	     (with-access::pclass type (name)
		(display name)
		(display ": [")
		(hashtable-for-each
		 props
		 (lambda (key val)
		    (display key)
		    (display " : ")
		    (display val)
		    (display ", ")))
		(display "]}"))))))

(define-method (object-print po::pobject out::output-port
			     p::procedure)
   (display po))

(define (pobject-dot-out po::pobject . Lfilter)
   (define new-id
      (let ((counter 0))
	 (lambda ()
	    (set! counter (+ 1 counter))
	    (string-append "obj" (integer->string counter)))))

   (print "digraph g {")
   (print "node [shape = record];")
   (print "rankdir=LR;")
   (let ((filter (if (null? Lfilter)
		     (lambda (id) #t)
		     (car Lfilter)))
	 (ht (make-hashtable #unspecified #unspecified eq?))
	 (links '()))
      (let loop ((objs (list po)))
	 (unless (null? objs)
	    (if (not (hashtable-contains? ht (car objs)))
		(hashtable-put! ht (car objs) (new-id)))
	    (let* ((o (car objs))
		   (obj-id (hashtable-get ht o))
		   (counter 0)
		   (remaining-objs (cdr objs))
		   (proto-ht (make-hashtable #unspecified #unspecified eq?)))

	       (define (get-id o)
		  (let ((id (hashtable-get ht o)))
		     (or id
			 (let ((id (new-id)))
			    (hashtable-put! ht o id)
			    ;; clearly inefficient, but dot wouldn't work with
			    ;; a huge list anyways...
			    (set! remaining-objs (append! remaining-objs (list o)))
			    id))))

	       (define (add-link! obj-id field-id target-id . Llabel)
		  (set! links (cons (string-append obj-id
						   ":"
						   field-id
						   " -> "
						   target-id
						   (if (not (null? Llabel))
						       (string-append
							"[label=\""
							(car Llabel)
							"\"]")
						       "")
						   ";")
				    links)))

	       (define (mangle x)
		  (define (string-escape str . Lescapes)
		     (let ((char-list (string->list str)))
			(let loop ((chars char-list)
				   (rev-res '()))
			   (if (null? chars)
			       (list->string (reverse! rev-res))
			       (if (member (car chars) Lescapes)
				   (loop (cdr chars)
					 (cons (car chars) (cons #\\ rev-res)))
				   (loop (cdr chars)
					 (cons (car chars) rev-res)))))))
				   
		  (let ((str (with-output-to-string (lambda ()
						       (display x)))))
		     (string-escape str #\\ #\< #\> #\| #\{ #\})))
	       
	       (define (tostring x obj-id field-id)
		  (with-output-to-string
		     (lambda ()
			(cond
			   ((hashtable? x)
			    (display ": hashtable"))
			   ((and (pair? x) (pobject? (car x))) ;; assume pobject-list
			    (let loop ((objs x)
				       (counter 0))
			       (unless (null? objs)
				  (add-link! obj-id
					     field-id
					     (get-id (car objs))
					     (integer->string counter))
				  (loop (cdr objs)
					(+ counter 1)))))
			   ((pobject? x)
			    (add-link! obj-id field-id (get-id x)))
			   ((procedure? x)
			    (display ": procedure"))
			   (else
			    (display ": ")
			    (display (mangle x)))))))

	       (define (print-props o already-printed)
		  (with-access::pobject o (props proto)
		     (if props
			 (hashtable-for-each
			  props
			  (lambda (key val)
			     (if (and (not (hashtable-get already-printed key))
				      (filter key))
				 (let ((field-id (string-append
						  "<f"
						  (integer->string counter)
						  ">")))
				    (display* "|"
					      field-id
					      (mangle key)
					      (tostring val obj-id field-id))
				    (set! counter (+ counter 1))
				    (hashtable-put! already-printed
						    key
						    #t))))))
		     (hashtable-put! proto-ht o #t)
		     (if (not (hashtable-get proto-ht proto))
			 (print-props proto already-printed))))
		  
	       (with-access::pobject o (type)
		  (with-access::pclass type (name)
		     (display* obj-id "[label =\"" name)
		     (print-props o (make-props-hashtable))
		     ;(print "|<proto> proto")
		     ;(add-link! obj-id "<proto>" (get-id proto))
		     (print "\"];")
		     ))
	       (loop remaining-objs))))
      (for-each print links))
   (print "}"))
