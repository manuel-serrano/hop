
(directives
;   (import protobject)
    )

(define-macro (pcall o f . args)
   `(,f ,o ,@args))

(define-macro (define-pclass . L)
   (match-case L
      (((?class-name . ?args) . ?body)
       (let ((constr-props (gensym 'props)))
	  `(define ,class-name
	      (create-pclass ',class-name
			     ,(if (null? body)
				  'pobject-id
				  `(pmethod ,args ,@body))))))
      (else (error "define-pclass" "invalid syntax" (cons define-pclass L)))))

(define-macro (pmethod . L)
   (match-case L
      ((?args . ?body)
       `(lambda ,(cons 'this::pobject args) ,@body))
      (else (error "pmethod" "invalid syntax" (cons pmethod
						    L)))))

(define-macro (define-pmethod signature . body)
   (match-case signature
      ((?name . ?args)
       `(define ,(cons name (cons 'this::pobject args)) ,@body))
      (else (error "define-pmethod" "invalid syntax" (cons signature body)))))

(define-macro (new pc . L)
   (let ((new-object (gensym 'new-object)))
      `(let ((,new-object (empty-pobject ,pc)))
	  (with-access::pclass ,pc (constr)
	     (and constr (constr ,new-object ,@L)))
	  ,new-object)))
