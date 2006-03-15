(define-macro (overload field extension bindings . Lbody)
   (let ((bindings (map (lambda (binding)
			   (if (pair? binding)
			       (list (symbol-append (car binding)
						    '.proto.
						    field)
				     (cadr binding))
			       (list (symbol-append binding
						    '.proto.
						    field)
				     (symbol-append binding '- extension))))
			bindings))
	 (result (gensym 'result)))
      `(begin
;	  ,@(map (lambda (binding)
;		    `(set! ,(car binding) (lambda L
;					     (print ,(car binding))
;					     (apply ,(cadr binding) L))))
;		 bindings)
	  ,@(map (lambda (binding)
		    `(set! ,(car binding) ,(cadr binding)))
		 bindings)
	  (let ((,result (begin
			    ,@Lbody)))
	     ,@(map (lambda (binding)
		       `(delete! ,(car binding)))
		    bindings)
	     ,result))))
