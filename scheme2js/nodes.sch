(define-macro (overload field extension bindings . Lbody)
   (let ((bindings (map (lambda (binding)
			   (if (pair? binding)
			       (list `(node ',(car binding))
				     (symbol-append '.proto.
						    field)
				     (cadr binding))
			       (list `(node ',binding)
				     (symbol-append '.proto.
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
		    `(set! ,@binding))
		 bindings)
	  (let ((,result (begin
			    ,@Lbody)))
	     ,@(map (lambda (binding)
		       `(delete! ,(car binding) ,(cadr binding)))
		    bindings)
	     ,result))))

(define-macro (new-node n . L)
   `(new (node ',n) ,@L))
