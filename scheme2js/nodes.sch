(define-macro (overload field extension orig-bindings . Lbody)
   (let* ((bindings (map (lambda (binding)
			    (if (pair? binding)
				(list `(node ',(car binding))
				      (symbol-append '.proto.
						     field)
				      (cadr binding))
				(list `(node ',binding)
				      (symbol-append '.proto.
						     field)
				      (symbol-append binding '- extension))))
			 orig-bindings))
	  (tmp-bindings (map (lambda (binding)
				(list (gensym 'tmp)
				      (car binding) (cadr binding)))
			     bindings))
	  (result (gensym 'result)))
;      `(begin
;	  ,@(map (lambda (binding)
;		    `(set! ,(car binding) (lambda L
;					     (print ,(car binding))
;					     (apply ,(cadr binding) L))))
;		 bindings)

      `(let (,@tmp-bindings)
	  ,@(map (lambda (binding)
		    `(set! ,@binding))
		 bindings)
	  (let ((,result (begin
			    ,@Lbody)))
	     ,@(map (lambda (binding)
		       `(if ,(car binding)
			    (set! ,(cadr binding) ,(caddr binding) ,(car binding))
			    (delete! ,(cadr binding) ,(caddr binding))))
		    tmp-bindings)
	     ,result))))

(define-macro (new-node n . L)
   `(new (node ',n) ,@L))
