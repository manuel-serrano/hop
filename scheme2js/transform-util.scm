(module transform-util
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   symbol
	   var)
   (export (parameter-assigs operands
			     formals
			     vaarg
			     take-reference?
			     id->js-var)))

(define (parameter-assigs operands formals vaarg take-reference? id->js-var)
   (let loop ((opnds operands)
	      (formals formals)
	      (res '()))
      (cond
	 ;; nothing left to do.
	 ((and (null? opnds)
	       (null? formals)
	       (not vaarg))
	  res)
	 
	 ;; no opnds, but formals
	 ((and (null? opnds)
	       (not (null? formals)))
	  (error #f "Not enough arguments. " #f))
	 ;; no formals, but opnds
	 ((and (not (null? opnds))
	       (null? formals)
	       (not vaarg))
	  (error #f "Too many arguments. " #f))
	 
	 ;; both, operands and formals are done, but there's a vaarg
	 ((and (null? opnds)
	       vaarg)
	  ;; just assign '() to the vaarg, and return the
	  ;; whole assig-list
	  (cons (if take-reference?
		    (vaarg.var.assig (new-node Const '()))
		    (new-node Set! vaarg (new-node Const '())))
		res))
	 
	 ;; no formals anymore, but vars left for vaarg
	 ((and (null? formals)
	       vaarg)
	  ;; create a list, and assign it to the vaarg.
	  ;; then return the whole list of assigs.
	  (let ((rvalue (new-node Call
			     ((id->js-var 'list).reference)
			     opnds)))
	     (cons (if take-reference?
		       (vaarg.assig rvalue)
		       (new-node Set! vaarg rvalue))
		   res)))
	  
	 ;; still formals and opnd-refs left.
	 (else
	  (loop (cdr opnds)
		(cdr formals)
		(cons (if take-reference?
			  ((car formals).var.assig (car opnds))
			  (new-node Set! (car formals) (car opnds)))
		      res))))))
