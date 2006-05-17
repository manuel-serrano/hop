(module transform-util
   (include "protobject.sch")
   (include "nodes.sch")
   (option (loadq "protobject-eval.sch"))
   (import protobject
	   nodes
	   symbol
	   var)
   (export (parameter-assig-mapping operands
				    formals
				    vaarg
				    id->js-var)))

(define (parameter-assig-mapping operands formals vaarg id->js-var)
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
	  ;; just map vaarg to '(), and return the
	  ;; whole assig-list
	  (cons (cons vaarg (new-node Const '()))
		res))
	 
	 ;; no formals anymore, but vars left for vaarg
	 ((and (null? formals)
	       vaarg)
	  ;; create a list, and map vaarg to it.
	  ;; then return the whole list of pairs.
	  (let ((rvalue (new-node Call
			     ((id->js-var 'list).reference)
			     opnds)))
	     (cons (cons vaarg rvalue)
		   res)))
	  
	 ;; still formals and opnd-refs left.
	 (else
	  (loop (cdr opnds)
		(cdr formals)
		(cons (cons (car formals) (car opnds))
		      res))))))
