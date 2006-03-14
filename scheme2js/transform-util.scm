;; $Id: transform-util.scm 112 2006-02-08 15:33:40Z flo $
(module transform-util
   (include "protobject.sch")
   (include "nodes.sch")
   (import protobject
	   nodes
	   symbol
	   var)
   (export (parameter-assigs operands formals vaarg take-reference?)))

(define (parameter-assigs operands formals vaarg take-reference?)
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
		    (vaarg.var.assig (new Const '()))
		    (new Set! vaarg (new Const '())))
		res))
	 
	 ;; no formals anymore, but vars left for vaarg
	 ((and (null? formals)
	       vaarg)
	  ;; create a list, and assign it to the vaarg.
	  ;; then return the whole list of assigs.
	  (let ((rvalue (new Call
			     ((id->js-var 'list).reference)
			     opnds)))
	     (cons (if take-reference?
		       (vaarg.assig rvalue)
		       (new Set! vaarg rvalue))
		   res)))
	  
	 ;; still formals and opnd-refs left.
	 (else
	  (loop (cdr opnds)
		(cdr formals)
		(cons (if take-reference?
			  ((car formals).var.assig (car opnds))
			  (new Set! (car formals) (car opnds)))
		      res))))))
