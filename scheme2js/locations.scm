(module locations
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (locations tree::pobject))
   (import protobject
	   config
	   verbose
	   nodes))

(define (locations tree)
   (define *file-lines-table* (make-hashtable))
   
   (define (get-file-lines file)
      (let ((lines (hashtable-get *file-lines-table* file)))
	 (if (not lines)
	     (let ((lines (file-lines file)))
		(hashtable-put! *file-lines-table* file lines)
		lines)
	     lines)))
   
   (define (pos->line file pos)
      (and (string? file)
	   (not (string=? file "[stdin]"))
	   (with-exception-handler
	      (lambda (exc)
		 #f)
	      (lambda ()
		 (let ((lines (get-file-lines file)))
		    (let ((r (file-position->line pos lines)))
		       (unless r
			  (tprint "*** WARNING:pos->list: NO LINE NUMBER " file " " pos))
		       r))))))
   
   (define (loc->line loc)
      (match-case loc
	 ((at ?fname ?pos)
	  (pos->line fname pos))
	 ((at ?fname ?pos ?lnum)
	  (warning "find-location" "obsolete `at' format -- " loc)
	  (pos->line fname pos))
	 (else
	  #f)))

   (define (multi-traverse n children)
      (let loop ((children children)
		 (loc/line (if n.loc
			       (cons n.loc (loc->line n.loc))
			       #f)))
	 (if (null? children)
	     (set! n.loc/line loc/line)
	     (let ((child-loc/line ((car children).traverse)))
		(loop (cdr children)
		      (or loc/line child-loc/line)))))
      n.loc/line)

   (define-pmethod (Node-loc)
      (let ((child-loc/line (this.traverse0)))
	 (if this.loc
	     (set! this.loc/line (cons this.loc (loc->line this.loc)))
	     (set! this.loc/line child-loc/line)))
      this.loc/line)

   (define-pmethod (Lambda-loc)
      (multi-traverse this (if this.vaarg
			       (append this.formals (list this.vaarg
							  this.body))
			       (append this.formals (list this.body)))))
   
   (define-pmethod (If-loc)
      (multi-traverse this (list this.test this.then this.else)))

   (define-pmethod (Case-loc)
      (multi-traverse this (cons this.key this.clauses)))

   (define-pmethod (Clause-loc)
      (multi-traverse this (append this.consts (list this.expr))))

   (define-pmethod (Begin-loc)
      (multi-traverse this this.exprs))

   (define-pmethod (Bind-exit-loc)
      (multi-traverse this (list this.escape
				 this.body
				 this.result-decl
				 this.invoc-body)))
   
   (define-pmethod (With-handler-loc)
      (multi-traverse this (list this.exception
				 this.catch
				 this.body)))

   (define-pmethod (Set!-loc)
      (multi-traverse this (list this.lvalue this.val)))

   (define-pmethod (Call-loc)
      (multi-traverse this (cons this.operator this.operands)))

   (define-pmethod (While-loc)
      (multi-traverse this (list this.test this.body)))

   (when (config 'print-locations)
      (verbose " locations")
      (overload traverse loc (Node
			      Lambda
			      If
			      Case
			      Clause
			      Begin
			      Bind-exit
			      With-handler
			      Set!
			      Call
			      While)
		(delete! (node 'Node).proto.default-traverse-value)
		(tree.traverse))))
