;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/dump_node.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 18 15:15:05 2013                          */
;*    Last change :  Mon Aug 19 08:30:52 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Simple tool to debug the compiler                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dump-node
   
   (import export-desc
	   nodes
	   tail
	   mark-statements
	   push-declarations)
   
   (export (generic node->list ::obj)))

;*---------------------------------------------------------------------*/
;*    *location*                                                       */
;*---------------------------------------------------------------------*/
(define *location* #f)
(define *verbose* #f)

;*---------------------------------------------------------------------*/
;*    node->list ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node->list n::obj)
   (string-append "#|" (typeof n) "|"))

;*---------------------------------------------------------------------*/
;*    node->list ::Var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->list v::Var)
   (with-access::Var v (id)
      (if *verbose* `(var ,id) id)))

;*---------------------------------------------------------------------*/
;*    node->list ::Const ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->list v::Const)
   (with-access::Const v (value location)
      (if *verbose*
	  `(const ,@(if *location* `(:location ,location) '()) ,value)
	  value)))

;*---------------------------------------------------------------------*/
;*    node->list ::Set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Set!)
   (with-access::Set! n (location lvalue val)
      `(,(class-name (object-class n))
	,@(if *location* `(:location ,location) '())
	,(node->list lvalue) ,(node->list val))))

;*---------------------------------------------------------------------*/
;*    node->list ::Decl-Set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Decl-Set!)
   (with-access::Set! n (location lvalue val)
      `(define ,@(if *location* `(:location ,location) '())
	  ,(node->list lvalue) ,(node->list val))))

;*---------------------------------------------------------------------*/
;*    node->list ::Ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Ref)
   (with-access::Ref n (location id)
      (let ((id (if (pair? id) (cons '@ id) id)))
	 (if *verbose*
	     `(ref ,@(if *location* `(:location ,location) '()) ,id)
	     id))))

;*---------------------------------------------------------------------*/
;*    node->list ::Begin ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Begin)
   (with-access::Begin n (location exprs)
      `(,(class-name (object-class n))
	,@(if *location* `(:location ,location) '())
	  ,@(map node->list exprs))))

;*---------------------------------------------------------------------*/
;*    node->list ::Lambda ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Lambda)
   (with-access::Lambda n (location formals body)
      `(lambda ,@(if *location* `(:location ,location) '())
	       ,(map node->list formals)
	       ,(node->list body))))

;*---------------------------------------------------------------------*/
;*    node->list ::Call ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Call)
   (with-access::Call n (location operator operands)
      `(,@(if *verbose* '(call) '())
	  ,@(if *location* `(:location ,location) '())
	  ,(node->list operator)
	  ,@(map node->list operands))))
   
;*---------------------------------------------------------------------*/
;*    node->list ::Tail-Call ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Tail-Call)
   (with-access::Tail-Call n (location operator operands)
      `(tail-call ,@(if *location* `(:location ,location) '())
	  ,@(if *location* `(:location ,location) '())
	  ,(node->list operator)
	  ,@(map node->list operands))))

;*---------------------------------------------------------------------*/
;*    node->list ::Stmt-Call ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Call)
   (with-access::Call n (location operator operands stmt-operands)
      `(Stmt-Call ,@(if *verbose* '(call) '())
	  ,@(if *location* `(:location ,location) '())
	  ,(node->list operator)
	  ,@(map node->list operands))))
   
;*---------------------------------------------------------------------*/
;*    node->list ::If ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node->list n::If)
   (with-access::If n (location test then else)
      `(,(class-name (object-class n))
	,@(if *location* `(:location ,location) '())
	,(node->list test)
	,(node->list then)
	,(node->list else))))
   
;*---------------------------------------------------------------------*/
;*    node->list ::Return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Return)
   (with-access::Return n (location val)
      (if *verbose*
	  `(return ,@(if *location* `(:location ,location) '())
	      ,(node->list val))
	  (node->list val))))

;*---------------------------------------------------------------------*/
;*    node->list ::Let ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Let)
   (with-access::Let n (location bindings body kind)
      `(,kind ,@(if *location* `(:location ,location) '())
	    ,(map (lambda (b)
		     (if (isa? b Set!)
			 (with-access::Set! b (lvalue val)
			    (with-access::Ref lvalue (var)
			       (list (node->list var) (node->list val))))
			 (node->list b)))
		bindings)
	    ,(node->list body))))

;*---------------------------------------------------------------------*/
;*    node->list ::Labeled ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Labeled)
   (with-access::Labeled n (location body label)
      `(labeled ,@(if *location* `(:location ,location) '())
	  ,(node->list label)
	  ,(node->list body))))

;*---------------------------------------------------------------------*/
;*    node->list ::Tail-rec ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Tail-rec)
   (with-access::Tail-rec n (location inits body label)
      `(tail-rec ,@(if *location* `(:location ,location) '())
	  ,(node->list label)
	  (begin
	     ,@(map node->list inits))
	  ,(node->list body))))

;*---------------------------------------------------------------------*/
;*    node->list ::Tail-rec-Call ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Tail-rec-Call)
   (with-access::Tail-rec-Call n (location updates label)
      `(tail-rec-call ,@(if *location* `(:location ,location) '())
	  ,(node->list label)
	  ,@(map node->list updates))))

;*---------------------------------------------------------------------*/
;*    node->list ::Label ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Label)
   (with-access::Label n (id)
      (if *verbose*
	  `(label ,id)
	  id)))

;*---------------------------------------------------------------------*/
;*    node->list ::Frame-push ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Frame-push)
   (with-access::Frame-push n (location body)
      `(frame-push ,@(if *location* `(:location ,location) '())
	  ,(node->list body))))
	 
;*---------------------------------------------------------------------*/
;*    node->list ::Frame-alloc ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Frame-alloc)
   (with-access::Frame-alloc n (location vars)
      `(frame-alloc ,@(if *location* `(:location ,location) '())
	  ,(map node->list vars))))
	 
;*---------------------------------------------------------------------*/
;*    node->list ::While ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->list n::While)
   (with-access::While n (location init test body label)
      `(while ,@(if *location* `(:location ,location) '())
	  :init ,(node->list init)
	  :test ,(node->list test)
	  ,(node->list body))))

;*---------------------------------------------------------------------*/
;*    node->list ::Break ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Break)
   (with-access::Break n (location val label)
      `(break ,@(if *location* `(:location ,location) '())
	  ,(node->list val)
	  ,(node->list label))))

;*---------------------------------------------------------------------*/
;*    node->list ::Continue ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Continue)
   (with-access::Continue n (location label)
      `(Continue ,@(if *location* `(:location ,location) '())
	  ,(node->list label))))

;*---------------------------------------------------------------------*/
;*    node->list ::Pragma ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Pragma)
   (with-access::Pragma n (location str args)
      `(Pragma ,@(if *location* `(:location ,location) '())
	  ,str ,@(map node->list args))))
