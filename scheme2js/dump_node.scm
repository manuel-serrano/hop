;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/dump_node.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 18 15:15:05 2013                          */
;*    Last change :  Tue Jul 23 10:23:19 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Simple tool to debug the compiler                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dump-node
   
   (import export-desc
	   nodes)
   
   (export (generic node->list ::Node)))

;*---------------------------------------------------------------------*/
;*    *location*                                                       */
;*---------------------------------------------------------------------*/
(define *location* #t)

;*---------------------------------------------------------------------*/
;*    node->list ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node->list n::Node)
   (typeof n))

;*---------------------------------------------------------------------*/
;*    node->list ::Set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Set!)
   (with-access::Set! n (location lvalue val)
      `(set! ,@(if *location* `(:location ,location) '())
	  ,(node->list lvalue) ,(node->list val))))

;*---------------------------------------------------------------------*/
;*    node->list ::Ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Ref)
   (with-access::Ref n (location id)
      `(ref ,@(if *location* `(:location ,location) '()) ,id)))

;*---------------------------------------------------------------------*/
;*    node->list ::Begin ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Begin)
   (with-access::Begin n (location exprs)
      `(begin ,@(if *location* `(:location ,location) '())
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
      `(call ,@(if *location* `(:location ,location) '())
	  ,(node->list operator)
	  ,@(map node->list operands))))
   
;*---------------------------------------------------------------------*/
;*    node->list ::If ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node->list n::If)
   (with-access::If n (location test then else)
      `(if ,@(if *location* `(:location ,location) '())
	   ,(node->list test)
	  ,(node->list then)
	  ,(node->list else))))
   
;*---------------------------------------------------------------------*/
;*    node->list ::Return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Return)
   (with-access::Return n (location val)
      `(return ,@(if *location* `(:location ,location) '())
	  ,(node->list val))))

;*---------------------------------------------------------------------*/
;*    node->list ::Let ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->list n::Let)
   (with-access::Let n (location bindings body)
      `(let ,@(if *location* `(:location ,location) '())
	    ,(map node->list bindings)
	    ,(node->list body))))

