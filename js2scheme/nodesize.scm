;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/nodesize.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 24 07:26:29 2017                          */
;*    Last change :  Mon Feb  7 08:34:44 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute an AST size (used when inlining)                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_node-size

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (export (generic node-size::long ::obj)))

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define FUN-TAX 3)
(define IF-TAX 2)
(define CALL-TAX 3)
(define ACCESS-TAX 3)
(define LOOP-TAX 80)
(define STMT-TAX 1)
(define LETBLOCK-TAX 1)
(define EXPR-TAX 1)
(define SUPER-TAX 100000)

;*---------------------------------------------------------------------*/
;*    node-size ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (node-size::long this::obj)
   (cond
      ((null? this) 0)
      ((pair? this) (apply + (map node-size this)))
      (else 1)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SNode)
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1))
		 (s 0))
	 (if (=fx i -1)
	     s
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (loop (-fx i 1)
		       (+fx (node-size ((class-field-accessor f) this)) s))
		    (loop (-fx i 1)
		       s)))))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SProgram)
   (with-access::J2SProgram this (decls nodes)
      (apply + (append (map node-size decls) (map node-size nodes)))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SSeq)
   (with-access::J2SSeq this (nodes)
      (apply + (map node-size nodes))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SStmt ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SStmt)
   (+fx STMT-TAX (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SReturn)
   (with-access::J2SReturn this (expr)
      ;; an extra weight as be counter when declarting the function
      (node-size expr)))
	 
;*---------------------------------------------------------------------*/
;*    node-size ::J2SLetBLock ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls)
      (+ LETBLOCK-TAX (apply + (map node-size decls)) (call-next-method))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SIf)
   (+fx IF-TAX (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SStmtExpr)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SNop ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SNop)
   0)

;*---------------------------------------------------------------------*/
;*    node-size ::J2SExpr ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SExpr)
   (+fx EXPR-TAX (call-next-method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SSuper ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SSuper)
   SUPER-TAX)

;*---------------------------------------------------------------------*/
;*    node-size ::J2SStmtExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SStmtExpr)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SFun)
   (with-access::J2SFun this (params)
      ;; 3 = 1 (return) + 1 (frame) + 1 (sp)
      (+ FUN-TAX (call-next-method) (length params))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SMethod)
   (with-access::J2SMethod this (method)
      (node-size method)))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SCall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SCall)
   (with-access::J2SCall this (fun args)
      ;; (length args) for the potential spills
      (+ CALL-TAX
	 (node-size fun) (length args)
	 (apply + (map node-size args)))))

;*---------------------------------------------------------------------*/
;*    node-size ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SAccess)
   (with-access::J2SAccess this (obj field)
      (+ ACCESS-TAX (node-size obj) (node-size field))))
		  
;*---------------------------------------------------------------------*/
;*    node-size ::J2SLoop ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size this::J2SLoop)
   (+ LOOP-TAX (call-next-method)))
