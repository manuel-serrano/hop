;*=====================================================================*/
;*    serrano/prgm/project/hop/3.4.x/js2scheme/arguments.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  5 09:14:00 2019                          */
;*    Last change :  Thu May 13 09:46:35 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arguments optimization                                           */
;*    -------------------------------------------------------------    */
;*    This stage annotates ARGUMENTS and OPTIONAL arguments usages     */
;*    so that the Scheme code generation can better allocate and       */
;*    use this special variable.                                       */
;*    -------------------------------------------------------------    */
;*    This analysis is "smart" enough to keep track of "arguments"     */
;*    that are merely aliases to local variable (see arguments-alias). */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_arguments

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_node-size
	   __js2scheme_alpha)

   (export j2s-arguments-stage))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-arguments-stage
   (instantiate::J2SStageProc
      (name "arguments")
      (comment "Arguments optimization (annotation)")
      (optional :optim-arguments)
      (proc j2s-arguments)))

;*---------------------------------------------------------------------*/
;*    j2s-arguments ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-arguments this conf)
   (when (isa? this J2SProgram)
      (unless (> (config-get conf :debug 0) 0)
	 (annotate-arguments this this #f #f)))
   this)

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::obj ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (annotate-arguments this::obj parent lhs::bool arguments)
   (when (pair? this)
      (for-each (lambda (n) (annotate-arguments n parent lhs arguments)) this)))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SNode parent lhs arguments)
   (vector-for-each (lambda (f)
		       (let ((info (class-field-info f)))
			  (when (and (pair? info) (member "ast" info))
			     (annotate-arguments
				((class-field-accessor f) this) this
				(or lhs
				    (and (eq? (class-field-name f) 'lhs)
					 (isa? f J2SAssig)))
				arguments))))
      (class-all-fields (object-class this))))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SFun parent lhs arguments)
   (with-access::J2SFun this (body argumentsp mode params loc)
      (when (and argumentsp
		 (or (memq mode '(strict hopscript)) (null? params)))
	 (with-access::J2SDeclArguments argumentsp (alloc-policy)
	    (set! alloc-policy 'lazy)))
      (when (pair? params)
	 (let ((decl (car (last-pair params))))
	    (when (isa? decl J2SDeclRest)
	       (with-access::J2SDeclRest decl (alloc-policy)
		  (set! alloc-policy 'lazy)))))
      (annotate-arguments body parent #f arguments)))

;*---------------------------------------------------------------------*/
;*    arguments-alias ...                                              */
;*---------------------------------------------------------------------*/
(define-struct arguments-alias arguments)

;*---------------------------------------------------------------------*/
;*    annotate-arguments ...                                           */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SRef parent lhs arguments)
   
   (define (arguments-invalidate! decl)
      (with-access::J2SDeclRest decl (alloc-policy)
	 (set! alloc-policy 'eager)))
   
   (define (get-length? node::J2SAccess)
      (with-access::J2SAccess node (field)
	 (and (not lhs)
	      (isa? field J2SString)
	      (with-access::J2SString field (val)
		 (string=? val "length")))))
   
   (define (apply? node::J2SCall)
      (with-access::J2SCall node (fun)
	 (when (isa? fun J2SAccess)
	    (with-access::J2SAccess fun (field)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (string=? val "apply")))))))
   
   (with-access::J2SRef this (decl)
      (let loop ((decl decl))
	 (cond
	    ((isa? decl J2SDeclRest)
	     (cond
		((isa? parent J2SAccess)
		 (with-access::J2SAccess parent (field)
		    (unless (or (memq (j2s-type field)
				   '(integer uint32 int32 fixnum int53))
				(get-length? parent))
		       (arguments-invalidate! decl))))
		((isa? parent J2SCall)
		 (unless (apply? parent)
		    (arguments-invalidate! decl)))
		((isa? parent J2SDeclInit)
		 (if (decl-usage-strict? parent '(init get))
		     (with-access::J2SDecl parent (%info)
			(unless (arguments-alias arguments)
			   (set! %info (arguments-alias arguments))))
		     (arguments-invalidate! decl)))
		(else
		 (arguments-invalidate! decl))))
	    ((isa? decl J2SDecl)
	     (with-access::J2SDecl decl (%info)
		(when (arguments-alias? %info)
		   (loop (arguments-alias-arguments %info)))))))))
   
