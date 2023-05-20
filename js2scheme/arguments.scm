;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/arguments.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  5 09:14:00 2019                          */
;*    Last change :  Thu May 18 07:40:53 2023 (serrano)                */
;*    Copyright   :  2019-23 Manuel Serrano                            */
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
	 (ause this)
	 '(annotate-arguments this this #f #f)))
   this)

;*---------------------------------------------------------------------*/
;*    ause ...                                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ause this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    ause ::J2SFun ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (ause this::J2SFun)
   (with-access::J2SFun this (argumentsp params)
      (when argumentsp
	 ;; will be restored if used in a "true" ref
	 (decl-usage-rem! argumentsp 'ref)
	 (decl-usage-rem! argumentsp 'get))
      (when (pair? params)
	 (let ((lastp (car (last-pair params))))
	    (when (isa? lastp J2SDeclRest)
	       (with-access::J2SDeclRest lastp (mode)
		  (decl-usage-rem! lastp 'ref)
		  (decl-usage-rem! lastp 'get)))))
      (call-default-walker)
      '(when argumentsp
	 (with-access::J2SDeclArguments argumentsp (alloc-policy usage)
	    (cond
	       ((usage-strict? usage '(length))
		(set! alloc-policy 'lonly)))))))
				
;*---------------------------------------------------------------------*/
;*    ause ::J2SSvc ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (ause this::J2SSvc)
   (call-default-walker))
   
;*---------------------------------------------------------------------*/
;*    ause ::J2SRef ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (ause this::J2SRef)
   (with-access::J2SRef this (decl)
      (when (isa? decl J2SDeclRest)
	 (decl-usage-add! decl 'ref))))

;*---------------------------------------------------------------------*/
;*    ause ::J2SAccess ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ause this::J2SAccess)

   (define (field-length? field)
      (and (isa? field J2SString)
	   (with-access::J2SString field (val)
	      (string=? val "length"))))

   (define (field-index? field)
      (type-fixnum? (j2s-type field)))

   (with-access::J2SAccess this (obj field)
      (ause field)
      (if (isa? obj J2SRef)
	  (with-access::J2SRef obj (decl)
	     (if (isa? decl J2SDeclRest)
		 (decl-usage-add! decl
		    (cond
		       ((field-length? field) 'length)
		       ((field-index? field) 'aref)
		       (else 'get)))
		 (call-default-walker)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    ause ::J2SCall ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (ause this::J2SCall)
   
   (define (apply? fun args)
      (when (and (isa? fun J2SAccess) (=fx (length args) 2))
	 (with-access::J2SAccess fun (field)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (string=? val "apply"))))))

   (define (array-prototype? obj)
      (when (isa? obj J2SAccess)
	 (with-access::J2SAccess obj (obj field)
	    (when (isa? obj J2SRef)
	       (with-access::J2SRef obj (decl)
		  (when (isa? decl J2SDeclExtern)
		     (with-access::J2SDeclExtern decl (id)
			(when (and (eq? id 'Array) (decl-ronly? decl))
			   (when (isa? field J2SString)
			      (with-access::J2SString field (val)
				 (string=? val "prototype")))))))))))

   (define (builtin-array-prototype-slice? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (when (and (isa? decl J2SDeclInit) (decl-ronly? decl))
	       (with-access::J2SDeclInit decl (val)
		  (array-prototype-slice? val))))))
	 
   (define (array-prototype-slice? obj)
      (cond
	 ((isa? obj J2SAccess)
	  (with-access::J2SAccess obj (obj field)
	     (when (isa? field J2SString)
		(with-access::J2SString field (val)
		   (when (string=? val "slice")
		      (array-prototype? obj))))))
	 ((isa? obj J2SRef)
	  (builtin-array-prototype-slice? obj))
	 (else
	  #f)))
      
   (define (slice? fun args)
      (when (and (isa? fun J2SAccess) (>=fx (length args) 2))
	 (with-access::J2SAccess fun (obj field)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (when (string=? val "call")
		     (array-prototype-slice? obj)))))))
   
   (with-access::J2SCall this (fun args)
      (cond
	 ((apply? fun args)
	  (let ((arg1 (cadr args)))
	     (if (isa? arg1 J2SRef)
		 (with-access::J2SRef arg1 (decl)
		    (if (isa? decl J2SDeclRest)
			(begin
			   (ause fun)
			   (ause (car args))
			   (decl-usage-add! decl 'apply))
			(call-default-walker)))
		 (call-default-walker))))
	 ((slice? fun args)
	  (let ((arg0 (car args)))
	     (if (isa? arg0 J2SRef)
		 (with-access::J2SRef arg0 (decl)
		    (if (isa? decl J2SDeclRest)
			(begin
			   (ause fun)
			   (for-each ause (cdr args))
			   (decl-usage-add! decl 'slice))
			(call-default-walker)))
		 (call-default-walker))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    arguments-alias ...                                              */
;*---------------------------------------------------------------------*/
(define-struct arguments-alias arguments)

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

   (define (rtwin? node::J2SCall)
      (with-access::J2SCall node (fun)
	 (when (isa? fun J2SAccess)
	    (with-access::J2SAccess fun (obj field)
	       (when (isa? obj J2SSuper)
		  (with-access::J2SSuper obj (context super)
		     (eq? context super)))))))
   
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
		 (unless (or (apply? parent) (rtwin? parent))
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
   
