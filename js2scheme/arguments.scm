;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/arguments.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  5 09:14:00 2019                          */
;*    Last change :  Thu Dec  5 09:44:23 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Arguments optimization                                           */
;*    -------------------------------------------------------------    */
;*    This stage annotates ARGUMENTS usages so that the Scheme         */
;*    code generation can better allocates and uses this special       */
;*    variable.                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_arguments

   (include "ast.sch")
   
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
      (annotate-arguments this this))
   this)

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::obj ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (annotate-arguments this::obj parent)
   (when (pair? this)
      (for-each (lambda (n) (annotate-arguments n parent)) this)))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SNode parent)
   (vector-for-each (lambda (f)
		       (let ((info (class-field-info f)))
			  (when (and (pair? info) (member "ast" info))
			     (annotate-arguments
				((class-field-accessor f) this) this))))
      (class-all-fields (object-class this))))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SFun parent)
   (with-access::J2SFun this (body argumentsp)
      (when argumentsp
	 (with-access::J2SDeclArguments argumentsp (%info)
	    (set! %info '())))
      (annotate-arguments body parent)
      (when argumentsp
	 (with-access::J2SDeclArguments argumentsp (%info)
	    (tprint "arg %info=" %info)))))
      
;*---------------------------------------------------------------------*/
;*    annotate-arguments ...                                           */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SRef parent)
   
   (define (arguments-invalidate! decl)
      (with-access::J2SDecl decl (%info)
	 (set! %info #f)))
   
   (define (arguments-mark-usage! decl use)
      (with-access::J2SDecl decl (%info)
	 (when (and %info (not (memq use %info)))
	    (set! %info (cons use %info)))))
   
   (define (get-length? node::J2SAccess)
      (with-access::J2SAccess node (field)
	 (and (isa? field J2SString)
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
      (when (isa? decl J2SDeclArguments)
	 (cond
	    ((isa? parent J2SAccess)
	     (with-access::J2SAccess parent (field)
		(cond
		   ((memq (j2s-type field) '(integer uint32 int32 fixnum))
		    (arguments-mark-usage! decl 'getint))
		   ((get-length? parent)
		    (arguments-mark-usage! decl 'getlength))
		   (else
		    (arguments-invalidate! decl)))))
	    ((isa? parent J2SCall)
	     (if (apply? parent)
		 (arguments-mark-usage! decl 'apply)
		 (arguments-invalidate! decl)))
	    (else
	     (arguments-invalidate! decl))))))
   
