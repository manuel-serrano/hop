;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/clevel.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr  2 19:46:13 2017                          */
;*    Last change :  Wed Jan 17 09:04:48 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Annotate property accesses with cache level information          */
;*    This analysis scans the AST to find property assignments and     */
;*    property declarations. In then separate properties in several    */
;*    kinds:                                                           */
;*       - Those that are known to be always defined as property       */
;*         values.                                                     */
;*       - Those that are known to be defined as accessor              */
;*         properties.                                                 */
;*       - Those that are known to be defined as prototype values.     */
;*       - Those that are likeley to be polymorphic.                   */
;*    It then set the accessor cache levels accordingly.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_clevel

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)
   
   (export j2s-clevel-stage))

;*---------------------------------------------------------------------*/
;*    j2s-clevel-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-clevel-stage
   (instantiate::J2SStageProc
      (name "clevel")
      (comment "Cache level annotation optimization")
      (proc j2s-clevel)
      (optional :optim-clevel)))

;*---------------------------------------------------------------------*/
;*    j2s-clevel ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-clevel this::obj args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls)
	 (let ((ptable (create-hashtable)))
	    (propcollect* decls ptable)
	    (propcollect* nodes ptable)
	    (propclevel* decls ptable)
	    (propclevel* nodes ptable))))
   this)

;*---------------------------------------------------------------------*/
;*    propinfo ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct propinfo get set value accessor prototype polymorphic)

;*---------------------------------------------------------------------*/
;*    propcollect* ...                                                 */
;*---------------------------------------------------------------------*/
(define (propcollect* nodes ptable)
   (for-each (lambda (n) (propcollect n ptable)) nodes))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SNode ptable)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SObjInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SObjInit ptable)
   (with-access::J2SObjInit this (inits)
      (for-each (lambda (i)
		   (with-access::J2SPropertyInit i (name)
		      (let ((pi (hashtable-get ptable name)))
			 (if (propinfo? i)
			     (propinfo-set-set! i (+fx 1 (propinfo-set i)))
			     (begin
				(set! pi (propinfo 0 1 0 0 0 0 ))
				(hashtable-put! ptable name pi)))
			 (cond
			    ((isa? i J2SDataPropertyInit)
			     (with-access::J2SDataPropertyInit i (val)
				(propcollect val ptable)))
			    ((isa? i J2SAccessorPropertyInit)
			     (with-access::J2SAccessorPropertyInit i (get set)
				(propcollect get ptable)
				(propcollect set ptable)
				(propinfo-accessor-set! pi
				   (+fx (propinfo-accessor pi) 1))))))))
	 inits)))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SAccess ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SAccess ptable)
   (call-default-walker)
   (with-access::J2SAccess this (clevel obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((i (hashtable-get ptable val)))
	       (if (propinfo? i)
		   (propinfo-get-set! i (+fx 1 (propinfo-get i)))
		   (hashtable-put! ptable val (propinfo 1 0 0 0 0 0))))))))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SAssig ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SAssig ptable)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (propcollect obj ptable)
	     (propcollect field ptable)
	     (when (isa? field J2SString)
		(with-access::J2SString field (val)
		   (let ((i (hashtable-get ptable val)))
		      (if (propinfo? i)
			  (propinfo-set-set! i (+fx 1 (propinfo-set i)))
			  (hashtable-put! ptable val (propinfo 0 1 0 0 0 0)))))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SCall ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SCall ptable)
   
   (define (defineProperty this)
      ;; a call to Object.defineProperty( obj, prop, val )
      (with-access::J2SCall this (fun args)
	 (when (and (pair? args) (pair? (cdr args))
		    (isa? (cadr args) J2SString))
	    (when (isa? fun J2SAccess)
	       (with-access::J2SAccess fun (obj field)
		  (when (and (isa? obj J2SGlobalRef) (isa? field J2SString))
		     (with-access::J2SString field (val)
			(when (string=? val "defineProperty")
			   (with-access::J2SGlobalRef obj (decl)
			      (with-access::J2SDecl decl (id)
				 (when (eq? id 'Object)
				    (with-access::J2SString (cadr args) (val)
				       val))))))))))))
      
   (call-default-walker)
   (let ((prop (defineProperty this)))
      (when (string? prop)
	 (let ((i (hashtable-get ptable prop)))
	    (if (propinfo? i)
		(propinfo-accessor-set! i (+fx 1 (propinfo-accessor i)))
		(hashtable-put! ptable prop (propinfo 0 1 0 1 0 0)))))))
	 
;*---------------------------------------------------------------------*/
;*    propclevel* ...                                                  */
;*---------------------------------------------------------------------*/
(define (propclevel* nodes ptable)
   (for-each (lambda (n) (propclevel n ptable)) nodes))

;*---------------------------------------------------------------------*/
;*    propclevel ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (propclevel this::J2SNode ptable)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propclevel ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (propclevel this::J2SAccess ptable)
   (call-default-walker)
   (with-access::J2SAccess this (clevel obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((i (hashtable-get ptable val)))
	       ;; if there is only one setter, which is not an accessor
	       ;; use simple cache here
	       (tprint "val: " val " " i)
	       (when (propinfo? i)
		  (if (and (=fx (propinfo-set i) 1)
			   (=fx (propinfo-accessor i) 0))
		      (tprint "YES val: " val " " i)
		      (set! clevel 0)))))))
   this)
