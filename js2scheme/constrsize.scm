;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/constrsize.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  1 13:36:09 2017                          */
;*    Last change :  Fri Jul  5 07:44:22 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Static approximation of constructors size                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_constrsize

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_classutils
	   __js2scheme_type-hint)

   (export j2s-constrsize-stage))

;*---------------------------------------------------------------------*/
;*    j2s-constrsize-stage ...                                         */
;*---------------------------------------------------------------------*/
(define j2s-constrsize-stage
   (instantiate::J2SStageProc
      (name "constrsize")
      (comment "Constructor static size approximation")
      (proc j2s-constrsize!)))

;*---------------------------------------------------------------------*/
;*    j2s-constrsize! ::obj ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-constrsize! this conf)
   (when (isa? this J2SProgram)
      (j2s-constrsize-program! this conf)
      (let ((log (config-get conf :profile-log #f)))
	 (when log
	    (j2s-update-constrsize-program! this log conf)))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-constrsize-program! ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-constrsize-program! this::J2SProgram args)
   (with-access::J2SProgram this (decls nodes)
      (for-each (lambda (n) (constrsize! n)) decls)
      (for-each (lambda (n) (constrsize! n)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constrsize-info ...                                              */
;*---------------------------------------------------------------------*/
(define-struct constrsize-info names)

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SFun)
   (with-access::J2SFun this (body constrsize %info generator)
      (unless (or (constrsize-info? %info) generator)
	 (let ((acc (make-cell '())))
	    (count-this-assig body acc)
	    (set! %info (constrsize-info (cell-ref acc)))
	    (set! constrsize (length (cell-ref acc))))
	 (call-default-walker)))
   this)

;*---------------------------------------------------------------------*/
;*    constrsize! ::J2SClass ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (constrsize! this::J2SClass)
   
   (define (prop-names props)
      (filter-map (lambda (p)
		     (with-access::J2SPropertyInit p (name)
			(when (isa? name J2SString)
			   (with-access::J2SString name (val)
			      val))))
	 props))

   (with-access::J2SClass this (constrsize %info name)
      (unless (constrsize-info? %info)
	 (call-default-walker)
	 (let ((props (j2s-class-instance-properties this)))
	    ;; class properties size
	    (let ((acc (make-cell (prop-names props))))
	       (let ((ctor (j2s-class-get-constructor this)))
		  ;; constructor size
		  (when ctor
		     (with-access::J2SClassElement ctor (prop)
			(with-access::J2SMethodPropertyInit prop (val)
			   (with-access::J2SFun val (body)
			      (count-this-assig body acc)))))
		  ;; super class size
		  (let ((super (j2s-class-super-val this)))
		     (when (or (isa? super J2SClass) (isa? super J2SFun))
			(constrsize! super)
			(with-access::J2SNode super (%info)
			   (unless (constrsize-info? %info)
			      (constrsize! super))
			   (cell-set! acc
			      (delete-duplicates!
				 (append (constrsize-info-names %info)
				    (cell-ref acc))))))))
	       (set! %info (constrsize-info (cell-ref acc)))
	       (set! constrsize (length (cell-ref acc)))))))
   this)


;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SNode acc::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SAssig ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SAssig acc::cell)
   (with-access::J2SAssig this (lhs rhs)
      (call-default-walker)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj field)
	    (when (isa? obj J2SThis)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (unless (member val (cell-ref acc))
			(cell-set! acc (cons val (cell-ref acc)))))))))))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SCond ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SCond acc::cell)
   (with-access::J2SCond this (test then else)
      (count-this-assig test acc)
      (count-this-assig then acc)
      (count-this-assig else acc)))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SIf ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (count-this-assig this::J2SIf acc::cell)
   (with-access::J2SIf this (test then else)
      (count-this-assig test acc)
      (count-this-assig then acc)
      (count-this-assig else acc)))

;*---------------------------------------------------------------------*/
;*    count-this-assig ::J2SCall ...                                   */
;*---------------------------------------------------------------------*/
(define-method (count-this-assig this::J2SCall acc::cell)
   (with-access::J2SCall this (protocol fun)
      (if (eq? protocol 'bounce)
	  (with-access::J2SRef fun (decl)
	     (with-access::J2SDeclFun decl (val)
		(with-access::J2SFun val (body)
		   (count-this-assig body acc))))
	  (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-update-constrsize-program! ...                               */
;*---------------------------------------------------------------------*/
(define (j2s-update-constrsize-program! this::J2SProgram logfile::bstring conf)
   (with-access::J2SProgram this (profiling decls nodes)
      (let ((prof (assq 'ctors profiling)))
	 (when (pair? prof)
	    (let ((log (cdr prof)))
	       (for-each (lambda (n) (pconstrsize! n log)) decls)
	       (for-each (lambda (n) (pconstrsize! n log)) nodes))))))

;*---------------------------------------------------------------------*/
;*    pconstrsize! ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (pconstrsize! this::J2SNode log)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    pconstrsize! ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (pconstrsize! this::J2SFun log)
   (with-access::J2SFun this (body constrsize loc)
      (match-case loc
	 ((at ?file ?point)
	  (let ((sz (find-log-constrsize log point)))
	     (when (and sz (>fx (cdr sz) constrsize))
		(set! constrsize (cdr sz))))))
      this))

;*---------------------------------------------------------------------*/
;*    find-log-constrsize ...                                          */
;*---------------------------------------------------------------------*/
(define (find-log-constrsize log point)
   (let loop ((i (-fx (vector-length log) 1)))
      (when (>=fx i 0)
	 (let* ((e (vector-ref log i))
		(p (assq 'point e)))
	    (when (pair? p)
	       (let ((v (cdr p)))
		  (cond
		     ((=fx v point)
		      (assq 'constrsize e))
		     ((>fx v point)
		      (loop (-fx i 1))))))))))
