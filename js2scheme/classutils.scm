;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/classutils.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 19 16:28:44 2021                          */
;*    Last change :  Thu Aug 19 18:47:47 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Class related utility functions                                  */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_classutils
   
   (include "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump)

   (export )
   
   (export (j2s-class-super-val ::J2SClass)
	   (j2s-class-root-val ::J2SClass)
	   
	   (j2s-class-property-constructor?::bool ::J2SPropertyInit)
	   
	   (j2s-class-instance-properties ::J2SClass #!key (super #t))
	   (j2s-class-instance-get-property ::J2SClass ::bstring)
	   
	   (j2s-class-get-property ::J2SClass ::bstring)
	   (j2s-class-get-constructor ::J2SClass)
	   (j2s-class-find-constructor ::J2SClass)
	   (j2s-class-find-initializer ::J2SClass)
	   (j2s-class-methods-use-super? this::J2SClass)))

;*---------------------------------------------------------------------*/
;*    j2s-class-super-val ...                                          */
;*    -------------------------------------------------------------    */
;*    If the super class is statically known, returns the value        */
;*    of the super class. Otherwise, returns #f.                       */
;*---------------------------------------------------------------------*/
(define (j2s-class-super-val clazz::J2SClass)
   (with-access::J2SClass clazz (super)
      (when (isa? super J2SRef)
	 (with-access::J2SRef super (decl)
	    (when (isa? decl J2SDeclInit)
	       (with-access::J2SDeclInit decl (val)
		  val))))))

;*---------------------------------------------------------------------*/
;*    j2s-class-root-val ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the root of the class inheritance.                       */
;*---------------------------------------------------------------------*/
(define (j2s-class-root-val clazz::J2SClass)
   (with-access::J2SClass clazz (super)
      (let loop ((root #f))
	 (cond
	    ((or (isa? root J2SUndefined) (isa? root J2SNull))
	     root)
	    ((j2s-class-super-val clazz)
	     =>
	     (lambda (val)
		(cond
		   ((isa? super J2SClass) (loop val))
		   ((isa? super J2SFun) val)
		   (else super))))
	    (else
	     root)))))

;*---------------------------------------------------------------------*/
;*    j2s-class-property-constructor? ...                              */
;*---------------------------------------------------------------------*/
(define (j2s-class-property-constructor? prop::J2SPropertyInit)
   (when (isa? prop J2SMethodPropertyInit)
      (with-access::J2SMethodPropertyInit prop (name)
	 (let loop ((name name))
	    (cond
	       ((isa? name J2SLiteralCnst)
		(with-access::J2SLiteralCnst name (val)
		   (loop val)))
	       ((isa? name J2SLiteralValue)
		(with-access::J2SLiteralValue name (val)
		   (equal? val "constructor"))))))))

;*---------------------------------------------------------------------*/
;*    j2s-class-instance-properties ...                                */
;*    -------------------------------------------------------------    */
;*    Returns the list of the instances properties of a class. If      */
;*    SUPER is #t, it follows the inheritance links.                   */
;*---------------------------------------------------------------------*/
(define (j2s-class-instance-properties clazz #!key (super #t))

   (define (instance-element-prop el)
      (with-access::J2SClassElement el (prop static)
	 (when (and (not static)
		    (isa? prop J2SDataPropertyInit)
		    (not (isa? prop J2SMethodPropertyInit)))
	    prop)))

   (let loop ((clazz clazz))
      (with-access::J2SClass clazz (elements)
	 (let ((props (filter-map instance-element-prop elements))
	       (superclass (and super (j2s-class-super-val clazz))))
	    (if (isa? superclass J2SClass)
		(if (isa? super J2SClass)
		    (append (loop superclass) props)
		    props)
		props)))))

;*---------------------------------------------------------------------*/
;*    j2s-class-get ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-class-get clazz field instancep::bool)

   (define (class-get-field-in-class clazz idx)
      (with-access::J2SClass clazz (elements)
	 (let loop ((i idx)
		    (els elements))
	    (if (pair? els)
		(with-access::J2SClassElement (car els) (prop static)
		   (if (or (not instancep)
			   (and (not static)
				(not (isa? prop J2SMethodPropertyInit))))
		       (if (isa? prop J2SDataPropertyInit)
			   (with-access::J2SPropertyInit prop (name)
			      (with-access::J2SString name (val)
				 (if (string=? val field)
				     (values i (car els))
				     (loop (+fx i 1) (cdr els))))))
		       (loop i (cdr els))))
		(values i #f)))))

   (let loop ((clazz clazz)
	      (i 0))
      (let ((super (j2s-class-super-val clazz)))
	 (if (isa? super J2SClass)
	     (multiple-value-bind (idx el)
		(loop super i)
		(if el
		    (values idx el)
		    (class-get-field-in-class clazz idx)))
	     (class-get-field-in-class clazz i)))))

;*---------------------------------------------------------------------*/
;*    j2s-class-intance-get-property ...                               */
;*---------------------------------------------------------------------*/
(define (j2s-class-instance-get-property clazz field)
   (j2s-class-get clazz field #t))

;*---------------------------------------------------------------------*/
;*    j2s-class-get-property ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-class-get-property clazz field)
   (j2s-class-get clazz field #f))

;*---------------------------------------------------------------------*/
;*    j2s-class-get-constructor ...                                    */
;*    -------------------------------------------------------------    */
;*    Get the class constructor if any.                                */
;*---------------------------------------------------------------------*/
(define (j2s-class-get-constructor clazz::J2SClass)
   (with-access::J2SClass clazz (elements)
      (find (lambda (m)
	       (with-access::J2SClassElement m (prop static)
		  (unless static
		     (j2s-class-property-constructor? prop))))
	 elements)))

;*---------------------------------------------------------------------*/
;*    j2s-class-find-constructor ...                                   */
;*    -------------------------------------------------------------    */
;*    Find a constructor in the class or its super classes.            */
;*    The result can either be:                                        */
;*      - a constructor element                                        */
;*      - a function (if a class inherits from it)                     */
;*---------------------------------------------------------------------*/
(define (j2s-class-find-constructor clazz::J2SClass)
   (let loop ((clazz clazz))
      (cond
	 ((isa? clazz J2SClass)
	  (let ((ctor (j2s-class-get-constructor clazz)))
	     (cond
		((isa? ctor J2SClassElement)
		 ctor)
		((j2s-class-super-val clazz)
		 =>
		 (lambda (super) (loop super)))
		(else
		 #f))))
	 ((or (isa? clazz J2SUndefined) (isa? clazz J2SNull))
	  #f)
	 (else
	  clazz))))

;*---------------------------------------------------------------------*/
;*    j2s-class-find-initializer ...                                   */
;*    -------------------------------------------------------------    */
;*    Find an initializer in the class or its super classes.           */
;*    The result can either be:                                        */
;*      - a class (if instances have properties)                       */
;*      - a constructor element                                        */
;*      - a function (if a class inherits from it)                     */
;*---------------------------------------------------------------------*/
(define (j2s-class-find-initializer clazz::J2SClass)
   (let loop ((clazz clazz))
      (cond
	 ((isa? clazz J2SClass)
	  (let ((ctor (j2s-class-get-constructor clazz)))
	     (cond
		((isa? ctor J2SClassElement)
		 ctor)
		((pair? (j2s-class-instance-properties clazz :super #f))
		 clazz)
		((j2s-class-super-val clazz)
		 =>
		 loop)
		(else
		 #f))))
	 ((or (isa? clazz J2SUndefined) (isa? clazz J2SNull))
	  #f)
	 (else
	  clazz))))

;*---------------------------------------------------------------------*/
;*    j2s-class-methods-use-super? ...                                 */
;*    -------------------------------------------------------------    */
;*    True iff at least one class methods uses "super".                */
;*---------------------------------------------------------------------*/
(define (j2s-class-methods-use-super? this::J2SClass)
   (with-access::J2SClass this (elements)
      (find (lambda (m)
	       (with-access::J2SClassElement m (prop static)
		  (unless static
		     (when (isa? prop J2SMethodPropertyInit)
			(with-access::J2SMethodPropertyInit prop (val)
			   (class-method-use-super? val))))))
	 elements)))

;*---------------------------------------------------------------------*/
;*    class-method-use-super? ...                                      */
;*    -------------------------------------------------------------    */
;*    Returns true iff the function uses "super".                      */
;*---------------------------------------------------------------------*/
(define (class-method-use-super? this::J2SFun)
   (let ((cell (make-cell #f)))
      (with-access::J2SFun this (body)
	 (method-use-super? body cell)
	 (cell-ref cell))))

;*---------------------------------------------------------------------*/
;*    method-use-super? ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (method-use-super? this::J2SNode cell)
   (or (cell-ref cell)
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    method-use-super? ::J2SSuper ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (method-use-super? this::J2SSuper cell)
   (cell-set! cell #t)
   #t)

;*---------------------------------------------------------------------*/
;*    method-use-super? ::J2SFun ...                                   */
;*---------------------------------------------------------------------*/
(define-method (method-use-super? this::J2SFun cell)
   (cell-ref cell))


