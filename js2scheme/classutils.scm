;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/classutils.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 19 16:28:44 2021                          */
;*    Last change :  Fri Oct  1 11:08:13 2021 (serrano)                */
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

   (export (class-info-name::bstring ::J2SClass)
	   (class-constructor-id::symbol ::J2SClass)
	   (class-class-id::symbol ::J2SClass)
	   (class-prototype-id::symbol ::J2SClass)
	   (class-predicate-id::symbol ::J2SClass)
	   (class-element-id::symbol ::J2SClass ::J2SClassElement)
	   (class-method-id::symbol ::J2SClass ::J2SClassElement)
	   
	   (j2s-class-super-val ::J2SClass)
	   (j2s-class-root-val ::J2SClass)
	   
	   (j2s-class-property-constructor?::bool ::J2SPropertyInit)
	   
	   (j2s-class-instance-properties ::J2SClass #!key (super #t))
	   (j2s-class-instance-get-property ::J2SClass ::bstring)
	   (j2s-class-instance-get-property-index ::J2SClass ::bstring)
	   (j2s-class-static-methods::pair-nil ::J2SClass)
	   (j2s-class-methods::pair-nil ::J2SClass #!key (super #t))

	   (j2s-class-find-element ::J2SClass ::bstring #!key (super #t))
	   (j2s-class-element-private? ::J2SClassElement)
	   
	   (j2s-class-get-property ::J2SClass ::bstring)
	   (j2s-class-get-constructor ::J2SClass)
	   (j2s-class-find-constructor ::J2SClass)
	   (j2s-class-find-initializer ::J2SClass)
	   
	   (j2s-class-constructor-might-return?::bool ::J2SClass)
	   (j2s-class-methods-use-super?::bool ::J2SClass)

	   (class-new-target?::bool ::J2SClass)
	   
	   (class-sort-class-methods! ::J2SClass)
	   (class-class-method-index ::J2SClass ::J2SClassElement)
	   (class-instance-property-index ::J2SClass ::J2SClassElement)

	   (class-private-element-access this::J2SAccess)))

;*---------------------------------------------------------------------*/
;*    class-info-name ...                                              */
;*---------------------------------------------------------------------*/
(define (class-info-name clazz::J2SClass)
   (with-access::J2SClass clazz (name loc)
      (if name
	  (symbol->string! name)
	  (format "~a:~a" (cadr loc) (caddr loc)))))

;*---------------------------------------------------------------------*/
;*    class-constructor-id ...                                         */
;*---------------------------------------------------------------------*/
(define (class-constructor-id::symbol clazz::J2SClass)
   (with-access::J2SClass clazz (name loc)
      (if name
	  (symbol-append '@ name '%CTOR)
	  (string->symbol (format "@~a:~a%CTOR" (cadr loc) (caddr loc))))))

;*---------------------------------------------------------------------*/
;*    class-class-id ...                                               */
;*---------------------------------------------------------------------*/
(define (class-class-id::symbol clazz::J2SClass)
   (with-access::J2SClass clazz (name loc)
      (if name
	  (symbol-append '@ name '%CLASS)
	  (string->symbol (format "@~a:~a%CLASS" (cadr loc) (caddr loc))))))

;*---------------------------------------------------------------------*/
;*    class-prototype-id ...                                           */
;*---------------------------------------------------------------------*/
(define (class-prototype-id::symbol clazz::J2SClass)
   (with-access::J2SClass clazz (name loc)
      (if name
	  (symbol-append '@ name '%PROTOTYPE)
	  (string->symbol (format "@~a:~a%PROTOTYPE" (cadr loc) (caddr loc))))))

;*---------------------------------------------------------------------*/
;*    class-predicate-id ...                                           */
;*---------------------------------------------------------------------*/
(define (class-predicate-id::symbol clazz::J2SClass)
   (with-access::J2SRecord clazz (name)
      (symbol-append 'js- name '?)))

;*---------------------------------------------------------------------*/
;*    class-element-id ...                                             */
;*---------------------------------------------------------------------*/
(define (class-element-id::symbol clazz::J2SClass el::J2SClassElement)
   (with-access::J2SClass clazz ((classname name))
      (with-access::J2SClassElement el (prop)
	 (with-access::J2SMethodPropertyInit prop (val name)
	    (cond
	       ((isa? name J2SString)
		(with-access::J2SString name ((str val))
		   (string->symbol
		      (string-append str "@"
			 (symbol->string! classname)))))
	       (else
		(let ((idx (class-class-method-index clazz el)))
		   (string->symbol
		      (string-append "%" (integer->string idx) "@"
			 (symbol->string! classname))))))))))

;*---------------------------------------------------------------------*/
;*    class-method-id ...                                              */
;*---------------------------------------------------------------------*/
(define (class-method-id::symbol clazz::J2SClass el::J2SClassElement)
   (symbol-append '! (class-element-id clazz el)))

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
   (let ((super (j2s-class-super-val clazz)))
      (cond
	 ((or (isa? super J2SUndefined) (isa? super J2SNull))
	  #f)
	 ((isa? super J2SClass)
	  (let loop ((clazz super)
		     (root super)
		     (stack (list clazz)))
	     (with-access::J2SClass clazz (super name loc)
		(cond
		   ((or (isa? clazz J2SUndefined) (isa? clazz J2SNull))
		    root)
		   ((j2s-class-super-val clazz)
		    =>
		    (lambda (val)
		       (cond
			  ((memq val stack)
			   (raise
			      (instantiate::&io-parse-error
				 (proc "hopc (symbol)")
				 (msg "Cyclic inheritances")
				 (obj name)
				 (fname (cadr loc))
				 (location (caddr loc)))))
			  ((isa? val J2SClass)
			   (loop val root (cons val stack)))
			  ((isa? val J2SFun)
			   root)
			  (else
			   root))))
		   (else
		    root)))))
	 (else
	  super))))

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
		    (isa? prop J2SPropertyInit)
		    (not (isa? prop J2SMethodPropertyInit)))
	    prop)))

   (let loop ((clazz clazz))
      (with-access::J2SClass clazz (elements)
	 (let ((props (filter-map instance-element-prop elements))
	       (superclass (and super (j2s-class-super-val clazz))))
	    (if (and super (isa? superclass J2SClass))
		(append (loop superclass) props)
		props)))))

;*---------------------------------------------------------------------*/
;*    j2s-class-static-methods ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-class-static-methods clazz)
   (with-access::J2SClass clazz (elements)
      (filter (lambda (el)
		 (with-access::J2SClassElement el (prop static)
		    (and static (isa? prop J2SMethodPropertyInit))))
	 elements)))
   
;*---------------------------------------------------------------------*/
;*    j2s-class-methods ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-class-methods clazz #!key (super #t))
   
   (define (class-method? el)
      (with-access::J2SClassElement el (prop static)
	 (and (not static)
	      (isa? prop J2SMethodPropertyInit)
	      (not (j2s-class-property-constructor? prop)))))
   
   (let loop ((clazz clazz))
      (with-access::J2SClass clazz (elements)
	 (let ((els (filter class-method? elements))
	       (superclass (and super (j2s-class-super-val clazz))))
	    (if (and super (isa? superclass J2SClass))
		(append (loop superclass) els)
		els)))))

;*---------------------------------------------------------------------*/
;*    j2s-class-find-element ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-class-find-element clazz field #!key (super #t))
   
   (define (class-get-field-in-class clazz)
      (with-access::J2SClass clazz (elements)
	 (find (lambda (el)
		  (with-access::J2SClassElement el (prop)
		     (with-access::J2SPropertyInit prop (name)
			(when (isa? name J2SString)
			   (with-access::J2SString name (val)
			      (string=? val field))))))
	    elements)))
   
   (let loop ((clazz clazz))
      (let ((super (j2s-class-super-val clazz)))
	 (if (isa? super J2SClass)
	     (or (loop super) (class-get-field-in-class clazz))
	     (class-get-field-in-class clazz)))))

;*---------------------------------------------------------------------*/
;*    j2s-class-element-private? ...                                   */
;*---------------------------------------------------------------------*/
(define (j2s-class-element-private? this::J2SClassElement)
   (with-access::J2SClassElement this (prop)
      (with-access::J2SPropertyInit prop (name)
	 (when (isa? name J2SString)
	    (with-access::J2SString name (private)
		  private)))))

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
		       (with-access::J2SPropertyInit prop (name)
			  (let ((ni (if (isa? prop J2SAccessorPropertyInit)
					i
					(+fx i 1))))
			     (if (isa? name J2SString)
				 (with-access::J2SString name (val)
				    (if (string=? val field)
					(values i (car els))
					(loop ni (cdr els))))
				 (loop ni (cdr els)))))
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
;*    j2s-class-instance-get-property-index ...                        */
;*---------------------------------------------------------------------*/
(define (j2s-class-instance-get-property-index clazz field)
   (multiple-value-bind (index el)
      (j2s-class-instance-get-property clazz field)
      (when (isa? el J2SClassElement)
	 (with-access::J2SClassElement el (prop)
	    (unless (or (isa? prop J2SAccessorPropertyInit)
			(isa? prop J2SMethodPropertyInit))
	       index)))))

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

;*---------------------------------------------------------------------*/
;*    j2s-class-constructor-might-return? ...                          */
;*---------------------------------------------------------------------*/
(define (j2s-class-constructor-might-return? this::J2SClass)
   (let ((ctor (j2s-class-get-constructor this)))
      (when ctor
	 (with-access::J2SClassElement ctor (prop)
	    (with-access::J2SMethodPropertyInit prop (val)
	       (with-access::J2SFun val (need-bind-exit-return)
		  (class-method-use-return? val)))))))

;*---------------------------------------------------------------------*/
;*    class-method-use-return? ...                                     */
;*    -------------------------------------------------------------    */
;*    Returns true iff the function uses "return".                     */
;*---------------------------------------------------------------------*/
(define (class-method-use-return? this::J2SFun)
   (let ((cell (make-cell #f)))
      (with-access::J2SFun this (body)
	 (method-use-return? body this cell)
	 (cell-ref cell))))

;*---------------------------------------------------------------------*/
;*    method-use-return? ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (method-use-return? this::J2SNode fun cell)
   (or (cell-ref cell)
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    method-use-return? ::J2SReturn ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (method-use-return? this::J2SReturn fun cell)
   (with-access::J2SReturn this (expr from)
      (when (and (not (isa? expr J2SUndefined)) (eq? from fun))
	 (cell-set! cell #t)
	 #t)))

;*---------------------------------------------------------------------*/
;*    method-use-return? ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (method-use-return? this::J2SFun fun cell)
   (cell-ref cell))

;*---------------------------------------------------------------------*/
;*    class-new-target? ...                                            */
;*    -------------------------------------------------------------    */
;*    This predicates is #f iff the class constructor NEEDs            */
;*    to bind new-target. It is conservative, in doubt it returns #t.  */
;*---------------------------------------------------------------------*/
(define (class-new-target?::bool this::J2SClass)

   (define (function-new-target?::bool this::J2SFun)
      (with-access::J2SFun this (new-target)
	 (when (memq new-target '(global argument)) #t)))
   
   (define (expr-new-target? val::J2SExpr)
      (cond
	 ((isa? val J2SClass)
	  (class-new-target? val))
	 ((isa? val J2SFun)
	  (function-new-target? val))
	 ((isa? val J2SParen)
	  (with-access::J2SParen val (expr)
	     (expr-new-target? expr)))
	 (else
	  #t)))
   
   (define (super-new-target? super)
      (cond
	 ((or (isa? super J2SUndefined) (isa? super J2SNull))
	  #f)
	 ((isa? super J2SRef)
	  (with-access::J2SRef super (decl)
	     (if (isa? decl J2SDeclInit)
		 (with-access::J2SDeclInit decl (val)
		    (expr-new-target? val))
		 #t)))
	 (else
	  (expr-new-target? super))))

   (with-access::J2SClass this (super)
      (let ((ctor (j2s-class-get-constructor this)))
	 (if (isa? ctor J2SClassElement)
	     (with-access::J2SClassElement ctor (prop)
		(with-access::J2SMethodPropertyInit prop (val)
		   (or (expr-new-target? val)
		       (super-new-target? super))))
	     (super-new-target? super)))))

;*---------------------------------------------------------------------*/
;*    class-sort-class-methods! ...                                    */
;*    -------------------------------------------------------------    */
;*    Walk over all inherited classes to assign class methods          */
;*    indexes.                                                         */
;*---------------------------------------------------------------------*/
(define (class-sort-class-methods! this::J2SClass)

   (define (assoc-index val lst)
      (let loop ((lst lst)
		 (index 0))
	 (cond
	    ((null? lst)
	     (values #f index))
	    ((string=? (caar lst) val)
	     (values (car lst) index))
	    (else
	     (loop (cdr lst) (+fx index 1))))))
   
   (define (sort-methods this::J2SRecord)
      ;; returns a list of : (name method1 method2 ... methodn)
      ;; where method1 ... methodn are all the class method implementations
      (let loop ((els (j2s-class-methods this))
		 (res '()))
	 (if (null? els)
	     (reverse! res)
	     (let ((el (car els)))
		(with-access::J2SClassElement el (prop index)
		   (with-access::J2SPropertyInit prop (name)
		      (if (isa? name J2SString)
			  (with-access::J2SString name (val)
			     (multiple-value-bind (old idx)
				(assoc-index val res)
				(set! index idx)
				(cond
				   ((pair? old)
				    (set-cdr! (last-pair old) (list el))
				    (loop (cdr els) res))
				   (else
				    (loop (cdr els) (append! res (list (list val el))))))))
			  (loop (cdr els) res))))))))

   (with-access::J2SClass this (methods name)
      (unless (pair? methods)
	 (set! methods (sort-methods this)))
      methods))

;*---------------------------------------------------------------------*/
;*    class-sort-instance-properties! ...                              */
;*    -------------------------------------------------------------    */
;*    Walk over all inherited classes to assign instance property      */
;*    indexes.                                                         */
;*---------------------------------------------------------------------*/
(define (class-sort-instance-properties! clazz)
   
   (define (class-get-field-in-class clazz idx)
      (with-access::J2SClass clazz (elements)
	 (let loop ((i idx)
		    (els elements))
	    (if (pair? els)
		(with-access::J2SClassElement (car els) (prop static index)
		   (if (or (and (not static)
				(isa? prop J2SPropertyInit)
				(not (isa? prop J2SMethodPropertyInit))))
		       (begin
			  (set! index i)
			  (loop (+fx i 1) (cdr els)))
		       (loop i (cdr els))))
		i))))
   
   (let loop ((clazz clazz)
	      (i 0))
      (let ((super (j2s-class-super-val clazz)))
	 (if (isa? super J2SClass)
	     (let ((idx (loop super i)))
		(class-get-field-in-class clazz idx))
	     (class-get-field-in-class clazz i)))))

;*---------------------------------------------------------------------*/
;*    class-class-method-index ...                                     */
;*---------------------------------------------------------------------*/
(define (class-class-method-index this::J2SClass el::J2SClassElement)
   (with-access::J2SClassElement el (index)
      (when (<fx index 0)
	 (class-sort-class-methods! this))
      index))
      
;*---------------------------------------------------------------------*/
;*    class-instance-property-index ...                                */
;*---------------------------------------------------------------------*/
(define (class-instance-property-index this::J2SClass el::J2SClassElement)
   (with-access::J2SClassElement el (index)
      (when (<fx index 0)
	 (class-sort-instance-properties! this))
      index))
   
;*---------------------------------------------------------------------*/
;*    class-private-element-access ...                                 */
;*    -------------------------------------------------------------    */
;*    If this is an access to a private class element, returns that    */
;*    elements. Returns #f otherwise.                                  */
;*---------------------------------------------------------------------*/
(define (class-private-element-access this::J2SAccess)
   (with-access::J2SAccess this (field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (private)
	    (when (isa? private J2SClassElement)
	       private)))))
