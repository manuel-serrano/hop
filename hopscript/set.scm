;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/set.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 25 13:32:40 2019                          */
;*    Last change :  Sat Oct  2 19:23:00 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native Bigloo support of JavaScript MAP object.                  */
;*    -------------------------------------------------------------    */
;*    https://www.ecma-international.org/ecma-262/6.0/#sec-map-objects */
;*    https://developer.mozilla.org/en-US/docs/Web/JavaScript/         */
;*       Reference/Global_Objects/Map                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_set
   
   (include "../nodejs/nodejs_debug.sch")
   
   (library hop)
   
   (include "types.sch" "stringliteral.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_lib
	   __hopscript_object
	   __hopscript_function
	   __hopscript_property
	   __hopscript_private
	   __hopscript_public
	   __hopscript_regexp
	   __hopscript_array
	   __hopscript_error
	   __hopscript_generator
	   __hopscript_worker
	   __hopscript_spawn
	   __hopscript_map)
   
   (export (js-init-set! ::JsGlobalObject)
	   (js-init-weakset! ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define-inline (DEFAULT-EMPTY-VECTOR-SIZE) 4)

;*---------------------------------------------------------------------*/
;*    js-init-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-init-set! %this::JsGlobalObject)
   (init-set! %this "Set" init-builtin-set-prototype! 'none))

;*---------------------------------------------------------------------*/
;*    js-init-weakset! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-weakset! %this::JsGlobalObject)
   (init-set! %this "WeakSet" init-builtin-weakset-prototype! 'keys))

;*---------------------------------------------------------------------*/
;*    init-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-set! %this::JsGlobalObject name init-prototype! weak::symbol)

   (unless (vector? __js_strings) (set! __js_strings (&init!)))

   (with-access::JsGlobalObject %this (js-function-prototype)
      
      (define (%js-set this #!optional (iterable #\F))
	 (if (eq? (js-new-target-pop! %this) (js-undefined))
	     (js-raise-type-error %this
		(format "Constructor ~a requires 'new'" name) this)
	     (begin
		(unless (eq? iterable #\F)
		   (js-set-construct %this this iterable))
		this)))
      
      (define (js-set-alloc %this constructor::JsFunction)
	 (js-new-target-push! %this constructor)
	 (instantiateJsMap
	    (__proto__ (js-get constructor (& "prototype") %this))
	    (mapdata (create-hashtable
			:weak weak
			:hash js-get-hashnumber
			:eqtest (lambda (x y) (js-same-value-zero? x y %this))))
	    (vec (make-vector (DEFAULT-EMPTY-VECTOR-SIZE) (js-absent)))
	    (cursor -1)))
      
      (define js-set-prototype
	 (instantiateJsObject
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector (if (eq? weak 'none) 12 4)))))
      
      (define js-set
	 (js-make-function %this %js-set
	    (js-function-arity %js-set)
	    (js-function-info :name name :len 0)
	    :__proto__ js-function-prototype
	    :prototype js-set-prototype
	    :size 0
	    :alloc js-set-alloc))
      
      ;; init the prototype properties
      (init-prototype! %this js-set js-set-prototype)
      
      ;; bind SET/WEAKSET in the global object
      (js-bind! %this %this (js-ascii-name->jsstring name)
	 :configurable #f :enumerable #f :value js-set
	 :hidden-class #t)
      
      ;; @@species
      ;; https://www.ecma-international.org/ecma-262/6.0/#sec-get-set-@@species
      (with-access::JsGlobalObject %this (js-symbol-species)
	 (js-bind! %this js-set js-symbol-species
	    :get (js-make-function %this (lambda (this) js-set)
		    (js-function-arity 0 0)
		    (js-function-info :name "get [Symbol.species]" :len 0))
	    :enumerable #f
	    :configurable #t))
      
      js-set))

;*---------------------------------------------------------------------*/
;*    js-set-construct ...                                             */
;*---------------------------------------------------------------------*/
(define (js-set-construct %this this::JsMap iterable)

   (define (js-set-construct-iterator this::JsMap iter next)
      (let ((add (js-get this (& "add") %this)))
	 (let loop ()
	    (let ((ni (js-call0 %this next iter)))
	       (cond
		  ((not ni)
		   this)
		  ((js-totest (js-get ni (& "done") %this))
		   this)
		  (else
		   (let ((value (js-get ni (& "value") %this)))
		      (js-call1 %this add this value)
		      (loop))))))))
   
   (define (js-set-construct-iterable this::JsMap iterable)

      (define (close-iterator iter)
	 (let ((return (js-get iter (& "return") %this)))
	    (when (js-procedure? return)
	       (js-call0 %this return iter))))
      
      (with-access::JsGlobalObject %this (js-symbol-iterator)
	 (let ((i (js-get iterable js-symbol-iterator %this)))
	    (if (js-procedure? i)
		(let ((iter (js-call0 %this i iterable)))
		   (if (js-object? iter)
		       (let ((next (js-get iter (& "next") %this)))
			  (if (not (js-procedure? next))
			      (js-raise-type-error %this
				 "Illegal IteratorValue ~a" next)
			      (with-handler
				 (lambda (e)
				    (close-iterator iter)
				    (raise e))
				 (js-set-construct-iterator
				    this iter next))))
		       (js-raise-type-error
			  %this "Wrong iterator" iter)))
		(js-raise-type-error %this "Not an iterator" i)))))
   
   (define (js-set-construct-array this::JsMap iterable::JsArray)
      (let ((len (js-array-length iterable))
	    (add (js-get this (& "add") %this)))
	 (let loop ((i #u32:0))
	    (if (=u32 i len)
		this
		(let ((value (js-array-index-ref iterable i %this)))
		   (js-call1 %this add this value)
		   (loop (+u32 i #u32:1)))))))
   
   (cond
      ((or (eq? iterable (js-undefined)) (eq? iterable '()))
       this)
      ((js-array? iterable)
       (js-set-construct-array this iterable))
      (else
       (js-set-construct-iterable this iterable))))

;*---------------------------------------------------------------------*/
;*    js-get-hashnumber ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-get-hashnumber key)
   (if (js-jsstring? key)
       (let ((s (js-jsstring->string key)))
	  ($string-hash s 0 (string-length s)))
       (get-hashnumber key)))

;*---------------------------------------------------------------------*/
;*    init-builtin-set-prototype! ...                                  */
;*---------------------------------------------------------------------*/
(define (init-builtin-set-prototype! %this js-set js-set-prototype)

   ;; add
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.set
   (define (set-add this key)
      (if (js-set? this)
	  (js-map-set this key #t %this)  
	  (js-raise-type-error %this "not a Set" this)))
   
   (js-bind! %this js-set-prototype (& "add")
      :value (js-make-function %this set-add
		(js-function-arity set-add)
		(js-function-info :name "add" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; clear
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.clear
   (define (set-clear this key)
      (if (js-set? this)
	  (with-access::JsMap this (mapdata vec cursor)
	     (hashtable-clear! mapdata)
	     (set! vec (make-vector (DEFAULT-EMPTY-VECTOR-SIZE) (js-absent)))
	     (set! cursor 0))
	  (js-raise-type-error %this "Not a Set" this)))
   
   (js-bind! %this js-set-prototype (& "clear")
      :value (js-make-function %this set-clear
		(js-function-arity set-clear)
		(js-function-info :name "clear" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; constructor
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.constructor
   (js-bind! %this js-set-prototype (& "constructor")
      :value js-set :enumerable #f
      :hidden-class #t)

   ;; delete
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.delete
   (define (set-delete this key)
      (if (js-set? this)
	  (js-map-delete this key %this)
	  (js-raise-type-error %this "Not a Set" this)))
   
   (js-bind! %this js-set-prototype (& "delete")
      :value (js-make-function %this set-delete
		(js-function-arity 1 0)
		(js-function-info :name "delete" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; entries
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.entries
   (define (set-entries this)
      (if (js-set? this)
	  (with-access::JsMap this (vec)
	     (js-make-vector-iterator vec
		(lambda (%this val)
		   (js-vector->jsarray (vector (car val) (car val)) %this))
		%this))
	  (js-raise-type-error %this "Not a Set" this)))
      
   (js-bind! %this js-set-prototype (& "entries")
      :value (js-make-function %this set-entries
		(js-function-arity set-entries)
		(js-function-info :name "entries" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; forEach
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.foreach
   (define (js-set-for-each this fn . thisarg)
      (if (js-set? this)
	  (if (js-procedure? fn)
	      (let ((t (if (pair? thisarg) (car thisarg) (js-undefined))))
		 (with-access::JsMap this (vec)
		    (let loop ((i 0))
		       (when (<fx i (vector-length vec))
			  (let ((v (vector-ref vec i)))
			     (unless (js-absent? v)
				(js-call3 %this fn t (car v) (car v) this)))
			  (loop (+fx i 1))))))
	      (js-raise-type-error %this "Not a function" fn))
	  (js-raise-type-error %this "not a Set" this)))

   (js-bind! %this js-set-prototype (& "forEach")
      :value (js-make-function %this js-set-for-each
		(js-function-arity js-set-for-each)
		(js-function-info :name "forEach" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; has
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.has
   (define (set-has this key)
      (if (js-set? this)
	  (js-map-has this key %this)
          (js-raise-type-error %this "Not a Set" this)))

   (js-bind! %this js-set-prototype (& "has")
      :value (js-make-function %this set-has
		(js-function-arity set-has)
		(js-function-info :name "has" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; keys
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.keys
   (js-bind! %this js-set-prototype (& "keys")
      :value (js-make-function %this js-set-values
		(js-function-arity js-set-values)
		(js-function-info :name "keys" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; size
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-get-set.prototype.size
   (define (js-set-size this)
      (if (js-set? this)
	  (with-access::JsMap this (mapdata)
	     (hashtable-size mapdata))
	  (js-raise-type-error %this "Not a Set" this)))
   
   (js-bind! %this js-set-prototype (& "size")
      :get (js-make-function %this js-set-size
	      (js-function-arity js-set-size)
	      (js-function-info :name "size" :len 0)
	      :prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; values
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.values
   (define (js-set-values this)
      (if (js-set? this)
	  (with-access::JsMap this (vec)
	     (js-make-vector-iterator vec
		(lambda (%this val) (car val)) %this))
	  (js-raise-type-error %this "Not a Set" this)))
   
   (js-bind! %this js-set-prototype (& "values")
      :value (js-make-function %this js-set-values
		(js-function-arity js-set-values)
		(js-function-info :name "values" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; @@iterator
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype-@@tostringtag
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (js-bind! %this js-set-prototype js-symbol-iterator
	 :value (js-make-function %this js-set-values
		   (js-function-arity js-set-values)
		   (js-function-info :name "values" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :configurable #t))
      
   ;; @@toStringTag
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype-@@tostringtag
   (with-access::JsGlobalObject %this (js-symbol-tostringtag)
      (js-bind! %this js-set-prototype js-symbol-tostringtag
	 :value (js-ascii->jsstring "Set")
	 :enumerable #f
	 :configurable #t))
   )

;*---------------------------------------------------------------------*/
;*    init-builtin-weakset-prototype! ...                              */
;*---------------------------------------------------------------------*/
(define (init-builtin-weakset-prototype! %this js-set js-set-prototype)

   ;; constructor
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.constructor
   (js-bind! %this js-set-prototype (& "constructor")
      :value js-set :enumerable #f
      :hidden-class #t)

   ;; add
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.add
   (define (set-add this key)
      (if (js-weakset? this)
	  (js-weakset-add this key %this)
	  (js-raise-type-error %this "not a WeakSet" this)))
   
   (js-bind! %this js-set-prototype (& "add")
      :value (js-make-function %this set-add
		(js-function-arity set-add)
		(js-function-info :name "add" :len 2)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; delete
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.delete
   (define (set-delete this key)
      (if (js-weakset? this)
	  (js-weakmap-delete this key %this)
	  (js-raise-type-error %this "Not a WeakSet" this)))

   (js-bind! %this js-set-prototype (& "delete")
      :value (js-make-function %this set-delete
		(js-function-arity set-delete)
		(js-function-info :name "delete" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; has
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-set.prototype.has
   (define (set-has this key)
      (if (js-weakset? this)
	  (js-weakmap-has this key %this)
          (js-raise-type-error %this "Not a WeakSet" this)))
      
   (js-bind! %this js-set-prototype (& "has")
      :value (js-make-function %this set-has
		(js-function-arity 1 0)
		(js-function-info :name "has" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   )

;*---------------------------------------------------------------------*/
;*    js-weakset-add ...                                               */
;*---------------------------------------------------------------------*/
(define (js-weakset-add this key %this)
   (if (js-object? key)
       (js-weakmap-set this key #t %this)
       (js-raise-type-error %this "not an object" key)))
	  
;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
