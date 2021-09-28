;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/map.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 25 13:32:40 2019                          */
;*    Last change :  Tue Sep 28 12:01:51 2021 (serrano)                */
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
(module __hopscript_map
   
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
	   __hopscript_spawn)
   
   (export (js-init-map! ::JsGlobalObject)
	   (js-init-weakmap! ::JsGlobalObject)
	   (js-map-has::bool ::JsMap ::obj ::JsGlobalObject)
	   (js-map-get ::JsMap ::obj ::JsGlobalObject)
	   (js-weakmap-get ::JsMap ::obj ::JsGlobalObject)
	   (js-map-set ::JsMap ::obj ::obj ::JsGlobalObject)
	   (js-weakmap-set ::JsMap ::obj ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    global parameters                                                */
;*---------------------------------------------------------------------*/
(define-inline (DEFAULT-EMPTY-VECTOR-SIZE) 16)

;*---------------------------------------------------------------------*/
;*    js-init-map! ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-init-map! %this::JsGlobalObject)
   (init-map! %this "Map" init-builtin-map-prototype! 'none))

;*---------------------------------------------------------------------*/
;*    js-init-weakmap! ...                                             */
;*---------------------------------------------------------------------*/
(define (js-init-weakmap! %this::JsGlobalObject)
   (init-map! %this "WeakMap" init-builtin-weakmap-prototype! 'keys))

;*---------------------------------------------------------------------*/
;*    init-map! ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-map! %this::JsGlobalObject name::bstring init-prototype! weak::symbol)

   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   
   (with-access::JsGlobalObject %this (js-function-prototype)
      
      (define (%js-map this #!optional (iterable #\F))
	 (if (eq? (js-new-target-pop! %this) (js-undefined))
	     (js-raise-type-error %this
		(format "Constructor ~a requires 'new'" name) this)
	     (begin
		(unless (eq? iterable #\F)
		   (js-map-construct %this this iterable))
		this)))
      
      (define (js-map-alloc %this constructor::JsFunction)
	 (js-new-target-push! %this constructor)
	 (instantiateJsMap
	    (__proto__ (js-get constructor (& "prototype") %this))
	    (mapdata (create-hashtable
			:weak weak
			:hash js-get-hashnumber
			:eqtest (lambda (x y) (js-same-value-zero? x y %this))))
	    (vec (if (eq? weak 'keys)
		     '#()
		     (make-vector (DEFAULT-EMPTY-VECTOR-SIZE) (js-absent))))
	    (cursor -1)))
      
      (define js-map-prototype
	 (instantiateJsObject
	    (__proto__ (js-object-proto %this))
	    (elements ($create-vector (if (eq? weak 'none) 13 5)))))
      
      (define js-map
	 (js-make-function %this %js-map
	    (js-function-arity %js-map)
	    (js-function-info :name name :len 0)
	    :__proto__ js-function-prototype
	    :prototype js-map-prototype
	    :size 0
	    :alloc js-map-alloc))
      
      ;; init the prototype properties
      (init-prototype! %this js-map js-map-prototype)
      
      ;; bind MAP/WEAKMAP in the global object
      (js-bind! %this %this (js-ascii-name->jsstring name)
	 :configurable #f :enumerable #f :value js-map
	 :hidden-class #t)
      
      ;; @@species
      ;; https://www.ecma-international.org/ecma-262/6.0/#sec-get-map-@@species
      (with-access::JsGlobalObject %this (js-symbol-species)
	 (js-bind! %this js-map js-symbol-species
	    :get (js-make-function %this (lambda (this) js-map)
		    (js-function-arity 0 0)
		    (js-function-info :name "get [Symbol.species]" :len 0))
	    :enumerable #f
	    :configurable #t))
      
      js-map))

;*---------------------------------------------------------------------*/
;*    js-map-construct ...                                             */
;*---------------------------------------------------------------------*/
(define (js-map-construct %this this::JsMap iterable)

   (define (close-iterator iter)
      (let ((return (js-get iter (& "return") %this)))
	 (when (js-procedure? return)
	    (js-call0 %this return iter))))
   
   (define (js-map-construct-iterator this::JsMap iter next)
      (let ((set (js-get this (& "set") %this)))
	 (let loop ()
	    (let ((ni (js-call0 %this next iter)))
	       (cond
		  ((not ni)
		   this)
		  ((not (js-object? ni))
		   (js-raise-type-error %this "Illegal IteratorValue" ni))
		  (else
		   (if (js-totest (js-get ni (& "done") %this))
		       this
		       (let ((value (js-get ni (& "value") %this)))
			  (if (js-object? value)
			      (let ((k (js-get value (& "0") %this))
				    (v (js-get value (& "1") %this)))
				 (js-call2 %this set this k v)
				 (loop))
			      (js-raise-type-error %this "Illegal IteratorValue" value))))))))))
   
   (define (js-map-construct-iterable this::JsMap iterable)
      (with-access::JsGlobalObject %this (js-symbol-iterator)
	 (let ((i (js-get iterable js-symbol-iterator %this)))
	    (if (js-procedure? i)
		(let ((iter (js-call0 %this i iterable)))
		   (if (js-object? iter)
		       (let ((next (js-get iter (& "next") %this)))
			  (if (not (js-procedure? next))
			      (js-raise-type-error %this
				 "Illegal IteratorValue" next)
			      (with-handler
				 (lambda (e)
				    (close-iterator iter)
				    (raise e))
				 (js-map-construct-iterator
				    this iter next))))
		       (js-raise-type-error
			  %this "Wrong iterator" iter)))
		(js-raise-type-error %this "Not an iterator" i)))))
   
   (define (js-map-construct-array this::JsMap iterable::JsArray)
      (let ((len (js-array-length iterable))
	    (set (js-get this (& "set") %this)))
	 (let loop ((i #u32:0))
	    (if (=u32 i len)
		this
		(let ((value (js-array-index-ref iterable i %this)))
		   (if (not (js-object? value))
		       (js-raise-type-error %this "Illegal IteratorValue"
			  i)
		       (let ((k (js-get value (& "0") %this))
			     (v (js-get value (& "1") %this)))
			  (js-call2 %this set this k v)
			  (loop (+u32 i #u32:1)))))))))
   
   (cond
      ((or (eq? iterable (js-undefined)) (eq? iterable '()))
       this)
      ((js-array? iterable)
       (js-map-construct-array this iterable))
      (else
       (js-map-construct-iterable this iterable))))

;*---------------------------------------------------------------------*/
;*    js-get-hashnumber ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (js-get-hashnumber key)
   (if (js-jsstring? key)
       (let ((s (js-jsstring->string key)))
	  ($string-hash s 0 (string-length s)))
       (get-hashnumber key)))

;*---------------------------------------------------------------------*/
;*    hashkey ...                                                      */
;*---------------------------------------------------------------------*/
(define (hashkey key)
   (cond
;*       ((js-jsstring? key) (js-jsstring->string key))                */
      ((and (flonum? key) (=fl key 0.0) (=fx (signbitfl key) 1)) 0.0)
      (else key)))

;*---------------------------------------------------------------------*/
;*    init-builtin-map-prototype! ...                                  */
;*---------------------------------------------------------------------*/
(define (init-builtin-map-prototype! %this js-map js-map-prototype)

   ;; clear
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.clear
   (define (map-prototype-clear this key)
      (if (isa? this JsMap)
	  (with-access::JsMap this (mapdata vec cursor)
	     (hashtable-clear! mapdata)
	     (set! vec (make-vector (DEFAULT-EMPTY-VECTOR-SIZE) (js-absent)))
	     (set! cursor 0))
	  (js-raise-type-error %this "Not a Map" this)))
   
   (js-bind! %this js-map-prototype (& "clear")
      :value (js-make-function %this map-prototype-clear
		(js-function-arity map-prototype-clear)
		(js-function-info :name "clear" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; constructor
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.constructor
   (js-bind! %this js-map-prototype (& "constructor")
      :value js-map :enumerable #f
      :hidden-class #t)

   ;; delete
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.delete
   (js-bind! %this js-map-prototype (& "delete")
      :value (js-make-function %this (js-map-delete %this)
		(js-function-arity 1 0)
		(js-function-info :name "delete" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; entries
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.entries
   (define (js-map-entries this)
      (if (isa? this JsMap)
	  (with-access::JsMap this (vec)
	     (js-make-vector-iterator vec
		(lambda (%this val)
		   (js-vector->jsarray (vector (car val) (cdr val)) %this))
		%this))
	  (js-raise-type-error %this "Not a Map" this)))
      
   (js-bind! %this js-map-prototype (& "entries")
      :value (js-make-function %this js-map-entries
		(js-function-arity js-map-entries)
		(js-function-info :name "entries" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; forEach
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.foreach
   (define (js-map-for-each this fn . thisarg)
      (if (isa? this JsMap)
	  (if (js-procedure? fn)
	      (let ((t (if (pair? thisarg) (car thisarg) (js-undefined))))
		 (with-access::JsMap this (vec)
		    (let loop ((i 0))
		       (when (<fx i (vector-length vec))
			  (let ((v (vector-ref vec i)))
			     (unless (js-absent? v)
				(js-call3 %this fn t (cdr v) (car v) this)))
			  (loop (+fx i 1))))))
	      (js-raise-type-error %this "Not a function" fn))
	  (js-raise-type-error %this "Not a Map" this)))

   (js-bind! %this js-map-prototype (& "forEach")
      :value (js-make-function %this js-map-for-each
		(js-function-arity js-map-for-each)
		(js-function-info :name "forEach" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; get
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.get
   (define (map-get this key)
      (if (isa? this JsMap)
	  (js-map-get this key %this)
	  (js-raise-type-error %this "Not a Map" this)))

   (js-bind! %this js-map-prototype (& "get")
      :value (js-make-function %this map-get
		(js-function-arity map-get)
		(js-function-info :name "get" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; has
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.has
   (define (map-has this key)
      (if (isa? this JsMap)
          (with-access::JsMap this (mapdata)
             (hashtable-contains? mapdata (hashkey key)))
          (js-raise-type-error %this "Not a Map" this)))

   (js-bind! %this js-map-prototype (& "has")
      :value (js-make-function %this map-has
		(js-function-arity map-has)
		(js-function-info :name "has" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; keys
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.keys
   (define (js-map-keys this)
      (if (isa? this JsMap)
	  (with-access::JsMap this (vec)
	     (js-make-vector-iterator vec (lambda (%this val) (car val)) %this))
	  (js-raise-type-error %this "Not a Map" this)))
      
   (js-bind! %this js-map-prototype (& "keys")
      :value (js-make-function %this js-map-keys
		(js-function-arity js-map-keys)
		(js-function-info :name "keys" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; set
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.set
   (define (map-set this key value)
      (if (isa? this JsMap)
	  (js-map-set this key value %this)
	  (js-raise-type-error %this "Not a Map" this)))
   
   (js-bind! %this js-map-prototype (& "set")
      :value (js-make-function %this map-set
		(js-function-arity map-set)
		(js-function-info :name "set" :len 2)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; size
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-get-map.prototype.size
   (define (js-map-size this)
      (if (isa? this JsMap)
	  (with-access::JsMap this (mapdata)
	     (hashtable-size mapdata))
	  (js-raise-type-error %this "Not a Map" this)))
   
   (js-bind! %this js-map-prototype (& "size")
      :get (js-make-function %this js-map-size
	      (js-function-arity js-map-size)
	      (js-function-info :name "size" :len 0)
	      :prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   ;; values
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.values
   (define (js-map-values this)
      (if (isa? this JsMap)
	  (with-access::JsMap this (vec)
	     (js-make-vector-iterator vec (lambda (%this val) (cdr val)) %this))
	  (js-raise-type-error %this "Not a Map" this)))
      
   ;; @@iterator
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype-@@tostringtag
   (with-access::JsGlobalObject %this (js-symbol-iterator)
      (js-bind! %this js-map-prototype js-symbol-iterator
	 :value (js-make-function %this js-map-entries
		   (js-function-arity js-map-entries)
		   (js-function-info :name "entries" :len 0)
		   :prototype (js-undefined))
	 :enumerable #f
	 :configurable #t))
      
   ;; @@toStringTag
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype-@@tostringtag
   (with-access::JsGlobalObject %this (js-symbol-tostringtag)
      (js-bind! %this js-map-prototype js-symbol-tostringtag
	 :value (js-ascii->jsstring "Map")
	 :enumerable #f
	 :configurable #t))
      
   (js-bind! %this js-map-prototype (& "values")
      :value (js-make-function %this js-map-values
		(js-function-arity js-map-values)
		(js-function-info :name "values" :len 0)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)
   
   )

;*---------------------------------------------------------------------*/
;*    init-builtin-weakmap-prototype! ...                              */
;*---------------------------------------------------------------------*/
(define (init-builtin-weakmap-prototype! %this js-map js-map-prototype)

   ;; constructor
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.constructor
   (js-bind! %this js-map-prototype (& "constructor")
      :value js-map :enumerable #f
      :hidden-class #t)

   ;; delete
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.delete
   (js-bind! %this js-map-prototype (& "delete")
      :value (js-make-function %this (js-map-delete %this)
		(js-function-arity 1 0)
		(js-function-info :name "delete" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; get
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.get
   (define (js-map-get this key)
      (if (isa? this JsMap)
	  (js-weakmap-get this key %this)
	  (js-raise-type-error %this "Not a Map" this)))

   (js-bind! %this js-map-prototype (& "get")
      :value (js-make-function %this js-map-get
		(js-function-arity js-map-get)
		(js-function-info :name "get" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; has
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.has
   (define (map-has this key)
      (if (isa? this JsMap)
	  (js-map-has this key %this)
          (js-raise-type-error %this "Not a Map" this)))
      
   (js-bind! %this js-map-prototype (& "has")
      :value (js-make-function %this map-has
		(js-function-arity map-has)
		(js-function-info :name "has" :len 1)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t)

   ;; set
   ;; https://www.ecma-international.org/ecma-262/6.0/#sec-map.prototype.set
   (define (map-set this key value)
      (if (isa? this JsMap)
	  (js-weakmap-set this key value %this)
	  (js-raise-type-error %this "Not a Map" this)))
   
   (js-bind! %this js-map-prototype (& "set")
      :value (js-make-function %this map-set
		(js-function-arity map-set)
		(js-function-info :name "set" :len 2)
		:prototype (js-undefined))
      :enumerable #f
      :hidden-class #t))

;*---------------------------------------------------------------------*/
;*    js-map-delete ...                                                */
;*    -------------------------------------------------------------    */
;*    https://www.ecma-international.org/ecma-262/6.0/                 */
;*       #sec-map.prototype.delete                                     */
;*---------------------------------------------------------------------*/
(define (js-map-delete %this)
   (lambda (this key)
      (if (isa? this JsMap)
	  (with-access::JsMap this (mapdata vec)
	     (let ((idx (hashtable-get mapdata key)))
		(when idx
		   (hashtable-remove! mapdata key)
		   (vector-set! vec idx (js-absent))
		   #t)))
	  (js-raise-type-error %this "Not a Map" this))))

;*---------------------------------------------------------------------*/
;*    js-map-has ...                                                   */
;*    -------------------------------------------------------------    */
;*    https://www.ecma-international.org/ecma-262/6.0/                 */
;*       #sec-map.prototype.has                                        */
;*---------------------------------------------------------------------*/
(define (js-map-has::bool this::JsMap key %this::JsGlobalObject)
   (if (js-object-mode-plain? this)
       (with-access::JsMap this (mapdata)
	  (hashtable-contains? mapdata (hashkey key)))
       (js-call1 %this
	  (js-get this (& "has") %this)
	  this key)))

;*---------------------------------------------------------------------*/
;*    js-map-get ...                                                   */
;*---------------------------------------------------------------------*/
(define (js-map-get this::JsMap key %this::JsGlobalObject)
   (if (js-object-mode-plain? this)
       (with-access::JsMap this (mapdata vec)
	  (let ((idx (hashtable-get mapdata (hashkey key))))
	     (if (fixnum? idx)
		 (cdr (vector-ref vec idx))
		 (js-undefined))))
       (js-call1 %this
	  (js-get this (& "get") %this)
	  this key)))

;*---------------------------------------------------------------------*/
;*    js-weakmap-get ...                                               */
;*---------------------------------------------------------------------*/
(define (js-weakmap-get this::JsMap key %this::JsGlobalObject)
   (if (js-object-mode-plain? this)
       (with-access::JsMap this (mapdata)
	  (or (hashtable-get mapdata (hashkey key))
	      (js-undefined)))
       (js-call1 %this
	  (js-get this (& "get") %this)
	  this key)))

;*---------------------------------------------------------------------*/
;*    js-map-set ...                                                   */
;*---------------------------------------------------------------------*/
(define (js-map-set this::JsMap key value %this::JsGlobalObject)
   
   (define (get-index! this::JsMap)
      (with-access::JsMap this (vec cursor)
	 (let ((idx (+fx cursor 1)))
	    (set! cursor idx)
	    (if (<fx idx (vector-length vec))
		idx
		(let ((nvec (copy-vector vec (*fx (vector-length vec) 2))))
		   (vector-fill! nvec (js-absent) (vector-length vec))
		   (set! vec nvec)
		   idx)))))
   
   (if (js-object-mode-plain? this)
       (with-access::JsMap this (mapdata vec)
	  (let ((k (hashkey key)))
	     (let ((idx (hashtable-get mapdata (hashkey k))))
		(if (fixnum? idx)
		    (set-cdr! (vector-ref vec idx) value)
		    (let ((idx (get-index! this)))
		       (hashtable-put! mapdata k idx)
		       (vector-set! vec idx (cons k value)))))
	     this))
       (js-call2 %this
	  (js-get this (& "set") %this)
	  this key value)))

;*---------------------------------------------------------------------*/
;*    js-weakmap-set ...                                               */
;*---------------------------------------------------------------------*/
(define (js-weakmap-set this::JsMap key value %this::JsGlobalObject)
   (if (js-object-mode-plain? this)
       (with-access::JsMap this (mapdata vec)
	  (hashtable-put! mapdata (hashkey key) value)
	  this)
       (js-call2 %this
	  (js-get this (& "set") %this)
	  this key value)))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
