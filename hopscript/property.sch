;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/property.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Sat Feb 17 15:10:35 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*    -------------------------------------------------------------    */
;*    See expanders.sch for the actual expander bindings.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives (option (loadq "property_expd.sch")))

;*---------------------------------------------------------------------*/
;*    profiling                                                        */
;*---------------------------------------------------------------------*/
(define-expander js-profile-log-cache
   js-profile-log-cache-expander)
(define-expander js-profile-log-index
   js-profile-log-index-expander)

;*---------------------------------------------------------------------*/
;*    %define-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define-expander %define-pcache
   %define-pcache-expander)
(define-expander js-make-pcache
   js-make-pcache-expander)
(define-expander js-pcache-ref
   js-pcache-ref-expander)
(define-expander js-pcache-cmap
   js-pcache-cmap-expander)
(define-expander js-pcache-pmap
   js-pcache-pmap-expander)
(define-expander js-pcache-index
   js-pcache-index-expander)
(define-expander js-pcache-vindex
   js-pcache-vindex-expander)
(define-expander js-pcache-owner
   js-pcache-owner-expander)
(define-expander js-pcache-method
   js-pcache-method-expander)

;*---------------------------------------------------------------------*/
;*    js-get-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-get-name/cache
   js-get-name/cache-expander)
(define-expander js-object-get-name/cache
   js-object-get-name/cache-expander)
(define-expander js-global-object-get-name
   js-global-object-get-name-expander)
(define-expander js-global-object-get-name/cache
   js-global-object-get-name/cache-expander)
(define-expander js-get-length
   js-get-length-expander)

;*---------------------------------------------------------------------*/
;*    js-put-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-put-name/cache!
   js-put-name/cache-expander)
(define-expander js-object-put-name/cache!
   js-object-put-name/cache-expander)

;*---------------------------------------------------------------------*/
;*    js-call-XXX ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander js-call/cache
   js-call/cache-expander)
(define-expander js-method-call-name/cache
   js-method-call-name/cache-expander)
(define-expander js-object-method-call-name/cache
   js-object-method-call-name/cache-expander)
(define-expander js-non-object-method-call-name
   js-non-object-method-call-name-expander)
		    
;*---------------------------------------------------------------------*/
;*    js-toname                                                        */
;*---------------------------------------------------------------------*/
(define-expander js-toname
   (lambda (x e)
      (match-case x
	 ((?- (and ?n (? integer?)))
	  (string->symbol (integer->string n)))
	 ((?- ?n)
	  `((@ js-toname __hopscript_property) ,(e n e)))
	 (else
	  (map (lambda (x) (e x e)) x)))))

;*---------------------------------------------------------------------*/
;*    descr ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct prop name flags)

;*---------------------------------------------------------------------*/
;*    flag ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (flags-writable) 1)
(define-inline (flags-enumerable) 2)
(define-inline (flags-configurable) 4)
(define-inline (flags-accessor) 8)

(define (flags-writable? f)
   (=fx (bit-and f (flags-writable)) (flags-writable)))

(define (flags-enumerable? f)
   (=fx (bit-and f (flags-enumerable)) (flags-enumerable)))

(define (flags-configurable? f)
   (=fx (bit-and f (flags-configurable)) (flags-configurable)))

(define (flags-accessor? f)
   (=fx (bit-and f (flags-accessor)) (flags-accessor)))

;*---------------------------------------------------------------------*/
;*    property-flags ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (property-flags writable enumerable configurable accessor)
   `(bit-or (if ,writable (flags-writable) 0)
       (bit-or (if ,enumerable (flags-enumerable) 0)
	  (bit-or (if ,configurable (flags-configurable) 0)
	     (if ,accessor (flags-accessor) 0)))))

;*---------------------------------------------------------------------*/
;*    property-flags-default ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (property-flags-default)
   '(property-flags #t #t #t #f #f))
