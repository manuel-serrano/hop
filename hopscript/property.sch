;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/property.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Thu Apr  9 07:16:09 2020 (serrano)                */
;*    Copyright   :  2016-21 Manuel Serrano                            */
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
;*    define-jseval                                                    */
;*---------------------------------------------------------------------*/
(define-expander define-jseval
   js-define-jseval-expander)

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
(define-expander js-make-pcache-table
   js-make-pcache-table-expander)
(define-expander js-pcache-ref
   js-pcache-ref-expander)
(define-expander js-pcache-imap
   js-pcache-imap-expander)
(define-expander js-pcache-cmap
   js-pcache-cmap-expander)
(define-expander js-pcache-emap
   js-pcache-emap-expander)
(define-expander js-pcache-pmap
   js-pcache-pmap-expander)
(define-expander js-pcache-nmap
   js-pcache-nmap-expander)
(define-expander js-pcache-xmap
   js-pcache-xmap-expander)
(define-expander js-pcache-iindex
   js-pcache-iindex-expander)
(define-expander js-pcache-eindex
   js-pcache-eindex-expander)
(define-expander js-pcache-cindex
   js-pcache-cindex-expander)
(define-expander js-pcache-pindex
   js-pcache-pindex-expander)
(define-expander js-pcache-nindex
   js-pcache-nindex-expander)
(define-expander js-pcache-aindex
   js-pcache-aindex-expander)
(define-expander js-pcache-vindex
   js-pcache-vindex-expander)
(define-expander js-pcache-owner
   js-pcache-owner-expander)
(define-expander js-pcache-method
   js-pcache-method-expander)

;*---------------------------------------------------------------------*/
;*    js-getprototypeof ...                                            */
;*---------------------------------------------------------------------*/
(define-expander js-getprototypeof
   js-getprototypeof-expander)

;*---------------------------------------------------------------------*/
;*    js-has-own-property ...                                          */
;*---------------------------------------------------------------------*/
(define-expander js-has-own-property
   js-has-own-property-expander)

;*---------------------------------------------------------------------*/
;*    js-get-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-get-name/cache
   js-get-name/cache-expander)
(define-expander js-get-jsobject-name/cache
   js-get-jsobject-name/cache-expander)
(define-expander js-global-object-get-name/cache
   js-global-object-get-name/cache-expander)
(define-expander js-get-length
   js-get-length-expander)

;*---------------------------------------------------------------------*/
;*    js-put-XXX ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander js-put-name/cache!
   js-put-name/cache-expander)
(define-expander js-put-jsobject-name/cache!
   js-put-jsobject-name/cache-expander)

;*---------------------------------------------------------------------*/
;*    js-call-XXX ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander js-call/cache
   js-call/cache-expander)
(define-expander js-method-call-name/cache
   js-method-call-name/cache-expander)
(define-expander js-method-jsobject-call-name/cache
   js-method-jsobject-call-name/cache-expander)
(define-expander js-method-non-jsobject-call-name
   js-method-non-jsobject-call-name-expander)
		    
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
   '(property-flags #t #t #t #f))

	   
