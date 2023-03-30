;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/property.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Thu Mar 30 13:26:17 2023 (serrano)                */
;*    Copyright   :  2016-23 Manuel Serrano                            */
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
(define-expander js-pcache-rewrite-miss-label
   js-pcache-rewrite-miss-label-expander)
(define-expander js-pcache-rewrite-hit
   js-pcache-rewrite-hit-expander)
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
(define-expander js-pcache-nextemap
   js-pcache-nextemap-expander)
(define-expander js-pcache-nextnmap
   js-pcache-nextnmap-expander)
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

(define-expander js-pcache-point-set!
   js-pcache-point-set-expander)

(define-expander js-record-cache-check-proto-method
   js-record-cache-check-proto-method-expander)
(define-expander js-record-cmap-cache-check-proto-method
   js-record-cmap-cache-check-proto-method-expander)
(define-expander js-object-cache-check-proto-method
   js-object-cache-check-proto-method-expander)
(define-expander js-object-cmap-cache-check-proto-method
   js-object-cmap-cache-check-proto-method-expander)

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
(define-expander js-get-lengthu32
   js-get-lengthu32-expander)
(define-expander js-get-length-maybe-string
   js-get-length-maybe-string-expander)
(define-expander js-get-lengthu32-maybe-string
   js-get-lengthu32-maybe-string-expander)
(define-expander js-get-length-maybe-arguments
   js-get-length-maybe-arguments-expander)
(define-expander js-get-lengthu32-maybe-arguments
   js-get-lengthu32-maybe-arguments-expander)

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
(define-expander js-method-jsrecord-call-index
   js-method-jsrecord-call-index-expander)

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
(define-inline (flags-inline) 16)

(define (flags-writable? f)
   (=fx (bit-and f (flags-writable)) (flags-writable)))

(define (flags-enumerable? f)
   (=fx (bit-and f (flags-enumerable)) (flags-enumerable)))

(define (flags-configurable? f)
   (=fx (bit-and f (flags-configurable)) (flags-configurable)))

(define (flags-accessor? f)
   (=fx (bit-and f (flags-accessor)) (flags-accessor)))

(define (flags-inline? f)
   (=fx (bit-and f (flags-inline)) (flags-inline)))

;*---------------------------------------------------------------------*/
;*    property-flags ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (property-flags writable enumerable configurable accessor inline)
   `(bit-or (if ,writable (flags-writable) 0)
       (bit-or (if ,enumerable (flags-enumerable) 0)
	  (bit-or (if ,configurable (flags-configurable) 0)
	     (bit-or (if ,accessor (flags-accessor) 0)
		(if ,inline (flags-inline) 0))))))

;*---------------------------------------------------------------------*/
;*    property-flags-default ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (property-flags-default)
   '(property-flags #t #t #t #f #f))

	   
