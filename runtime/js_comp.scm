;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/js_comp.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 15:55:02 2005                          */
;*    Last change :  Fri Jun 26 16:04:12 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JS compilation tools                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_js-comp

   (library web)
	    
   (import  __hop_param
	    __hop_types
	    __hop_hop-inline
	    __hop_xml-types
	    __hop_xml
	    __hop_service
	    __hop_charset
	    __hop_clientc
	    __hop_read-js)

   (export  (obj->javascript-attr ::obj ::output-port)
	    (obj->javascript-expr ::obj ::output-port)
	    (generic hop-register-value ::obj ::procedure)
	    (generic hop->javascript ::obj ::output-port ::procedure ::bool)
	    (hop->js-callback ::obj)))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for i from to . Lbody)
   (let ((loop (gensym 'loop))
	 (to-tmp (gensym 'to)))
      `(let ((,to-tmp ,to))
	  (let ,loop ((,i ,from))
	       (when (<fx ,i ,to-tmp)
		  ,@Lbody
		  (,loop (+fx ,i 1)))))))

;*---------------------------------------------------------------------*/
;*    hop-register-value ...                                           */
;*    -------------------------------------------------------------    */
;*    A call back invoked by scheme2js when it compiles Hop values.    */
;*---------------------------------------------------------------------*/
(define-generic (hop-register-value o::obj register::procedure)
   '())

;*---------------------------------------------------------------------*/
;*    hop-register-value ::object ...                                  */
;*---------------------------------------------------------------------*/
(define-method (hop-register-value o::object register::procedure)
   (let ((fields (class-all-fields (object-class o))))
      (for i 0 (vector-length fields)
	 (let* ((f (vector-ref fields i))
		(iv (class-field-info f)))
	    (cond
	       ((and (pair? iv) (memq :client iv))
		=>
		(lambda (x)
		   (when (pair? (cdr x)) (register (cadr x)))))
	       (else
		(let ((gv (class-field-accessor f)))
		   (register (gv o)))))))))

;*---------------------------------------------------------------------*/
;*    hop-register-value ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hop-register-value o::xml register::procedure)
   '())

;*---------------------------------------------------------------------*/
;*    obj->javascript-attr ...                                         */
;*---------------------------------------------------------------------*/
(define (obj->javascript-attr obj op::output-port)
   
   (define (host-compiler obj op compile)
      (hop->javascript obj op compile #f))

   (with-access::clientc (hop-clientc) (valuec)
      (valuec obj op host-compiler hop-register-value #f)))

;*---------------------------------------------------------------------*/
;*    obj->javascript-expr ...                                         */
;*---------------------------------------------------------------------*/
(define (obj->javascript-expr obj op)
   
   (define (host-compiler obj op compile)
      (hop->javascript obj op compile #t))

   (with-access::clientc (hop-clientc) (valuec)
      (valuec obj op host-compiler hop-register-value #f)))

;*---------------------------------------------------------------------*/
;*    hop->javascript ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (hop->javascript obj op::output-port compile isexpr)
   (cond
      ((procedure? obj)
       (let ((attr (procedure-attr obj)))
	  (cond
	     ((service? obj)
	      (compile attr op))
	     (attr
	      (hop->javascript attr op compile isexpr))
	     (else
	      (error "hop->javascript"
		 "Illegal procedure in JavaScript conversion"
		 obj)))))
      ((number? obj)
       (display obj op))
      ((string? obj)
       (display "\"" op)
       (display (string-for-read (string-replace obj #\" #\')) op)
       (display "\"" op))
      ((boolean? obj)
       (display (if obj "true" "false") op))
      ((eq? obj #unspecified)
       (display "undefined" op))
      ((eq? obj '())
       (display "null" op))
      (else
       (error "hop->javascript"
	  (format "Cannot compile value \"~a\"" (typeof obj))
	  obj))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::object ...                                     */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript obj::object op compile isexpr)
   
   (define (display-seq vec op proc)
      (let ((len (vector-length vec)))
	 (when (>fx len 0)
	    (proc (vector-ref vec 0) op)
	    (let loop ((i 1))
	       (when (<fx i len)
		  (display "," op)
		  (proc (vector-ref vec i) op)
		  (loop (+fx i 1)))))))
   
   (define (display-fields fields op)
      (display "{ " op)
      (display-seq fields op
	 (lambda (f op)
	    (let ((iv (class-field-info f)))
	       (display "\"" op)
	       (display (class-field-name f) op)
	       (display "\": " op)
	       (cond
		  ((and (pair? iv) (memq :client iv))
		   =>
		   (lambda (x)
		      (compile (when (pair? (cdr x)) (cadr x)) op)))
		  (else 
		   (compile ((class-field-accessor f) obj) op))))))
      (display "}" op))

   (let ((klass (object-class obj)))
      (if (nil? obj)
	  (begin
	     (display "sc_class_nil(sc_class_exists(sc_string2symbol(sc_jsstring2string(\"" op)
	     (display (class-name klass) op)
	     (display "\"))))" op))
	  (let ((fields (class-all-fields klass)))
	     (display "hop_js_to_object(\""  op)
	     (display (class-name klass) op)
	     (display "\", " op)
	     (display (class-hash klass) op)
	     (display ", " op)
	     (display-fields fields op)
	     (display ")" op)))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::xml ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript obj::xml op compile isexpr)
   (error "hop->javascript" "Cannot translate xml element" xml))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::xml-markup ...                                 */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript obj::xml-markup op compile isexpr)
   (display "hop_create_encoded_element(\"" op)
   (let ((s (url-path-encode
	     (call-with-output-string
	      (lambda (op) (xml-write obj op (hop-xml-backend)))))))
      (display s op))
   (display "\")" op))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::xml-element ...                                */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript obj::xml-element op compile isexpr)
   (if isexpr
       (call-next-method)
       (with-access::xml-element obj (id)
	  (fprintf op "document.getElementById( '~a' )" id))))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::xml-tilde ...                                  */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript obj::xml-tilde op compile isexpr)
   (display (xml-tilde->expression obj) op))

;*---------------------------------------------------------------------*/
;*    hop->javascript ::hop-service ...                                */
;*---------------------------------------------------------------------*/
(define-method (hop->javascript obj::hop-service op compile isexpr)
   (with-access::hop-service obj (javascript path resource id)
      (display (format javascript path (or resource "/hop")) op)))

;*---------------------------------------------------------------------*/
;*    hop->js-callback ...                                             */
;*---------------------------------------------------------------------*/
(define (hop->js-callback obj)
   (cond
      ((isa? obj xml-tilde)
       (format "function( event ) { ~a }" (xml-tilde->return obj)))
      ((string? obj)
       (format "function( event ) { ~a }" obj))
      (else
       "false")))
