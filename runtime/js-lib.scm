;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/js-lib.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 15:55:02 2005                          */
;*    Last change :  Sun May  7 16:44:11 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple JS lib                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_js-lib

   (import  __hop_param
	    __hop_types
	    __hop_xml)

   (export  (generic scheme->javascript ::obj)))

;*---------------------------------------------------------------------*/
;*    list->arguments ...                                              */
;*---------------------------------------------------------------------*/
(define (list->arguments lst)
   (let loop ((lst lst)
	      (res '()))
      (if (null? lst)
	  (apply string-append (reverse! res))
	  (loop (cdr lst)
		(cons* (if (pair? (cdr lst)) "," ")")
		       (scheme->javascript (car lst))
		       res)))))

;*---------------------------------------------------------------------*/
;*    list->array ...                                                  */
;*---------------------------------------------------------------------*/
(define (list->array lst)
   (string-append "new Array(" (list->arguments lst)))

;*---------------------------------------------------------------------*/
;*    scheme->javascript ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (scheme->javascript obj)
   (cond
      ((string? obj)
       (string-append "\"" (string-for-read obj) "\""))
      ((number? obj)
       (number->string obj))
      ((symbol? obj)
       (string-append "'" (symbol->string obj)  "'"))
      ((eq? obj #t)
       "true")
      ((eq? obj #f)
       "false")
      ((null? obj)
       "null")
      ((list? obj)
       (let ((car (scheme->javascript (car obj)))
	     (cdr (scheme->javascript (cdr obj))))
	  (format "new sc_Pair( ~a, ~a )" car cdr)))
      ((vector? obj)
       (if (=fx (vector-length obj) 0)
	   "new Array()"
	   (list->array (vector->list obj))))
      ((eq? obj #unspecified)
       "undefined")
      ((procedure? obj)
       (error 'scheme->javascript
	      "Illegal procedure in JavaScript conversion"
	      obj))
      ((date? obj)
       (format "new Date( ~a000 )" (date->seconds obj)))
      (else
       (error 'javascript "Illegal Javascript value" obj))))

;*---------------------------------------------------------------------*/
;*    scheme->javascript ::object ...                                  */
;*---------------------------------------------------------------------*/
(define-method (scheme->javascript obj::object)
   (define (list->block lst)
      (let loop ((lst lst)
		 (res '()))
	 (if (null? lst)
	     (apply string-append (reverse! res))
	     (loop (cdr lst)
		   (cons* (if (pair? (cdr lst)) "," ")")
			  (if (symbol? (car lst))
			      (symbol->string (car lst))
			      (car lst))
			  res)))))
   (let* ((klass (object-class obj))
	  (kname (symbol->string! (class-name klass)))
	  (name (if (bigloo-need-mangling? kname)
		    (bigloo-mangle kname)
		    kname))
	  (fields (class-all-fields klass)))
      (format "function ~a( ~a { ~a }; new ~a( ~a"
	      name
	      (list->block (map class-field-name fields))
	      (apply string-append
		     (map (lambda (f)
			     (let ((n (class-field-name f)))
				(format "this.~a = ~a;" n n)))
			  fields))
	      name
	      (list->arguments (map (lambda (f) ((class-field-accessor f) obj))
				    fields)))))
   
;*---------------------------------------------------------------------*/
;*    scheme->javascript ::xml ...                                     */
;*---------------------------------------------------------------------*/
(define-method (scheme->javascript obj::xml)
   (format "document.getElementById( \"~a\" )" (xml-id obj)))

;*---------------------------------------------------------------------*/
;*    scheme->javascript ...                                           */
;*---------------------------------------------------------------------*/
(define-method (scheme->javascript obj::hop-service)
   (hop-service-javascript obj))
