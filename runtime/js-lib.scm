;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/js-lib.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 15:55:02 2005                          */
;*    Last change :  Thu Feb  2 16:14:54 2006 (serrano)                */
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
;*    scheme->javascript ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (scheme->javascript obj)
   (define (list->array lst)
      (let loop ((lst lst)
		 (res '()))
	 (if (null? lst)
	     (apply string-append "new Array(" (reverse! res))
	     (loop (cdr lst)
		   (cons* (if (pair? (cdr lst)) "," ")")
			  (scheme->javascript (car lst))
			  res)))))
   (cond
      ((string? obj)
       (string-append "\"" obj "\""))
      ((number? obj)
       (number->string obj))
      ((symbol? obj)
       (string-append "'" (symbol->string obj)  "'"))
      ((eq? obj #t)
       "true")
      ((eq? obj #f)
       "false")
      ((null? obj)
       "new Array()")
      ((list? obj)
       (list->array obj))
      ((vector? obj)
       (if (=fx (vector-length obj) 0)
	   "new Array()"
	   (list->array (vector->list obj))))
      ((eq? obj #unspecified)
       "undefined")
      ((hop-request-service? obj)
       (hop-request-service-javascript obj))
      ((hop-request-filter? obj)
       (string-append (hop-filter-base) "/" (hop-request-filter-url obj)))
      ((procedure? obj)
       (error 'scheme->javascript
	      "Illegal procedure in JavaScript conversion"
	      obj))
      ((date? obj)
       (format "new Date( ~a000 )" (date->seconds obj)))
      (else
       (error 'javascript "Illegal Javascript value" obj))))

;*---------------------------------------------------------------------*/
;*    scheme->javascript ::xml ...                                     */
;*---------------------------------------------------------------------*/
(define-method (scheme->javascript obj::xml)
   (format "document.getElementById( \"~a\" )" (xml-id obj)))
