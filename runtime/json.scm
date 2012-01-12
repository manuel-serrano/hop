;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/json.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 19 11:52:55 2010                          */
;*    Last change :  Fri Jan  6 14:37:45 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JSON lib.                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_json
   
   (import  __hop_param
	    __hop_types
	    __hop_hop-inline
	    __hop_dom
	    __hop_xml-types
	    __hop_service
	    __hop_charset
	    __hop_clientc)
   
   (export  (generic hop->json ::obj ::output-port)
            (byte-array->json ::bstring ::output-port)))

;*---------------------------------------------------------------------*/
;*    hop->json ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (hop->json obj op::output-port)
   (cond
      ((vector? obj)
       (vector->json obj op))
      ((pair? obj)
       (pair->json obj op))
      ((procedure? obj)
       (if (service? obj)
	   (hop->json (procedure-attr obj) op)
	   (error "hop->json"
	      "Illegal procedure in JSON conversion"
	      obj)))
      ((date? obj)
       (format "new Date( ~a000 )" (date->seconds obj)))
      (else
       (with-access::clientc (hop-clientc) (valuec)
	  (valuec obj op hop->json #f)))))

;*---------------------------------------------------------------------*/
;*    byte-array->json ...                                             */
;*---------------------------------------------------------------------*/
(define (byte-array->json str::bstring op::output-port)
   (let ((len (string-length str)))
      (case len
	 ((0)
	  (display "[]" op))
	 ((1)
	  (display "[" op)
	  (display-fixnum (char->integer (string-ref-ur str 0)) op)
	  (display "]" op))
	 (else
	  (display "[" op)
	  (display-fixnum (char->integer (string-ref-ur str 0)) op)
	  (let loop ((i 1))
	     (if (=fx i len)
		 (display "]" op)
		 (begin
		    (display "," op)
		    (display-fixnum (char->integer (string-ref-ur str i)) op)
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    vector->json ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector->json vec op::output-port)
   (let ((len (vector-length vec)))
      (case len
	 ((0)
	  (display "[]" op))
	 ((1)
	  (display "[" op)
	  (hop->json (vector-ref vec 0) op)
	  (display "]" op))
	 (else
	  (display "[" op)
	  (hop->json (vector-ref vec 0) op)
	  (let loop ((i 1))
	     (if (=fx i len)
		 (display "]" op)
		 (begin
		    (display "," op)
		    (hop->json (vector-ref vec i) op)
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    pair->json ...                                                   */
;*---------------------------------------------------------------------*/
(define (pair->json pair op::output-port)
   (display "{" op)
   (display "\"__uuid\":" op) (display "\"pair\"," op)
   (display "\"car\":" op) (hop->json (car pair) op)
   (display ",\"cdr\":" op) (hop->json (cdr pair) op)
   (display "}" op))
   


