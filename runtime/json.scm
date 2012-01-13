;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/json.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 19 11:52:55 2010                          */
;*    Last change :  Fri Jan 13 18:28:40 2012 (serrano)                */
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
	    __hop_clientc
	    __hop_js-comp)
   
   (export  (generic obj->json ::obj ::output-port)
            (byte-array->json ::bstring ::output-port)
	    (json->obj ::obj)))

;*---------------------------------------------------------------------*/
;*    obj->json ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (obj->json obj op::output-port)
   (cond
      ((vector? obj)
       (vector->json obj op))
      ((pair? obj)
       (pair->json obj op))
      ((procedure? obj)
       (if (service? obj)
	   (obj->json (procedure-attr obj) op)
	   (error "obj->json"
	      "Illegal procedure in JSON conversion"
	      obj)))
      (else
       (with-access::clientc (hop-clientc) (valuec)
	  (valuec obj op obj->json #f)))))

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
	  (obj->json (vector-ref vec 0) op)
	  (display "]" op))
	 (else
	  (display "[" op)
	  (obj->json (vector-ref vec 0) op)
	  (let loop ((i 1))
	     (if (=fx i len)
		 (display "]" op)
		 (begin
		    (display "," op)
		    (obj->json (vector-ref vec i) op)
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    pair->json ...                                                   */
;*---------------------------------------------------------------------*/
(define (pair->json pair op::output-port)
   (display "{" op)
   (display "\"__uuid\":" op) (display "\"pair\"," op)
   (display "\"car\":" op) (obj->json (car pair) op)
   (display ",\"cdr\":" op) (obj->json (cdr pair) op)
   (display "}" op))
   
;*---------------------------------------------------------------------*/
;*    json->obj ...                                                    */
;*---------------------------------------------------------------------*/
(define (json->obj obj)
   (javascript->obj obj))
