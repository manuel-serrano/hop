;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/runtime/debug.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 21 12:09:24 2013                          */
;*    Last change :  Thu Aug  1 09:04:32 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Debugging facilities                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_debug

   (include "xml.sch"
	    "service.sch"
	    "verbose.sch"
	    "param.sch")

   (library web)

   (import __hop_misc
	   __hop_read
	   __hop_types
	   __hop_service
	   __hop_param
	   __hop_hop
	   __hop_http-error)
   
   (export (hop-debug-init! ::output-port)))

;*---------------------------------------------------------------------*/
;*    services ...                                                     */
;*---------------------------------------------------------------------*/
(define *debug-service* #f)
(define *tprint-service* #f)
(define *sourcemap-service* #f)

;*---------------------------------------------------------------------*/
;*    hop-debug-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-debug-init! port)
   
   ;; public/server/debug/exception
   (set! *debug-service*
      (service :name "public/server-debug/exception" :timeout 0
	 (#!key exc url msg obj stack)
	 (synchronize (verb-mutex)
	    (newline port)
	    (display-trace-stack-source stack port)
	    (display "*** CLIENT ERROR: " port)
	    (display-circle url port)
	    (display #":\n" port)
	    (display-circle msg port)
	    (display " -- " port)
	    (display-circle obj port)
	    (newline port)
	    (display-trace-stack stack port)
	    #t)))
   
   ;; public/server/debug/tprint
   (set! *tprint-service*
      (service :name "public/server-debug/tprint" :timeout 0
	 (file pos arguments)
	 (synchronize (verb-mutex)
	    (apply tprint port
	       (hop-color 0 "" (format "~~ ~a,~a" file pos))
	       ": "
	       arguments))))

   ;; public/server/debug/source-map
   (set! *sourcemap-service*
      (service :name "public/server-debug/source-map" :timeout 0
	 (file)
	 (if (file-exists? file)
	     (instantiate::http-response-file
		(request (current-request))
		(file file)
		(content-type "application/json")
		(bodyp #t)
		(charset (hop-locale)))
	     (http-file-not-found file)))))
	 





