;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/debug.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 21 12:09:24 2013                          */
;*    Last change :  Wed Dec 16 21:37:48 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
	    "param.sch"
	    "client-exception.sch"
	    "sourcemap.sch"
	    "../scheme2js/base64_vlq.sch")

   (library web)

   (import __hop_misc
	   __hop_read
	   __hop_types
	   __hop_service
	   __hop_param
	   __hop_hop
	   __hop_http-error
	   __hop_clientc
	   __hop_json)
   
   (export (hop-debug-init! ::output-port)
	   (hop-debug-exception-stack ::obj)))

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
		(file file)
		(content-type "application/json")
		(bodyp #t)
		(charset (hop-locale)))
	     (http-file-not-found file)))))
	 
;*---------------------------------------------------------------------*/
;*    hop-source-map ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-source-map file line col)
   (let ((i (string-index file #\?)))
      (if (and (integer? i)
	       (substring-at? file (hop-scm-compile-suffix) (+fx i 1)))
	  (let ((cache (clientc-cached-response (substring file 0 i))))
	     (or (source-map-translate cache file line col)
		 (values file #f #f)))
	  file)))

;*---------------------------------------------------------------------*/
;*    *smap-cache* ...                                                 */
;*    -------------------------------------------------------------    */
;*    Simple one-entry cache.                                          */
;*---------------------------------------------------------------------*/
(define *smap-cache* (cons "" #f))
(define *smap-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    get-smap ...                                                     */
;*---------------------------------------------------------------------*/
(define (get-smap smap)
   (synchronize *smap-mutex*
      (if (string=? (car *smap-cache*) smap)
	  (cdr *smap-cache*)
	  (let ((o (javascript->obj (call-with-input-file smap read-string))))
	     (set-car! *smap-cache* smap)
	     (set-cdr! *smap-cache* o)
	     o))))

;*---------------------------------------------------------------------*/
;*    source-map-translate ...                                         */
;*---------------------------------------------------------------------*/
(define (source-map-translate cache file line col)
   (when (string? cache)
      (let ((smap (string-append cache ".map")))
	 (if (file-exists? smap)
	    (with-handler
	       (lambda (e)
		  #f)
	       (let ((s (get-smap smap)))
		  (when (pair? s)
		     (let ((mappings (assq 'mappings s))
			   (sources (assq 'sources s)))
			(when (and (pair? mappings) (pair? sources))
			   (hop-debug-source-map-file/smap (cdr mappings)
			      (cdr sources) file line col))))))))))


