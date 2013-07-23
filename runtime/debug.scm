;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/runtime/debug.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 21 12:09:24 2013                          */
;*    Last change :  Mon Jul 22 14:35:42 2013 (serrano)                */
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
	   __hop_service)
   
   (export (hop-debug-init! ::output-port)))

;*---------------------------------------------------------------------*/
;*    services ...                                                     */
;*---------------------------------------------------------------------*/
(define *debug-service* #f)
(define *tprint-service* #f)

;*---------------------------------------------------------------------*/
;*    hop-debug-init! ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-debug-init! port)
   ;; public/server/debug/exception
   (set! *debug-service*
      (service :name "public/server-debug/exception"
	 :timeout 0
	 (#!key exc url msg obj stack)
	 (synchronize (verb-mutex)
	    (display (hop-color 0 "" "*** CLIENT ERROR: ") port)
	    (display-circle url port)
	    (display #":\n" port)
	    (display-circle msg port)
	    (display " -- " port)
	    (display-circle obj port)
	    (newline port)
	    (display-trace-stack stack port))))
   ;; public/server/debug/tprint
   (set! *tprint-service*
      (service :name "public/server-debug/tprint"
	 :timeout 0
	 (file pos arguments)
	 (synchronize (verb-mutex)
	    (apply tprint port
	       (hop-color 0 "" (format "~~ ~a,~a" file pos))
	       ":"
	       arguments)))))




