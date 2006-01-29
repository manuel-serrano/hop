;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/builtin.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:57:04 2006                          */
;*    Last change :  Sun Jan 29 08:14:12 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Builtin services                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_builtin
   
   (include "compiler-macro.sch"
	    "service.sch")

   (import  __hop_param
	    __hop_types
	    __hop_misc
	    __hop_xml
	    __hop_service
	    __hop_js-lib
	    __hop_html-extra
	    __hop_cgi
	    __hop_http-response)

   (export  builtin/mailto
            anonymous
            (%eval::%http-response ::bstring ::procedure))

   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    builtin ...                                                      */
;*---------------------------------------------------------------------*/
(define-weblet (builtin)
   (let ((req (the-current-request)))
      (with-access::http-request req (method localhostp)
         (if (and localhostp (eq? method 'HOP))
             (hop-service builtin req)))))

;*---------------------------------------------------------------------*/
;*    mailto ...                                                       */
;*---------------------------------------------------------------------*/
(define-service (builtin/mailto addr)
   (system (format (string-append (hop-mailer) "&") addr))
   (instantiate::http-response-string))

;*---------------------------------------------------------------------*/
;*    anonymous ...                                                    */
;*---------------------------------------------------------------------*/
(define-weblet (anonymous))

;*---------------------------------------------------------------------*/
;*    %eval ...                                                        */
;*---------------------------------------------------------------------*/
(define (%eval exp cont)
   (let ((svc (scheme->javascript
	       (procedure->service
		(lambda (res)
		   (cont res))))))
      (instantiate::http-response-hop
	 (xml (<HTML>
		 (<HOP-HEAD>)
		 (<BODY>
		    (<SCRIPT> (format "hop( ~a( eval( '~a' ) ), true )"
				      svc
				      exp))))))))
