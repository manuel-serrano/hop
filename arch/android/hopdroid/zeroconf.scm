;*=====================================================================*/
;*    .../project/hop/2.4.x/arch/android/hopdroid/zeroconf.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 22 11:41:40 2011                          */
;*    Last change :  Sun Jul  1 18:52:55 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Android zerconf support                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-zeroconf

   (library phone pthread hop)

   (import __hopdroid-phone)
   
   (export (class androidzeroconf::zeroconf
	      (android::androidphone (default (android)))
	      (plugin (default #f)))))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-start ::androidzeroconf ...                     */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-start o::androidzeroconf)
   (with-access::androidzeroconf o (android plugin onready)
      (unless plugin
	 (set! plugin (android-load-plugin android "zeroconf")))
      (when (android-send-command/result android plugin #\s)
	 (onready o))
      (hop-verb 1 (format "Zeroconf (~a) setup...\n"
		     (with-access::androidphone android (sdk)
			(if (>= sdk 16) "android" "jmdns"))))))

;*---------------------------------------------------------------------*/
;*    zeroconf-stop ::androidzeroconf ...                              */
;*---------------------------------------------------------------------*/
;* (define-method (zeroconf-backend-stop o::androidzeroconf)           */
;*    (with-access::androidzeroconf o (android plugin)                 */
;*       (android-send-command android plugin #\e)))                   */

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-publish-service! ::androidzeroconf ...          */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-publish-service! o::androidzeroconf name port type opts)
   (with-access::androidzeroconf o (android plugin)
      (android-send-command android plugin #\p name port type
	 (format "~( )" opts))))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-add-service-listener! ::androidzeroconf ...     */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-add-service-event-listener! o::androidzeroconf event proc)
   (cond
      ((not (string? event))
       (error "add-event-listener! ::zeroconf-service-discoverer"
	  "Illegal event (should be string or #f)"
	  event))
      ((string=? event "")
       (with-access::androidzeroconf o (plugin android)
	  (android-send-command android plugin #\l)
	  (add-event-listener! android "zeroconf-add-service"
	     (lambda (e::event)
		(with-access::event e (value)
		   (match-case value
		      ((?name ?intf ?proto ?svc ?type ?domain ?host ?port ?addr ?txt)
		       (proc (instantiate::zeroconf-service-event
				(name name)
				(target o)
				(interface intf)
				(protocol proto)
				(value svc)
				(type type)
				(domain domain)
				(hostname host)
				(port port)
				(address addr)
				(options txt))))
		      (else
		       #f)))))))
      (else
       (with-access::androidzeroconf o (plugin android)
	  (android-send-command android plugin #\t event)
	  (add-event-listener! android (string-append "zeroconf-add-service-" event)
	     (lambda (e::event)
		(with-access::event e (value)
		   (match-case value
		      ((?name ?intf ?proto ?svc ?- ?domain ?host ?port ?addr ?txt)
		       (proc (instantiate::zeroconf-service-event
				(name name)
				(target o)
				(interface intf)
				(protocol proto)
				(value svc)
				(type event)
				(domain domain)
				(hostname host)
				(port port)
				(address addr)
				(options txt))))
		      (else
		       #f)))))))))

;*---------------------------------------------------------------------*/
;*    Register the avahi backend                                       */
;*---------------------------------------------------------------------*/
(tprint "HOPDROID ZEROCONF INIT")
(zeroconf-register-backend!
   (instantiate::androidzeroconf
      (android (android))))

