;*=====================================================================*/
;*    .../project/hop/2.4.x/arch/android/hopdroid/zeroconf.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 22 11:41:40 2011                          */
;*    Last change :  Thu Jun 28 15:01:33 2012 (serrano)                */
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
	      (android::androidphone read-only)
	      (plugin (default #f)))))

;*---------------------------------------------------------------------*/
;*    zeroconf-start ::androidzeroconf ...                             */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-start o::androidzeroconf)
   (with-access::androidzeroconf o (android plugin)
      (set! plugin
	 (android-load-plugin
	    android
	    (make-file-name (hop-lib-directory) "etc/HopPluginJmDns.jar")))
      (hop-verb 1
	 (format "Zeroconf (jmdns/??? ~a) setup...\n"
	    (android-send-command/result android plugin #\v)))))

;*---------------------------------------------------------------------*/
;*    zeroconf-publish-service! ::androidzeroconf ...                  */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-add-service-event-listener! o::androidzeroconf zd event proc)
   (cond
      ((not (string? event))
       (error "add-event-listener! ::zeroconf-service-discoverer"
	  "Illegal event (should be string or #f)"
	  event))
      ((string=? event "")
       (with-access::androidzeroconf o (plugin android)
	  (add-event-listener! android "jmdns-add-service"
	     (lambda (e::event)
		(with-access::event e (value)
		   (match-case value
		      ((?name ?intf ?proto ?svc ?type ?domain ?host ?port ?addr ?txt)
		       (proc (instantiate::zeroconf-service-event
				(name name)
				(target zd)
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
       (tprint "TODO"))))
