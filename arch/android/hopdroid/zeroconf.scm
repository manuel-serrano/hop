;*=====================================================================*/
;*    .../project/hop/3.1.x/arch/android/hopdroid/zeroconf.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 22 11:41:40 2011                          */
;*    Last change :  Mon Jul 18 07:00:23 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Android zerconf support                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-zeroconf

   (library phone hop)

   (import __hopdroid-phone)
   
   (export (class androidzeroconf::zeroconf
	      (android::androidphone read-only (default (instantiate::androidphone)))
	      (plugin (default #f)))))

;*---------------------------------------------------------------------*/
;*    zeroconf-debug ...                                               */
;*---------------------------------------------------------------------*/
(define (zeroconf-debug)
   (>= (bigloo-debug) 1))

;*---------------------------------------------------------------------*/
;*    zeroconf-backend-start ::androidzeroconf ...                     */
;*---------------------------------------------------------------------*/
(define-method (zeroconf-backend-start o::androidzeroconf)
   (with-access::androidzeroconf o ((aphone android) plugin onready hostname)
      (unless plugin
	 (set! plugin (android-load-plugin aphone "zeroconf"))
	 (unless plugin
	    (error "zeroconf-backend-start"
	       "Cannot start zeroconf plugin"
	       plugin)))
      (when (string=? hostname "")
	 (if (or (pregexp-match "(?:[0-9]{1,3}[.]){3}[0-9]{1,3}"
		    (hop-server-hostname))
		 (string=? (hop-server-hostname) "localhost"))
	     ;; we have no correct host name, forge one out of the model
	     ;; and mac address
	     (with-access::androidphone aphone (model)
		(set! hostname (string-replace model #\space #\_)))
	     ;; just use the network host name
	     (set! hostname (hop-server-hostname))))
      (when (android-send-command/result aphone plugin #\s hostname)
	 (onready o)
	 (hop-verb 1
	    (format "  zeroconf: ~a\n"
	       (hop-color 2 ""
		  (android-send-command/result aphone plugin #\v)))))))

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
	 (append-map (lambda (s)
			(let ((i (string-index s #\=)))
			   (if i
			       (list (substring s 0 i)
				  (substring s (+fx i 1)))
			       (list "misc" s))))
	    opts))))

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
	  (when plugin
	     (android-send-command android plugin #\l)
	     (add-event-listener! android "zeroconf-add-service"
		(lambda (e::event)
		   (with-access::event e (value)
		      (when (zeroconf-debug)
			 (tprint (format "service discovered: ~s" value)))
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
			  #f))))))))
      ((string=? event "onready")
       (with-access::androidzeroconf o (plugin android onready)
	  (if plugin
	      (proc o)
	      (let ((old onready))
		 (set! onready
		    (lambda (o)
		       (old o)
		       (proc o)))))))
      (else
       (with-access::androidzeroconf o (plugin android)
	  (when plugin
	     (android-send-command android plugin #\t event)
	     (add-event-listener! android (string-append "zeroconf-add-service-" event)
		(lambda (e::event)
		   (with-access::event e (value)
		      (when (zeroconf-debug)
			 (tprint (format "service discovered: ~s" value)))
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
			  #f))))))))))
