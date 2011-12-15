;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/zeroconf_avahi.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:04:07 2011                          */
;*    Last change :  Thu Dec 15 20:26:43 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Avahi support for Hop                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(directives
   (import __hop_configure
	   __hop_param
	   __hop_misc
	   __hop_weblets
	   __hop_types)
   (library avahi pthread))

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-publish ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-zeroconf-publish svc)
   (hop-verb 1 "Zeroconf (avahi) setup...\n")
   (let* ((poll (instantiate::avahi-simple-poll))
	  (client (instantiate::avahi-client
		     (proc (lambda (c s) (client-callback c s svc)))
		     (poll poll))))
      (thread-start!
	 (instantiate::pthread
	    (body (lambda ()
		     (unwind-protect
			(avahi-simple-poll-loop poll)
			(begin
			   (avahi-client-close client)
			   (avahi-simple-poll-close poll)))))))))

;*---------------------------------------------------------------------*/
;*    client-callback ...                                              */
;*---------------------------------------------------------------------*/
(define (client-callback client::avahi-client state::symbol svc::pair-nil)
   (case state
      ((avahi-client-running)
       (create-services client svc))
      ((avahi-client-failure)
       (warning "hop-zeroconf" (avahi-client-error-message client) state)
       (avahi-simple-poll-quit (-> client poll)))
      ((avahi-client-collision avahi-client-registering avahi-client-connecting)
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    create-services ...                                              */
;*---------------------------------------------------------------------*/
(define (create-services client::avahi-client extra-services)
   (let ((group (instantiate::avahi-entry-group
		   (proc entry-group-callback)
		   (client client))))
      (let loop ((name "Hop"))
	 (with-handler
	    (lambda (e)
	       (if (isa? e &avahi-collision-error)
		   (begin
		      (avahi-entry-group-reset! group)
		      (loop (avahi-alternative-service-name name)))
		   (raise e)))
	    (begin
	       ;; add the service for hop
	       (avahi-entry-group-add-service! group
		  :name name
		  :type "_http._tcp"
		  :port (hop-port)
		  (format "version=~a" (hop-version))
		  (format "path=~a" (hop-service-base)))
	       ;; additional init services
	       (map (lambda (wi)
		       (apply avahi-entry-group-add-service! group wi))
		  extra-services)
	       ;; weblets zeroconf info
	       (map (lambda (wi)
		       (apply avahi-entry-group-add-service! group wi))
		  (get-weblets-zeroconf))
	       ;; tell the server to register the service
	       (avahi-entry-group-commit group))))))

;*---------------------------------------------------------------------*/
;*    entry-group-callback ...                                         */
;*---------------------------------------------------------------------*/
(define (entry-group-callback egroup::avahi-entry-group state::symbol)
   (case state
      ((avahi-entry-group-collision)
       (raise
	  (instantiate::&avahi-collision-error
	     (proc "zeroconf-avahi")
	     (msg "Service name collision")
	     (obj egroup))))
      ((avahi-entry-group-failure)
       (error "zeroconfg-avahi"
	  "Cannot register service"
	  (avahi-client-error-message (-> egroup client))))))


