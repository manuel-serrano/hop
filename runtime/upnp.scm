;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/upnp.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 10 16:49:31 2013                          */
;*    Last change :  Wed Apr 10 17:02:52 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    UPNP Hop support                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_upnp

   (library upnp)

   (import __hop_configure
	   __hop_service
	   __hop_types
	   __hop_hop
	   __hop_param
	   __hop_misc
	   __hop_read
	   __hop_event
	   __hop_user)

   (static (class upnp-backend
	      (mutex::mutex read-only (default (make-mutex "upnp")))
	      (cache::pair-nil (default '()))))
   
   (export (upnp)))

;*---------------------------------------------------------------------*/
;*    upnp-mutex ...                                                   */
;*---------------------------------------------------------------------*/
(define upnp-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    *upnp-backend* ...                                               */
;*---------------------------------------------------------------------*/
(define *upnp-backend* #f)

;*---------------------------------------------------------------------*/
;*    upnp ...                                                         */
;*---------------------------------------------------------------------*/
(define (upnp)
   (synchronize upnp-mutex
      (unless *upnp-backend*
	 (set! *upnp-backend*
	    (instantiate::upnp-backend))))
   *upnp-backend*)

;*---------------------------------------------------------------------*/
;*    add-event-listener! ::upnp-backend ...                           */
;*---------------------------------------------------------------------*/
(define-method (add-event-listener! o::upnp-backend evt proc . capture)
   (with-access::upnp-backend o (mutex)
      o))

