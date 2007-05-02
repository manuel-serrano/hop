;*=====================================================================*/
;*    serrano/prgm/project/hop/src/hop-param.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Mon Apr 23 15:58:09 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP global parameters                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_param
   
   (library hop)
   
   (export  (hop-max-accept-thread::int)
	    (hop-max-accept-thread-set! ::int)
	    
	    (hop-max-reply-thread::int)
	    (hop-max-reply-thread-set! ::int)
	    
	    (hop-max-reply-persite-thread::int)
	    (hop-max-reply-persite-thread-set! ::int)
	    
	    (hop-autoload-directories::pair-nil)
	    (hop-autoload-directories-set! ::pair-nil)
	    (hop-autoload-directory-add! ::bstring)
	    
	    (hop-preload-libraries::pair-nil)
	    (hop-preload-libraries-set! ::pair-nil)
	    
	    (hop-proxy-authentication::bool)
	    (hop-proxy-authentication-set! ::bool)
	    
	    (hop-proxy-allow-remote-client::bool)
	    (hop-proxy-allow-remote-client-set! ::bool)
	    
	    (hop-proxy-remote-authentication::bool)
	    (hop-proxy-remote-authentication-set! ::bool)
	    
	    (hop-proxy-ip-mask::bstring)
	    (hop-proxy-ip-mask-set! ::bstring)
	    
	    (hop-proxy-ip-mask-word::elong)
	    
	    (hop-log-file::obj)
	    (hop-log-file-set! ::obj)
	    
	    (hop-scheduling::symbol)
	    (hop-scheduling-set! ::symbol)
	    
	    (hop-enable-https::bool)
	    (hop-enable-https-set! ::bool)
	    
	    (hop-https-protocol::symbol)
	    (hop-https-protocol-set! ::symbol))

   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    Thread management                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-max-accept-thread
   7)

(define-parameter hop-max-reply-thread
   13)

(define-parameter hop-max-reply-persite-thread
   4)

;*---------------------------------------------------------------------*/
;*    Autoload                                                         */
;*---------------------------------------------------------------------*/
(define-parameter hop-autoload-directories
   (list (hop-weblets-directory)))

(define (hop-autoload-directory-add! d)
   (hop-autoload-directories-set! (cons d (hop-autoload-directories))))

;*---------------------------------------------------------------------*/
;*    hop-preload-libraries ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hop-preload-libraries
   '(hop web hopscheme scheme2js))

;*---------------------------------------------------------------------*/
;*    hop-proxy-authentication ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-authentication
   #f)

;*---------------------------------------------------------------------*/
;*    hop-proxy-allow-remote-client ...                                */
;*    -------------------------------------------------------------    */
;*    Accept or not to relay request from distant client?              */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-allow-remote-client
   #f)

;*---------------------------------------------------------------------*/
;*    hop-proxy-remote-authentication ...                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-remote-authentication
   #t)

;*---------------------------------------------------------------------*/
;*    hop-proxy-ip-mask ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-ip-mask
   "255.255.255.255"
   (lambda (v)
      (if (string? v)
	  (let ((l (pregexp-match "([0-9]{1,3})[.]([0-9]{1,3})[.]([0-9]{1,3})[.]([0-9]{1,3})" v)))
	     (if (not l)
		 (error 'hop-proxy-ip-mask-set!
			"Illegal IPv4 mask"
			v)
		 (begin
		    (hop-proxy-ip-mask-word-set! (ipv4->elong v))
		    v)))
	  (ipv4->elong "255.255.255.255"))))

;*---------------------------------------------------------------------*/
;*    hop-proxy-ip-mask-word ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hop-proxy-ip-mask-word
   (ipv4->elong "255.255.255.255"))

;*---------------------------------------------------------------------*/
;*    hop-log-file ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hop-log-file
   #f)

;*---------------------------------------------------------------------*/
;*    hop-scheduling ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hop-scheduling
   'cohort)

;*---------------------------------------------------------------------*/
;*    hop-enable-https ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-enable-https
   #f)

(define-parameter hop-https-protocol
   'tlsv1)
