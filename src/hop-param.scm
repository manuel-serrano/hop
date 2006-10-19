;*=====================================================================*/
;*    serrano/prgm/project/hop/src/hop-param.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Tue Oct 17 17:35:24 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
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
	    
	    (hop-log-file::obj)
	    (hop-log-file-set! ::obj)
	    
	    (hop-scheduling::symbol)
	    (hop-scheduling-set! ::symbol)
	    
	    (hop-enable-https::bool)
	    (hop-enable-https-set! ::bool))

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
