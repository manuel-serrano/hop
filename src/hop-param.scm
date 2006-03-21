;*=====================================================================*/
;*    serrano/prgm/project/hop/src/hop-param.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Tue Mar 21 07:57:37 2006 (serrano)                */
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
	    (hop-preload-libraries-set! ::pair-nil))
   
   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    Thread management                                                */
;*---------------------------------------------------------------------*/
(define-parameter hop-max-accept-thread
   10)

(define-parameter hop-max-reply-thread
   10)

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
