;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/hophz/hophz_param.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Fri Jun  1 09:15:22 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP global parameters                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hophz_param
   
   (library hop)
   
   (export  (hophz-version)

            (hophz-verbose::int)
            (hophz-verbose-set! ::int)
	    
            (hophz-rc-file::bstring)
	    (hophz-rc-file-set! ::bstring)

	    (hophz-host::bstring)
	    (hophz-host-set! ::bstring)
	    
	    (hophz-enable-https::bool)
	    (hophz-enable-https-set! ::bool)

	    (hophz-timeout::int)
	    (hophz-timeout-set! ::int)

	    (hophz-noconfirm::bool)
	    (hophz-noconfirm-set! ::bool)
   
	    (hophz-force-download::bool)
	    (hophz-force-download-set! ::bool)
   
	    (hophz-force-action::bool)
	    (hophz-force-action-set! ::bool)

	    (hophz-publishers::pair-nil)
	    (hophz-publishers-set! ::pair-nil)

	    (hophz-colors)
	    (hophz-colors-set! ::obj))
   
   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    hophz-version ...                                                */
;*---------------------------------------------------------------------*/
(define (hophz-version)
   (hop-version))

;*---------------------------------------------------------------------*/
;*    hophz-verbose ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hophz-verbose
   0)

;*---------------------------------------------------------------------*/
;*    hophz-rc-file ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hophz-rc-file
   "hophzrc.hop")

;*---------------------------------------------------------------------*/
;*    hophz-host ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hophz-host
   "localhost:8080")

;*---------------------------------------------------------------------*/
;*    hophz-enable-https ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hophz-enable-https
   #f)

;*---------------------------------------------------------------------*/
;*    hophz-timeout ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hophz-timeout
   10000)

;*---------------------------------------------------------------------*/
;*    hophz-noconfirm ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hophz-noconfirm
   #f)

;*---------------------------------------------------------------------*/
;*    hophz-force-download ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hophz-force-download
   #f)

;*---------------------------------------------------------------------*/
;*    hophz-force-action ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hophz-force-action
   #f)

;*---------------------------------------------------------------------*/
;*    hophz-publishers ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hophz-publishers
   '())

;*---------------------------------------------------------------------*/
;*    hophz-colors ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hophz-colors
   '((error 1)
     (warning 7)
     (category 3)
     (weblet 7)
     (version 0)
     (install 4)
     (out-of-date 5)
     (keyword 2)
     (path 6))
   (lambda (v)
      (unless (list? v)
	 (error "hophz-colors-set!" "Illegal colors" v))
      (for-each (lambda (o)
		   (match-case o
		      (((? symbol?) (? integer?))
		       #t)
		      (else
		       (error "hophz-colors" "Illegal color" o))))
	 v)
      v))
