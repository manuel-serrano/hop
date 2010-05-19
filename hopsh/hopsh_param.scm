;*=====================================================================*/
;*    serrano/prgm/project/hop/hopsh/hopsh-param.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Wed Oct 18 11:46:22 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP global parameters                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh_param
   
   (library hop)
   
   (export  (hopsh-rc-file::bstring)
	    (hopsh-rc-file-set! ::bstring)
	    
	    (hopsh-host::bstring)
	    (hopsh-host-set! ::bstring)
	    
	    (hopsh-eval-service::bstring)
	    (hopsh-eval-service-set! ::bstring)
   
	    (hopsh-enable-https::bool)
	    (hopsh-enable-https-set! ::bool))
   
   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    hopsh-rc-file ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hopsh-rc-file
   "hopshrc.hop")

;*---------------------------------------------------------------------*/
;*    hopsh-host ...                                                   */
;*---------------------------------------------------------------------*/
(define-parameter hopsh-host
   "localhost")

;*---------------------------------------------------------------------*/
;*    hopsh-eval-service ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hopsh-eval-service
   "hopsh")

;*---------------------------------------------------------------------*/
;*    hopsh-enable-https ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hopsh-enable-https
   #f)
