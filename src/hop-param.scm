;*=====================================================================*/
;*    serrano/prgm/project/hop/src/hop-param.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Thu Jan 19 10:13:21 2006 (serrano)                */
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
	    (hop-max-reply-persite-thread-set! ::int))
   
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
