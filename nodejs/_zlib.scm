;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/_zlib.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 27 19:12:38 2015                          */
;*    Last change :  Mon Apr 15 13:55:14 2019 (serrano)                */
;*    Copyright   :  2015-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Zlib bindings                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__zlib

   (library hopscript)

   (include "nodejs_async.sch")

   (static  (class JsZlib::JsObject))
   
   (import  __nodejs_uv
	    __nodejs_process)

   (export  (process-zlib ::WorkerHopThread ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    constructors                                                     */
;*---------------------------------------------------------------------*/
(define-instantiate JsZlib)

;*---------------------------------------------------------------------*/
;*    process-zlib ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/zlib.html                                  */
;*---------------------------------------------------------------------*/
(define (process-zlib %worker %this process)

   (define __init (set! __js_strings (&init!)))
   
   (define (zlib-write this . l)
      (tprint "zlib-write l=" l)
      (error "zlib" "binding not implemented" "write"))
   
   (define (zlib-init this . l)
      (tprint "zlib-init l=" l))

   (define (zlib-close this)
      (error "zlib" "binding not implemented" "close"))
      
   (define (zlib-reset this)
      (error "zlib" "binding not implemented" "reset"))
      
   (define zlib-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto (& "write")
	    (js-make-function %this zlib-write
	       7 "write")
	    #f %this)
	 (js-put! proto (& "init")
	    (js-make-function %this zlib-init
	       5 "init")
	    #f %this)
	 (js-put! proto (& "close")
	    (js-make-function %this zlib-close
	       0 "close")
	    #f %this)
	 (js-put! proto (& "reset")
	    (js-make-function %this zlib-reset
	       0 "reset")
	    #f %this)
	 proto))
   
   (define (zlib this)
      (instantiateJsZlib
	 (__proto__ zlib-proto)))

   (let* ((zlib (js-make-function %this zlib 0 "Zlib"
		   :construct zlib
		   :prototype zlib-proto)))
      (js-alist->jsobject
	 `((Zlib . ,zlib))
	 %this) ))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

