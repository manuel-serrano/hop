;*=====================================================================*/
;*    .../project/hop/2.3.x/arch/android/hopdroid/zeroconf.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 22 11:41:40 2011                          */
;*    Last change :  Tue Jan 17 10:47:13 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Android zerconf support                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-zeroconf

   (library hop phone)

   (import __hopdroid-phone)

   (static (class jmdns::zeroconf
	      (phone::androidphone read-only)))

   (export (androidzeroconf-init ::phone)))

;*---------------------------------------------------------------------*/
;*    androidzeroconf-init ...                                         */
;*---------------------------------------------------------------------*/
(define (androidzeroconf-init o::phone)
   'todo)
