;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/thread.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 29 10:33:58 2013                          */
;*    Last change :  Tue May 14 12:35:34 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop thread base class.                                           */
;*    -------------------------------------------------------------    */
;*    The purpose of this class is to offer an abstraction on top of   */
;*    the actual Bigloo thread to avoid dependent code in the library  */
;*    and in the clients of that library (such as src).                */
;*    When one wants to change the Bigloo multithreading backend, he   */
;*    should change the HOPTHREAD class definition.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_thread
   
   (library pthread)
   
   (export (hop-sigterm-default-handler ::int)
	   (hop-sigterm-handler)
	   (hop-sigterm-handler-set! ::procedure))
   
   (export (class hopthread::pthread
	      (%loading-file (default #f)))))

;*---------------------------------------------------------------------*/
;*    hop-sigterm-default-handler ...                                  */
;*---------------------------------------------------------------------*/
(define (hop-sigterm-default-handler n)
   (exit (+fx 128 n)))

;*---------------------------------------------------------------------*/
;*    sigterm-handler ...                                              */
;*---------------------------------------------------------------------*/
(define sigterm-handler hop-sigterm-default-handler)

;*---------------------------------------------------------------------*/
;*    hop-sigterm-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-sigterm-handler)
   sigterm-handler)

;*---------------------------------------------------------------------*/
;*    hop-sigterm-handler-set! ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-sigterm-handler-set! proc)
   (set! sigterm-handler proc))

