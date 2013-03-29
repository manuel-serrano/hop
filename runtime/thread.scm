;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/thread.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 29 10:33:58 2013                          */
;*    Last change :  Fri Mar 29 10:43:03 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop thread class.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_thread
   
   (cond-expand
      ((and enable-threads (library pthread))
       (library pthread)))

   (cond-expand
      ((and enable-threads (library pthread))
       (export (class hopthread::pthread)))
      (else
       (export (class hopthread::thread)))))
