;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/enable-thread.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 23 10:29:32 2007                          */
;*    Last change :  Sat Feb 23 06:49:41 2008 (serrano)                */
;*    Copyright   :  2007-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The __hop_thread module directives when threads are              */
;*    enabled.                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (library pthread)

   (static (class %thread::pthread
	      (hop-thread::hop-thread read-only))))
   
;*---------------------------------------------------------------------*/
;*    call-in-background ...                                           */
;*    -------------------------------------------------------------    */
;*    In a multi-threaded environment there is no need to spawn        */
;*    a new process, we simply execute in the current thread.          */
;*---------------------------------------------------------------------*/
(define (call-in-background thunk)
   (thunk))
