;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/thread.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 29 10:33:58 2013                          */
;*    Last change :  Thu Mar  5 09:08:13 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
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
   
   (include "thread.sch")

   (export (hop-sigterm-default-handler ::int)
	   (hop-sigterm-handler)
	   (hop-sigterm-handler-set! ::procedure))
   
   (cond-expand
      ((and enable-threads (library pthread))
       (export (class hopthread::pthread
		  (%loading-file (default #f)))))
      (else
       (export (class hopthread::thread
		  (body::procedure read-only)
		  (%loading-file (default #f))
		  (%specific::obj read-only (default #f))
		  (%cleanup::obj read-only (default #f)))))))

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

;*---------------------------------------------------------------------*/
;*    Basic implementation for nothread configuration                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((not enable-threads)

;*---------------------------------------------------------------------*/
;*    thread-start! ::hopthread ...                                    */
;*---------------------------------------------------------------------*/
(define-method (thread-start! th::hopthread . sc)
   (with-access::hopthread th (body %cleanup)
      (unwind-protect
	 (body)
	 (when (procedure? %cleanup) (%cleanup)))))

;*---------------------------------------------------------------------*/
;*    thread-start-joinable! ::hopthread ...                           */
;*---------------------------------------------------------------------*/
(define-method (thread-start-joinable! th::hopthread)
   (thread-start! th))

;*---------------------------------------------------------------------*/
;*    thread-join! ::hopthread ...                                     */
;*---------------------------------------------------------------------*/
(define-method (thread-join! th::hopthread . tm)
   #f)

;*---------------------------------------------------------------------*/
;*    Bigloo implementation of threads                                 */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo4.0a
;*---------------------------------------------------------------------*/
;*    thread-get-specific ::hopthread ...                              */
;*---------------------------------------------------------------------*/
(define-method (thread-get-specific th::hopthread)
   (with-access::hopthread th (%specific)
      %specific))

;*---------------------------------------------------------------------*/
;*    thread-set-specific! ::hopthread ...                             */
;*---------------------------------------------------------------------*/
(define-method (thread-set-specific! th::hopthread v)
   (with-access::hopthread th (%specific)
      (set! %specific v)))

;*---------------------------------------------------------------------*/
;*    thread-get-cleanup ::hopthread ...                               */
;*---------------------------------------------------------------------*/
(define-method (thread-get-cleanup th::hopthread)
   (with-access::hopthread th (%cleanup)
      %cleanup))

;*---------------------------------------------------------------------*/
;*    thread-set-cleanup! ::hopthread ...                              */
;*---------------------------------------------------------------------*/
(define-method (thread-set-cleanup! th::hopthread v)
   (with-access::hopthread th (%cleanup)
      (set! %cleanup v)))
(else
;*---------------------------------------------------------------------*/
;*    thread-specific ::hopthread ...                                  */
;*---------------------------------------------------------------------*/
(define-method (thread-specific th::hopthread)
   (with-access::hopthread th (%specific)
      %specific))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ::hopthread ...                             */
;*---------------------------------------------------------------------*/
(define-method (thread-specific-set! th::hopthread v)
   (with-access::hopthread th (%specific)
      (set! %specific v)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup ::hopthread ...                                   */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup th::hopthread)
   (with-access::hopthread th (%cleanup)
      %cleanup))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ::hopthread ...                              */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup-set! th::hopthread v)
   (with-access::hopthread th (%cleanup)
      (set! %cleanup v))))))

;*---------------------------------------------------------------------*/
;*    End conditional compilation                                      */
;*---------------------------------------------------------------------*/
))
