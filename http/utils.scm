;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/http/utils.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 15 11:28:31 2004                          */
;*    Last change :  Tue May 14 10:08:20 2024 (serrano)                */
;*    Copyright   :  2004-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utilities                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __http_utils
   (export  (macro trace)
	    *trace-level*
	    (inline trace-level::int)
	    (trace-level-set! ::int)
	    (trace ::int ::int ::bstring . args)))
   
;*---------------------------------------------------------------------*/
;*    trace ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (trace level id key . args)
   (let ((l (gensym)))
      `(let ((,l ,level))
	  (when (>=fx (trace-level) ,l)
	     ((@ trace __http_utils) ,l ,id ,key ,@args)))))

;*---------------------------------------------------------------------*/
;*    when-trace ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (when-trace-level level expr)
   (when (>=fx (trace-level) ,l)
      ,expr))

;*---------------------------------------------------------------------*/
;*    *trace-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *trace-mutex* (make-spinlock "hopsched-trace"))

;*---------------------------------------------------------------------*/
;*    trace-level ...                                                  */
;*---------------------------------------------------------------------*/
(define *trace-level* 0)
(define-inline (trace-level) *trace-level*)
(define (trace-level-set! l) (set! *trace-level* l))

;*---------------------------------------------------------------------*/
;*    trace ...                                                        */
;*---------------------------------------------------------------------*/
(define (trace level id key . args)
   (when (>=fx (trace-level) level)
      (synchronize *trace-mutex*
	 (display (trace-color (+ (modulo id 16) 1) key) (current-error-port))
         (for-each (lambda (a) (display a (current-error-port))) args)
	 (newline (current-error-port))
         (flush-output-port (current-error-port)))))
