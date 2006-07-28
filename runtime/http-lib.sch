;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/http-lib.sch                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 11 16:53:29 2005                          */
;*    Last change :  Thu Jul 27 10:19:19 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HTTP-LIB macros                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    http-write-line ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (http-write-line p . args)
   (if (> (bigloo-compiler-debug) 1)
       `(begin
	   (trace-item "http-write-line=" ,@args)
	   ,@(map (lambda (a) `(display ,a ,p)) args)
	   (display "\r\n" ,p))
       `(begin
	   ,@(map (lambda (a) `(display ,a ,p)) args)
	   (display "\r\n" ,p))))
