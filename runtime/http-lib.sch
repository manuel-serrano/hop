;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/http-lib.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 11 16:53:29 2005                          */
;*    Last change :  Wed Aug 27 13:18:05 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HTTP-LIB macros                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    http-write-line ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (http-write-line p . args)
   (if (> (bigloo-compiler-debug) 1)
       `(begin
	   (when (>fx (bigloo-debug) 0)
	      (trace-item "http-write-line=" ,@args))
	   ,@(map (lambda (a) `(display ,a ,p)) args)
	   (display-string "\r\n" ,p))
       `(begin
	   ,@(map (lambda (a) `(display ,a ,p)) args)
	   (display-string "\r\n" ,p))))

;*---------------------------------------------------------------------*/
;*    http-write-line-string ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (http-write-line-string p . args)
   (if (> (bigloo-compiler-debug) 1)
       `(begin
	   (when (>fx (bigloo-debug) 0)
	      (trace-item "http-write-line=" ,@args))
	   ,@(map (lambda (a) `(display-string ,a ,p)) args)
	   (display-string "\r\n" ,p))
       `(begin
	   ,@(map (lambda (a) `(display-string ,a ,p)) args)
	   (display-string "\r\n" ,p))))
