;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/http_lib.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 11 16:53:29 2005                          */
;*    Last change :  Tue May 14 09:19:22 2024 (serrano)                */
;*    Copyright   :  2005-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    utils macros                                                     */
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
