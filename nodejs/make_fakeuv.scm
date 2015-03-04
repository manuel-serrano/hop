;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/make_fakeuv.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar  3 18:53:45 2015                          */
;*    Last change :  Tue Mar  3 19:11:04 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Small utility to build a fake __nodejs_uv module                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mkfakeuv
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let loop ()
      (let ((e (read)))
	 (unless (eof-object? e)
	    (display (fake e))
	    (newline)
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    fake ...                                                         */
;*---------------------------------------------------------------------*/
(define (fake expr)
   (match-case expr
      ((module ?- . ?-) #f)
      (((or define-inline define-method define) (?- . ?-) . ?-)
       (fake-define expr))
      ((cond-expand (enable-libuv . ?body))
       `(cond-expand ((not enable-libuv) ,@(map fake body))))
      (else expr)))

;*---------------------------------------------------------------------*/
;*    fake-define ...                                                  */
;*---------------------------------------------------------------------*/
(define (fake-define expr)
   (match-case expr
      ((?kwd ?proto . ?body) `(,kwd ,proto #unspecified))
      (else expr)))
	 
