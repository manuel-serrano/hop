;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/param.sch                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct  1 05:02:42 2005                          */
;*    Last change :  Mon Feb 13 07:34:00 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The definition of the param macros                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-parameter ...                                             */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-parameter id default . setter)
   (let ((vid (symbol-append '* id '*))
	 (set (symbol-append id '-set!)))
      `(begin
          (define ,vid ,default)
	  ,(when (pair? setter) `(,(car setter) ,vid))
          (define (,id)
             ,vid)
          (define (,set v)
             ,(if (pair? setter)
                  `(set! ,vid (,(car setter) v))
                  `(set! ,vid v))
             v))))

