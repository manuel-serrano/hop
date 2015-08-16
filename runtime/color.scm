;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/color.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  2 15:42:45 2006                          */
;*    Last change :  Wed Nov 16 11:53:17 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple color tools                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_color
   
   (library multimedia)
   
   (export  (color-lighter::bstring ::bstring #!optional (coef 1))
	    (color-darker::bstring ::bstring #!optional (coef 1))))

;*---------------------------------------------------------------------*/
;*    change-luminance ...                                             */
;*---------------------------------------------------------------------*/
(define (change-luminance r::int g::int b::int coef::double)
   (multiple-value-bind (h s l)
      (rgb->hsl r g b)
      (let ((nl (minfx 100 (+fx l (inexact->exact (* l coef))))))
	 (multiple-value-bind (r g b)
	    (hsl->rgb h s nl)
	    (make-hex-color r g b)))))
   
;*---------------------------------------------------------------------*/
;*    color-lighter ...                                                */
;*---------------------------------------------------------------------*/
(define (color-lighter color #!optional (coef 1))
   (with-handler
      (lambda (e)
	 (if (isa? e &io-parse-error)
	     color
	     (raise e)))
      (multiple-value-bind (r g b)
	 (parse-web-color color)
	 (change-luminance r g b (/fl (fixnum->flonum coef) 10.)))))
   
;*---------------------------------------------------------------------*/
;*    color-darker ...                                                 */
;*---------------------------------------------------------------------*/
(define (color-darker color #!optional (coef 1))
   (with-handler
      (lambda (e)
	 (if (isa? e &io-parse-error)
	     color
	     (raise e)))
      (multiple-value-bind (r g b)
	 (parse-web-color color)
	 (change-luminance r g b (negfl (/fl (fixnum->flonum coef) 10.))))))
