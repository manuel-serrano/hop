;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-font.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 14 06:16:17 2009                          */
;*    Last change :  Sat Nov 14 07:17:26 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Font utilities                                                   */
;*    -------------------------------------------------------------    */
;*    This code is based on the JavaScript CSS Font detector           */
;*    (http://www.lalit.org/lab/jsoncookies) by Lalit Patel.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-font
   (export (font-exists? font . size)))

;*---------------------------------------------------------------------*/
;*    default-font ...                                                 */
;*---------------------------------------------------------------------*/
(define (default-font) "serif")

;*---------------------------------------------------------------------*/
;*    %font-checker                                                    */
;*---------------------------------------------------------------------*/
(define %font-checker #f)

;*---------------------------------------------------------------------*/
;*    make-font-checker ...                                            */
;*---------------------------------------------------------------------*/
(define (make-font-checker default-font)

   (let ((h (car (dom-get-elements-by-tag-name "body")))
	 (d (<DIV>))
	 (s (<SPAN>))
	 (dwidth20 0)
	 (dheight20 0)
	 (dwidth72 0)
	 (height72 0))

      (define (init!)
	 ;; insert the new DIV and SPAN and measure the default font
	 (dom-append-child! d s)
	 (node-style-set! d :font-family default-font)
	 (node-style-set! s :font-family default-font)
	 (innerHTML-set! s "mmmmmmmmmmlil")
	 (dom-append-child! h d)
	 ;; measure the 20pt size
	 (node-style-set! s :font-size "20pt")
	 (set! dwidth20 s.offsetWidth)
	 (set! dheight20 s.offsetHeight)
	 ;; measure the 72pt size
	 (node-style-set! s :font-size "72pt")
	 (set! dwidth72 s.offsetWidth)
	 (set! dheight72 s.offsetHeight)
	 (dom-remove-child! h d))

      (define (compare-default-size font)
	 (dom-append-child! h d)
	 (node-style-set! s :font-family font)
	 (node-style-set! s :font-size "20pt")
	 (let ((res (or (not (= dwidth20 s.offsetWidth))
			(not (= dheight20 s.offsetHeight)))))
	    (unless res
	       (node-style-set! s :font-size "72pt")
	       (set! res (or (not (= dwidth72 s.offsetWidth))
			     (not (= dheight72 s.offsetHeight))))
	       (node-style-set! s :font-size "20pt"))
	    (dom-remove-child! h d)
	    res))

      (define (compare-size font size)
	 (dom-append-child! h d)
	 (node-style-set! s :font-size (format "~apt" size))
	 (node-style-set! s :font-family default-font)
	 (let ((dwidth s.offsetWidth)
	       (dheight s.offsetHeight))
	    (node-style-set! s :font-family font)
	    (let ((res (or (not (= dwidth s.offsetWidth))
			   (not (= dheight s.offsetHeight)))))
	       (dom-remove-child! h d)
	       res)))

      (define (compare-font font size)
	 (cond
	    ((string=? (string-downcase font) default-font) #t)
	    ((not size) (compare-default-size font))
	    (else (compare-size font size))))

      (init!)
      compare-font))

;*---------------------------------------------------------------------*/
;*    font-exists? ...                                                 */
;*---------------------------------------------------------------------*/
(define (font-exists? font . size)
   (unless (procedure? %font-checker)
      (set! %font-checker (make-font-checker (default-font))))
   (let ((s (if (and (pair? size) (integer? (car size))) (car size) #f)))
      (%font-checker font s)))

