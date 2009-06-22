;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/color.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb  2 15:42:45 2006                          */
;*    Last change :  Wed Jun 17 21:28:09 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple color tools                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_color
   (export (color-lighter::bstring ::bstring #!optional (coef 1))
	   (color-darker::bstring ::bstring #!optional (coef 1))
	   (hsv->rgb ::int ::int ::int)
	   (rgb->hsv ::int ::int ::int)))

;*---------------------------------------------------------------------*/
;*    dotimes ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (dotimes binding . body)
   (if (list? binding)
       ;; binding is a list
       (let ((var   #f) (count #f) (result #f))
	  (case (length binding)
	     ((2)  (set! var    (car binding))
		   (set! count  (cadr binding)))
	     ((3)  (set! var    (car binding))
		   (set! count  (cadr binding))
		   (set! result (caddr binding)))
	     (else (error "dotimes" "bad binding construct" binding)))
	  `(do ((,var 0 (+ ,var 1)))
		 ((= ,var ,count) ,result)
	      ,@body))
       ;; binding is ill-formed
       (error "dotimes" "binding is not a list: " binding)))

;*---------------------------------------------------------------------*/
;*    integer->string-2 ...                                            */
;*---------------------------------------------------------------------*/
(define (integer->string-2 comp)
   (if (>=fx comp 16)
       (integer->string comp 16)
       (string #\0 (string-ref "0123456789abcdef" comp))))

;*---------------------------------------------------------------------*/
;*    parse-color ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-color color)
   (define (char->int c)
      (cond
	 ((and (char>=? c #\0) (char<=? c #\9))
	  (*fx 16 (-fx (char->integer c) (char->integer #\0))))
	 ((and (char>=? c #\a) (char<=? c #\f))
	  (*fx 16 (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
	 ((and (char>=? c #\A) (char<=? c #\F))
	  (*fx 16 (+fx 10 (-fx (char->integer c) (char->integer #\A)))))
	 (else
	  0)))
   (cond
      ((not (char=? (string-ref color 0) #\#))
       (values -1 -1 -1))
      ((=fx (string-length color) 7)
       (values (string->integer (substring color 1 3) 16)
	       (string->integer (substring color 3 5) 16)
	       (string->integer (substring color 5 7) 16)))
      ((=fx (string-length color) 4)
       (values (char->int (string-ref color 1))
	       (char->int (string-ref color 2))
	       (char->int (string-ref color 3))))
      (else
       (values -1 -1 -1))))
	  
;*---------------------------------------------------------------------*/
;*    make-rgb-lighter ...                                             */
;*---------------------------------------------------------------------*/
(define (make-rgb-lighter coef red green blue)
   ;; Make a lighter color: to do this, round each color component
   ;; up by 15% or 1/3 of the way to full white, whichever is greater.
   (let ((rgb (vector red green blue))
	 (light (make-vector 3)))
      (dotimes (i 3)
	       (let* ((c (vector-ref rgb i))
		      (inc1 (* c (* 0.15 coef)))
		      (inc2 (/ (- 255 c) 3)))
		  (set! c (+ c (max inc1 inc2)))
		  (vector-set! light i (inexact->exact (min c 255)))))
      (string-append "#"
		     (integer->string-2 (vector-ref light 0))
		     (integer->string-2 (vector-ref light 1))
		     (integer->string-2 (vector-ref light 2)))))

;*---------------------------------------------------------------------*/
;*    make-rgb-darker ...                                              */
;*---------------------------------------------------------------------*/
(define (make-rgb-darker coef red green blue)
   (string-append "#"
		  (integer->string-2 (/fx (* (-fx 10 coef) red) 10))
		  (integer->string-2 (/fx (* (-fx 10 coef) green) 10))
		  (integer->string-2 (/fx (* (-fx 10 coef) blue) 10))))

;*---------------------------------------------------------------------*/
;*    color-lighter ...                                                */
;*---------------------------------------------------------------------*/
(define (color-lighter color #!optional (coef 1))
   (multiple-value-bind (r g b)
      (parse-color color)
      (if (or (=fx r -1) (<fx coef 1) (>fx coef 10))
	  color
	  (make-rgb-lighter coef r g b))))
   
;*---------------------------------------------------------------------*/
;*    color-darker ...                                                 */
;*---------------------------------------------------------------------*/
(define (color-darker color #!optional (coef 1))
   (multiple-value-bind (r g b)
      (parse-color color)
      (if (or (=fx r -1) (<fx coef 1) (>fx coef 10))
	  color
	  (make-rgb-darker coef r g b))))
   
;*---------------------------------------------------------------------*/
;*    hsv->rgb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hsv->rgb h s v)
   (let ((r 0)
	 (g 0)
	 (b 0))
      (if (> s 0)
	  (let* ((h/60 (/ h 60))
		 (fh/60 (floor h/60))
		 (hi (modulo (inexact->exact fh/60) 6))
		 (f (- h/60 fh/60))
		 (s/100 (/ s 100))
		 (v/100 (/ v 100))
		 (p (inexact->exact (* 255 (* v/100 (- 1 s/100)))))
		 (q (inexact->exact (* 255 (* v/100 (- 1 (* f s/100))))))
		 (t (inexact->exact (* 255 (* v/100 (- 1 (* (- 1 f) s/100))))))
		 (v*255 (inexact->exact (round (* v/100 255))))
		 (r 0)
		 (g 0)
		 (b 0))
	     (case hi
		((0) (set! r v*255) (set! g t) (set! b p))
		((1) (set! r q) (set! g v*255) (set! b p))
		((2) (set! r p) (set! g v*255) (set! b t))
		((3) (set! r p) (set! g q) (set! b v*255))
		((4) (set! r t) (set! g p) (set! b v*255))
		((5) (set! r v*255) (set! g p) (set! b q)))
	     (values r g b))
	  (let ((v (inexact->exact (round (* (/ v 100) 255)))))
	     (values v v v)))))

;*---------------------------------------------------------------------*/
;*    rgb->hsv ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgb->hsv r g b)
   (define (h max min r g b)
      (cond
	 ((= max min)
	  0)
	 ((= max r)
	  (modulo
	   (inexact->exact (round (+ (* 60 (/ (- g b) (- max min))) 360))) 360))
	 ((= max g)
	  (inexact->exact (round (+ (* 60 (/ (- b r) (- max min))) 120))))
	 (else
	  (inexact->exact (round (+ (* 60 (/ (- r g) (- max min))) 240))))))
   (define (s max min r g b)
      (if (= max 0)
	  0
	  (inexact->exact (round (* 100 (/ (- max min) max))))))
   (let* ((r (/ r 255))
	  (g (/ g 255))
	  (b (/ b 255))
	  (max (max r g b))
	  (min (min r g b)))
      (values (h max min r g b)
	      (s max min r g b)
	      (inexact->exact (round (* 100 max))))))
