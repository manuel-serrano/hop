;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/share/hop-color.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 14 11:31:04 2009                          */
;*    Last change :  Fri Sep 13 05:57:17 2013 (serrano)                */
;*    Copyright   :  2009-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Client side support for color selectors.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-color
   
   (export (parse-hex-color color)
	   (parse-rgb-color color)
	   (parse-hsl-color color)
	   (parse-web-color color)
	   (hsv->rgb h s v)
	   (rgb->hsv r g b)
	   (hsl->rgb h s l)
	   (rgb->hsl r g b)
	   (colorchooser-reset-mousemove! cc)
	   (colorchooser-init-hmodel hmodel)
	   (colorchooser-init-colorscale scale)
	   (colorchooser-satval-mousedown event cc)
	   (colorchooser-hue-mousedown event cc)
	   (colorchooser-saturation-mousedown event cc)
	   (colorchooser-value-mousedown event cc)
	   (colorchooser-red-mousedown event cc)
	   (colorchooser-green-mousedown event cc)
	   (colorchooser-blue-mousedown event cc)
	   (colorchooser-hmodel-mousedown event cc)
	   (colorchooser-update-colorscales! cc)
	   (colorchooser-update-hue! cc h)
	   (colorchooser-update-saturation! cc s)
	   (colorchooser-update-value! cc v)
	   (colorchooser-update-red! cc r)
	   (colorchooser-update-green! cc g)
	   (colorchooser-update-blue! cc b)
	   (hop-colorchooser-onchange cc)
	   (hop-colorchooser-oncancel cc)
	   (hop-colorchooser-onselect cc)
	   (colorchooser-value el)
	   (colorchooser-value-set! el val)
	   (colorchooser-opacity el)
	   (colorchooser-opacity-set! el a)))

;*---------------------------------------------------------------------*/
;*    head (for client-side dependencies)                              */
;*---------------------------------------------------------------------*/
(<HEAD> :include "hop-slider" "hop-spinbutton")

;*---------------------------------------------------------------------*/
;*    raise-color-error ...                                            */
;*---------------------------------------------------------------------*/
(define (raise-color-error color)
   (error "parse-web-color" "Illegal color" color))

;*---------------------------------------------------------------------*/
;*    parse-hex-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-hex-color color)
   
   (define (char->int c)
      (cond
         ((and (char>=? c #\0) (char<=? c #\9))
          (*fx 16 (-fx (char->integer c) (char->integer #\0))))
         ((and (char>=? c #\a) (char<=? c #\f))
          (*fx 16 (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
         ((and (char>=? c #\A) (char<=? c #\F))
          (*fx 16 (+fx 10 (-fx (char->integer c) (char->integer #\A)))))
         (else
          (raise-color-error color))))

   (cond
      ((or (<fx (string-length color) 4)
           (not (char=? (string-ref color 0) #\#)))
       (raise-color-error color))
      ((=fx (string-length color) 7)
       (values (string->integer (substring color 1 3) 16)
               (string->integer (substring color 3 5) 16)
               (string->integer (substring color 5 7) 16)))
      ((=fx (string-length color) 4)
       (values (char->int (string-ref color 1))
               (char->int (string-ref color 2))
               (char->int (string-ref color 3))))
      (else
       (raise-color-error color))))

;*---------------------------------------------------------------------*/
;*    parse-rgb-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-rgb-color c)
   (cond
      ((pregexp-match "rgb([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*)" c)
       =>
       (lambda (m)
          (values (string->number (cadr m))
                  (string->number (caddr m))
                  (string->number (cadddr m)))))
      ((pregexp-match "rgb([ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*)" c)
       =>
       (lambda (m)
          (values (* 255 (/ (string->number (cadr m)) 100))
                  (* 255 (/ (string->number (caddr m)) 100))
                  (* 255 (/ (string->number (cadddr m)) 100)))))
      (else
       (raise-color-error c))))
                         
;*---------------------------------------------------------------------*/
;*    parse-hsl-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-hsl-color c)
   (cond
      ((pregexp-match "hsl([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*)" c)
       =>
       (lambda (m)
          (hsl->rgb (string->integer (cadr m))
                    (string->integer (caddr m))
                    (string->integer (cadddr m)))))
      (else
       (raise-color-error c))))
                         
;*---------------------------------------------------------------------*/
;*    parse-web-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-web-color color)
   (cond
      ((=fx (string-length color) 0)
       (raise-color-error color))
      ((char=? (string-ref color 0) #\#)
       (parse-hex-color color))
      ((substring-at? color "rgb(" 0)
       (parse-rgb-color color))
      ((substring-at? color "hsl(" 0)
       (parse-hsl-color color))
      (else
       (let ((val (assoc color css2-colors)))
          (if val
              (apply values (cdr val))
              (raise-color-error color))))))

;*---------------------------------------------------------------------*/
;*    css2-colors ...                                                  */
;*    -------------------------------------------------------------    */
;*    Official CSS2 colors.                                            */
;*---------------------------------------------------------------------*/
(define css2-colors
   '(("maroon" #x80 #x00 #x00)
     ("red" #xff #x00 #x00)
     ("orange" #xff #xa5 #x00)
     ("yellow" #xff #xff #x00)
     ("olive" #x80 #x80 #x00)
     ("purple" #x80 #x00 #x80)
     ("fuchsia" #xff #x00 #xff)
     ("white" #xff #xff #xff)
     ("lime" #x00 #xff #x00)
     ("green" #x00 #x80 #x00)
     ("navy" #x00 #x00 #x80)
     ("blue" #x00 #x00 #xff)
     ("aqua" #x00 #xff #xff)
     ("teal" #x00 #x80 #x80)
     ("black" #x00 #x00 #x00)
     ("silver" #xc0 #xc0 #xc0)
     ("gray" #x80 #x80 #x80)))
     
;*---------------------------------------------------------------------*/
;*    hopcolor-sv-size ...                                             */
;*---------------------------------------------------------------------*/
(define hopcolor-hsv-size 170)

;*---------------------------------------------------------------------*/
;*    hopcolor-followmouse ...                                         */
;*---------------------------------------------------------------------*/
(define hopcolor-followmouse
   (and (>= (hop-config 'cpu_speed) 80)
	(>= (hop-config 'js_speed) 80)))

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

;*---------------------------------------------------------------------*/
;*    h ...                                                            */
;*---------------------------------------------------------------------*/
(define (h max min r g b)
   (cond
      ((= max min)
       0)
      ((= max r)
       (modulo
        (inexact->exact
         (round (+ (* 60. (/ (- g b) (- max min))) 360.))) 360))
      ((= max g)
       (inexact->exact
        (round (+ (* 60. (/ (- b r) (- max min))) 120.))))
      (else
       (inexact->exact
        (round (+ (* 60. (/ (- r g) (- max min))) 240.))))))

;*---------------------------------------------------------------------*/
;*    hsl->rgb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hsl->rgb h s l)
   (define (tc t)
      (cond
         ((< t .0) (+ t 1.))
         ((> t 1.) (- t 1.))
         (else t)))
   (define (colorc t p q)
      (let ((v (cond
                  ((< t (/ 1. 6.))
                   (+ p (* (- q p) (* 6. t))))
                  ((< t 0.5)
                   q)
                  ((< t (/ 2. 3.))
                   (+ p (* (- q p) (* 6. (- (/ 2. 3.) t)))))
                  (else
                   p))))
         (inexact->exact (round (* 255. v)))))
   (if (= s 0)
       (let ((v (inexact->exact (round (* (/ l 100.) 255.)))))
          (values v v v))
       (let* ((l/100 (/ (exact->inexact l) 100.))
              (s/100 (/ (exact->inexact s) 100.))
              (q (if (<fx l 50)
                     (* l/100 (+ 1. s/100))
                     (+ l/100 (- s/100 (* l/100 s/100)))))
              (p (- (* 2. l/100) q))
              (hk (/ (exact->inexact h) 360.))
              (tcr (tc (+ hk (/ 1. 3.))))
              (tcg (tc hk))
              (tcb (tc (- hk (/ 1. 3.)))))
          (values (colorc tcr p q) (colorc tcg p q) (colorc tcb p q)))))
         
;*---------------------------------------------------------------------*/
;*    rgb->hsl ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgb->hsl r g b)
   (define (s max min r g b l)
      (cond
         ((= max min)
          0)
         ((<= l 0.5)
          (inexact->exact
           (round (* 100. (/ (- max min) (+ max min))))))
         (else
          (inexact->exact
           (round (* 100. (/ (- max min) (- 2. (+ max min)))))))))
   (let* ((r (/ (exact->inexact r) 255.))
          (g (/ (exact->inexact g) 255.))
          (b (/ (exact->inexact b) 255.))
          (max (max r g b))
          (min (min r g b))
          (l (/ (+ max min) 2.)))
      (values (h max min r g b)
              (s max min r g b l)
              (inexact->exact (round (* 100. l))))))

;*---------------------------------------------------------------------*/
;*    hopcolor-mousemove-listener ...                                  */
;*---------------------------------------------------------------------*/
(define hopcolor-mousemove-listener #f)

;*---------------------------------------------------------------------*/
;*    add-mousemove-listener! ...                                      */
;*---------------------------------------------------------------------*/
(define (add-mousemove-listener! cc proc)
   (let ((listener (if hopcolor-followmouse
		       (lambda (e)
			  (proc e)
			  (colorchooser-update-colorscales! cc))
		       proc)))
      (colorchooser-reset-mousemove! cc)
      (add-event-listener! document "mousemove" listener)
      (set! hopcolor-mousemove-listener listener)))

;*---------------------------------------------------------------------*/
;*    colorchooser-reset-mousemove! ...                                */
;*---------------------------------------------------------------------*/
(define (colorchooser-reset-mousemove! cc)
   (colorchooser-update-colorscales! cc)
   (when hopcolor-mousemove-listener
      (remove-event-listener! document "mousemove" hopcolor-mousemove-listener)
      (set! hopcolor-mousemove-listener #f)))

;*---------------------------------------------------------------------*/
;*    colorchooser-init-hmodel ...                                     */
;*---------------------------------------------------------------------*/
(define (colorchooser-init-hmodel hmodel)
   (let* ((hSV hopcolor-hsv-size)
	  (360/hSV (/ 360 hSV))
	  (cursor (<DIV> :class "hop-colorchooser-hmodel-cursor")))
      (set! hmodel.cursor cursor)
      (let loop ((i 0)
		 (l '()))
	 (if (>= i hSV)
	     (innerHTML-set! hmodel (cons cursor l))
	     (multiple-value-bind (r g b)
		(hsv->rgb (round (* i 360/hSV)) 100 100)
		(let* ((bg (format "background: rgb(~a,~a,~a)" r g b))
		       (d (<DIV> :style bg
			     :class "hop-colorchooser-hmodel-sample")))
		   (loop (+ i 1) (cons d l))))))))

;*---------------------------------------------------------------------*/
;*    colorchooser-init-colorscale ...                                 */
;*---------------------------------------------------------------------*/
(define (colorchooser-init-colorscale scale)
   (let* ((hSV hopcolor-hsv-size)
	  (360/hSV (/ 360 hSV))
	  (cursor (<DIV> :class "hop-colorchooser-colorscale-cursor")))
      (let loop ((i 0)
		 (l '()))
	 (if (>= i hSV)
	     (innerHTML-set! scale (cons cursor l))
	     (multiple-value-bind (r g b)
		(hsv->rgb (round (* i 360/hSV)) 100 100)
		(let* ((bg (format "background: rgb(~a,~a,~a)" r g b))
		       (d (<SPAN> :style bg
			     :class "hop-colorchooser-colorscale-sample" "")))
		   (loop (+ i 1) (cons d l))))))))

;*---------------------------------------------------------------------*/
;*    update-colorscale! ...                                           */
;*---------------------------------------------------------------------*/
(define (update-colorscale! scale left fun)
   (let* ((nodes (dom-child-nodes scale))
	  (cursor (car nodes)))
      (node-style-set! cursor :left (format "~apx" left))
      (let loop ((i 0)
		 (n (cdr nodes)))
	 (when (pair? n)
	    (let ((bg (fun i)))
	       (node-style-set! (car n) :background bg)
	       (loop (+fx i 1)(cdr n)))))))

;*---------------------------------------------------------------------*/
;*    satval-grid-position-set! ...                                    */
;*---------------------------------------------------------------------*/
(define (satval-grid-position-set! satval x y)
   (node-style-set! satval.grid1 :width (format "~apx" x))
   (node-style-set! satval.grid1 :height (format "~apx" y))
   (node-style-set! satval.grid2 :left (format "~apx" x))
   (node-style-set! satval.grid2 :top (format "~apx" y)))

;*---------------------------------------------------------------------*/
;*    update-satval-grid! ...                                          */
;*---------------------------------------------------------------------*/
(define (update-satval-grid! cc)
   (satval-grid-position-set!
    cc.satval
    (inexact->exact (* cc.value (/ hopcolor-hsv-size 100)))
    (- hopcolor-hsv-size
       (inexact->exact (* cc.saturation (/ hopcolor-hsv-size 100))))))

;*---------------------------------------------------------------------*/
;*    colorchooser-satval-mousedown ...                                */
;*---------------------------------------------------------------------*/
(define (colorchooser-satval-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval))
      (satval-start-drag! event cc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    range ...                                                        */
;*---------------------------------------------------------------------*/
(define (range a b c)
   (ceiling (* a (/ c b))))

;*---------------------------------------------------------------------*/
;*    satval-start-drag! ...                                           */
;*---------------------------------------------------------------------*/
(define (satval-start-drag! event cc)

   (define (handler event)
      (let* ((ex (event-mouse-x event))
	     (ey (event-mouse-y event))
	     (satval cc.satval)
	     (satvalx (node-bounding-box-x satval))
	     (satvaly (node-bounding-box-y satval)))
	 (cond
	    ((< ex (+ satvalx 1))
	     (set! ex (+ satvalx 1)))
	    ((> ex (+ satvalx satval.clientWidth))
	     (set! ex (+ satvalx satval.clientWidth))))
	 (cond
	    ((< ey (+ satvaly 1))
	     (set! ey (+ satvaly 1)))
	    ((> ey (+ satvaly satval.clientHeight))
	     (set! ey (+ satvaly satval.clientHeight))))
	 (let ((v (range 100 hopcolor-hsv-size (- ex satvalx 1)))
	       (s (- 100 (range 100 hopcolor-hsv-size (- ey satvaly 1))))
	       (x (- ex satvalx 1))
	       (y (- ey satvaly 1)))
	    (update-color! cc cc.hue s v)
	    (satval-grid-position-set! satval x y))))
   
   (add-mousemove-listener! cc handler)
   (handler event))

;*---------------------------------------------------------------------*/
;*    colorchooser-hue-mousedown ...                                   */
;*---------------------------------------------------------------------*/
(define (colorchooser-hue-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (node-bounding-box-x cc.valcolorscale))
			  (v (range 360 hopcolor-hsv-size (- x vx 1))))
		      (set! cc.hue v)
		      (update-hsv! cc)
		      (update-hmodel! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    colorchooser-saturation-mousedown ...                            */
;*---------------------------------------------------------------------*/
(define (colorchooser-saturation-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (node-bounding-box-x cc.valcolorscale))
			  (v (range 100 hopcolor-hsv-size (- x vx 1))))
		      (set! cc.saturation v)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    colorchooser-value-mousedown ...                                 */
;*---------------------------------------------------------------------*/
(define (colorchooser-value-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (node-bounding-box-x cc.valcolorscale))
			  (v (range 100 hopcolor-hsv-size (- x vx 1))))
		      (set! cc.value v)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    colorchooser-red-mousedown ...                                   */
;*---------------------------------------------------------------------*/
(define (colorchooser-red-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (node-bounding-box-x cc.valcolorscale))
			  (r (range 255 hopcolor-hsv-size (- x vx 1))))
		      (multiple-value-bind (_ g b)
			 (hsv->rgb cc.hue cc.saturation cc.value)
			 (multiple-value-bind (h s v)
			    (rgb->hsv r g b)
			    (update-color! cc h s v)))
		      (update-hsv! cc)
		      (update-hmodel! cc)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    colorchooser-green-mousedown ...                                 */
;*---------------------------------------------------------------------*/
(define (colorchooser-green-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (node-bounding-box-x cc.valcolorscale))
			  (g (range 255 hopcolor-hsv-size (- x vx 1))))
		      (multiple-value-bind (r _ b)
			 (hsv->rgb cc.hue cc.saturation cc.value)
			 (multiple-value-bind (h s v)
			    (rgb->hsv r g b)
			    (update-color! cc h s v)))
		      (update-hsv! cc)
		      (update-hmodel! cc)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    colorchooser-blue-mousedown ...                                  */
;*---------------------------------------------------------------------*/
(define (colorchooser-blue-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (node-bounding-box-x cc.valcolorscale))
			  (b (range 255 hopcolor-hsv-size (- x vx 1))))
		      (multiple-value-bind (r g _)
			 (hsv->rgb cc.hue cc.saturation cc.value)
			 (multiple-value-bind (h s v)
			    (rgb->hsv r g b)
			    (update-color! cc h s v)))
		      (update-hsv! cc)
		      (update-hmodel! cc)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    colorspace-start-drag! ...                                       */
;*---------------------------------------------------------------------*/
(define (colorspace-start-drag! event cc proc)
   
   (define (handler event)
      (let* ((ex (event-mouse-x event))
	     (ey (event-mouse-y event))
	     (satval cc.satval)
	     (satvalx (node-bounding-box-x satval))
	     (satvaly (node-bounding-box-y satval))
	     (value cc.valcolorscale)
	     (valuex (node-bounding-box-x value))
	     (valuey (node-bounding-box-y value)))
	 (cond
	    ((< ex (+ valuex 1))
	     (set! ex (+ valuex 1)))
	    ((> ex (+ valuex value.clientWidth))
	     (set! ex (+ valuex value.clientWidth))))
	 (let ((x (- ex satvalx 1))
	       (y (- ey satvaly 1)))
	    (proc ex cc)
	    (update-color! cc cc.hue cc.saturation cc.value))))
   
   (add-mousemove-listener! cc handler)
   (handler event))

;*---------------------------------------------------------------------*/
;*    colorchooser-hmodel-mousedown ...                                */
;*---------------------------------------------------------------------*/
(define (colorchooser-hmodel-mousedown event cc)
   (let ((cc (dom-get-element-by-id cc)))
      (hmodel-start-drag! event cc)))

;*---------------------------------------------------------------------*/
;*    hmodel-start-drag! ...                                           */
;*---------------------------------------------------------------------*/
(define (hmodel-start-drag! event cc)
   
   (define (handler event)
      (let* ((hmodel cc.hmodel)
	     (ey (event-mouse-y event))
	     (360/hSV (/ 360 hopcolor-hsv-size))
	     (y (node-bounding-box-y hmodel)))
	 (cond
	    ((< ey (+ (node-bounding-box-y hmodel) 1))
	     (set! ey (+ (node-bounding-box-y hmodel) 1)))
	    ((> ey (+ (node-bounding-box-y hmodel) hmodel.clientHeight))
	     (set! ey (+ (node-bounding-box-y hmodel) hmodel.clientHeight))))
	 (let* ((i (- hopcolor-hsv-size (- ey y)))
		(h (round (* i 360/hSV))))
	    (update-color! cc h cc.saturation cc.value)
	    (update-hmodel! cc)
	    (update-hsv! cc))))
   
   (add-mousemove-listener! cc handler)
   (handler event))

;*---------------------------------------------------------------------*/
;*    update-hsv! ...                                                  */
;*---------------------------------------------------------------------*/
(define (update-hsv! cc)
   (multiple-value-bind (r g b)
      (hsv->rgb cc.hue 100 100)
      (node-style-set! cc.satval :background-color (format "rgb(~a,~a,~a)" r g b))))

;*---------------------------------------------------------------------*/
;*    colorchooser-update-colorscales! ...                             */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-colorscales! cc)
   (update-colorscale! cc.huecolorscale
		       (round (* (/ cc.hue 360) hopcolor-hsv-size))
		       (lambda (i)
			  (let ((h (round (* 360 (/ i hopcolor-hsv-size)))))
			     (multiple-value-bind (r g b)
				(hsv->rgb h cc.saturation cc.value)
				(format "rgb(~a,~a,~a)" r g b)))))
   (update-colorscale! cc.satcolorscale
		       (round (* (/ cc.saturation 100) hopcolor-hsv-size))
		       (lambda (i)
			  (let ((s (round (* 100 (/ i hopcolor-hsv-size)))))
			     (multiple-value-bind (r g b)
				(hsv->rgb cc.hue s cc.value)
				(format "rgb(~a,~a,~a)" r g b)))))
   (update-colorscale! cc.valcolorscale
		       (round (* (/ cc.value 100) hopcolor-hsv-size))
		       (lambda (i)
			  (let ((v (round (* 100 (/ i hopcolor-hsv-size)))))
			     (multiple-value-bind (r g b)
				(hsv->rgb cc.hue cc.saturation v)
				(format "rgb(~a,~a,~a)" r g b)))))
   (multiple-value-bind (r g b)
      (hsv->rgb cc.hue cc.saturation cc.value)
      (update-colorscale! cc.redcolorscale
			  (round (* (/ r 255) hopcolor-hsv-size))
			  (lambda (i)
			     (let ((r (round (* 255 (/ i hopcolor-hsv-size)))))
				(format "rgb(~a,~a,~a)" r g b))))
      (update-colorscale! cc.greencolorscale
			  (round (* (/ g 255) hopcolor-hsv-size))
			  (lambda (i)
			     (let ((g (round (* 255 (/ i hopcolor-hsv-size)))))
				(format "rgb(~a,~a,~a)" r g b))))
      (update-colorscale! cc.bluecolorscale
			  (round (* (/ b 255) hopcolor-hsv-size))
			  (lambda (i)
			     (let ((b (round (* 255 (/ i hopcolor-hsv-size)))))
				(format "rgb(~a,~a,~a)" r g b))))))
      
;*---------------------------------------------------------------------*/
;*    update-hmodel! ...                                               */
;*---------------------------------------------------------------------*/
(define (update-hmodel! cc)
   (let ((top (format "~apx" (- hopcolor-hsv-size
				(* (/ cc.hue 360) hopcolor-hsv-size)))))
      (node-style-set! cc.hmodel.cursor :top top)))

;*---------------------------------------------------------------------*/
;*    colorchooser-update-hue! ...                                     */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-hue! cc h)
   (let ((cc (dom-get-element-by-id cc)))
      (update-color! cc h cc.saturation cc.value)
      (update-hmodel! cc)
      (update-hsv! cc)
      (colorchooser-update-colorscales! cc)))

;*---------------------------------------------------------------------*/
;*    colorchooser-update-saturation! ...                              */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-saturation! cc s)
   (let ((cc (dom-get-element-by-id cc)))
      (update-color! cc cc.hue s cc.value)
      (update-satval-grid! cc)
      (colorchooser-update-colorscales! cc)))

;*---------------------------------------------------------------------*/
;*    colorchooser-update-value! ...                                   */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-value! cc v)
   (let ((cc (dom-get-element-by-id cc)))
      (update-color! cc cc.hue cc.saturation v)
      (update-satval-grid! cc)
      (colorchooser-update-colorscales! cc)))

;*---------------------------------------------------------------------*/
;*    update-rgb! ...                                                  */
;*---------------------------------------------------------------------*/
(define (update-rgb! cc r g b)
   (multiple-value-bind (h s v)
      (rgb->hsv r g b)
      (update-color! cc h s v)
      (update-hmodel! cc)
      (update-hsv! cc)
      (update-hmodel! cc)
      (colorchooser-update-colorscales! cc)
      (node-style-set! cc.satval.grid1 :visibility "visible")
      (node-style-set! cc.satval.grid2 :visibility "visible")
      (update-satval-grid! cc)))

;*---------------------------------------------------------------------*/
;*    colorchooser-update-red! ...                                     */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-red! cc r)
   (let ((cc (dom-get-element-by-id cc)))
      (multiple-value-bind (_ g b)
	 (hsv->rgb cc.hue cc.saturation cc.value)
	 (update-rgb! cc r g b))))
   
;*---------------------------------------------------------------------*/
;*    colorchooser-update-green! ...                                   */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-green! cc g)
   (let ((cc (dom-get-element-by-id cc)))
      (multiple-value-bind (r _ b)
	 (hsv->rgb cc.hue cc.saturation cc.value)
	 (update-rgb! cc r g b))))
   
;*---------------------------------------------------------------------*/
;*    colorchooser-update-blue! ...                                    */
;*---------------------------------------------------------------------*/
(define (colorchooser-update-blue! cc b)
   (let ((cc (dom-get-element-by-id cc)))
      (multiple-value-bind (r g _)
	 (hsv->rgb cc.hue cc.saturation cc.value)
	 (update-rgb! cc r g b))))
   
;*---------------------------------------------------------------------*/
;*    update-color! ...                                                */
;*---------------------------------------------------------------------*/
(define (update-color! cc h s v)
   (multiple-value-bind (r g b)
      (hsv->rgb h s v)
      (set! cc.hue h)
      (set! cc.saturation s)
      (set! cc.value v)
      (spinbutton-value-set! cc.huespinbutton h)
      (spinbutton-value-set! cc.satspinbutton s)
      (spinbutton-value-set! cc.valspinbutton v)
      (spinbutton-value-set! cc.redspinbutton r)
      (spinbutton-value-set! cc.greenspinbutton g)
      (spinbutton-value-set! cc.bluespinbutton b)
      (let ((colspec (format "#~x~x~x~x~x~x" 
			     (quotient r 16) (modulo r 16)
			     (quotient g 16) (modulo g 16)
			     (quotient b 16) (modulo b 16))))
	 (node-style-set! cc.color :background-color colspec)
	 (set! cc.rgb.value colspec)
	 (hop-colorchooser-onchange cc))))

;*---------------------------------------------------------------------*/
;*    hop-colorchooser-onchange ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-colorchooser-onchange cc)
   (cond
      ((procedure? cc.onchange)
       (cc.onchange))
      ((string? (cc.getAttribute "onchange"))
       (set! cc.onchange (lambda (e) (eval cc.getAttribute)))
       (if (procedure? cc.onchange)
	   (cc.onchange #unspecified)
	   (error "colorchooser" "Illegal onchange action" cc)))))

;*---------------------------------------------------------------------*/
;*    hop-colorchooser-oncancel ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-colorchooser-oncancel cc)
   (cond
      ((procedure? cc.oncancel)
       (cc.oncancel))
      ((string? (cc.getAttribute "oncancel"))
       (set! cc.oncancel
	     (eval (string-append "function(event) {"
				  (cc.getAttribute "oncancel")
				  "}")))
       (hop-colorchooser-oncancel cc))))

;*---------------------------------------------------------------------*/
;*    hop-colorchooser-onselect ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-colorchooser-onselect cc)
   (cond
      ((procedure? cc.oncolorselect)
       (cc.oncolorselect))
      ((string? (cc.getAttribute "oncolorselect"))
       (set! cc.oncolorselect 
	  (eval (format "(function (event) { ~a })"
		   (cc.getAttribute "oncolorselect"))))
       (hop-colorchooser-onselect cc))))

;*---------------------------------------------------------------------*/
;*    colorchooser-value ...                                           */
;*---------------------------------------------------------------------*/
(define (colorchooser-value el)
   (let ((cc (if (string? el) (dom-get-element-by-id el) el)))
      cc.rgb.value))

;*---------------------------------------------------------------------*/
;*    colorchooser-value-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (colorchooser-value-set! el val)
   (let ((cc (if (string? el) (dom-get-element-by-id el) el)))
      (cond
	 ((pregexp-match "^#([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])$" val)
	  =>
	  (lambda (m)
	     (apply update-rgb! cc
		    (map (lambda (i)
			    (let ((n (string->number i 16)))
			       (+ (* n 16) n)))
			 (cdr m)))))
	 ((pregexp-match "^#([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])$" val)
	  =>
	  (lambda (m)
	     (apply update-rgb! cc
		    (map (lambda (i) (string->number i 16))
			 (cdr m)))))
	 ((pregexp-match "rgb[(][ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*[)]" val)
	  =>
	  (lambda (m)
	     (apply update-rgb! cc (map string->number (cdr m)))))
	 ((pregexp-match "rgb[(][ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*[)]" val)
	  =>
	  (lambda (m)
	     (apply update-rgb! cc
		    (map (lambda (i)
			    (* 255 (/ (string->number i) 100)))
			 (cdr m)))))
	 ((assoc val css2-colors)
	  =>
	  (lambda (v) (apply update-rgb! cc v))))))

;*---------------------------------------------------------------------*/
;*    colorchooser-opacity ...                                         */
;*---------------------------------------------------------------------*/
(define (colorchooser-opacity el)
   (let ((cc (if (string? el) (dom-get-element-by-id el) el)))
      (/ (spinbutton-value cc.spinbuttonopacity) 255)))
   
;*---------------------------------------------------------------------*/
;*    colorchooser-opacity-set! ...                                    */
;*---------------------------------------------------------------------*/
(define (colorchooser-opacity-set! el a)
   (let ((cc (if (string? el) (dom-get-element-by-id el) el))
	 (v (* a 255)))
      (spinbutton-value-set! cc.spinbuttonopacity v)
      (slider-value-set! cc.slideropacity v)
      (node-style-set! cc.color :opacity v)))
   
