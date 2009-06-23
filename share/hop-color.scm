;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-color.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 14 11:31:04 2009                          */
;*    Last change :  Mon Jun 22 10:23:28 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Client side support for color selectors.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    head (for client-side dependencies)                              */
;*---------------------------------------------------------------------*/
(<HEAD> :include "hop-slider" "hop-spinbutton")

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
   (and (>= hop_config.cpu_speed 80)
	(>= hop_config.js_speed 80)))

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
;*    hopcolor-mousemove-listener ...                                  */
;*---------------------------------------------------------------------*/
(define hopcolor-mousemove-listener #f)

;*---------------------------------------------------------------------*/
;*    add-mousemove-listener! ...                                      */
;*---------------------------------------------------------------------*/
(define (add-mousemove-listener! cc proc)
   (let ((listener (if hopcolor-followmouse
		       (lambda (e) (proc e) (update-colorscales! cc))
		       proc)))
      (remove-mousemove-listener! cc)
      (add-event-listener! document "mousemove" listener)
      (set! hopcolor-mousemove-listener listener)))

;*---------------------------------------------------------------------*/
;*    remove-mousemove-listener! ...                                   */
;*---------------------------------------------------------------------*/
(define (remove-mousemove-listener! cc)
   (update-colorscales! cc)
   (when hopcolor-mousemove-listener
      (remove-event-listener! document "mousemove" hopcolor-mousemove-listener)
      (set! hopcolor-mousemove-listener #f)))

;*---------------------------------------------------------------------*/
;*    init-hmodel ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-hmodel hmodel)
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
;*    init-colorscale ...                                              */
;*---------------------------------------------------------------------*/
(define (init-colorscale scale)
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
;*    satval-mousedown ...                                             */
;*---------------------------------------------------------------------*/
(define (satval-mousedown event cc)
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
      (let ((ex (event-mouse-x event))
	    (ey (event-mouse-y event))
	    (satval cc.satval))
	 (cond
	    ((< ex (+ (hop_element_x satval) 1))
	     (set! ex (+ (hop_element_x satval) 1)))
	    ((> ex (+ (hop_element_x satval) satval.clientWidth))
	     (set! ex (+ (hop_element_x satval) satval.clientWidth))))
	 (cond
	    ((< ey (+ (hop_element_y satval) 1))
	     (set! ey (+ (hop_element_y satval) 1)))
	    ((> ey (+ (hop_element_y satval) satval.clientHeight))
	     (set! ey (+ (hop_element_y satval) satval.clientHeight))))
	 (let ((v (range 100 hopcolor-hsv-size (- ex (hop_element_x satval) 1)))
	       (s (- 100 (range 100 hopcolor-hsv-size (- ey (hop_element_y satval) 1))))
	       (x (- ex (hop_element_x satval) 1))
	       (y (- ey (hop_element_y satval) 1)))
	    (update-color! cc cc.hue s v)
	    (satval-grid-position-set! satval x y))))
   
   (add-mousemove-listener! cc handler)
   (handler event))

;*---------------------------------------------------------------------*/
;*    hue-mousedown ...                                                */
;*---------------------------------------------------------------------*/
(define (hue-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (hop_element_x cc.valcolorscale))
			  (v (range 360 hopcolor-hsv-size (- x vx 1))))
		      (set! cc.hue v)
		      (update-hsv! cc)
		      (update-hmodel! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    saturation-mousedown ...                                         */
;*---------------------------------------------------------------------*/
(define (saturation-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (hop_element_x cc.valcolorscale))
			  (v (range 100 hopcolor-hsv-size (- x vx 1))))
		      (set! cc.saturation v)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    value-mousedown ...                                              */
;*---------------------------------------------------------------------*/
(define (value-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (hop_element_x cc.valcolorscale))
			  (v (range 100 hopcolor-hsv-size (- x vx 1))))
		      (set! cc.value v)
		      (update-satval-grid! cc)))))
      (colorspace-start-drag! event cc proc)
      (node-style-set! satval.grid1 :visibility "visible")
      (node-style-set! satval.grid2 :visibility "visible")))

;*---------------------------------------------------------------------*/
;*    red-mousedown ...                                                */
;*---------------------------------------------------------------------*/
(define (red-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (hop_element_x cc.valcolorscale))
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
;*    green-mousedown ...                                              */
;*---------------------------------------------------------------------*/
(define (green-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (hop_element_x cc.valcolorscale))
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
;*    blue-mousedown ...                                               */
;*---------------------------------------------------------------------*/
(define (blue-mousedown event cc)
   (let* ((cc (dom-get-element-by-id cc))
	  (satval cc.satval)
	  (proc (lambda (x cc)
		   (let* ((vx (hop_element_x cc.valcolorscale))
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
      (let ((ex (event-mouse-x event))
	    (ey (event-mouse-y event))
	    (satval cc.satval)
	    (value cc.valcolorscale))
	 (cond
	    ((< ex (+ (hop_element_x value) 1))
	     (set! ex (+ (hop_element_x value) 1)))
	    ((> ex (+ (hop_element_x value) value.clientWidth))
	     (set! ex (+ (hop_element_x value) value.clientWidth))))
	 (let ((x (- ex (hop_element_x satval) 1))
	       (y (- ey (hop_element_y satval) 1)))
	    (proc ex cc)
	    (update-color! cc cc.hue cc.saturation cc.value))))

   (add-mousemove-listener! cc handler)
   (handler event))

;*---------------------------------------------------------------------*/
;*    hmodel-mousedown ...                                             */
;*---------------------------------------------------------------------*/
(define (hmodel-mousedown event cc)
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
	     (y (hop_element_y hmodel)))
	 (cond
	    ((< ey (+ (hop_element_y hmodel) 1))
	     (set! ey (+ (hop_element_y hmodel) 1)))
	    ((> ey (+ (hop_element_y hmodel) hmodel.clientHeight))
	     (set! ey (+ (hop_element_y hmodel) hmodel.clientHeight))))
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
;*    update-colorscales! ...                                          */
;*---------------------------------------------------------------------*/
(define (update-colorscales! cc)
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
;*    update-hue! ...                                                  */
;*---------------------------------------------------------------------*/
(define (update-hue! cc h)
   (let ((cc (dom-get-element-by-id cc)))
      (update-color! cc h cc.saturation cc.value)
      (update-hmodel! cc)
      (update-hsv! cc)
      (update-colorscales! cc h cc.value cc.saturation)))

;*---------------------------------------------------------------------*/
;*    update-saturation! ...                                           */
;*---------------------------------------------------------------------*/
(define (update-saturation! cc s)
   (let ((cc (dom-get-element-by-id cc)))
      (update-color! cc cc.hue s cc.value)
      (update-satval-grid! cc)
      (update-colorscales! cc cc.hue s cc.value)))

;*---------------------------------------------------------------------*/
;*    update-value! ...                                                */
;*---------------------------------------------------------------------*/
(define (update-value! cc v)
   (let ((cc (dom-get-element-by-id cc)))
      (update-color! cc cc.hue cc.saturation v)
      (update-satval-grid! cc)
      (update-colorscales! cc cc.hue cc.saturation v)))

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
      (update-colorscales! cc h cc.value cc.saturation)
      (node-style-set! cc.satval.grid1 :visibility "visible")
      (node-style-set! cc.satval.grid2 :visibility "visible")
      (update-satval-grid! cc)))

;*---------------------------------------------------------------------*/
;*    update-red! ...                                                  */
;*---------------------------------------------------------------------*/
(define (update-red! cc r)
   (let ((cc (dom-get-element-by-id cc)))
      (multiple-value-bind (_ g b)
	 (hsv->rgb cc.hue cc.saturation cc.value)
	 (update-rgb! cc r g b))))
   
;*---------------------------------------------------------------------*/
;*    update-green! ...                                                */
;*---------------------------------------------------------------------*/
(define (update-green! cc g)
   (let ((cc (dom-get-element-by-id cc)))
      (multiple-value-bind (r _ b)
	 (hsv->rgb cc.hue cc.saturation cc.value)
	 (update-rgb! cc r g b))))
   
;*---------------------------------------------------------------------*/
;*    update-blue! ...                                                 */
;*---------------------------------------------------------------------*/
(define (update-blue! cc b)
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
       (set! cc.onchange
	     (eval (string-append "function(event) {"
				  (cc.getAttribute "onchange")
				  "}")))
       (hop-colorchooser-onchange cc))))

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
      ((procedure? cc.onselect)
       (cc.onselect))
      ((string? (cc.getAttribute "onselect"))
       (set! cc.onselect
	     (eval (string-append "function(event) {"
				  (cc.getAttribute "onselect")
				  "}")))
       (hop-colorchooser-onselect cc))))

;*---------------------------------------------------------------------*/
;*    colorchooser-value ...                                           */
;*---------------------------------------------------------------------*/
(define (colorchooser-value el)
   (let ((cc (if (string? el) (dom-get-element-by-id el) el)))
      cc.rgb.value))

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
	 ((pregexp-match "rgb([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*)" val)
	  =>
	  (lambda (m)
	     (apply update-rgb! cc (map string->number (cdr m)))))
	 ((pregexp-match "rgb([ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*)" val)
	  =>
	  (lambda (m)
	     (apply update-rgb! cc
		    (map (lambda (i)
			    (* 255 (/ (string->number i) 100)))
			 (cdr m)))))
	 ((assoc val css2-colors)
	  =>
	  (lambda (v) (apply update-rgb! cc v))))))

