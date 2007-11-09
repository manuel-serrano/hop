;*=====================================================================*/
;*    serrano/prgm/project/hop/share/hop-canvas.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov  3 08:24:25 2007                          */
;*    Last change :  Fri Nov  9 15:48:09 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOP Canvas interface                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    canvas-get-context ...                                           */
;*---------------------------------------------------------------------*/
(define (canvas-get-context canvas context)
   (canvas.getContext context))

;*---------------------------------------------------------------------*/
;*    canvas-properties ...                                            */
;*---------------------------------------------------------------------*/
(define (canvas-properties ctx)
   `(:fill-style ,ctx.fillStyle
		 :global-alpha ,ctx.globalAlpha
		 :global-composite-operation ,ctx.globalCompositeOperation
		 :line-cap ,ctx.lineCap
		 :line-join ,ctx.lineJoin
		 :line-width ,ctx.lineWidth
		 :miter-limit ,ctx.miterLimit
		 :shadow-blur ,ctx.shadowBlue
		 :shadow-color ,ctx.shadowColor
		 :shadow-offset-x ,ctx.shadowOffsetX
		 :shadow-offset-y ,ctx.shadowOffsetY
		 :stroke-style  ,ctx.strokeStyle))
		 
;*---------------------------------------------------------------------*/
;*    canvas-properties-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (canvas-properties-set! ctx . props)
   (let loop ((props props))
      (when (pair? props)
	 (cond
	    ((null? (cdr props))
	     (error 'canvas-property-set! "Illegal property attribute" props))
	    ((not (keyword? (car props)))
	     (error 'canvas-property-set! "Illegal property keyword" props))
	    (else
	     (case (car props)
		((:fill-style)
		 (set! ctx.fillStyle (cadr props)))
		((:global-alpha)
		 (set! ctx.globalAlpha (cadr props)))
		((:global-composition-operation)
		 (set! ctx.globalCompositionOperation (cadr props)))
		((:line-cap)
		 (set! ctx.lineCap (cadr props)))
		((:line-join)
		 (set! ctx.lineJoin (cadr props)))
		((:line-width)
		 (set! ctx.lineWidth (cadr props)))
		((:miter-limit)
		 (set! ctx.miterLimit (cadr props)))
		((:shadow-blur)
		 (set! ctx.shadowBlue (cadr props)))
		((:shadow-color)
		 (set! ctx.shadowColor (cadr props)))
		((:shadow-offset-x)
		 (set! ctx.shadowOffsetX (cadr props)))
		((:shadow-offset-y)
		 (set! ctx.shadowOffsetY (cadr props)))
		((:stroke-style)
		 (set! ctx.strokeStyle (cadr props))))))
	 (loop (cddr props)))))

;*---------------------------------------------------------------------*/
;*    canvas-restore ...                                               */
;*---------------------------------------------------------------------*/
(define (canvas-restore ctx)
   (ctx.restore))

;*---------------------------------------------------------------------*/
;*    canvas-save ...                                                  */
;*---------------------------------------------------------------------*/
(define (canvas-save ctx)
   (ctx.save))

;*---------------------------------------------------------------------*/
;*    canvas-rotate ...                                                */
;*---------------------------------------------------------------------*/
(define (canvas-rotate ctx angle)
   (ctx.rotate angle))

;*---------------------------------------------------------------------*/
;*    canvas-scale ...                                                 */
;*---------------------------------------------------------------------*/
(define (canvas-scale ctx sx sy)
   (ctx.scale sx sy))

;*---------------------------------------------------------------------*/
;*    canvas-translate ...                                             */
;*---------------------------------------------------------------------*/
(define (canvas-translate ctx tx ty)
   (ctx.translate tx ty))

;*---------------------------------------------------------------------*/
;*    canvas-arc ...                                                   */
;*---------------------------------------------------------------------*/
(define (canvas-arc ctx x y radius sa ea clockwise)
   (ctx.arc x y radius sa ea clockwise))

;*---------------------------------------------------------------------*/
;*    canvas-arc-to ...                                                */
;*---------------------------------------------------------------------*/
(define (canvas-arc-to ctx x0 y0 x1 y1 radius)
   (ctx.arc-to x0 y0 x1 y1 radius))

;*---------------------------------------------------------------------*/
;*    canvas-begin-path ...                                            */
;*---------------------------------------------------------------------*/
(define (canvas-begin-path ctx)
   (ctx.beginPath))

;*---------------------------------------------------------------------*/
;*    canvas-close-path ...                                            */
;*---------------------------------------------------------------------*/
(define (canvas-close-path ctx)
   (ctx.closePath))

;*---------------------------------------------------------------------*/
;*    canvas-stroke ...                                                */
;*---------------------------------------------------------------------*/
(define (canvas-stroke ctx)
   (ctx.stroke))

;*---------------------------------------------------------------------*/
;*    canvas-fill ...                                                  */
;*---------------------------------------------------------------------*/
(define (canvas-fill ctx)
   (ctx.fill))

;*---------------------------------------------------------------------*/
;*    canvas-clip ...                                                  */
;*---------------------------------------------------------------------*/
(define (canvas-clip ctx)
   (ctx.clip))

;*---------------------------------------------------------------------*/
;*    canvas-line ...                                                  */
;*---------------------------------------------------------------------*/
(define (canvas-line ctx x0 y0 . rest)
   (ctx.moveTo x0 y0)
   (let loop ((rest rest))
      (when (pair? rest)
	 (if (null? (cdr rest))
	     (error 'canvas-line "Illegal number of arguments" rest)
	     (begin
		(ctx.moveTo (car rest) (cadr rest))
		(loop (cddr rest)))))))
   
;*---------------------------------------------------------------------*/
;*    canvas-line-to ...                                               */
;*---------------------------------------------------------------------*/
(define (canvas-line-to ctx x y)
   (ctx.lineTo x y))

;*---------------------------------------------------------------------*/
;*    canvas-move-to ...                                               */
;*---------------------------------------------------------------------*/
(define (canvas-move-to ctx x y)
   (ctx.moveTo x y))

;*---------------------------------------------------------------------*/
;*    canvas-fill ...                                                  */
;*---------------------------------------------------------------------*/
(define (canvas-fill ctx)
   (ctx.fill))
		       
;*---------------------------------------------------------------------*/
;*    canvas-move-to ...                                               */
;*---------------------------------------------------------------------*/
(define (canvas-move-to ctx x y)
   (ctx.moveTo x y))

;*---------------------------------------------------------------------*/
;*    canvas-bezier-curve-to ...                                       */
;*---------------------------------------------------------------------*/
(define (canvas-bezier-curve-to ctx x0 y0 x1 y1 x y)
   (ctx.bezierCurveTo x0 y0 x1 y1 x y))

;*---------------------------------------------------------------------*/
;*    canvas-bezier-curve ...                                          */
;*---------------------------------------------------------------------*/
(define (canvas-bezier-curve ctx x0 y0 . rest)
   (ctx.moveTo x0 y0)
   (let loop ((r rest))
      (when (pair? r)
	 (let* ((cp1x (car r))
		(cp1y (cadr r))
		(r (cddr r))
		(cp2x (car r))
		(cp2y (cadr r))
		(r (cddr r)))
	    (ctx.bezierCurveTo cp1x cp1y cp2x cp2y (car r) (cadr r))
	    (loop (cddr r))))))

;*---------------------------------------------------------------------*/
;*    canvas-quadratic-curve-to ...                                    */
;*---------------------------------------------------------------------*/
(define (canvas-quadratic-curve-to ctx x0 y0 x1 y1)
   (ctx.quadraticCurveTo x0 y0 x1 y1))

;*---------------------------------------------------------------------*/
;*    canvas-quadratic-curve ...                                       */
;*---------------------------------------------------------------------*/
(define (canvas-quadratic-curve ctx x0 y0 . rest)
   (ctx.moveTo x0 y0)
   (let loop ((r rest))
      (when (pair? r)
	 (ctx.quadraticCurveTo (car r) (cadr r) (caddr r) (cadddr r))
	 (loop (cddddr r)))))

;*---------------------------------------------------------------------*/
;*    canvas-clear-rect ...                                            */
;*---------------------------------------------------------------------*/
(define (canvas-clear-rect ctx x0 y0 x1 y1)
   (ctx.clearRect x0 y0 x1 y1))

;*---------------------------------------------------------------------*/
;*    canvas-fill-rect ...                                             */
;*---------------------------------------------------------------------*/
(define (canvas-fill-rect ctx x0 y0 x1 y1)
   (ctx.fillRect x0 y0 x1 y1))

;*---------------------------------------------------------------------*/
;*    canvas-stroke-rect ...                                           */
;*---------------------------------------------------------------------*/
(define (canvas-stroke-rect ctx x0 y0 x1 y1)
   (ctx.strokeRect x0 y0 x1 y1))

;*---------------------------------------------------------------------*/
;*    canvas-create-linear-gradient ...                                */
;*---------------------------------------------------------------------*/
(define (canvas-create-linear-gradient ctx x1 y1 x2 y2)
   (ctx.createLinearGradient x1 y1 x2 y2))

;*---------------------------------------------------------------------*/
;*    canvas-create-radial-gradient ...                                */
;*---------------------------------------------------------------------*/
(define (canvas-create-radial-gradient ctx x1 y1 r1 x2 y2 r2)
   (ctx.createRadialGradient x1 y1 r1 x2 y2 r2))

;*---------------------------------------------------------------------*/
;*    canvas-add-color-stop ...                                        */
;*---------------------------------------------------------------------*/
(define (canvas-add-color-stop gradient position color)
   (gradient.addColorStop position color))

;*---------------------------------------------------------------------*/
;*    canvas-create-pattern ...                                        */
;*---------------------------------------------------------------------*/
(define (canvas-create-pattern ctx img type)
   (ctx.createPattern img type))

;*---------------------------------------------------------------------*/
;*    canvas-draw-image ...                                            */
;*---------------------------------------------------------------------*/
(define (canvas-draw-image ctx image x y . rest)
   (if (null? rest)
       (ctx.drawImage image x y)
       (if (and (pair? rest) (pair? (cdr rest)))
	   (if (null? (cdr rest))
	       (ctx.drawImage image x y (car rest) (cadr rest))
	       (let* ((sw (car rest))
		      (sh (cadr rest))
		      (rest (cddr rest)))
		  (ctx.drawImage image x y sw sh
				 (car rest) (cadr rest)
				 (caddr rest) (cadddr rest))))
	   (error 'canvas-draw-image "Illegal number of arguments"  rest))))

;*---------------------------------------------------------------------*/
;*    canvas-arrow-to ...                                              */
;*---------------------------------------------------------------------*/
(define (canvas-arrow-to ctx x0 y0 x1 y1 . args)
   (let* ((len (* 5 ctx.lineWidth))
	  (an 0.4)
	  (head #t)
	  (tail #f))
      ;; arguments parsing
      (let loop ((args args))
	 (when (pair? args)
	    (if (null? (cdr args))
		(error 'canvas-arrow-to "Illegal arguments" args)
		(cond
		   ((eq? (car args) :angle)
		    (set! an (cadr args))
		    (loop (cddr args)))
		   ((eq? (car args) :length)
		    (set! len (cadr args))
		    (loop (cddr args)))
		   ((eq? (car args) :to)
		    (set! head (cadr args))
		    (loop (cddr args)))
		   ((eq? (car args) :from)
		    (set! tail (cadr args))
		    (loop (cddr args)))
		   (else
		    (error 'canvas-arrow-to "Illegal arguments" args))))))
      (let* ((d (sqrt (+ (* (- x1 x0) (- x1 x0)) (* (- y1 y0) (- y1 y0)))))
	     (ah (let ((acos (Math.acos (/ (- x1 x0) d))))
		    (if (> y1 y0)
			acos
			(- acos))))
	     (at (+ ah Math.PI))
	     (x1a (- x1 (* len (cos (+ ah an)))))
	     (y1a (- y1 (* len (sin (+ ah an)))))
	     (x1b (- x1 (* len (cos (- ah an)))))
	     (y1b (- y1 (* len (sin (- ah an)))))
	     (x0a (- x0 (* len (cos (+ at an)))))
	     (y0a (- y0 (* len (sin (+ at an)))))
	     (x0b (- x0 (* len (cos (- at an)))))
	     (y0b (- y0 (* len (sin (- at an))))))
	 ;; the line
	 (let ((lx0 (if tail (+ x0a (/ (- x0b x0a) 2)) x0))
	       (ly0 (if tail (+ y0a (/ (- y0b y0a) 2)) y0))
	       (lx1 (if head (+ x1a (/ (- x1b x1a) 2)) x1))
	       (ly1 (if head (+ y1a (/ (- y1b y1a) 2)) y1)))
	    (ctx.beginPath)
	    (ctx.moveTo lx0 ly0)
	    (ctx.lineTo lx1 ly1)
	    (ctx.stroke))
	 ;; the head
	 (when head
	    (ctx.beginPath)
	    (ctx.moveTo x1 y1)
	    (ctx.lineTo x1a y1a)
	    (ctx.lineTo x1b y1b)
	    (ctx.closePath)
	    (ctx.fill))
	 ;; the tail
	 (when tail
	    (ctx.beginPath)
	    (ctx.moveTo x0 y0)
	    (ctx.lineTo x0a y0a)
	    (ctx.lineTo x0b y0b)
	    (ctx.closePath)
	    (ctx.fill)))))

;*---------------------------------------------------------------------*/
;*    canvas-quadratic-arrow-to ...                                    */
;*---------------------------------------------------------------------*/
(define (canvas-quadratic-arrow-to ctx x0 y0 cpx cpy x1 y1 . args)
   (let* ((len (* 5 ctx.lineWidth))
	  (an 0.4)
	  (head #t)
	  (tail #f))
      ;; arguments parsing
      (let loop ((args args))
	 (when (pair? args)
	    (if (null? (cdr args))
		(error 'canvas-arrow-to "Illegal arguments" args)
		(cond
		   ((eq? (car args) :angle)
		    (set! an (cadr args))
		    (loop (cddr args)))
		   ((eq? (car args) :length)
		    (set! len (cadr args))
		    (loop (cddr args)))
		   ((eq? (car args) :to)
		    (set! head (cadr args))
		    (loop (cddr args)))
		   ((eq? (car args) :from)
		    (set! tail (cadr args))
		    (loop (cddr args)))
		   (else
		    (error 'canvas-arrow-to "Illegal arguments" args))))))
      (let* ((dh (sqrt (+ (* (- x1 cpx) (- x1 cpx)) (* (- y1 cpy) (- y1 cpy)))))
	     (ah (let ((acos (Math.acos (/ (- x1 cpx) dh))))
		    (if (> y1 cpy)
			acos
			(- acos))))
	     (dt (sqrt (+ (* (- x0 cpx) (- x0 cpx)) (* (- y0 cpy) (- y0 cpy)))))
	     (at (let ((acos (Math.acos (/ (- x0 cpx) dt))))
		    (if (> y0 cpy)
			acos
			(- acos))))
	     (x1a (- x1 (* len (cos (+ ah an)))))
	     (y1a (- y1 (* len (sin (+ ah an)))))
	     (x1b (- x1 (* len (cos (- ah an)))))
	     (y1b (- y1 (* len (sin (- ah an)))))
	     (x0a (- x0 (* len (cos (+ at an)))))
	     (y0a (- y0 (* len (sin (+ at an)))))
	     (x0b (- x0 (* len (cos (- at an)))))
	     (y0b (- y0 (* len (sin (- at an))))))
	 ;; the curve
	 (let ((cx0 (if tail (+ x0a (/ (- x0b x0a) 2)) x0))
	       (cy0 (if tail (+ y0a (/ (- y0b y0a) 2)) y0))
	       (cx1 (if head (+ x1a (/ (- x1b x1a) 2)) x1))
	       (cy1 (if head (+ y1a (/ (- y1b y1a) 2)) y1)))
	    (ctx.beginPath)
	    (ctx.moveTo cx0 cy0)
	    (ctx.quadraticCurveTo cpx cpy cx1 cy1)
	    (ctx.stroke))
	 ;; the head
	 (when head
	    (ctx.beginPath)
	    (ctx.moveTo x1 y1)
	    (ctx.lineTo x1a y1a)
	    (ctx.lineTo x1b y1b)
	    (ctx.closePath)
	    (ctx.fill))
	 ;; the tail
	 (when tail
	    (ctx.beginPath)
	    (ctx.moveTo x0 y0)
	    (ctx.lineTo x0a y0a)
	    (ctx.lineTo x0b y0b)
	    (ctx.closePath)
	    (ctx.fill)))))
      
;*---------------------------------------------------------------------*/
;*    Internet Explorer 7 Canvas support                               */
;*    -------------------------------------------------------------    */
;*    Copyright 2006 Google Inc.                                       */
;*                                                                     */
;*      http://sourceforge.net/projects/excanvas/                      */
;*                                                                     */
;*    Licensed under the Apache License, Version 2.0 (the "License");  */
;*    you may not use this file except in compliance with the License. */
;*    You may obtain a copy of the License at                          */
;*                                                                     */
;*      http://www.apache.org/licenses/LICENSE-2.0                     */
;*                                                                     */
;*    Unless required by applicable law or agreed to in writing,       */
;*    software distributed under the License is distributed on an      */
;*    "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,     */
;*    either express or implied. See the License for the specific      */
;*    language governing permissions and limitations under the License.*/
;*---------------------------------------------------------------------*/
(pragma { (function() {
if (!window.CanvasRenderingContext2D) {
(function () {

  // alias some functions to make (compiled) code shorter
  var m = Math;
  var mr = m.round;
  var ms = m.sin;
  var mc = m.cos;

  // this is used for sub pixel precision
  var Z = 10;
  var Z2 = Z / 2;

  var G_vmlCanvasManager_ = {
    init: function (opt_doc) {
      var doc = opt_doc || document;
      if (/MSIE/.test(navigator.userAgent) && !window.opera) {
        var self = this;
        doc.attachEvent("onreadystatechange", function () {
          self.init_(doc);
        });
      }
    },

    init_: function (doc) {
      if (doc.readyState == "complete") {
        // create xmlns
        if (!doc.namespaces["g_vml_"]) {
          doc.namespaces.add("g_vml_", "urn:schemas-microsoft-com:vml");
        }

        // setup default css
        var ss = doc.createStyleSheet();
        ss.cssText = "canvas{display:inline-block;overflow:hidden;" +
            // default size is 300x150 in Gecko and Opera
            "text-align:left;width:300px;height:150px}" +
            "g_vml_\\:*{behavior:url(#default#VML)}";

        // find all canvas elements
        var els = doc.getElementsByTagName("canvas");
        for (var i = 0; i < els.length; i++) {
          if (!els[i].getContext) {
            this.initElement(els[i]);
          }
        }
      }
    },

    fixElement_: function (el) {
      // in IE before version 5.5 we would need to add HTML: to the tag name
      // but we do not care about IE before version 6
      var outerHTML = el.outerHTML;

      var newEl = el.ownerDocument.createElement(outerHTML);
      // if the tag is still open IE has created the children as siblings and
      // it has also created a tag with the name "/FOO"
      if (outerHTML.slice(-2) != "/>") {
        var tagName = "/" + el.tagName;
        var ns;
        // remove content
        while ((ns = el.nextSibling) && ns.tagName != tagName) {
          ns.removeNode();
        }
        // remove the incorrect closing tag
        if (ns) {
          ns.removeNode();
        }
      }
      el.parentNode.replaceChild(newEl, el);
      return newEl;
    },

    /**
     * Public initializes a canvas element so that it can be used as canvas
     * element from now on. This is called automatically before the page is
     * loaded but if you are creating elements using createElement you need to
     * make sure this is called on the element.
     * @param {HTMLElement} el The canvas element to initialize.
     * @return {HTMLElement} the element that was created.
     */
    initElement: function (el) {
      el = this.fixElement_(el);
      el.getContext = function () {
        if (this.context_) {
          return this.context_;
        }
        return this.context_ = new CanvasRenderingContext2D_(this);
      };

      // do not use inline function because that will leak memory
      el.attachEvent('onpropertychange', onPropertyChange);
      el.attachEvent('onresize', onResize);

      var attrs = el.attributes;
      if (attrs.width && attrs.width.specified) {
        // TODO: use runtimeStyle and coordsize
        // el.getContext().setWidth_(attrs.width.nodeValue);
        el.style.width = attrs.width.nodeValue + "px";
      } else {
        el.width = el.clientWidth;
      }
      if (attrs.height && attrs.height.specified) {
        // TODO: use runtimeStyle and coordsize
        // el.getContext().setHeight_(attrs.height.nodeValue);
        el.style.height = attrs.height.nodeValue + "px";
      } else {
        el.height = el.clientHeight;
      }
      //el.getContext().setCoordsize_()
      return el;
    }
  };

  function onPropertyChange(e) {
    var el = e.srcElement;

    switch (e.propertyName) {
      case 'width':
        el.style.width = el.attributes.width.nodeValue + "px";
        el.getContext().clearRect();
        break;
      case 'height':
        el.style.height = el.attributes.height.nodeValue + "px";
        el.getContext().clearRect();
        break;
    }
  }

  function onResize(e) {
    var el = e.srcElement;
    if (el.firstChild) {
      el.firstChild.style.width =  el.clientWidth + 'px';
      el.firstChild.style.height = el.clientHeight + 'px';
    }
  }

  G_vmlCanvasManager_.init();

  // precompute "00" to "FF"
  var dec2hex = [];
  for (var i = 0; i < 16; i++) {
    for (var j = 0; j < 16; j++) {
      dec2hex[i * 16 + j] = i.toString(16) + j.toString(16);
    }
  }

  function createMatrixIdentity() {
    return [
      [1, 0, 0],
      [0, 1, 0],
      [0, 0, 1]
    ];
  }

  function matrixMultiply(m1, m2) {
    var result = createMatrixIdentity();

    for (var x = 0; x < 3; x++) {
      for (var y = 0; y < 3; y++) {
        var sum = 0;

        for (var z = 0; z < 3; z++) {
          sum += m1[x][z] * m2[z][y];
        }

        result[x][y] = sum;
      }
    }
    return result;
  }

  function copyState(o1, o2) {
    o2.fillStyle     = o1.fillStyle;
    o2.lineCap       = o1.lineCap;
    o2.lineJoin      = o1.lineJoin;
    o2.lineWidth     = o1.lineWidth;
    o2.miterLimit    = o1.miterLimit;
    o2.shadowBlur    = o1.shadowBlur;
    o2.shadowColor   = o1.shadowColor;
    o2.shadowOffsetX = o1.shadowOffsetX;
    o2.shadowOffsetY = o1.shadowOffsetY;
    o2.strokeStyle   = o1.strokeStyle;
    o2.arcScaleX_    = o1.arcScaleX_;
    o2.arcScaleY_    = o1.arcScaleY_;
  }

  function processStyle(styleString) {
    var str, alpha = 1;

    styleString = String(styleString);
    if (styleString.substring(0, 3) == "rgb") {
      var start = styleString.indexOf("(", 3);
      var end = styleString.indexOf(")", start + 1);
      var guts = styleString.substring(start + 1, end).split(",");

      str = "#";
      for (var i = 0; i < 3; i++) {
        str += dec2hex[Number(guts[i])];
      }

      if ((guts.length == 4) && (styleString.substr(3, 1) == "a")) {
        alpha = guts[3];
      }
    } else {
      str = styleString;
    }

    return [str, alpha];
  }

  function processLineCap(lineCap) {
    switch (lineCap) {
      case "butt":
        return "flat";
      case "round":
        return "round";
      case "square":
      default:
        return "square";
    }
  }

  /**
   * This class implements CanvasRenderingContext2D interface as described by
   * the WHATWG.
   * @param {HTMLElement} surfaceElement The element that the 2D context should
   * be associated with
   */
   function CanvasRenderingContext2D_(surfaceElement) {
    this.m_ = createMatrixIdentity();

    this.mStack_ = [];
    this.aStack_ = [];
    this.currentPath_ = [];

    // Canvas context properties
    this.strokeStyle = "#000";
    this.fillStyle = "#000";

    this.lineWidth = 1;
    this.lineJoin = "miter";
    this.lineCap = "butt";
    this.miterLimit = Z * 1;
    this.globalAlpha = 1;
    this.canvas = surfaceElement;

    var el = surfaceElement.ownerDocument.createElement('div');
    el.style.width =  surfaceElement.clientWidth + 'px';
    el.style.height = surfaceElement.clientHeight + 'px';
    el.style.overflow = 'hidden';
    el.style.position = 'absolute';
    surfaceElement.appendChild(el);

    this.element_ = el;
    this.arcScaleX_ = 1;
    this.arcScaleY_ = 1;
  };

  var contextPrototype = CanvasRenderingContext2D_.prototype;
  contextPrototype.clearRect = function() {
    this.element_.innerHTML = "";
    this.currentPath_ = [];
  };

  contextPrototype.beginPath = function() {
    // TODO: Branch current matrix so that save/restore has no effect
    //       as per safari docs.

    this.currentPath_ = [];
  };

  contextPrototype.moveTo = function(aX, aY) {
    this.currentPath_.push({type: "moveTo", x: aX, y: aY});
    this.currentX_ = aX;
    this.currentY_ = aY;
  };

  contextPrototype.lineTo = function(aX, aY) {
    this.currentPath_.push({type: "lineTo", x: aX, y: aY});
    this.currentX_ = aX;
    this.currentY_ = aY;
  };

  contextPrototype.bezierCurveTo = function(aCP1x, aCP1y,
                                            aCP2x, aCP2y,
                                            aX, aY) {
    this.currentPath_.push({type: "bezierCurveTo",
                           cp1x: aCP1x,
                           cp1y: aCP1y,
                           cp2x: aCP2x,
                           cp2y: aCP2y,
                           x: aX,
                           y: aY});
    this.currentX_ = aX;
    this.currentY_ = aY;
  };

  contextPrototype.quadraticCurveTo = function(aCPx, aCPy, aX, aY) {
    // the following is lifted almost directly from
    // http://developer.mozilla.org/en/docs/Canvas_tutorial:Drawing_shapes
    var cp1x = this.currentX_ + 2.0 / 3.0 * (aCPx - this.currentX_);
    var cp1y = this.currentY_ + 2.0 / 3.0 * (aCPy - this.currentY_);
    var cp2x = cp1x + (aX - this.currentX_) / 3.0;
    var cp2y = cp1y + (aY - this.currentY_) / 3.0;
    this.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, aX, aY);
  };

  contextPrototype.arc = function(aX, aY, aRadius,
                                  aStartAngle, aEndAngle, aClockwise) {
    aRadius *= Z;
    var arcType = aClockwise ? "at" : "wa";

    var xStart = aX + (mc(aStartAngle) * aRadius) - Z2;
    var yStart = aY + (ms(aStartAngle) * aRadius) - Z2;

    var xEnd = aX + (mc(aEndAngle) * aRadius) - Z2;
    var yEnd = aY + (ms(aEndAngle) * aRadius) - Z2;

    // IE won't render arches drawn counter clockwise if xStart == xEnd.
    if (xStart == xEnd && !aClockwise) {
      xStart += 0.125; // Offset xStart by 1/80 of a pixel. Use something
                       // that can be represented in binary
    }

    this.currentPath_.push({type: arcType,
                           x: aX,
                           y: aY,
                           radius: aRadius,
                           xStart: xStart,
                           yStart: yStart,
                           xEnd: xEnd,
                           yEnd: yEnd});

  };

  contextPrototype.rect = function(aX, aY, aWidth, aHeight) {
    this.moveTo(aX, aY);
    this.lineTo(aX + aWidth, aY);
    this.lineTo(aX + aWidth, aY + aHeight);
    this.lineTo(aX, aY + aHeight);
    this.closePath();
  };

  contextPrototype.strokeRect = function(aX, aY, aWidth, aHeight) {
    // Will destroy any existing path (same as FF behaviour)
    this.beginPath();
    this.moveTo(aX, aY);
    this.lineTo(aX + aWidth, aY);
    this.lineTo(aX + aWidth, aY + aHeight);
    this.lineTo(aX, aY + aHeight);
    this.closePath();
    this.stroke();
  };

  contextPrototype.fillRect = function(aX, aY, aWidth, aHeight) {
    // Will destroy any existing path (same as FF behaviour)
    this.beginPath();
    this.moveTo(aX, aY);
    this.lineTo(aX + aWidth, aY);
    this.lineTo(aX + aWidth, aY + aHeight);
    this.lineTo(aX, aY + aHeight);
    this.closePath();
    this.fill();
  };

  contextPrototype.createLinearGradient = function(aX0, aY0, aX1, aY1) {
    var gradient = new CanvasGradient_("gradient");
    return gradient;
  };

  contextPrototype.createRadialGradient = function(aX0, aY0,
                                                   aR0, aX1,
                                                   aY1, aR1) {
    var gradient = new CanvasGradient_("gradientradial");
    gradient.radius1_ = aR0;
    gradient.radius2_ = aR1;
    gradient.focus_.x = aX0;
    gradient.focus_.y = aY0;
    return gradient;
  };

  contextPrototype.drawImage = function (image, var_args) {
    var dx, dy, dw, dh, sx, sy, sw, sh;

    // to find the original width we overide the width and height
    var oldRuntimeWidth = image.runtimeStyle.width;
    var oldRuntimeHeight = image.runtimeStyle.height;
    image.runtimeStyle.width = 'auto';
    image.runtimeStyle.height = 'auto';

    // get the original size
    var w = image.width;
    var h = image.height;

    // and remove overides
    image.runtimeStyle.width = oldRuntimeWidth;
    image.runtimeStyle.height = oldRuntimeHeight;

    if (arguments.length == 3) {
      dx = arguments[1];
      dy = arguments[2];
      sx = sy = 0;
      sw = dw = w;
      sh = dh = h;
    } else if (arguments.length == 5) {
      dx = arguments[1];
      dy = arguments[2];
      dw = arguments[3];
      dh = arguments[4];
      sx = sy = 0;
      sw = w;
      sh = h;
    } else if (arguments.length == 9) {
      sx = arguments[1];
      sy = arguments[2];
      sw = arguments[3];
      sh = arguments[4];
      dx = arguments[5];
      dy = arguments[6];
      dw = arguments[7];
      dh = arguments[8];
    } else {
      throw "Invalid number of arguments";
    }

    var d = this.getCoords_(dx, dy);

    var w2 = sw / 2;
    var h2 = sh / 2;

    var vmlStr = [];

    var W = 10;
    var H = 10;

    // For some reason that I've now forgotten, using divs didn't work
    vmlStr.push(' <g_vml_:group',
                ' coordsize="', Z * W, ',', Z * H, '"',
                ' coordorigin="0,0"' ,
                ' style="width:', W, ';height:', H, ';position:absolute;');

    // If filters are necessary (rotation exists), create them
    // filters are bog-slow, so only create them if abbsolutely necessary
    // The following check doesn't account for skews (which don't exist
    // in the canvas spec (yet) anyway.

    if (this.m_[0][0] != 1 || this.m_[0][1]) {
      var filter = [];

      // Note the 12/21 reversal
      filter.push("M11='", this.m_[0][0], "',",
                  "M12='", this.m_[1][0], "',",
                  "M21='", this.m_[0][1], "',",
                  "M22='", this.m_[1][1], "',",
                  "Dx='", mr(d.x / Z), "',",
                  "Dy='", mr(d.y / Z), "'");

      // Bounding box calculation (need to minimize displayed area so that
      // filters don't waste time on unused pixels.
      var max = d;
      var c2 = this.getCoords_(dx + dw, dy);
      var c3 = this.getCoords_(dx, dy + dh);
      var c4 = this.getCoords_(dx + dw, dy + dh);

      max.x = Math.max(max.x, c2.x, c3.x, c4.x);
      max.y = Math.max(max.y, c2.y, c3.y, c4.y);

      vmlStr.push("padding:0 ", mr(max.x / Z), "px ", mr(max.y / Z),
                  "px 0;filter:progid:DXImageTransform.Microsoft.Matrix(",
                  filter.join(""), ", sizingmethod='clip');")
    } else {
      vmlStr.push("top:", mr(d.y / Z), "px;left:", mr(d.x / Z), "px;")
    }

    vmlStr.push(' ">' ,
                '<g_vml_:image src="', image.src, '"',
                ' style="width:', Z * dw, ';',
                ' height:', Z * dh, ';"',
                ' cropleft="', sx / w, '"',
                ' croptop="', sy / h, '"',
                ' cropright="', (w - sx - sw) / w, '"',
                ' cropbottom="', (h - sy - sh) / h, '"',
                ' />',
                '</g_vml_:group>');

    this.element_.insertAdjacentHTML("BeforeEnd",
                                    vmlStr.join(""));
  };

  contextPrototype.stroke = function(aFill) {
    var lineStr = [];
    var lineOpen = false;
    var a = processStyle(aFill ? this.fillStyle : this.strokeStyle);
    var color = a[0];
    var opacity = a[1] * this.globalAlpha;

    var W = 10;
    var H = 10;

    lineStr.push('<g_vml_:shape',
                 ' fillcolor="', color, '"',
                 ' filled="', Boolean(aFill), '"',
                 ' style="position:absolute;width:', W, ';height:', H, ';"',
                 ' coordorigin="0 0" coordsize="', Z * W, ' ', Z * H, '"',
                 ' stroked="', !aFill, '"',
                 ' strokeweight="', this.lineWidth, '"',
                 ' strokecolor="', color, '"',
                 ' path="');

    var newSeq = false;
    var min = {x: null, y: null};
    var max = {x: null, y: null};

    for (var i = 0; i < this.currentPath_.length; i++) {
      var p = this.currentPath_[i];

      if (p.type == "moveTo") {
        lineStr.push(" m ");
        var c = this.getCoords_(p.x, p.y);
        lineStr.push(mr(c.x), ",", mr(c.y));
      } else if (p.type == "lineTo") {
        lineStr.push(" l ");
        var c = this.getCoords_(p.x, p.y);
        lineStr.push(mr(c.x), ",", mr(c.y));
      } else if (p.type == "close") {
        lineStr.push(" x ");
      } else if (p.type == "bezierCurveTo") {
        lineStr.push(" c ");
        var c = this.getCoords_(p.x, p.y);
        var c1 = this.getCoords_(p.cp1x, p.cp1y);
        var c2 = this.getCoords_(p.cp2x, p.cp2y);
        lineStr.push(mr(c1.x), ",", mr(c1.y), ",",
                     mr(c2.x), ",", mr(c2.y), ",",
                     mr(c.x), ",", mr(c.y));
      } else if (p.type == "at" || p.type == "wa") {
        lineStr.push(" ", p.type, " ");
        var c  = this.getCoords_(p.x, p.y);
        var cStart = this.getCoords_(p.xStart, p.yStart);
        var cEnd = this.getCoords_(p.xEnd, p.yEnd);

        lineStr.push(mr(c.x - this.arcScaleX_ * p.radius), ",",
                     mr(c.y - this.arcScaleY_ * p.radius), " ",
                     mr(c.x + this.arcScaleX_ * p.radius), ",",
                     mr(c.y + this.arcScaleY_ * p.radius), " ",
                     mr(cStart.x), ",", mr(cStart.y), " ",
                     mr(cEnd.x), ",", mr(cEnd.y));
      }


      // TODO: Following is broken for curves due to
      //       move to proper paths.

      // Figure out dimensions so we can do gradient fills
      // properly
      if(c) {
        if (min.x == null || c.x < min.x) {
          min.x = c.x;
        }
        if (max.x == null || c.x > max.x) {
          max.x = c.x;
        }
        if (min.y == null || c.y < min.y) {
          min.y = c.y;
        }
        if (max.y == null || c.y > max.y) {
          max.y = c.y;
        }
      }
    }
    lineStr.push(' ">');

    if (typeof this.fillStyle == "object") {
      var focus = {x: "50%", y: "50%"};
      var width = (max.x - min.x);
      var height = (max.y - min.y);
      var dimension = (width > height) ? width : height;

      focus.x = mr((this.fillStyle.focus_.x / width) * 100 + 50) + "%";
      focus.y = mr((this.fillStyle.focus_.y / height) * 100 + 50) + "%";

      var colors = [];

      // inside radius (%)
      if (this.fillStyle.type_ == "gradientradial") {
        var inside = (this.fillStyle.radius1_ / dimension * 100);

        // percentage that outside radius exceeds inside radius
        var expansion = (this.fillStyle.radius2_ / dimension * 100) - inside;
      } else {
        var inside = 0;
        var expansion = 100;
      }

      var insidecolor = {offset: null, color: null};
      var outsidecolor = {offset: null, color: null};

      // We need to sort 'colors' by percentage, from 0 > 100 otherwise ie
      // won't interpret it correctly
      this.fillStyle.colors_.sort(function (cs1, cs2) {
        return cs1.offset - cs2.offset;
      });

      for (var i = 0; i < this.fillStyle.colors_.length; i++) {
        var fs = this.fillStyle.colors_[i];

        colors.push( (fs.offset * expansion) + inside, "% ", fs.color, ",");

        if (fs.offset > insidecolor.offset || insidecolor.offset == null) {
          insidecolor.offset = fs.offset;
          insidecolor.color = fs.color;
        }

        if (fs.offset < outsidecolor.offset || outsidecolor.offset == null) {
          outsidecolor.offset = fs.offset;
          outsidecolor.color = fs.color;
        }
      }
      colors.pop();

      lineStr.push('<g_vml_:fill',
                   ' color="', outsidecolor.color, '"',
                   ' color2="', insidecolor.color, '"',
                   ' type="', this.fillStyle.type_, '"',
                   ' focusposition="', focus.x, ', ', focus.y, '"',
                   ' colors="', colors.join(""), '"',
                   ' opacity="', opacity, '" />');
    } else if (aFill) {
      lineStr.push('<g_vml_:fill color="', color, '" opacity="', opacity, '" />');
    } else {
      lineStr.push(
        '<g_vml_:stroke',
        ' opacity="', opacity,'"',
        ' joinstyle="', this.lineJoin, '"',
        ' miterlimit="', this.miterLimit, '"',
        ' endcap="', processLineCap(this.lineCap) ,'"',
        ' weight="', this.lineWidth, 'px"',
        ' color="', color,'" />'
      );
    }

    lineStr.push("</g_vml_:shape>");

    this.element_.insertAdjacentHTML("beforeEnd", lineStr.join(""));

    this.currentPath_ = [];
  };

  contextPrototype.fill = function() {
    this.stroke(true);
  }

  contextPrototype.closePath = function() {
    this.currentPath_.push({type: "close"});
  };

  /**
   * @private
   */
  contextPrototype.getCoords_ = function(aX, aY) {
    return {
      x: Z * (aX * this.m_[0][0] + aY * this.m_[1][0] + this.m_[2][0]) - Z2,
      y: Z * (aX * this.m_[0][1] + aY * this.m_[1][1] + this.m_[2][1]) - Z2
    }
  };

  contextPrototype.save = function() {
    var o = {};
    copyState(this, o);
    this.aStack_.push(o);
    this.mStack_.push(this.m_);
    this.m_ = matrixMultiply(createMatrixIdentity(), this.m_);
  };

  contextPrototype.restore = function() {
    copyState(this.aStack_.pop(), this);
    this.m_ = this.mStack_.pop();
  };

  contextPrototype.translate = function(aX, aY) {
    var m1 = [
      [1,  0,  0],
      [0,  1,  0],
      [aX, aY, 1]
    ];

    this.m_ = matrixMultiply(m1, this.m_);
  };

  contextPrototype.rotate = function(aRot) {
    var c = mc(aRot);
    var s = ms(aRot);

    var m1 = [
      [c,  s, 0],
      [-s, c, 0],
      [0,  0, 1]
    ];

    this.m_ = matrixMultiply(m1, this.m_);
  };

  contextPrototype.scale = function(aX, aY) {
    this.arcScaleX_ *= aX;
    this.arcScaleY_ *= aY;
    var m1 = [
      [aX, 0,  0],
      [0,  aY, 0],
      [0,  0,  1]
    ];

    this.m_ = matrixMultiply(m1, this.m_);
  };

  /******** STUBS ********/
  contextPrototype.clip = function() {
    // TODO: Implement
  };

  contextPrototype.arcTo = function() {
    // TODO: Implement
  };

  contextPrototype.createPattern = function() {
    return new CanvasPattern_;
  };

  // Gradient / Pattern Stubs
  function CanvasGradient_(aType) {
    this.type_ = aType;
    this.radius1_ = 0;
    this.radius2_ = 0;
    this.colors_ = [];
    this.focus_ = {x: 0, y: 0};
  }

  CanvasGradient_.prototype.addColorStop = function(aOffset, aColor) {
    aColor = processStyle(aColor);
    this.colors_.push({offset: 1-aOffset, color: aColor});
  };

  function CanvasPattern_() {}

  // set up externs
  G_vmlCanvasManager = G_vmlCanvasManager_;
  CanvasRenderingContext2D = CanvasRenderingContext2D_;
  CanvasGradient = CanvasGradient_;
  CanvasPattern = CanvasPattern_;

})();

} // if

   })()
})

