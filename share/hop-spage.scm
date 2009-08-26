;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-spage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 14 08:20:41 2009                          */
;*    Last change :  Tue Aug 25 12:11:56 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    SlidePage client-side implementation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    spage-slide ...                                                  */
;*---------------------------------------------------------------------*/
(define (spage-slide node width step)
   (if (< step 0)
       (begin
	  (node-style-set! node :left (format "~apx" width))
	  (node-style-set! node :overflow "auto")
	  (node-style-set! node :visibility "visible"))
       (node-style-set! node :overflow "hidden"))
   (let ((w (if (> step 0) 0 width)))
      (timeout 10
	       (lambda ()
		  (set! w (+ w step))
		  (if (if (> step 0) (< w width) (>= w 0))
		      (let ((s (format "~apx" w))
			    (r (if (> step 0)
				   (format "rect(0,0,0,~a)" w)
				   (format "rect(0,~a,0,0)" (- width w)))))
			 (node-style-set! node :clip r)
			 (node-style-set! node :left s)
			 #t)
		      (begin
			 (node-style-set! node :clip "auto")
			 (if (> step 0)
			     (dom-remove-child! (dom-parent-node node) node)
			     (begin
				(node-style-set! node :left 0)
				(node-style-set! node :right 0)))
			 #f))))))

;*---------------------------------------------------------------------*/
;*    spage-fade ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-fade node step)
   (if (> step 0)
       (begin
	  (node-style-set! node :opacity 0.)
	  (node-style-set! node :visibility "visible"))
       (node-style-set! node :opacity 1.0))
   (let ((o (if (> step 0) 0. 1.)))
      (timeout 10
	       (lambda ()
		  (set! o (+ o step))
		  (if (if (> step 0) (<= o 1.) (>= o 0.))
		      (begin
			 (node-style-set! node :opacity (number->string o))
			 #t)
		      (begin
			 (if (> step 0)
			     (node-style-set! node :opacity 1.0)
			     (dom-remove-child! (dom-parent-node node) node))
			 #f))))))

;*---------------------------------------------------------------------*/
;*    spage-none ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-none node step)
   (if (> step 0)
       (node-style-set! node :visible "visible")
       (dom-remove-child! (dom-parent-node node) node)))

;*---------------------------------------------------------------------*/
;*    spage-effect ...                                                 */
;*---------------------------------------------------------------------*/
(define (spage-effect node width step)
   (cond
      (#t
       (spage-fade node (if (> step 0) -0.3 0.2)))
      (#f
       (spage-slide node width step))
      (#t
       (if (< width 600)
	   (spage-slide node width step)
	   (spage-fade node (if (> step 0) -0.3 0.25))))
      (else
       (spage-none node step))))

;*---------------------------------------------------------------------*/
;*    spage-show ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-show parent svc title)
   (with-hop (svc)
      (lambda (node)
	 (let* ((c (dom-first-child parent))
		(z (node-style-get c :zIndex))
		(nz (cond
		       ((integer? z) (+ 1 z))
		       ((string? z) (+ 1 (string->number z)))
		       (else 1))))
	    (node-style-set! node :zIndex nz)
	    (node-style-set! node :visibility "hidden")
	    (node-style-set! node :overflow "hidden")
	    (dom-insert-before! parent node c)
	    (let ((c (dom-first-child (dom-first-child node))))
	       (set! c.title title))
	    (spage-effect node parent.offsetWidth -20)))))

;*---------------------------------------------------------------------*/
;*    spage-push ...                                                   */
;*---------------------------------------------------------------------*/
(define (spage-push el svc)
   (let ((parent (if (string? el) (dom-get-element-by-id el) el)))
      (let* ((n (dom-first-child parent))
	     (title (if (string=? (dom-get-attribute n "hssclass") "hop-spage-head")
			(let ((c (dom-first-child n)))
			   c.title)
			(let ((c (cadr (dom-child-nodes (dom-first-child n)))))
			   c.innerHTML))))
	 (spage-show parent svc title))))

;*---------------------------------------------------------------------*/
;*    spage-pop ...                                                    */
;*---------------------------------------------------------------------*/
(define (spage-pop el)
   (let ((el (if (string? el) (dom-get-element-by-id el) el)))
      (spage-effect el el.offsetWidth 20)))

;*---------------------------------------------------------------------*/
;*    spage-update ...                                                 */
;*---------------------------------------------------------------------*/
(define (spage-update el svc)
   (let ((el (if (string? el) (dom-get-element-by-id el) el)))
      (with-hop (svc)
	 (lambda (node)
	    (innerHTML-set! el "")
	    (for-each (lambda (n)
			 (dom-append-child! el n))
		      (if (pair? node)
			  node
			  (dom-child-nodes node)))))))
