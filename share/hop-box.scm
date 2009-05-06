;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-box.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 15:57:32 2009                          */
;*    Last change :  Fri Apr  3 18:35:58 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop BOX implementation                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    <BOX> ...                                                        */
;*---------------------------------------------------------------------*/
(define (<BOX> . args)
   (let loop ((args args)
	      (attrs '())
	      (listeners '())
	      (cla "__HSS_box"))
      (cond
	 ((null? args)
	  (let ((o ((@ dom_create _) "div"
				     :class cla
				     (<SPAN> :class "hop_box_layout" "")
				     ,@(reverse! attrs))))
	     (for-each (lambda (listener)
			  (apply add-event-listener! o listener))
		       listeners)
	     (add-event-listener! o "update" hop-box-update)
	     (set! o.get-style hop-box-style-get)
	     (set! o.set-style hop-box-style-set!)
	     (set! o.orientation #f)
	     o))
	 ((or (null? (cdr args)) (not (keyword? (car args))))
	  (loop (cdr args)
		(cons (car args) attrs)
		listeners
		cla))
	 ((eq? (car args) "class")
	  (loop (cddr args)
		attrs
		listeners
		(string-append "__HSS_box " (cadr args))))
	 ((string-prefix? "on" (keyword->string (car args)))
	  (let ((s (keyword->string (car args))))
	     (loop (cddr args)
		   attrs
		   (cons (list (substring s 2 (string-length s))
			       `(lambda (event) ,(cadr args)))
			 listeners)
		   cla)))
	 (else
	  (loop (cddr args)
		(cons* (cadr args) (car args) attrs)
		listeners
		cla))))
   (<DIV> :class (string-append "__HSS_box " class)
      (<SPAN> :class "hop_box_layout" "")
      els))

;*---------------------------------------------------------------------*/
;*    hop-box-style-get ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-box-style-get el prop)
   (if (eq? prop :layout)
       (let ((span (dom-first-child el)))
	  (node-style-get span "content"))
       ((@ node-style-get _) el prop)))
   
;*---------------------------------------------------------------------*/
;*    hop-box-style-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (hop-box-style-set! el prop val)
   (if (eq? prop :layout)
       (let ((span (vector-ref el.childNodes 0)))
	  (node-style-set! span "content" val))
       ((@ node-style-set! _) el prop val)))
   
;*---------------------------------------------------------------------*/
;*    hop-box-update ...                                               */
;*---------------------------------------------------------------------*/
(define (hop-box-update event)
   (let* ((el this)
	  (orient (hop-box-style-get el :layout))
	  (oorient (if (symbol? el.orientation) el.orientation 'horizontal)))
      (unless (eq? orient oorient)
	 (let ((table (dom-last-child el)))
	    (set! el.orientation orient)
	    (let ((ntable (if (eq? orient 'horizontal)
			      ;; vertical -> horizontal
			      (<TABLE>
				 (map (lambda (row)
					 (<TR> (<TD> (dom-first-child row))))
				      (dom-child-nodes table)))
			      ;; horizontal -> vertical
			      (<TABLE>
				 (map (lambda (el)
					 (<TR> (<TD> el)))
				      (dom-first-child-node table))))))
	       (dom-replace-child! table ntable))))))
		 
      
