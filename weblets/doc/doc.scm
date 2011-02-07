;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/weblets/doc/doc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 22 08:00:34 2007                          */
;*    Last change :  Wed Feb  2 06:59:49 2011 (serrano)                */
;*    Copyright   :  2007-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Doc weblet library                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopdoc_client
   
   $(import hopdoc
	    hopdoc_tutorials
	    hopdoc_api)
   
   (export (select-api-doc svc entry el node-id)
	   (select-api-pad api k pad)
	   (tutorialref id path)

	   api-history
	   weblet-history
	   tutorial-history))

;*---------------------------------------------------------------------*/
;*    api-history-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (api-history-handler id entry)
   (notepad-select "doc-notepad" "doc-api" #f)
   (with-hop ($doc/api entry)
      (lambda (r)
	 (let ((el (dom-get-element-by-id document id)))
	    (innerHTML-set! el r)))))

;*---------------------------------------------------------------------*/
;*    weblet-history-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (weblet-history-handler id entry)
   (notepad-select "doc-notepad" "doc-weblets" #f)
   (with-hop ($doc/api-sans-title entry)
      (lambda (r)
	 (let ((el (dom-get-element-by-id document id)))
	    (innerHTML-set! el r)))))

;*---------------------------------------------------------------------*/
;*    tutorial-history-handler ...                                     */
;*---------------------------------------------------------------------*/
(define (tutorial-history-handler id entry)
   (notepad-select "doc-notepad" "doc-tutorials" #f)
   (with-hop ($doc/tutorial entry)
      (lambda (r)
	 (let ((el (dom-get-element-by-id document id)))
	    (innerHTML-set! el r)))))

;*---------------------------------------------------------------------*/
;*    history managers                                                 */
;*---------------------------------------------------------------------*/
(define api-history
   (make-history "api" api-history-handler))

(define weblet-history
   (make-history "web" weblet-history-handler))

(define tutorial-history
   (make-history "tut" tutorial-history-handler))

;*---------------------------------------------------------------------*/
;*    make-fade-vector ...                                             */
;*---------------------------------------------------------------------*/
(define (make-fade-vector last-color)
   (vector "#fffd6b"
	   "#ffff6f"
	   "#ffff8f"
	   "#ffffaf"
	   "#ffffdf"
	   "#ffffdf"
	   (if (eq? last-color #unspecified) "transparent" last-color)))

;*---------------------------------------------------------------------*/
;*    select-api-doc ...                                               */
;*---------------------------------------------------------------------*/
(define (select-api-doc svc entry el node-id)
   (history-add! api-history el.id entry)
   (with-hop (svc entry)
      (lambda (r)
	 (innerHTML-set! el r)
	 (when (and node-id (not (eq? node-id #unspecified)))
	    (let ((eo (dom-get-element-by-id document node-id)))
	       (when (and eo (not (null? eo)))
		  (let ((po eo.offsetTop))
		     (when (> po 50) (window.scrollTo 0 (- po 50)))
		     (let* ((p (dom-parent-node eo))
			    (bd (node-style-get p "border"))
			    (ob (if (eq? bd #unspecified) bd bd.background))
			    (fad (make-fade-vector ob)))
			(hop-fx-fade-background p 500 40 fad)))))))))

;*---------------------------------------------------------------------*/
;*    select-api-pad ...                                               */
;*---------------------------------------------------------------------*/
(define (select-api-pad api k pad)
   (let ((el (document.getElementById (string-append "pad-" pad))))
       (with-history
	  (lambda ()
	     (notepad-select "doc-notepad" (string-append "doc-" pad))
	     (select-api-doc $doc/api api el k)))))

;*---------------------------------------------------------------------*/
;*    tutorialref ...                                                  */
;*---------------------------------------------------------------------*/
(define (tutorialref id path)
   (with-history
    (lambda ()
       (history-add! tutorial-history id path)
       (with-hop ($doc/tutorial path)
	  (lambda (h)
	     (let ((el (dom-get-element-by-id document id)))
		(innerHTML-set! el h)
		(notepad-select "doc-notepad" "doc-tutorials")
		(window.scrollTo 0 0)))))))

;*---------------------------------------------------------------------*/
;*    jsref ...                                                        */
;*---------------------------------------------------------------------*/
(define (jsref)
   (notepad-select "doc-notepad" "doc-js")
   (window.scrollTo 0 0))


