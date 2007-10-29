;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/doc/doc.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 22 08:00:34 2007                          */
;*    Last change :  Mon Oct 29 14:51:49 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Doc weblet library                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    api-history-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (api-history-handler id entry)
   (notepad-select "doc-notepad" "doc-api" #f)
   (with-hop (api-history-service entry)
      (lambda (r)
	 (let ((el (dom-get-element-by-id document id)))
	    (set! el.innerHTML r)))))

;*---------------------------------------------------------------------*/
;*    weblet-history-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (weblet-history-handler id entry)
   (notepad-select "doc-notepad" "doc-weblets" #f)
   (with-hop (weblet-history-service entry)
      (lambda (r)
	 (let ((el (dom-get-element-by-id document id)))
	    (set! el.innerHTML r)))))

;*---------------------------------------------------------------------*/
;*    tutorial-history-handler ...                                     */
;*---------------------------------------------------------------------*/
(define (tutorial-history-handler id entry)
   (notepad-select "doc-notepad" "doc-tutorials" #f)
   (with-hop (tutorial-history-service entry)
      (lambda (r)
	 (let ((el (dom-get-element-by-id document id)))
	    (set! el.innerHTML r)))))

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
;*    history services ...                                             */
;*    -------------------------------------------------------------    */
;*    These variables are initialized at loadtime by the document.     */
;*---------------------------------------------------------------------*/
(define api-history-service #unspecified)
(define weblet-history-service #unspecified)
(define tutorial-history-service #unspecified)

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
	 (set! el.innerHTML r)
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
