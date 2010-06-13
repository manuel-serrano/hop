;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/share/hop-orientation.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  8 08:57:19 2009                          */
;*    Last change :  Tue Dec  8 12:13:32 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Client side code to switch from portrait to landscape mode.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-orientation
   (export (hop-orientation-listener e))
   (JS hop_orientation))

;*---------------------------------------------------------------------*/
;*    hop-orientation-event ...                                        */
;*---------------------------------------------------------------------*/
(define hop-orientation-event
   (let ((e (document.createEvent "HTMLEvents")))
      (e.initEvent "resize" #t #t)
      e))

;*---------------------------------------------------------------------*/
;*    hop-orientation-listener ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-orientation-listener e)
   (let ((o (cond
	       ((> e.x 0.5) 'portrait)
	       ((< e.x -0.5) 'upsidedown)
	       (else 'landscape))))
      (unless (eq? o hop_orientation)
	 (set! hop_orientation o)
	 (let ((t (case o
		     ((portrait)
		      (format "matrix(0,-1,1,0,0,~apx)" window.innerHeight))
		     (else
		      "rotate(0deg)"))))
	    (node-style-set! document.body :MozTransformOrigin "top left")
	    (node-style-set! document.body :MozTransform t)
	    (window.dispatchEvent hop-orientation-event)))))
      
;*---------------------------------------------------------------------*/
;*    hop-orientation-init ...                                         */
;*---------------------------------------------------------------------*/
(add-event-listener! window "load"
   (lambda (_)
      (add-event-listener! window "MozOrientation"
	 hop-orientation-listener #t)))
   
