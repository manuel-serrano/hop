;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/toc.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 12 15:53:32 2006                          */
;*    Last change :  Fri Apr 14 07:43:17 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wiki toc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwiki_toc

   (library hop)

   (export  (hop-wiki->toc ::obj #!key (ul <UL>) (li <LI>) (max-depth 3))))

;*---------------------------------------------------------------------*/
;*    hop-wiki->toc ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-wiki->toc obj #!key (ul <UL>) (li <LI>) (max-depth 3))
   (multiple-value-bind (res _)
      (inner-toc obj ul li max-depth)
      res))

;*---------------------------------------------------------------------*/
;*    inner-toc ...                                                    */
;*---------------------------------------------------------------------*/
(define (inner-toc obj ul li max-depth)
   (let loop ((obj obj)
	      (res '())
	      (level 0))
      (cond
	 ((not (pair? obj))
	  (values (reverse! res) '()))
	 ((pair? (car obj))
	  (loop (append (car obj) (cdr obj)) res level))
	 ((and (xml-element? (car obj))
	       (eq? (xml-element-markup (car obj)) 'div))
	  (loop (append (xml-element-body (car obj)) (cdr obj)) res level))
	 ((null? (cdr obj))
	  (values (reverse! res) '()))
	 (else
	  (let ((fst (car obj))
		(snd (cadr obj)))
	     (cond
		((is-section? fst snd)
		 (let ((depth (section-depth fst snd)))
		    (if (>fx depth max-depth)
			(loop (cddr obj) res level)
			(cond
			   ((=fx depth level)
			    (let* ((mark (section-mark fst snd))
				   (a (<A> :href (string-append "#" mark)
					   (section-title fst snd))))
			       (loop (cddr obj) (cons (li a) res) level)))
			   ((>fx depth level)
			    (multiple-value-bind (res2 rest)
			       (loop obj '() depth)
			       (if (pair? res2)
				   (loop rest (cons (ul res2) res) level)
				   (loop rest res level))))
			   (else
			    (values (reverse! res) obj))))))
		((and (xml-element? fst) (eq? (xml-element-markup fst) 'div))
		 (loop (append (xml-element-body fst) (cdr obj)) res level))
		(else
		 (loop (cdr obj) res level))))))))
   
;*---------------------------------------------------------------------*/
;*    is-section? ...                                                  */
;*---------------------------------------------------------------------*/
(define (is-section? fst snd)
   (and (xml-element? fst)
	(xml-element? snd)
	(eq? (xml-element-markup fst) 'a)
	(memq (xml-element-markup snd) '(h1 h2 h3 h4))))

;*---------------------------------------------------------------------*/
;*    section-depth ...                                                */
;*---------------------------------------------------------------------*/
(define (section-depth fst snd)
   (case (xml-element-markup snd)
      ((h1) 1)
      ((h2) 2)
      ((h3) 3)
      ((h4) 4)
      (else 5)))

;*---------------------------------------------------------------------*/
;*    section-mark ...                                                 */
;*---------------------------------------------------------------------*/
(define (section-mark fst snd)
   (dom-get-attribute fst "name"))

;*---------------------------------------------------------------------*/
;*    section-title ...                                                */
;*---------------------------------------------------------------------*/
(define (section-title fst snd)
   (xml-element-body snd))
   
      
