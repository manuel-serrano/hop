;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/wiki_toc.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 12 15:53:32 2006                          */
;*    Last change :  Thu Nov 10 18:12:37 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Wiki toc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki-toc

   (import  __hop_xml-types
	    __hop_html-base
	    __hop_read
	    __hop_dom)

   (export  (hop-wiki->toc ::obj
			   #!key
			   (ul <UL>) (li <LI>) (max-depth 3)
			   is-section?
			   section-depth)))

;*---------------------------------------------------------------------*/
;*    hop-wiki->toc ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-wiki->toc obj #!key (ul <UL>) (li <LI>) (max-depth 3)
		       is-section? section-depth)
   (multiple-value-bind (res _)
      (inner-toc obj ul li max-depth
		 (or is-section? (@ is-section? __hop_wiki-toc))
		 (or section-depth (@ section-depth __hop_wiki-toc)))
      (if (pair? res)
	  (ul res)
	  res)))

;*---------------------------------------------------------------------*/
;*    inner-toc ...                                                    */
;*---------------------------------------------------------------------*/
(define (inner-toc obj ul li max-depth is-section? section-depth)
   (let loop ((obj obj)
	      (res '())
	      (depth 0))
      (cond
	 ((not (pair? obj))
	  (values (reverse! res) '()))
	 ((pair? (car obj))
	  (loop (append (car obj) (cdr obj)) res depth))
	 ((and (isa? (car obj) xml-element)
	       (with-access::xml-element (car obj) (tag)
		  (eq? tag 'div)))
	  (with-access::xml-element (car obj) (body)
	     (loop (append body (cdr obj)) res depth)))
	 ((null? (cdr obj))
	  (values (reverse! res) '()))
	 (else
	  (let ((fst (car obj))
		(snd (cadr obj)))
	     (cond
		((is-section? fst snd)
		 (let ((d (section-depth fst snd)))
		    (cond
		       ((>fx d max-depth)
			(loop (cddr obj) res depth))
		       ((<=fx d depth)
			(values (reverse! res) obj))
		       (else
			(let* ((mark (section-mark fst snd))
			       (a (<A> :href (string-append "#" mark)
				       (section-title fst snd))))
			   (multiple-value-bind (res2 rest)
			      (loop (cddr obj) '() d)
			      (if (pair? res2)
				  (loop rest (cons (li a (ul res2)) res) depth)
				  (loop rest (cons (li a) res) depth))))))))
		((and (isa? fst xml-element)
		      (eq? (with-access::xml-element fst (tag) tag) 'div))
		 (with-access::xml-element fst (body)
		    (loop (append body (cdr obj)) res depth)))
		(else
		 (loop (cdr obj) res depth))))))))
   
;*---------------------------------------------------------------------*/
;*    is-section? ...                                                  */
;*---------------------------------------------------------------------*/
(define (is-section? fst snd)
   (and (isa? fst xml-element)
	(isa? snd xml-element)
	(eq? (with-access::xml-element fst (tag) tag) 'a)
	(memq (with-access::xml-element snd (tag) tag) '(h1 h2 h3 h4))))

;*---------------------------------------------------------------------*/
;*    section-depth ...                                                */
;*---------------------------------------------------------------------*/
(define (section-depth fst snd)
   (case (with-access::xml-element snd (tag) tag)
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
   (with-access::xml-element snd (body)
      body))
   
      
