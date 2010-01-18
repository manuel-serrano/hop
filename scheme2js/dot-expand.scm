;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module dot-expand
   (import verbose
	   mutable-strings
	   config
	   expand))

(define (field-name sym)
   (if (use-mutable-strings?)
       `(quote ,sym)
       (symbol->string sym)))

(define (split-dot id)
   (let ((splitted (map! string->symbol
			 (string-split (symbol->string id) "."))))
      splitted))

(define (split-multi-dot id)
   (let ((res (split-dot id)))
      (and (not (null? (cdr res))) ;; more than one element
	   res)))

;; but not at start.
(define (dotted-symbol? id)
   (and (symbol? id)
	(let ((i (string-index (symbol->string id) #\.)))
	   (and (fixnum? i) (>fx i 0)))))

(define (starts-with-dot? id)
   (and (symbol? id)
	(let ((str (symbol->string id)))
	   (char=? #\. (string-ref str 0)))))

(define (get-dot-expand splitted)
   (let loop ((res (car splitted))
	      (splitted (cdr splitted)))
      (if (null? splitted)
	  res
	  (loop `(js-field ,res ,(field-name (car splitted)))
		(cdr splitted)))))

(define (set!-dot-expand splitted val-L set!-loc o-loc f-loc)
   (let loop ((res (car splitted))
	      (splitted (cdr splitted)))
      (if (null? (cdr splitted))
	  (loc-attach
	   `(js-field-set! ,res ,(field-name (car splitted)) ,@val-L)
	   set!-loc o-loc f-loc)
	  (loop (loc-attach `(js-field ,res ,(field-name (car splitted)))
			    f-loc f-loc f-loc)
		(cdr splitted)))))

(define (expand-indirect-accesses! x)
   (if (and (pair? x)
	    (pair? (cdr x)))
       (begin
	  (if (and (pair? (car x))
		   (starts-with-dot? (cadr x)))
	      (begin
		 (set-car! x
			   (get-dot-expand (cons (car x)
						 (split-dot (cadr x)))))
		 (set-cdr! x (cddr x))))
	  (expand-indirect-accesses! (cdr x)))))

(define (split-last p)
   (let loop ((res '())
	      (p p))
      (if (null? (cdr p))
	  (values (reverse! res) (car p))
	  (loop (cons (car p) res) (cdr p)))))

(define (undot x)
   (match-case x
      ((? symbol?)
       (cond
	  ((split-multi-dot x) => (lambda (splitted)
				     (get-dot-expand splitted)))
	  (else x)))
      ;(set! x.y ...)
      ((set! (and (? dotted-symbol?) ?x-dot-y) . ?val-L)
       (let ((splitted (split-dot x-dot-y)))
	  (set!-dot-expand splitted val-L
			   x (cdr x) (cdr x))))
      ;(set! (get-x).y ...)
      ((set! (and (? pair?) ?p) (and (? starts-with-dot?) ?dot-y) . ?val-L)
       (let ((splitted (cons p (split-dot dot-y))))
	  (set!-dot-expand splitted val-L
			   x p (cddr x))))
      ;(x.f ...)
      (((and (? dotted-symbol?) ?x-dot-f) . ?args)
       (multiple-value-bind (o f)
	  (split-last (split-dot x-dot-f))
	  (loc-attach
	   `(js-method-call ,(get-dot-expand o) ,(field-name f) ,@args)
	   x x x)))
      ;((get-x).f ...)
      (((and (? pair?) ?p) (and (? starts-with-dot?) ?f) . ?args)
       (let* ((o (gensym 'o))
	      (o-dot-f (symbol-append o f))
	      (mapping (loc-attach `(,o ,p)
				   (car x) (car x)))
	      (body (loc-attach `(,o-dot-f ,@args)
				(cdr x))))
	  `(let (,mapping) ,@body)))
      ;(quote ...)
      (((kwote quote) ???-)
       x)
      ;(quasiquote ...)
      (((kwote quasiquote) ???-)
       x)
      ;(let(*) (bindings) ...)
      (((or let let*) (and (? pair?) ?bindings) . ?body)
       (for-each expand-indirect-accesses! bindings)
       x)
      ;(let name (bindings) ...)
      ((let (? symbol?) (and (? pair?) ?bindings) . ?body)
       (for-each expand-indirect-accesses! bindings)
       x)
      ((do ?bindings ?test+finally . ?commands)
       (for-each expand-indirect-accesses! bindings)
       (expand-indirect-accesses! test+finally)
       x)
      ((define-struct ?name . ?fields)
       (for-each expand-indirect-accesses! fields)
       x)
      (else
       (expand-indirect-accesses! x)
       x)))

(add-pre-expand! 1
		 (lambda (x)
		    (if (config 'direct-js-object-access)
			(undot x)
			x)))
