;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module export-desc
   (import gen-js error)
   (export
    (find-desc-in-exports sym::symbol l/ht)
    (empty-exports?::bool l/ht)
    (final-class Export-Desc
       (id::symbol read-only)
       (js-id::bstring read-only)
       (exported-as-const?::bool (default #f))
       (runtime?::bool (default #f) read-only)
       (peephole       (default #f) read-only)
       (higher?::bool  (default #f) read-only)
       (higher-params  (default #f) read-only) 
       (return-type    (default #f) read-only)
       (arity          (default #f) read-only))
    (create-Export-Desc::Export-Desc info module-name runtime?::bool)))


(define (empty-exports? l/ht)
   (or (null? l/ht)
       (and (hashtable? l/ht)
	    (zerofx? (hashtable-size l/ht)))))

(define (find-desc-in-exports sym l/ht)
   (cond
      ((pair? l/ht)
       (any (lambda (desc)
	       (and (eq? (with-access::Export-Desc desc (id) id) sym)
		    desc))
	  l/ht))
      ((hashtable? l/ht)
       (hashtable-get l/ht sym))
      ((null? l/ht)
       #f)
      (else
       (error "export.scm" "internal error" sym))))

(define (entry-val sym l)
   (let ((try (assq sym (cdr l))))
      (and try
	   (cadr try))))

(define (normalize-export export module-name)
   (cond
      ((symbol? export)
       (list export
	     (list 'JS
		   (mangle-qualified-var export module-name))))
      ((pair? export)
       (cond
	  ((assq 'JS (cdr export))
	   export)
	  (else
	   (let ((scheme-sym (car export)))
	      (cons* scheme-sym
		     (list 'JS
			   (mangle-qualified-var scheme-sym module-name))
		     (cdr export))))))
      (else
       (scheme2js-error "normalize-export"
			"bad import/export clause"
			export
			export))))

(define (normalize-js-id normalized)
   (let* ((clause (assq 'JS (cdr normalized)))
	  (id (and clause
		   (pair? (cdr clause))
		   (cadr clause))))
      (cond
	 ((symbol? id) (symbol->string id))
	 ((string? id)
	  id)
	 (else 
	  (scheme2js-error "exported variable"
			   "JS-clause must be either symbol or string"
			   id
			   clause)))))

(define (create-Export-Desc::Export-Desc info module-name runtime?)
   (let* ((normalized (normalize-export info module-name))
	  (scheme-sym (car normalized))
	  (js-id (normalize-js-id normalized))
	  (peephole (entry-val 'peephole normalized))
	  (higher? (entry-val 'call/cc? normalized))
	  (higher-params (entry-val 'call/cc-params normalized))
	  (return-type (entry-val 'type normalized))
	  (exported-as-const? (or (entry-val 'constant? normalized)))
	  (arity (entry-val 'arity normalized)))
      (instantiate::Export-Desc
	 (id scheme-sym)
	 (js-id js-id)
	 (exported-as-const? exported-as-const?)
	 (runtime? runtime?)
	 (peephole peephole)
	 (higher? higher?)
	 (higher-params higher-params)
	 (return-type return-type)
	 (arity arity))))
