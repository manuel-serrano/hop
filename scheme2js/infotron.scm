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

(module infotron
   (import verbose
	   error
	   module-system
	   mutable-strings
	   config)
   (export (module->infotron! module::WIP-Unit)))

;; Infotrons: http://foundry.maya.com/jda/
;; transform modules into infotrons.
;; We simply apply a source->source transformation.

(define *default-config-name* 'config)

   
(define (module->infotron! module)
   (define (make-uuid a-list name)
      (let ((entry (assq 'uuid a-list)))
	 (cond
	    ((and entry
		  (or (not (pair? (cdr entry)))
		      (not (null? (cddr entry)))))
	     (scheme2js-error "infotron"
			      "bad 'uuid' clause"
			      entry
			      entry))
	    ((and entry
		  (not (string? (cadr entry))))
	     (scheme2js-error "infotron"
			      "uuid must be given as string"
			      (cadr entry)
			      (cdr entry)))
	    (else
	     (let ((seed-entry (assq 'uuid-seed a-list)))
		(if seed-entry
		    (md5sum-string (with-output-to-string
				      (lambda ()
					 (print name)
					 (print (cadr seed-entry)))))
		    (begin
		       (warning "uuid will be randomly generated")
		       (md5sum-string
			(with-output-to-string
			   (lambda () (print (current-seconds))))))))))))

   (define (get-and-verify-config-name a-list)
      (let ((entry (assq 'config-name a-list)))
	 (cond
	    ((and entry
		  (or (not (pair? (cdr entry)))
		      (not (null? (cddr entry)))))
	     (scheme2js-error "infotron"
			      "bad 'config-name' clause"
			      entry
			      entry))
	    ((and entry
		  (not (symbol? (cadr entry))))
	     (scheme2js-error "infotron"
			      "config-name must be a symbol"
			      (cadr entry)
			      (cdr entry)))
	    (else
	     *default-config-name*))))
      
   (verbose "*infotron*")
   (define (extract-entries a-list type)
      (append-map cdr
		  (filter (lambda (entry)
			     (eq? type (car entry)))
			  a-list)))

   (config-set! 'procedures-provide-js-this #t)
   (config-set! 'encapsulate-modules #t)
   (config-set! 'export-globals #f)
   
   (let* ((header (WIP-Unit-header module))
	  (module-name (WIP-Unit-name module))
	  (a-list (filter pair? header))
	  (uuid (make-uuid a-list module-name))
	  (config-name (get-and-verify-config-name a-list))
	  (iterms (extract-entries a-list 'iterms))
	  (oterms (extract-entries a-list 'oterms))
	  (properties (extract-entries a-list 'properties))
	  (BLUEPRINT (gensym 'BLUEPRINT))
	  (Object (gensym 'Object))

	  (imports (WIP-Unit-imports module))
	  (exports (WIP-Unit-exports module))
	  (macros (WIP-Unit-macros module)))
      (verbose "Infotron " module-name " has uuid " uuid) 
      (unless (null? exports)
	 (scheme2js-error "infotron"
			  "infotrons must not export variables"
			  exports
			  exports))
      (unless (null? macros)
	 (scheme2js-error "infotron"
			  "infotrons must not export macros"
			  macros
			  macros))
      (let ((tl (infotron-preexpand (WIP-Unit-top-level module)
				    BLUEPRINT
				    Object
				    module-name
				    uuid
				    config-name
				    iterms
				    oterms
				    properties))
	    (imps (append imports
			  `((,BLUEPRINT (JS "BLUEPRINT")
					(constant? #t))
			    (,Object (JS "Object")
				     (constant? #t))))))
      (WIP-Unit-top-level-set! module tl)
      (WIP-Unit-imports-set! module imps))))

(define (symbol->jsstring s)
   (if (use-mutable-strings?)
       (list 'quote s)
       (symbol->string s)))

(define (string->jsstring s)
   (if (use-mutable-strings?)
       (list 'quote (string->symbol s))
       s))


;; infotron should be of the following form:
; BLUEPRINT(
;     "UUID...",
;     {
;      "iterms" :
;         [
;           ["input-slot", "inputfun", buffer_size], ...
;         ],
;      "oterms" :[
; 	           "output-slot"
;                ],
;      "properties" : [ "prop1", .... ],
;      "name" : "infotron-name",
;     },
;     function(Class) {
;       Class.prototype._onInit = function(config) {
;
;         // calls to output-slot are done the following way:
;         // self.postMessage("output-slot", msg);
;         // we therefore introduce:
;         output-slot = function(msg) {
;           self.postMessage("output-slot", msg);
;         }
;
;         inputfun = function(msg) {
;            ...
;         }
;         -={ TOP-LEVEL }=-;
;
;         self.inputfun = inputfun;
;         ...;
;       }
;     });
(define (infotron-preexpand top-level BLUEPRINT Object
			    name uuid config-name
			    iterms oterms properties)
   (let ((vector-iterms
	  (map (lambda (iterm)
		  `(vector ,(symbol->jsstring (car iterm))
			   ,(symbol->jsstring (cadr iterm))
			   ,(caddr iterm)))
	       iterms))
	 (string-oterms (map (lambda (oterm)
				 (symbol->jsstring oterm))
			      oterms))
	 (string-properties (map (lambda (prop)
				    (symbol->jsstring prop))
				 properties))
	 (msg (gensym 'msg))
	 (self (gensym 'self))
	 (o (gensym 'o))
	 (c (gensym 'class)))
      
      (list
       `(,BLUEPRINT
	 ,(string->jsstring uuid)
	 (let ((,o (new ,Object)))
	    (js-property-set! ,o (string->jsstring "iterms")
			      (vector ,@vector-iterms))
	    (js-property-set! ,o (string->jsstring "oterms")
			      '#(,@string-oterms))
	    (js-property-set! ,o (string->jsstring "properties")
			      '#(,@string-properties))
	    (js-property-set! ,o (string->jsstring "name")
			      ,(symbol->jsstring name))
	    ,o)
	 (lambda (,c)
	    (js-property-set!
	     (js-property ,c "prototype")
	     (string->jsstring "_onInit")
	     (lambda (,config-name)
		(define ,self this)
		,@(map (lambda (oterm)
			  `(define (,oterm ,msg)
			      ((js-property ,self "postMessage")
			       ,(symbol->jsstring oterm)
			       ,msg)))
		       oterms)
		,@top-level
		,@(map (lambda (iterm)
			  `(js-property-set! ,self
					     ,(symbol->jsstring (cadr iterm))
					     ,(cadr iterm)))
		       iterms)
		#unspecified))
	    ,c)))))
