;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/hopscheme/tilde.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 15 09:22:54 2013                          */
;*    Last change :  Fri Nov 15 12:34:28 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compile the inner tilde into javascript expressions              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_tilde
   (export (tilde->javascript js)))

;*---------------------------------------------------------------------*/
;*    tilde->javascript ...                                            */
;*---------------------------------------------------------------------*/
(define (tilde->javascript js)
   (match-case js
      ((let* ?bindings (vector (quote ?expr) ?var ?- ?- (string-append ?jstr) . ?rest))
       `(let* ,bindings
	   (pragma
	      ,(format "new hop_tilde( function() { ~a; return ~a; } )"
		  (string-append->body jstr)
		  (symbol->string (cadr var)))
	      ;; dummy pragma arguments passing
	      ;; otherwise the scheme2js compile will consider the
	      ;; bound variables unused and will remove them!
	      ,@(map car bindings))))
      ((let* () (vector (quote ?expr) ?var ?- ?- (and (? string?) ?jstr) . ?rest))
       `(pragma
	   ,(format "new hop_tilde( function() { ~a; return ~a; } )"
	       jstr
	       (symbol->string (cadr var)))))
      (else
       (error "hopscheme" "Unkown tilde format" js))))

;*---------------------------------------------------------------------*/
;*    string-append->body ...                                          */
;*---------------------------------------------------------------------*/
(define (string-append->body jstr)
   (apply string-append
      (map (lambda (x)
	      (match-case x
		 ((? string?)
		  x)
		 ((call-with-output-string
		     (lambda (op) (obj->javascript-attr ?var op)))
		  (symbol->string var))))
	 (cdr jstr))))
