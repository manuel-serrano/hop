;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/export_expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 19 13:17:15 2022                          */
;*    Last change :  Sat Mar 19 15:23:36 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript JS-EXPORT expander                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-export-expander ...                                           */
;*---------------------------------------------------------------------*/
(define (js-export-expander x e)
   (match-case x
      ((js-export ?bindings . ?body)
       (let* ((len (length bindings))
	      (nums (iota len)))
	  (e `(&with!
		 (define %evars
		    (with-access::JsModule %module (evars exports checksum)
		       (set! checksum 0)
		       (set! exports
			  (vector
			     ,@(map (lambda (v i)
				       `(js-evar-info (& ,(symbol->string v))
					   '(,i) '() #f))
				  bindings nums)
			     `(js-evar-info (& "default")
				`(,len) '() #f)))
		       (set! evars (make-vector ,len (js-undefined)))
		       evars))
		 ,@body
		 ,@(map (lambda (v i)
			   `(vector-set! %evars ,i ,v))
		      nums bindings)
		 (vector-set! %evars ,len
		    (js-get %module (& "exports" 1) %this)))
	     e)))
      (else
       (error "js-export" "Bad form" x))))
