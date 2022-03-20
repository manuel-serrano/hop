;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/export_expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 19 13:17:15 2022                          */
;*    Last change :  Sat Mar 19 17:43:40 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript JS-EXPORT expander                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-export-expander ...                                           */
;*---------------------------------------------------------------------*/
(define (js-export-expander x e)
   (match-case x
      ((js-export ?vars . ?body)
       (let* ((len (length vars))
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
				  vars nums)
			     `(js-evar-info (& "default")
				 `(,len) '() #f)))
		       (set! evars (make-vector ,(+fx len 1) (js-undefined)))
		       evars))
		 ,@body
		 ,@(map (lambda (v i)
			   `(vector-set! %evars ,i ,v))
		      vars nums)
		 (vector-set! %evars ,len
		    (js-get %module (& "exports" 1) %this)))
	     e)))
      (else
       (error "js-export" "Bad form" x))))
