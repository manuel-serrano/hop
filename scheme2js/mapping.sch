;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/mapping.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 13:51:13 2013                          */
;*    Last change :                                                    */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Mapping                                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    get-exports :: ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (get-exports file)
   (let* ((m-clause (with-input-from-file file read))
	  ;; skip 'module' and module-name
	  (a-list (cddr m-clause))
	  (exports (assq 'export a-list)))
      (list 'quote (cdr exports))))
