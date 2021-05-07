;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/index.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  7 17:05:22 2021                          */
;*    Last change :  Fri May  7 17:05:35 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Index manipulation                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-toindex ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (js-toindex p)
   (cond-expand
      ((or bint30 bint32)
       `(cond
	   ((and (fixnum? ,p) (>=fx ,p 0))
	    (fixnum->uint32 ,p))
	   ((uint32? ,p)
	    ,p)
	   ((and (js-jsstring? ,p) (js-jsstring-index? ,p))
	    (with-access::JsStringLiteralIndex ,p (index)
	       index))
	   (else
	    ((@ js-toindex  __hopscript_public) ,p))))
      ((or bint61 bint64)
       `(cond
	   ((and (fixnum? ,p) (>=fx ,p 0) (<fx ,p (-fx (bit-lsh 1 32) 1)))
	    (fixnum->uint32 ,p))
	   ((and (js-jsstring? ,p) (js-jsstring-index? ,p))
	    (with-access::JsStringLiteralIndex ,p (index)
	       index))
	   (else
	    ((@ js-toindex  __hopscript_public) ,p))))
      (else
       `((@ js-toindex  __hopscript_public) ,p))))

