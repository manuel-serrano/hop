;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/call.sch                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  7 18:28:45 2017                          */
;*    Last change :  Thu Apr  9 09:11:34 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js-call expansion                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-call-method ...                                               */
;*---------------------------------------------------------------------*/
(define-expander js-call-method
   (lambda (x e)
      (match-case x
	 ((?- ?%this ?obj ?prop)
	  (e `(js-call-method0 ,%this ,obj ,prop)) e)
	 ((?- ?%this ?obj ?prop ?a0)
	  (e `(js-call-method1 ,%this ,obj ,prop ,a0)) e)
	 ((?- ?%this ?obj ?prop ?a0 ?a1)
	  (e `(js-call-method2 ,%this ,obj ,prop ,a0 ,a1)) e)
	 ((?- ?%this ?obj ?prop ?a0 ?a1 ?a2)
	  (e `(js-call-method3 ,%this ,obj ,prop ,a0 ,a1 ,a2)) e)
	 ((?- ?%this ?obj ?prop ?a0 ?a1 ?a2 ?a3)
	  (e `(js-call-method4 ,%this ,obj ,prop ,a0 ,a1 ,a2 ,a3)) e)
	 (else
	  (map (lambda (x) (e x e) x))))))
