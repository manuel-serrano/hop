;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-11 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module mapping2
   (import export-desc
	   tools)
   (include "mapping.sch")
   (export *default-runtime-var-mapping*
	   *call/cc-runtime-var-mapping*))

(define *default-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (map (lambda (e)
	      (let ((desc (create-Export-Desc e #f #t)))
		 (with-access::Export-Desc desc (id)
		    (hashtable-put! ht id desc))))
	 (get-exports "runtime/mod-runtime.sch"))
      ht))
(define *call/cc-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (map (lambda (e)
	      (let ((desc (create-Export-Desc e #f #t)))
		 (with-access::Export-Desc desc (id)
		    (hashtable-put! ht id desc))))
	 (get-exports "runtime/mod-runtime-callcc.sch"))
      ht))
