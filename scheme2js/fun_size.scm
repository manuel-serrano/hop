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

(module fun-size
   (import nodes
	   export-desc
	   walk
	   verbose)
   (export (fun-size tree::Module)))

;; rough estimation of function sizes.
;; nested functions are added to the size of the surrounding fun.

(define (fun-size tree)
   (verbose " fun-size")
   (size tree #f #f))

(define-nmethod (Node.size surrounding-fun)
   ;; TODO: optimize fun-size.
   (when surrounding-fun
      (with-access::Lambda surrounding-fun (size)
	 (set! size (+fx size 1))))
   (default-walk this surrounding-fun))

(define-nmethod (Lambda.size surrounding-fun)
   (with-access::Lambda this (size)
      (set! size 0))
   (default-walk this this)
   (when surrounding-fun
      (with-access::Lambda surrounding-fun (size)
	 (with-access::Lambda this ((tsize size))
	    (set! size (+fx size tsize))))))
