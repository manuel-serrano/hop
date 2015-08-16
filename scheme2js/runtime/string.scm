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

(define (starts-with? s sub-s)
   (substring-at? s sub-s 0))

(define (ends-with? s sub-s)
   (let ((s-length (string-length s))
	 (sub-s-length (string-length sub-s)))
      (and (>= s-length sub-s-length)
	   (substring-at? s sub-s (- s-length sub-s-length)))))

(define (remove-prefix s prefixes)
   ;; first matched prefix holds removal.
   (let ((matched-prefix (any (lambda (pre)
				 (and (starts-with? s pre)
				      pre))
			      prefixes)))
      (if matched-prefix
	  (substring s
		     (string-length matched-prefix)
		     (string-length s))
	  s)))

