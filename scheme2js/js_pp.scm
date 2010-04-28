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

(module js-pp
   (import js-parser
	   js-nodes
	   js-out)
   (export (js-pp in-p::input-port out-p::output-port
		  next-pragma!::procedure compress?::bool indent-width::bint)))

(define (js-pp in-p out-p next-pragma! compress? indent-width)
   (js-out (parse in-p next-pragma!) out-p
	   :compress? compress?
	   :indent-width indent-width))
