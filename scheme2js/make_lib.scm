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

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scheme2js_makelib

   (option (set! *dlopen-init* "scheme2js_e"))
   
   (import scheme2js
	   (default-scheme2js-config
	      set-optim-level scheme2js-config
	      extend-config extend-config*
	      config)
	   (mangle-qualified-var gen-js)
	   expand
	   export-desc
	   (Compilation-Unit WIP-Unit module-exported-macro-add!
	    module-system)
	   (srfi0-declare! srfi0-declared? srfi0))

   (eval   (export-all)))
