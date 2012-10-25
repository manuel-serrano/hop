;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
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
(module module-resolver
   (import config)
   (export (scheme2js-module-resolver mod file)))

;*---------------------------------------------------------------------*/
;*    scheme2js-module-resolver ...                                    */
;*---------------------------------------------------------------------*/
(define (scheme2js-module-resolver mod file)
   (cond
      ((config 'module-resolver)
       =>
       (lambda (resolver) (resolver mod)))
      ((string? file)
       (let ((dir (dirname file)))
	  (or ((bigloo-module-resolver) mod dir)
	      (extension-resolver mod (cons dir (config 'include-paths))))))
      (else
       (extension-resolver mod (config 'include-paths)))))
       
;*---------------------------------------------------------------------*/
;*    *module-extensions* ...                                          */
;*---------------------------------------------------------------------*/
(define *module-extensions* '("scm" "sch"))

;*---------------------------------------------------------------------*/
;*    extension-resolver ...                                           */
;*    -------------------------------------------------------------    */
;*    currently scheme2js only supports a simple module-lookup based   */
;*    on extensions.                                                   */
;*---------------------------------------------------------------------*/
(define (extension-resolver module include-paths)
   (let* ((module-str (symbol->string module))
	  (module-filenames (map (lambda (ext)
				    (string-append module-str "." ext))
			       *module-extensions*))
	  ;; for now just get the first hit.
	  ;; we can later add support for more possible results.
	  (module-file (any (lambda (file)
			       (find-file/path file include-paths))
			  module-filenames)))
      (if module-file
	  (list module-file)
	  '())))
