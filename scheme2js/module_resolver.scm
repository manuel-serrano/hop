;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-15 Florian Loitsch, see LICENSE file         */
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
   (export (scheme2js-module-resolver mod files file)))

;*---------------------------------------------------------------------*/
;*    scheme2js-module-resolver ...                                    */
;*---------------------------------------------------------------------*/
(define (scheme2js-module-resolver mod files file)
   (with-trace 'scheme2js "scheme2js-module-resolver"
      (trace-item "mod=" mod)
      (trace-item "files=" files)
      (trace-item "file=" file)
      (let ((abase (if (not (string? file))
		       (pwd)
		       (or (find-afile file) (dirname file)))))
	 (trace-item "abase=" abase)
	 (or ((config 'module-resolver) mod files abase)
	     ((bigloo-module-resolver) mod files abase)
	     (let ((path (if (string? file)
			     (cons abase (config 'include-paths))
			     (config 'include-paths))))
		(extension-resolver mod path))))))

;*---------------------------------------------------------------------*/
;*    find-afile ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-afile file)
   (when (string? file)
      (let loop ((dir (dirname file)))
	 (if (file-exists? (make-file-name dir ".afile"))
	     dir
	     (let ((parent (dirname dir)))
		(unless (string=? parent dir)
		   (if (string=? parent ".")
		       (when (file-exists? ".afile")
			  ".")
		       (find-afile parent))))))))

;*---------------------------------------------------------------------*/
;*    config-runtime-resolver ...                                      */
;*---------------------------------------------------------------------*/
(define (config-runtime-resolver mod dir)
   (let ((cfg (config 'module-runtime-resolver)))
      (when (procedure? cfg)
	 (cfg mod))))

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

