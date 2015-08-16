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

(module interface
   (main my-main))

(define *ignored-prefixes* '())
(define *prefix* #f)

(define (interface-name var)
   (string-append *prefix*
		  (remove-prefix (symbol->string var)
				 *ignored-prefixes*)))

(define (print-meta m)
   (display "/*** META ")
   (pp m)
   (print " */"))
   
(define (print-var/meta var/kind/meta)
   (let ((var (car var/kind/meta))
	 (meta (caddr var/kind/meta)))
      (if var
	  (begin
	     (print-meta meta)
	     (print "var " (interface-name var) " = " var ";")
	     (print))
	  (begin
	     (print-meta meta)
	     (print)))))

(define *out-file* #f)
(define *in-files* '())

(define (handle-args args)
   (args-parse (cdr args)
      (section "Help")
      (("?")
       (args-parse-usage #f))
      ((("-h" "--help") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f))
      (section "Misc")
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (("--interface-prefix" ?prefix (help "interface-prefix"))
       (set! *prefix* prefix))
      (("--ignored-prefixes" ?list
			     (help "scheme-list of ignored original prefixes"))
       (set! *ignored-prefixes* (with-input-from-string list read)))
      (else
       (set! *in-files* (append! *in-files* (list else))))))

(define (my-main args)
   (handle-args args)
   (if (not *out-file*)
       (error "interface" "no out-file given" #f))
   (if (null? *in-files*)
       (error "interface" "no input-file(s) given" #f))
   (if (not *prefix*)
       (error "interface" "no prefix given" #f))
   (let ((metas (read-metas (open-multi-file *in-files*))))
      (with-output-to-file *out-file*
	 (lambda ()
	    (for-each print-var/meta metas))))
   (exit 0))
