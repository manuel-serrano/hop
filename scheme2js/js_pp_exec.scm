;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/js_pp_exec.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Thu Jul 18 13:40:11 2013 (serrano)                */
;*    Copyright   :  2013 Florian Loitsch/Manuel Serrano               */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2JS/HOP.                              */
;*    -------------------------------------------------------------    */
;*    This a the main entry point of the Scheme2JS standalone          */
;*    pretty-printer.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module js-pp-exec
   (import js-parser
	   js-nodes
	   js-out)
   (main my-main))

;*---------------------------------------------------------------------*/
;*    global state                                                     */
;*---------------------------------------------------------------------*/
(define *in-file* #f)
(define *out-file* #f)
(define *compress?* #f)

;*---------------------------------------------------------------------*/
;*    args-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (args-parser args exit-code)
   (args-parse args
      ((("-h" "--help") (help "This help message"))
       (args-parse-usage #f)
       (exit exit-code))
      (("-o" ?file (help "Output file"))
       (set! *out-file* file))
      ((("-c" "--compress") (help "Compress Output"))
       (set! *compress?* #t))
      (else
       (if *in-file*
	   (error "js pretty print"
	      "only one input-file allowed"
	      else)
	   (set! *in-file* else)))))

;*---------------------------------------------------------------------*/
;*    my-main ...                                                      */
;*---------------------------------------------------------------------*/
(define (my-main args)
   (args-parser (cdr args) 0)
   (if (or (not *in-file*) (not *out-file*))
       (args-parser '("--help") 1))
   (let ((in-p (if (string=? *in-file* "-")
		   (current-input-port)
		   (open-input-file *in-file*)))
	 (out-p (if (string=? *out-file* "-")
		    (current-output-port)
		    (open-output-file *out-file*))))
      (with-handler
	 (lambda (e)
	    (unless (string=? *out-file* "-")
	       (delete-file *out-file*))
	    (raise e))
	 (unwind-protect
	    (js-out (parse in-p
		       (lambda ()
			  (error "pp"
			     "pragma-call in pp"
			     #f)))
	       out-p
	       :compress? *compress?*)
	    (begin
	       (close-input-port in-p)
	       (close-output-port out-p))))
      0))
