;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/css.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:44:22 2005                          */
;*    Last change :  Wed Sep 12 08:21:25 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP css loader                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_css

   (library web)
   
   (import  __hop_read)

   (use     __hop_types)

   (export  (hop-load-hss ::bstring)
	    (hop-read-hss ::input-port)

	    (hop-hss-type! ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    hop-load-hss ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-load-hss file)
   (if (file-exists? file)
       (let ((p (open-input-file file))
	     (mod (eval-module))
	     (loadingf (the-loading-file)))
	  (loading-file-set! file)
	  (if (input-port? p)
	      (unwind-protect
		 (begin
		    ;; each hss file is read inside a dummy empty module
		    (eval `(module ,(gensym)))
		    (hop-read-hss p))
		 (begin
		    (when mod (eval-module-set! mod))
		    (close-input-port p)
		    (loading-file-set! loadingf)))
	      (raise (instantiate::&io-port-error
			(proc 'hop-load)
			(msg "Can't open file")
			(obj file)))))
       (raise (instantiate::&io-port-error
		 (proc 'hop-load)
		 (msg "file does not exist")
		 (obj file)))))

;*---------------------------------------------------------------------*/
;*    hop-read-hss ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function is invoked when the initial "@{" string has        */
;*    already been parsed.                                             */
;*---------------------------------------------------------------------*/
(define (hop-read-hss iport)
   (with-handler
      (lambda (e)
	 (if (not (&io-parse-error? e))
	     (raise e)
	     (match-case (&io-parse-error-obj e)
		((?token ?val ?file ?pos)
		 (raise (duplicate::&io-parse-error e
			   (obj (format "~a (~a)" token val))
			   (fname file)
			   (location pos))))
		(else
		 (raise e)))))
      (css-parse iport
		 :extension hss-extension
		 :element-name (lambda (i)
				  (let ((new (hashtable-get *hss-types* i)))
				     (if (string? new)
					 new
					 i))))))

;*---------------------------------------------------------------------*/
;*    hss-extension ...                                                */
;*---------------------------------------------------------------------*/
(define (hss-extension c ip)
   (when (char=? c #\$)
      (let ((exp (hop-read ip))
	    (pos (input-port-position ip)))
	 (if (eof-object? exp)
	     (read-error/location "Unexpected end-of-file"
				  "Unclosed list"
				  (input-port-name ip)
				  pos)
	     (let ((val (with-handler
			   (lambda (e)
			      (if (&eval-warning? e)
				  (begin
				     (warning-notify e)
				     #unspecified)
				  (raise e)))
			   (eval exp))))
		(cond
		   ((string? val)
		    val)
		   (else
		    #unspecified)))))))

;*---------------------------------------------------------------------*/
;*    aliasing control ...                                             */
;*---------------------------------------------------------------------*/
(define *hss-type-mutex* (make-mutex "hop-hss-type"))
(define *hss-types* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    hop-hss-type! ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-hss-type! old new)
   (with-lock *hss-type-mutex*
      (lambda ()
	 (hashtable-put! *hss-types* (string-upcase old) new)
	 (hashtable-put! *hss-types* (string-downcase old) new)
	 "")))

;*---------------------------------------------------------------------*/
;*    default hss type ...                                             */
;*---------------------------------------------------------------------*/
(hop-hss-type! "iwindow" "table.hop-iwindow td.hop-iwindow-content")
