;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/css.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:44:22 2005                          */
;*    Last change :  Mon Apr 21 15:28:18 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP css loader                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_css

   (library web)
   
   (include "xml.sch"
	    "service.sch")

   (import  __hop_read
	    __hop_param
	    __hop_cache)

   (use	    __hop_user
	    __hop_hop
	    __hop_cgi
	    __hop_misc
	    __hop_service
	    __hop_mime
	    __hop_types)

   (export  (init-hss-compiler!)
	    (hss-response::%http-response ::http-request ::bstring)
	    (hss->css ::bstring)
	    (hss->css-url ::bstring)
	    (hop-load-hss ::bstring)
	    (hop-read-hss ::input-port)

	    (hop-hss-type! ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    *hss-builtin-types* ...                                          */
;*---------------------------------------------------------------------*/
(define *hss-builtin-types*
   '(;; notepad
     ("notepad" "div.hop-notepad")
     ("notepad" "div.hop-notepad")
     ("notepad-tabs" "td.hop-notepad-tabs")
     ("notepad-body" "td.hop-notepad-body")
     ("nphead" "div.hop-nphead")
     ("nptab" "span.hop-nptab")
     ("nptab-active" "span.hop-nptab-active")
     ("nptab-inactive" "span.hop-nptab-inactive")
     ("nptab-body" "div.hop-notepad-tab-body")
     ("hptabhead" "span.hop-nptab-head")
     ;; paned
     ("paned" "div.hop-paned")
     ;; sorttable
     ("sorttable" "span.hop-sorttable table")
     ;; audio
     ("audio" "div.hop-audio-controls")
     ;; filechooser
     ("filechooser" "div.filechooser")
     ;; prefs
     ("prefs" "table.hop-prefs")
     ("prlabel" "table.hop-prefs td.hop-prefs-label")
     ("pr" "table.hop-prefs tr.hop-pr")
     ("pr-type" "table.hop-prefs td.hop-pr-type")
     ("pr-editor" "table.hop-prefs td.hop-pr-editor")
     ("pr-editor-expr" "table.hop-prefs input.hop-pr-editor-expr")))
   
;*---------------------------------------------------------------------*/
;*    hss-mutex ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-mutex (make-mutex 'hss))

;*---------------------------------------------------------------------*/
;*    hss-write ...                                                    */
;*---------------------------------------------------------------------*/
(define (hss-write hss p)
   (let loop ((hss hss))
      (if (string? hss)
	  (display hss p)
	  (for-each loop hss))))

;*---------------------------------------------------------------------*/
;*    hss-cache ...                                                    */
;*---------------------------------------------------------------------*/
(define hss-cache
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hss->css ...                                                     */
;*---------------------------------------------------------------------*/
(define (hss->css path)
   (with-lock hss-mutex
      (lambda ()
	 (let ((cache (cache-get hss-cache path))
	       (mime (mime-type path "text/css")))
	    (if (string? cache)
		(with-input-from-file cache read-string)
		(let* ((hss (hop-load-hss path))
		       (cache (cache-put! hss-cache path hss)))
		   (let ((p (open-output-string)))
		      (hss-write hss p)
		      (close-output-port p))))))))

;*---------------------------------------------------------------------*/
;*    hss->css-url ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss->css-url path)
   (string-append path (hop-hss-compile-suffix)))

;*---------------------------------------------------------------------*/
;*    init-hss-compiler! ...                                           */
;*---------------------------------------------------------------------*/
(define (init-hss-compiler!)
   ;; builtin hss types
   (for-each (lambda (t) (apply hop-hss-type! t)) *hss-builtin-types*)
   ;; hss cache
   (set! hss-cache
	 (instantiate::cache-disk
	    (path (make-file-path (hop-rc-directory)
				  "cache"
				  (format "hss-~a" (hop-port))))
	    (out (lambda (o p) (hss-write o p))))))

;*---------------------------------------------------------------------*/
;*    hss-response ...                                                 */
;*---------------------------------------------------------------------*/
(define (hss-response req path)
   (if (authorized-path? req path)
       (with-lock hss-mutex
	  (lambda ()
	     (let ((cache (cache-get hss-cache path))
		   (mime (mime-type path "text/css"))
		   (method (http-request-method req)))
		(if (string? cache)
		    (instantiate::http-response-file
		       (request req)
		       (charset (hop-locale))
		       (content-type mime)
		       (bodyp (eq? method 'GET))
		       (file cache))
		    (let* ((hss (hop-load-hss path))
			   (cache (cache-put! hss-cache path hss)))
		       (if (string? cache)
			   (instantiate::http-response-file
			      (request req)
			      (charset (hop-locale))
			      (content-type mime)
			      (bodyp (eq? method 'GET))
			      (file cache))
			   (instantiate::http-response-procedure
			      (request req)
			      (charset (hop-locale))
			      (content-type mime)
			      (bodyp (eq? method 'GET))
			      (proc (lambda (p)
				       (hss-write hss p))))))))))
       (user-access-denied req)))

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
       (raise (instantiate::&io-file-not-found-error
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
(hop-hss-type! "window" "table.hop-window td.hop-window-content")
