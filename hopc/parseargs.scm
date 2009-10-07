;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/hopc/parseargs.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Wed Oct  7 05:44:51 2009 (serrano)                */
;*    Copyright   :  2004-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_parseargs

   (library hop)
   
   (import  hopc_param)
   
   (export  (parse-args ::pair-nil)
	    (hopc-load-rc ::bstring)))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (let ((rc-file #unspecified)
	 (loadp #t)
	 (exprs '())
	 (h "localhost")
	 (p (hop-port))
	 (login #f)
	 (command-string #f))
      (bind-exit (stop)
	 (args-parse (cdr args)
	    ((("-h" "--help") (help "This message"))
	     (print "Hopc v" (hop-version))
	     (print "usage: hopc [options] -- [bigloo-options]")
	     (newline)
	     (args-parse-usage #f)
	     (exit 0))
	    (("--version" (help "Print the version and exit"))
	     (print (hop-name) (hop-version))
	     (exit 0))
	    (("-q" (help "Do not load an init file"))
	     (set! loadp #f))
	    (("--rc-file" ?file (help "Load alternate rc file"))
	     (set! rc-file file))
	    (("--rc-dir" ?dir (help "Set rc directory"))
	     (hop-rc-directory-set! dir))
	    (("-L" ?dir (help "Add Hop library path"))
	     (bigloo-library-path-set! (cons dir (bigloo-library-path))))
	    (("--share-dir" ?dir (help "Set hopc share directory"))
	     (hopc-share-directory-set! dir))
	    (("-v?level" (help "Increase or set verbosity level (-v0 crystal silence)"))
	     (if (string=? level "")
		 (hop-verbose-set! (+fx 1 (hop-verbose)))
		 (hop-verbose-set! (string->integer level))))
	    (("-g?level" (help "Increase or set debug level"))
	     (if (string=? level "")
		 (bigloo-debug-set! (+fx 1 (bigloo-debug)))
		 (bigloo-debug-set! (string->integer level))))
	    (("-w?level" (help "Increase or set warning level (-w0 no warning)"))
	     (if (string=? level "")
		 (bigloo-warning-set! (+fx 1 (bigloo-warning)))
		 (bigloo-warning-set! (string->integer level))))
	    (("--no-color" (help "Disable colored traces"))
	     (bigloo-trace-color-set! #f))
	    (("--eval" ?string (help "Evaluate STRING"))
	     (set! exprs (cons string exprs)))
	    (("-o" ?string (help "Name the output FILE"))
	     (hopc-destination-set! string))
	    (("-s" (help "Stop after Bigloo code generation"))
	     (hopc-pass-set! 'bigloo))
	    (("-c" (help "Stop after code object generation"))
	     (hopc-pass-set! 'object)
	     (hopc-bigloo-options-set!
	      (append (hopc-bigloo-options) (list "-c"))))
	    (("--bigloo=?bigloo" (help "Set the Bigloo binary file path"))
	     (hopc-bigloo-set! bigloo))
	    (else
	     (if (string=? else "--")
		 (begin
		    (hopc-bigloo-options-set!
		     (append (hopc-bigloo-options)
			     (map (lambda (s)
				     (if (string-index s #\space)
					 (string-append "\"" s "\"")
					 s))
				  (cdr rest))))
		    (stop #t))
		 (hopc-sources-set! (append (hopc-sources) (list else)))))))
      (when loadp
	 (if (string? rc-file)
	     (%hopc-load-rc rc-file)
	     (let ((path (make-file-name (hop-rc-directory) (hopc-rc-file))))
		(if (file-exists? path)
		    (%hopc-load-rc path)
		    (%hopc-load-rc (make-file-name (hop-etc-directory) (hopc-rc-file)))))))))

;*---------------------------------------------------------------------*/
;*    %hopc-load-rc ...                                                */
;*---------------------------------------------------------------------*/
(define (%hopc-load-rc path)
   (if (string? path)
       (when (file-exists? path)
	  (hop-verb 2 "Loading `" path "'...\n")
	  (hop-load path :menv #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    hopc-load-rc ...                                                 */
;*---------------------------------------------------------------------*/
(define (hopc-load-rc file)
   (let ((path (make-file-name (hop-rc-directory) file)))
      (when (file-exists? path)
	 (%hopc-load-rc path))))
      

