;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/tarball.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 19:49:32 2005                          */
;*    Last change :  Wed Aug 10 08:01:42 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    dired tarball                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    dired-tar-filter ...                                             */
;*---------------------------------------------------------------------*/
(define-service (dired/tar path file)
   (define (tar-row row)
      (let ((len (string-length row)))
	 (let loop ((i 0)
		    (c 4))
	    (cond
	       ((=fx i len)
		(<TD> row))
	       ((char=? (string-ref row i) #\space)
		(let liip ((i (+fx 1 i)))
		   (cond
		      ((=fx i len)
		       (<TD> row))
		      ((char=? (string-ref row i) #\Space)
		       (liip (+fx i 1)))
		      (else
		       (if (=fx c 0)
			   (let* ((before (substring row 0 i))
				  (after (substring row i len))
				  (url ($dired/tar path after)))
			      (list (<TD> before)
				    (<TD> (<A> :href url after))))
			   (loop (+fx i 1) (-fx c 1)))))))
	       (else
		(loop (+fx i 1) c))))))
   (define (tar-rows proc)
      (let ((pi (process-output-port proc)))
	 (let loop ((rows '()))
	    (let ((row (read-line pi)))
	       (if (eof-object? row)
		   (begin
		      (close-input-port pi)
		      (reverse! rows))
		   (loop (cons (<TR> (tar-row row)) rows)))))))
   (define (tar-file path file)
      (instantiate::http-response-procedure
	 (content-type (mime-type file "text/html"))
	 (proc (lambda (p)
		  (let* ((opt (cond
				 ((is-suffix? path "tar")
				  "xfO")
				 ((or (is-suffix? path "tgz")
				      (is-suffix? path "tar.gz"))
				  "xfzO")
				 (else
				  "xfjO")))
			 (pr (run-process "tar"
					  opt
					  path
					  file
					  error: "/dev/null"
					  output: pipe:))
			 (pi (process-output-port pr)))
		     (send-chars pi p)
		     (close-input-port pi))))))
   (define (tar-tarball path)
      (let* ((req (the-current-request))
	     (l (dired-cookie->exp req "dired"))
	     (m (or (dired-plist-assq :mode l) 'icon))
	     (hide (or (dired-plist-assq :hide l) 'true))
	     (sort (or (dired-plist-assq :sort l) 'name))
	     (order (or (dired-plist-assq :order l) 'increase))
	     (opt (cond
		     ((is-suffix? path "tar")
		      "tvf")
		     ((or (is-suffix? path "tgz")
			  (is-suffix? path "tar.gz"))
		      "tvfz")
		     (else
		      "tvfj")))
	     (pr (run-process "tar"
			      opt
			      path
			      error: "/dev/null"
			      output: pipe:)))
	 (instantiate::http-response-hop
	    (xml (<HTML>
		  (<DIRED-HEAD> req (dirname path))
		  (<BODY> :class "dired"
			  (<DIV> :class "header"
				 (<DIRED-DIR> req (dirname path))
				 (<DIRED-CONFIG> req path hide 'icon sort order))
			  (<A> :name "0")
			  (<DIV> :class "entries"
				 (<PRE>
				  (<TABLE> (tar-rows pr))))))))))
   (if (string? file)
       (tar-file path file)
       (tar-tarball path)))

;*---------------------------------------------------------------------*/
;*    tarball filter ...                                               */
;*---------------------------------------------------------------------*/
(dired-icon-filter-add!
 (lambda (req dir name ident)
    (when (and (suffix-member name '("tar" "tgz" "tar.gz" "tbz" "tar.bz2"))
	       (file-exists? (make-file-name dir name)))
       (let* ((img (dired-find-icon req dir name))
	      (tgz (make-file-name dir name))
	      (icon (<DIRED-IMG-CLICK> img
				       {hop( $dired/tar( $tgz, null ),
					     hop_replace )}
				       name))
	      (info (<DIRED-FILE-INFO> tgz)))
	  (apply <DIRED-ICON-ENTRY> req dir "text" name ident icon info
		 (append
		  (dired-find-buttons req dir name tgz)
		  (list
		   (let* ((tar (cond
				  ((is-suffix? name "tar")
				   "tar xf")
				  ((or (is-suffix? name "tgz")
				       (is-suffix? name "tar.gz"))
				   "tar xfz")
				  (else
				   "tar xfj")))
			  (cmd (format "cd ~a; ~a" dir tar)))
		      (<DIRED-BUTTON> "tar"
				      {hop( $dired/exec( $cmd, $tgz ),
				            function( _ ){ location = $dir })}
				      "tar.png"))
		   (<DIRED-BUTTON-DOWNLOAD> req dir name))))))))

