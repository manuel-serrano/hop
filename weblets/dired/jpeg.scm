;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/jpeg.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 19:19:24 2005                          */
;*    Last change :  Wed Jan 18 14:50:29 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    dired jpeg                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    <DIRED-IMAGE-HEAD> ...                                           */
;*---------------------------------------------------------------------*/
(define (<DIRED-IMAGE-HEAD> req dir)
   (with-access::http-request req (path)
      (let ((css1 (make-file-name dir "dired.css"))
	    (css2 (make-file-name dir ".dired.css")))
	 (<HEAD>
	  (<HOP-HEAD>)
	  (<LINK> :rel "stylesheet"
		  :type "text/css"
		  :href (format "~a/dired.hss" (dired-install-directory)))
	  (if (file-exists? css1)
	      (<LINK> :rel "stylesheet" :type "text/css" :href css1)
	      "")
	  (if (file-exists? css2)
	      (<LINK> :rel "stylesheet" :type "text/css" :href css2)
	      "")
	  (<SCRIPT> :type "text/javascript"
		    :src (format "~a/dired.js" (dired-install-directory)))))))

;*---------------------------------------------------------------------*/
;*    <DIRED-JPEG-INFO> ...                                            */
;*---------------------------------------------------------------------*/
(define (<DIRED-JPEG-INFO> file)
   (define (format-real num digit)
      (cond
	 ((fixnum? num)
	  num)
	 ((pair? num)
	  (format-real (/ (car num) (cdr num)) digit))
	 ((elong? num)
	  (elong->fixnum num))
	 (else
	  (let* ((s (with-output-to-string 
		       (lambda () (display num))))
		 (l (string-length s)))
	     (let loop ((i 0))
		(cond
		   ((= i l)
		    s)
		   ((char=? (string-ref s i) #\.)
		    (substring s 0 (min (+ i 1 digit) l)))
		   (else
		    (loop (+ i 1)))))))))
   (define (format-elong va)
      (if (elong? va)
	  (elong->string va)
	  va))
   (define (format-nrational rat)
      (if (not (pair? rat))
	  rat
	  (let ((n (elong->fixnum (car rat)))
		(d (elong->fixnum (cdr rat))))
	     (cond
		((=fx n 0)
		 0)
		((< n d)
		 (let ((g (gcd n d)))
		    (format "~a/~a" (/fx n g) (/fx d g))))
		(else
		 (/ n d))))))
   (define (format-rational rat)
      (if (not (pair? rat))
	  rat
	  (let ((n (elong->fixnum (car rat)))
		(d (elong->fixnum (cdr rat))))
	     (cond
		((=fx n 0)
		 0)
		((< n d)
		 (format "~a/~a" n d))
		(else
		 (/ n d))))))
   (let ((e (jpeg-exif file)))
      (if (not (exif? e))
	  "No exif"
	  (<TABLE>
	   (<TR> (<TH> :nowrap "nowrap" "File size:")
		 (let ((s (file-size file)))
		    (cond
		       ((>elong s (*elong #e1024 #e1024))
			(<TD>
			 (elong->string (/elong s (*elong #e1024 #e1024)))
			 "m"))
		       ((>elong s #e1024)
			(<TD> (elong->string (/elong s #e1024)) "k"))
		       (else
			(<TD> (elong->string s))))))
	   (<TR> (<TH> :nowrap "nowrap" "Date:")
		 (<TD> :nowrap "nowrap" (exif-date e)))
	   (<TR> (<TH> :nowrap "nowrap" "Dimension:")
		 (<TD> :nowrap "nowrap" (exif-width e) " x " (exif-height e)))
	   (<TR> (<TH> :nowrap "nowrap" "Orientation:")
		 (<TD> :nowrap "nowrap" (exif-orientation e)))
	   (<TR> (<TH> :nowrap "nowrap" "Focal:")
		 (<TD> :nowrap "nowrap"
		       (let ((fl (exif-focal-length e)))
			  (if (and (pair? fl)
				   (number? (exif-cdd-width e)))
			      (format "~amm" 
				      (inexact->exact
				       (round
					(+ 0.5 (/ (* 35 
						     (/ (car fl) (cdr fl))) 
						  (exif-cdd-width e))))))
			      "?"))))
	   (<TR> (<TH> :nowrap "nowrap" "Iso:")
		 (<TD> :nowrap "nowrap"
		       (let ((is (exif-iso e)))
			  (if (elong? is) (format-elong is) "?"))))
	   (<TR> (<TH> :nowrap "nowrap" "Exposure:")
		 (<TD> :nowrap "nowrap"
		       (let ((et (exif-exposure-time e)))
			  (if (pair? et)
			      (format "~as" (format-nrational et))
			      "?"))))
	   (<TR> (<TH> :nowrap "nowrap" "Aperture:")
		 (<TD> :nowrap "nowrap"
		       (let ((ap (exif-aperture e)))
			  (if (or (number? ap) (pair? ap))
			      (format "f~a" (format-real ap 1))
			      "?"))))
	   (<TR> (<TH> :nowrap "nowrap" "Bias:")
		 (<TD> :nowrap "nowrap"
		       (let ((bi (exif-exposure-bias-value e)))
			  (if (pair? bi)
			      (format "~aev" (format-rational bi))
			      "?"))))
	   (<TR> (<TH> :nowrap "nowrap" "Flash:")
		 (<TD> :nowrap "nowrap"
		       (let ((fa (exif-flash e)))
			  (if fa "on" "off"))))
	   (<TR> (<TH> :nowrap "nowrap" "Metering:")
		 (<TD> :nowrap "nowrap"
		       (let ((me (exif-metering-mode e)))
			  (if (string? me) me "?"))))
	   (<TR> (<TH> :nowrap "nowrap" "Camera make:")
		 (<TD> :nowrap "nowrap" (exif-make e)))
	   (<TR> (<TH> :nowrap "nowrap" "Camera model:")
		 (<TD> :nowrap "nowrap" (exif-model e)))
	   (<TR> (<TH> :nowrap "nowrap" "Cdd width:")
		 (<TD> :nowrap "nowrap"
		       (let ((cw (exif-cdd-width e)))
			  (if (not (or (number? cw) (pair? cw)))
			      "? "
			      (format "~amm" (format-real cw 2))) )))
	   (<TR> (<TH> :nowrap "nowrap" "Jpeg encoding:")
		 (<TD> :nowrap "nowrap" (exif-jpeg-encoding e)))
	   (<TR> (<TH> :nowrap "nowrap" "Jpeg compr.:")
		 (<TD> :nowrap "nowrap" (format-elong (exif-jpeg-compress e))))
	   (if (and (string? (exif-comment e))
		    (>fx (string-length (exif-comment e)) 0))
	       (list
		(<TR> (<TD> :colspan 2 (<HR>)))
		(<TR> (<TD> :colspan 2 (exif-comment e)))))))))

;*---------------------------------------------------------------------*/
;*    orientation-rotation ...                                         */
;*---------------------------------------------------------------------*/
(define (orientation-rotation orientation)
   (case orientation
      ((seascape) 180)
      ((portrait) 90)
      ((upsidedown) 270)
      (else 0)))

;*---------------------------------------------------------------------*/
;*    http-response-thumbnail ...                                      */
;*---------------------------------------------------------------------*/
(define (http-response-thumbnail apath)
   (define (http-response-thumbnail-sans-thumbnail cache)
      (if (<elong (file-size apath) (*elong #e100 #e1024))
	  ;; small file dont deserve cache
	  (instantiate::http-response-procedure
	     (content-type (mime-type apath "image/jpeg"))
	     (proc (lambda (p)
		      (let* ((pr (run-process "convert"
					      "-resize"
					      "160x120"
					      apath
					      "-"
					      output: pipe:))
			     (pi (process-output-port pr)))
			 (send-chars pi p)
			 (close-input-port pi)))))
	  (let ((cmd (format "convert -resize 160x120 ~a ~a" apath cache)))
	     (if (=fx (system cmd) 0)
		 (instantiate::http-response-file
		    (content-type (mime-type apath "image/jpeg"))
		    (content-length (file-size cache))
		    (file cache))
		 (http-file-not-found apath)))))
   (let ((cache (string-append (dirname apath)
			       (string (file-separator))
			       "." (prefix (basename apath))
			       "-thumbnail.jpg")))
      (if (and (file-exists? cache)
	       (>=second (file-modification-time cache)
			 (file-modification-time apath)))
	  (instantiate::http-response-file
	     (content-type (mime-type apath "image/jpeg"))
	     (content-length (file-size cache))
	     (file cache))
	  (let ((exif (jpeg-exif apath)))
	     (if (not (exif? exif))
		 (http-response-thumbnail-sans-thumbnail cache)
		 (with-access::exif exif (thumbnail orientation)
		    (if (string? thumbnail)
			(if (or (not orientation) (eq? orientation 'landscape))
			    (instantiate::http-response-string
			       (content-type (mime-type apath "image/jpeg"))
			       (content-length (fixnum->elong
						(string-length thumbnail)))
			       (body thumbnail))
			    (let ((rot (integer->string
					(orientation-rotation orientation))))
			       (instantiate::http-response-procedure
				  (content-type (mime-type apath "image/jpeg"))
				  (proc (lambda (p)
					   (let* ((pr (run-process
						       "convert"
						       "-"
						       "-rotate" rot "-"
						       input: pipe:
						       output: pipe:))
						  (pi (process-output-port pr))
						  (po (process-input-port pr)))
					      (display thumbnail po)
					      (flush-output-port po)
					      (close-output-port po)
					      (send-chars pi p)
					      (close-input-port pi)))))))
			(http-response-thumbnail-sans-thumbnail cache))))))))

;*---------------------------------------------------------------------*/
;*    dired-image? ...                                                 */
;*---------------------------------------------------------------------*/
(define (dired-image? path)
   (suffix-member-ci path '("jpg" "jpeg" "png" "gif")))

;*---------------------------------------------------------------------*/
;*    next-directory-image ...                                         */
;*---------------------------------------------------------------------*/
(define (next-directory-image path cmp)
   (if (string=? path "/")
       #f
       (let* ((parent (dirname path))
	      (base (basename path))
	      (dirs (cdr (member base
				 (sort (dired-directory->list parent) cmp)))))
	  (if (null? dirs)
	      (next-directory-image parent cmp)
	      (let loop ((dirs dirs))
		 (if (null? dirs)
		     (next-directory-image parent cmp)
		     (let* ((dir (make-file-name parent (car dirs)))
			    (files (filter dired-image?
					   (sort (dired-directory->list dir)
						 cmp))))
			(cond
			   ((pair? files)
			    (make-file-name dir (car files)))
			   (else
			    #f)))))))))

;*---------------------------------------------------------------------*/
;*    next-image ...                                                   */
;*---------------------------------------------------------------------*/
(define (next-image path offset)
   (let* ((dir (dirname path))
	  (base (basename path)))
      (let loop ((files (dired-get-files dir #t 'name 'increase))
		 (prev #f))
	 (cond
	    ((null? files)
	     #f)
	    ((string=? (car files) base)
	     (if (=fx offset 1)
		 (let liip ((next (cdr files)))
		    (cond
		       ((null? next)
			#f)
		       ((dired-image? (car next))
			(make-file-name dir (car next)))
		       (else
			(liip (cdr next)))))
		 (if prev
		     (make-file-name dir prev)
		     #f)))
	    ((dired-image? (car files))
	     (if (string>? (car files) base)
		 (if (=fx offset 1)
		     (make-file-name dir (car files))
		     (if prev
			 (make-file-name dir prev)
			 #f))
		 (loop (cdr files) (car files))))
	    (else
	     (loop (cdr files) prev))))))

;*---------------------------------------------------------------------*/
;*    dired-rotate-image ...                                           */
;*---------------------------------------------------------------------*/
(define-service (dired/rotate-image path rot)
   (if (file-exists? path)
       (instantiate::http-response-procedure
	  (content-type (mime-type path "image/jpeg"))
	  (proc (lambda (p)
		   (let* ((proc (run-process "convert"
					     path
					     "-rotate" rot
					     "-"
					     output: pipe:))
			  (po (process-output-port proc)))
		      (send-chars po p)))))
       (http-file-not-found path)))

;*---------------------------------------------------------------------*/
;*    dired-image ...                                                  */
;*---------------------------------------------------------------------*/
(define (dired-image req path ro w h fs tm pp)
   (when (file-exists? path)
      (with-access::exif (jpeg-exif path) (width height orientation)
	 (let (jw jh)
	    (if (or (and (memq orientation '(portrait upsidedown))
			 (>fx width height))
		    (memq ro '(90 270)))
		(begin
		   (set! jh width)
		   (set! jw height))
		(begin
		   (set! jw width)
		   (set! jh height)))
	    (let* ((ww (-fx w 50))
		   (wh (-fx h 50))
		   (sw (if (<fx jw ww) 1 (/ ww jw)))
		   (sh (if (<fx jh wh) 1 (/ wh jh)))
		   (shrink (if (< sw sh) sw sh))
		   (iw (inexact->exact (* shrink jw)))
		   (ih (inexact->exact (* shrink jh)))
		   (id (xml-make-id 'EXIF))
		   (tip (<TOOLTIP> :id id (<DIRED-JPEG-INFO> path)))
		   (src (if (and (or (not (integer? ro)) (=fx ro 0))
				 (or (not orientation)
				     (eq? orientation 'landscape)))
			    path
			    (make-service-url
			     dired/rotate-image
			     path
			     (cond
				((integer? ro)
				 ro)
				((and (memq orientation '(portrait upsidedown))
				      (<fx width height))
				 0)
				(else
				 (orientation-rotation orientation))))))
		   (dir (make-file-name (dired-install-directory) "icons/22x22")))
	       (instantiate::http-response-hop
		  (xml (<HTML>
			(<DIRED-IMAGE-HEAD> req (dirname path))
			(<BODY>
			 :class "dired-image"
			 :onload
			 {dired_image_init($dired/image,$dired/remove,$path,$ro,$fs,$tm,$id,$pp,$dir)}
			 (list tip
			       (<DIV> :class "dired-image"
				      :id "dired-image"
				      (<IMG> :class "dired-image"
					     :width iw
					     :height ih
					     :alt (basename path)
					     :src src))
			       (<DIV> :class "dired-image-popup"
				      :id "dired-image-popup")))))))))))

;*---------------------------------------------------------------------*/
;*    dired/image ...                                                  */
;*---------------------------------------------------------------------*/
(define-service (dired/image path rotation w h fs next timer popup)
   (if (=fx next 0)
       (dired-image (the-current-request) path rotation w h fs timer popup)
       (let ((npath (or (next-image path next)
			(next-directory-image (dirname path)
					      (if (>= next 0)
						  string<?
						  string>?)))))
	  (if (string? npath)
	      (dired-image (the-current-request) npath rotation w h fs timer popup)
	      (http-service-unavailable path)))))
   
;*---------------------------------------------------------------------*/
;*    dired-thumbnail ...                                              */
;*---------------------------------------------------------------------*/
(define-service (dired/thumbnail path)
   (if (file-exists? path)
       (http-response-thumbnail path)
       (http-file-not-found path)))

;*---------------------------------------------------------------------*/
;*    dired/put-comment ...                                            */
;*---------------------------------------------------------------------*/
(define-service (dired/put-comment path com)
   (define (cm req header)
      (if (file-exists? path)
	  (if (string? (jpeg-exif-comment-set! path com))
	      (instantiate::http-response-string
		 (body "ok"))
	      (http-internal-warning "Can't store comment"))
	  (http-file-not-found path)))
   (dired-sysadmin (the-current-request) cm path))

;*---------------------------------------------------------------------*/
;*    JPG filter ...                                                   */
;*---------------------------------------------------------------------*/
(dired-icon-filter-add!
 (lambda (req dir name ident) 
    (when (and (or (is-suffix-ci? name "jpg") (is-suffix-ci? name "jpeg"))
	       (not (string-contains name "-thumbnail")))
       (with-access::http-request req (host port)
	  (let* ((file (make-file-name dir name))
		 (id (xml-make-id))
		 (e (jpeg-exif file))
		 (com (and (exif? e) (or (exif-comment e) "")))
		 (show {dired_image_show( $dired/image, $file, $name, !event.shiftKey )}) 
		 (gimp {dired_exec( $dired/exec, "gimp-2.0", $file )})
		 (save {dired_exif_comment( event, $dired/put-comment, $file, $id, $com )})
		 (tooltip (<TOOLTIP> (<DIRED-JPEG-INFO> file)))
		 (src (make-service-url dired/thumbnail file)))
	     (<DIRED-ICON-ENTRY>
	      req
	      dir
	      "exif"
	      name
	      ident
	      (<IMG> :class "thumbnail" :onclick show :alt file :src src)
	      (and (string? com) (<TEXTAREA> :id id :readonly #t com))
	      (<DIRED-BUTTON> "Edit comment" save "keyboard.png")
 	      (list
	       tooltip
	       (<DIRED-BUTTON> "Exif" {hop_tooltip_show( event,$tooltip )} "camera.png" "help"))
	      (<DIRED-BUTTON> "Gimp" gimp "draw.png")))))))

