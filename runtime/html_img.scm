;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/runtime/html_img.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 18 08:04:49 2007                          */
;*    Last change :  Mon Nov 18 10:04:01 2013 (serrano)                */
;*    Copyright   :  2007-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dealing with IMG markups.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_html-img

   (library web)

   (include "param.sch"
	    "xml.sch")

   (import  __hop_types
	    __hop_mime
	    __hop_misc
	    __hop_priv
	    __hop_param
	    __hop_configure
	    __hop_charset
	    __hop_xml-types
	    __hop_xml
	    __hop_hop
	    __hop_cache
	    __hop_user
	    __hop_security)

   (export  (<IMG> . ::obj)
	    (img-base64-encode::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    img-disk-cache ...                                               */
;*---------------------------------------------------------------------*/
(define img-disk-cache #f)
(define img-memory-cache #f)

;*---------------------------------------------------------------------*/
;*    init-img-caches! ...                                             */
;*---------------------------------------------------------------------*/
(define (init-img-caches!)
   (unless img-disk-cache
      (set! img-disk-cache
	    (instantiate::cache-disk
	       (clear #f)
	       (path (make-cache-name "img"))
	       (out display)))
      (set! img-memory-cache
	    (instantiate::cache-memory
	       (max-file-size #e4096)))))

;*---------------------------------------------------------------------*/
;*    inline-base64 ...                                                */
;*---------------------------------------------------------------------*/
(define (inline-base64 src content)
   (format "data:~a;base64,~a"
	   (mime-type src (format "image/~a" (suffix src)))
	   (base64-encode content -1)))
		       
;*---------------------------------------------------------------------*/
;*    img-base64-encode ...                                            */
;*---------------------------------------------------------------------*/
(define (img-base64-encode src)
   (let ((p (open-input-file src)))
      (if (input-port? p)
	  (unwind-protect
	     (inline-base64 src (read-string p))
	     (close-input-port p))
	  src)))

;*---------------------------------------------------------------------*/
;*    cache-memory-inline-image ...                                    */
;*---------------------------------------------------------------------*/
(define (cache-memory-inline-image path)
   (and (file-exists? path)
	(<elong (file-size path) #e4096)
	(let ((ce (cache-get img-memory-cache path)))
	   (if (isa? ce cache-entry)
	       (with-access::cache-entry ce (value)
		  value)
	       (let ((img (img-base64-encode path)))
		  (cache-put! img-memory-cache path img)
		  img)))))

;*---------------------------------------------------------------------*/
;*    cache-disk-inline-image ...                                      */
;*---------------------------------------------------------------------*/
(define (cache-disk-inline-image path)
   (let ((ce (cache-get img-disk-cache path)))
      (if (isa? ce cache-entry)
	  (with-access::cache-entry ce (value)
	     (with-input-from-file value read-string))
	  (let ((img (img-base64-encode path)))
	     (cache-put! img-disk-cache path img)
	     img))))

;*---------------------------------------------------------------------*/
;*    inline-image ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-image path)
   (init-img-caches!)
   (or (cache-memory-inline-image path) (cache-disk-inline-image path)))

;*---------------------------------------------------------------------*/
;*    IMG ...                                                          */
;*---------------------------------------------------------------------*/
(define-tag <IMG> ((id #f string)
		   (inline #f boolean)
		   (alt #f)
		   (src #unspecified)
		   (attributes)
		   body)
   
   (define (plain-img src cssrc)
      (instantiate::xml-empty-element
	 (tag 'img)
	 (id (xml-make-id id 'img))
	 (attributes `(:src ,cssrc :alt ,(or alt (basename src)) ,@attributes))
	 (body '())))

   (define (empty-img)
      (instantiate::xml-empty-element
	 (tag 'img)
	 (id (xml-make-id id 'img))
	 (attributes (if alt `(:alt ,alt ,@attributes) attributes))
	 (body '())))
   
   (define (onerror-img attributes src)
      (let* ((val (format "if( !this.onhoperror ) { this.onhoperror = true; hop_deinline_image(this, \"~a\") }" src))
	     (onerror (plist-assq :onerror attributes))
	     (oval (when onerror (cadr onerror))))
	 (cond
	    ((string? oval)
	     (let ((nval (string-append oval "; " val)))
		(set-car! (cdr onerror) nval)
		attributes))
	    ((isa? oval xml-tilde)
	     (with-access::xml-tilde oval (env menv)
		(let ((nval (sexp->xml-tilde
			       `(begin
				   ,(xml-tilde->sexp oval)
				   ,(secure-javascript-attr val))
			       env
			       menv)))
		   (set-car! (cdr onerror) nval)
		   attributes)))
	    (else
	     `(:onerror ,(secure-javascript-attr val) ,@attributes)))))
   
   (define (inline-img src cssrc isrc)
      (if isrc
	  (instantiate::xml-empty-element
	     (tag 'img)
	     (id (xml-make-id id 'img))
	     (attributes `(:src ,isrc :alt ,(or alt (basename src))
				,@(onerror-img attributes src)))
	     (body '()))
	  (plain-img src cssrc)))

   (cond
      ((isa? src xml-tilde)
       ;; see xml-write-initializations
       (instantiate::xml-empty-element
	  (tag 'img)
	  (id id)
	  (attributes `(:src ,src :alt ,alt ,@attributes))
	  (body '())))
      ((string? src)
       (if (string-prefix? "data:" src)
	   (inline-img src src src)
	   (let ((cssrc (charset-convert src (hop-locale) (hop-charset))))
	      (cond
		 ((and (pair? body) (string? (car body)) (null? (cdr body)))
		  (let ((req (current-request)))
		     (if (or (not req) (authorized-path? (current-request) src))
			 (inline-img src cssrc (inline-base64 src (car body)))
			 (plain-img src cssrc))))
		 (inline
		  (let ((req (current-request)))
		     (if (or (not req) (authorized-path? (current-request) src))
			 (inline-img src cssrc (inline-image src))
			 (plain-img src cssrc))))
		 (else
		  (plain-img src cssrc))))))
      ((eq? src #unspecified)
       (empty-img))
      (else
       (error "<IMG>" "Illegal image src" src))))
