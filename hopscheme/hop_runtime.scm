;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/hopscheme/hop_runtime.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov  5 19:11:10 2012                          */
;*    Last change :  Fri Jun 21 09:12:05 2013 (serrano)                */
;*    Copyright   :  2012-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScheme runtime interface                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_hop_runtime
   (library scheme2js)
   (import __hopscheme_precompilation
	   __hopscheme_config)
   (export (hop-runtime-modules::pair-nil)
	   (hopscheme-runtime-resolver ::symbol ::pair-nil ::obj)))

;*---------------------------------------------------------------------*/
;*    *hop-runtime-afile* ...                                          */
;*    -------------------------------------------------------------    */
;*    afile is supposed do be in share-directory (which don't          */
;*    yet know).                                                       */
;*---------------------------------------------------------------------*/
(define *hop-runtime-afile* ".afile")

;*---------------------------------------------------------------------*/
;*    *hop-runtime-modules* ...                                        */
;*---------------------------------------------------------------------*/
(define *hop-runtime-modules*
   '(__hop __hop_password __hop-exception
     __hop-canvas __hop-spage __hop-spinbutton __hop-color __hop-window
     __hop-dashboard __hop-audio __hop-file __hop-font  __hop-gauge))

;*---------------------------------------------------------------------*/
;*    *hopscheme-alist* ...                                            */
;*---------------------------------------------------------------------*/
(define *hopscheme-alist* '())

;*---------------------------------------------------------------------*/
;*    load-hopscheme-alist! ...                                        */
;*---------------------------------------------------------------------*/
(define (load-hopscheme-alist!)
   (let ((afile (make-file-path *hop-share-directory* *hop-runtime-afile*)))
      (let ((al (with-input-from-file afile read)))
	 (set! *hopscheme-alist*
	    (map (lambda (e)
		    (cons (car e)
		       (map (lambda (f) (make-file-path *hop-share-directory* f))
			  (cdr e))))
	       al)))))

;*---------------------------------------------------------------------*/
;*    hopscheme-runtime-resolver ...                                   */
;*---------------------------------------------------------------------*/
(define (hopscheme-runtime-resolver mod files _)
   (with-trace 1 "hopscheme-runtime-resolver"
      (trace-item "mod=" mod)
      (trace-item "files=" files)
      (if (pair? files)
	  files
	  (let ((t (assq mod *hopscheme-alist*)))
	     (when (pair? t)
		(cdr t))))))
   
;*---------------------------------------------------------------------*/
;*    *hop-precompiled-modules* ...                                    */
;*---------------------------------------------------------------------*/
(define *hop-precompiled-modules* #f)

;*---------------------------------------------------------------------*/
;*    precompile-runtime! ...                                          */
;*---------------------------------------------------------------------*/
(define (precompile-runtime!)
   (load-hopscheme-alist!)
   (let ((resolver (lambda (m f _)
		      (let ((f (hopscheme-runtime-resolver m f _)))
			 (if (not f)
			     (error "hop-runtime"
				"Internal error: runtime module not in alist"
				m)
			     f)))))
      (set! *hop-precompiled-modules*
	 (map (lambda (m)
		 (precompile-module m resolver))
	    *hop-runtime-modules*))))

;*---------------------------------------------------------------------*/
;*    hop-runtime-modules ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-runtime-modules)
   (unless *hop-precompiled-modules*
      (precompile-runtime!))
   *hop-precompiled-modules*)
