;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopc/libdir.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Fri Sep  4 08:39:02 2009                          */
;*    Last change :  Tue Oct  3 09:42:25 2017 (serrano)                */
;*    Copyright   :  2009-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Processes the lib-dir-compilation parameter.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_libdir
   (export (bigloo-override-config! path)))

;*---------------------------------------------------------------------*/
;*    safe-read ...                                                    */
;*---------------------------------------------------------------------*/
(define (safe-read p type f)
   
   (define (correct-type? obj)
      (case type
	 ((string) (string? obj))
	 ((pair) (pair? obj))
	 (else (error 'lib-dir "Internal Error" type))))
   
   (let ((tmp (read p)))
      (unless (correct-type? tmp)
	 (error "lib-dir" "Bad bigloo_config.sch file" f))
      tmp))

;*---------------------------------------------------------------------*/
;*    process-lib-version ...                                          */
;*---------------------------------------------------------------------*/
(define (process-lib-version p f)
   (let* ((lib-version (safe-read p 'string f))
	  (lib-specific-version (safe-read p 'string f)))
      (unless (and (string=? (bigloo-config 'release-number)
		      lib-version)
		   (string=? (bigloo-config 'specific-version)
		      lib-specific-version))
	 ;; replace with error?
	 (warning "Cross compilation for different Bigloos is risky."))))

;*---------------------------------------------------------------------*/
;*    process-lib-config ...                                           */
;*---------------------------------------------------------------------*/
(define (process-lib-config p f)
   ;; read-config is a back-quoted list. -> we need to eval it.
   (let* ((read-config (safe-read p 'pair f))
	  (lib-config (eval read-config)))
      ;; override the existing config entries
      (for-each (lambda (c)
		   (bigloo-configuration-add-entry! (car c) (cdr c)))
	 lib-config)))

;*---------------------------------------------------------------------*/
;*    read-config_sch ...                                              */
;*---------------------------------------------------------------------*/
(define (read-config_sch f)
   (let ((port (open-input-file f)))
      (unwind-protect
	 (begin
	    (process-lib-version port f)
	    (process-lib-config port f))
	 ;; TODO: we might want to determine the cross-prefix out of 'f' for
	 ;;   -L (*lib-dir*) and -I
	 (close-input-port port))))

;*---------------------------------------------------------------------*/
;*    bigloo-override-config! ...                                      */
;*---------------------------------------------------------------------*/
(define (bigloo-override-config! path)
   (let ((dir (file-name-canonicalize! path))
	 (opath (bigloo-config 'library-directory)))
      (unless (directory? dir)
	 (error "lib-dir" "Not a directory" dir))
      (bigloo-configuration-add-entry! 'library-directory dir)
      (let ((config_sch (make-file-path dir "bigloo_config.sch")))
	 (if (file-exists? config_sch)
	     (read-config_sch config_sch)
	     (error "lib-dir" "config does not exist" config_sch))
	 (bigloo-library-path-set!
	    (cons dir (delete! opath (bigloo-library-path)))))))
