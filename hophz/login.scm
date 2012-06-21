;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/hophz/login.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 10 07:50:17 2006                          */
;*    Last change :  Thu May 31 14:54:11 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dealing with Hophz login                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*                                                                     */
;*---------------------------------------------------------------------*/
(module hophz_login

   (library hop)
   
   (export (hophz-user::bstring)
	   (hophz-login::obj)
	   (login! #!optional user))

   (eval   (export-exports)))
   
;*---------------------------------------------------------------------*/
;*    hophz-login ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hophz-user "")
(define-parameter hophz-login #f)

;*---------------------------------------------------------------------*/
;*    login! ...                                                       */
;*---------------------------------------------------------------------*/
(define (login! #!optional user)
   (unless (string? user)
      (let* ((tty (open-output-file "/dev/tty"))
	     (out (if (output-port? tty) tty (current-error-port))))
	 (display "login: " out)
	 (flush-output-port out)
	 (set! user (read-line))
	 (when (output-port? tty) (close-output-port tty))))
   (unless (or (not (string? user)) (string=? user ""))
      (hophz-user-set! user)
      (let ((i (string-index user #\:)))
	 (if (or (not i) (<fx i 0))
	     (let ((passwd (password "password: ")))
		(hophz-login-set!
		 (string-append "Basic "
				(base64-encode
				 (string-append user ":" passwd)))))
	     (hophz-login-set!
	      (string-append "Basic " (base64-encode user)))))))
		

