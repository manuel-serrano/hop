;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/hopreplay/login.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 10 07:50:17 2006                          */
;*    Last change :  Sat Apr 26 08:51:24 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dealing with Hoprp login                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*                                                                     */
;*---------------------------------------------------------------------*/
(module hoprp_login

   (library hop)
   
   (export (hoprp-user::bstring)
	   (hoprp-login::obj)
	   (login! #!optional user))

   (eval   (export-exports)))
   
;*---------------------------------------------------------------------*/
;*    hoprp-login ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hoprp-user "")
(define-parameter hoprp-login #f)

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
      (hoprp-user-set! user)
      (let ((i (string-index user #\:)))
	 (if (or (not i) (<fx i 0))
	     (let ((passwd (password "password: ")))
		(hoprp-login-set!
		 (string-append "Basic "
				(base64-encode
				 (string-append user ":" passwd)))))
	     (hoprp-login-set!
	      (string-append "Basic " (base64-encode user)))))))
		

