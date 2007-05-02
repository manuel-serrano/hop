;*=====================================================================*/
;*    serrano/prgm/project/hop/hopsh/login.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 10 07:50:17 2006                          */
;*    Last change :  Mon Apr 23 07:02:42 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dealing with HopSh login                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*                                                                     */
;*---------------------------------------------------------------------*/
(module hopsh_login

   (library hop)
   
   (export (hopsh-user::bstring)
	   (hopsh-login::obj)
	   (login! #!optional user))

   (eval   (export-exports)))
   
;*---------------------------------------------------------------------*/
;*    hopsh-login ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopsh-user "")
(define-parameter hopsh-login #f)

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
      (hopsh-user-set! user)
      (let ((i (string-index user #\:)))
	 (if (or (not i) (<fx i 0))
	     (let ((passwd (password "password: ")))
		(hopsh-login-set!
		 (string-append "Basic "
				(base64-encode
				 (string-append user ":" passwd)))))
	     (hopsh-login-set!
	      (string-append "Basic " (base64-encode user)))))))
		

