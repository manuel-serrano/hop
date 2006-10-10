;*=====================================================================*/
;*    serrano/prgm/project/hop/hopsh/login.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 10 07:50:17 2006                          */
;*    Last change :  Tue Oct 10 08:11:45 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
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
      (display "login: ")
      (set! user (read-line))
      (flush-output-port (current-output-port)))
   (unless (string=? user "")
      (hopsh-user-set! user)
      (display "password: ")
      (flush-output-port (current-output-port))
      (let ((passwd (password)))
	 (newline)
	 (flush-output-port (current-output-port))
	 (hopsh-login-set!
	  (string-append "Basic "
			 (base64-encode (string-append user ":" passwd)))))))

