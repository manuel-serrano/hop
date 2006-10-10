;*=====================================================================*/
;*    serrano/prgm/project/hop/hopsh/repl.scm                          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct  7 16:45:39 2006                          */
;*    Last change :  Tue Oct 10 08:08:57 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The HopSh read-eval-print loop                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh_repl
   
   (library web
	    hop)

   (import  hopsh_param
	    hopsh_login)
   
   (export  (hopsh-eval ::bstring)
	    (hopsh-repl)))

;*---------------------------------------------------------------------*/
;*    hopsh-eval ...                                                   */
;*---------------------------------------------------------------------*/
(define (hopsh-eval str)
   (with-handler
      (lambda (e)
	 (if (&error? e)
	     (begin
		(error-notify e)
		(when (eof-object? (&error-obj e))
		   (reset-eof (current-input-port)))
		(sigsetmask 0)
		#unspecified)
	     (raise e)))
      (cond
	 ((=fx (string-length str) 0)
	  ;; an empty command
	  "")
	 ((char=? (string-ref str 0) #\()
	  ;; a parenthetical expression
	  (eval-expression str))
	 (else
	  ;; a command
	  (eval-command str)))))

;*---------------------------------------------------------------------*/
;*    eval-expression ...                                              */
;*---------------------------------------------------------------------*/
(define (eval-expression str)
   (hopsh-exec (expression->url str)))

;*---------------------------------------------------------------------*/
;*    expression->url ...                                              */
;*---------------------------------------------------------------------*/
(define (expression->url str)
   (let ((obj (with-input-from-string str read)))
      (make-hopsh-url "hop"
		      (hopsh-eval-service)
		      (string-append "&exp="
				     (url-encode (obj->string obj))))))

;*---------------------------------------------------------------------*/
;*    eval-command ...                                                 */
;*---------------------------------------------------------------------*/
(define (eval-command str)
   (hopsh-exec (command->url str)))

;*---------------------------------------------------------------------*/
;*    command->url ...                                                 */
;*---------------------------------------------------------------------*/
(define (command->url str)
   (with-input-from-string str
      (lambda ()
	 (let ((args (port->string-list (current-input-port))))
	    (if (null? args)
		(error 'command->url "Illegal command" str)
		(make-hopsh-url "no"
				(car args)
				(apply string-append
				       (command-options str (cdr args)))))))))

;*---------------------------------------------------------------------*/
;*    make-hopsh-url ...                                               */
;*---------------------------------------------------------------------*/
(define (make-hopsh-url encoding cmd options)
   (format "http://~a:~a~a/~a?hop-encoding=~a~a"
	   (hopsh-host)
	   (hop-port)
	   (hop-service-base)
	   cmd
	   encoding
	   options))

;*---------------------------------------------------------------------*/
;*    hopsh-exec ...                                                   */
;*---------------------------------------------------------------------*/
(define (hopsh-exec url)
   (let loop ((header (login->header (hopsh-login)))
	      (count 4))
      (with-url url
	 (lambda (s) s)
	 :fail (lambda (status port)
		  (case status
		     ((404)
		      (error 'hopsh "url not found" url))
		     ((401)
		      (if (=fx count 0)
			  (error 'hop-sh "permission denied'" url)
			  (begin
			     (login!)
			     (when (hopsh-login)
				(loop (login->header (hopsh-login))
				      (-fx count 1))))))
		     (else
		      (error 'hopsh
			     (format "Illegal status code `~a'" status)
			     (read-string port)))))
	 :header header)))

;*---------------------------------------------------------------------*/
;*    login->header ...                                                */
;*---------------------------------------------------------------------*/
(define (login->header login)
   (list (cons :authorization login)))

;*---------------------------------------------------------------------*/
;*    command-options ...                                              */
;*---------------------------------------------------------------------*/
(define (command-options str opts)
   (let loop ((opts opts))
      (cond
	 ((null? opts)
	  '())
	 ((null? (cdr opts))
	  (error 'command->url
		 (format "Illegal command option `~a'" (car opts))
		 str))
	 ((not (char=? (string-ref (car opts) 0) #\-))
	  (error 'command->url
		 (format "Illegal command option `~a'" (car opts))
		 str))
	 (else
	  (cons (format "&~a=~a"
			(substring (car opts)
				   1
				   (string-length (car opts)))
			(url-encode (cadr opts)))
		(loop (cddr opts)))))))

;*---------------------------------------------------------------------*/
;*    hopsh-repl ...                                                   */
;*---------------------------------------------------------------------*/
(define (hopsh-repl)
   (let loop ()
      (hopsh-prompt)
      (let ((str (read-line)))
	 (unless (eof-object? str)
	    (print (hopsh-eval str))
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    hopsh-prompt ...                                                 */
;*---------------------------------------------------------------------*/
(define (hopsh-prompt)
   (printf "~a@~a> " (hopsh-user) (hopsh-host)))
