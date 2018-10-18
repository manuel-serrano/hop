;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopsh/repl.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct  7 16:45:39 2006                          */
;*    Last change :  Fri Nov 28 11:28:51 2014 (serrano)                */
;*    Copyright   :  2006-14 Manuel Serrano                            */
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
   
   (export  (hopsh-eval ::obj)
	    (hopsh-eval-string ::bstring)
	    (hopsh-repl)))

;*---------------------------------------------------------------------*/
;*    hopsh-eval ...                                                   */
;*---------------------------------------------------------------------*/
(define (hopsh-eval exp)
   (with-handler
      (lambda (e)
	 (if (isa? e &error)
	     (begin
		(exception-notify e)
		(when (with-access::&error e (obj) (eof-object? obj))
		   (reset-eof (current-input-port)))
		(sigsetmask 0)
		#unspecified)
	     (raise e)))
      (if (string? exp)
	  (hopsh-eval-string exp)
	  (hopsh-eval-expression exp))))

;*---------------------------------------------------------------------*/
;*    hopsh-eval-string ...                                            */
;*---------------------------------------------------------------------*/
(define (hopsh-eval-string str)
   (if (=fx (string-length str) 0)
       ""
       (eval-command str)))

;*---------------------------------------------------------------------*/
;*    hopsh-eval-expression ...                                        */
;*---------------------------------------------------------------------*/
(define (hopsh-eval-expression exp)
   (hopsh-exec (expression->url exp)))

;*---------------------------------------------------------------------*/
;*    expression->url ...                                              */
;*---------------------------------------------------------------------*/
(define (expression->url obj)
   (make-hopsh-url (hopsh-eval-service) (list obj)))

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
		(error "command->url" "Illegal command" str)
		(make-hopsh-url (car args)
				(command-options str (cdr args))))))))

;*---------------------------------------------------------------------*/
;*    make-hopsh-url ...                                               */
;*---------------------------------------------------------------------*/
(define (make-hopsh-url cmd options)
   (hop-apply-url (format "http://~a:~a~a/~a"
			  (hopsh-host)
			  (hop-port)
			  (hop-service-base)
			  cmd)
		  options))

;*---------------------------------------------------------------------*/
;*    hopsh-exec ...                                                   */
;*---------------------------------------------------------------------*/
(define (hopsh-exec url)
   (let loop ((header (login->header (hopsh-login)))
	      (count 3))
      (let liip ((ttl 5))
	 (with-handler
	    (lambda (e)
	       (if (and (isa? e &io-timeout-error) (> ttl 0))
		   (begin
		      (sleep 100000)
		      (liip (- ttl 1)))
		   (raise e)))
	    (with-url url
	       (lambda (s) s)
	       :timeout (hopsh-timeout)
	       :fail (lambda (xhr)
			(with-access::xml-http-request xhr (status input-port)
			   (case status
			      ((404)
			       (error "hopsh" "document not found" url))
			      ((401)
			       (if (=fx count 0)
				   (error "hop-sh" "permission denied'" url)
				   (begin
				      (login!)
				      (when (hopsh-login)
					 (loop (login->header (hopsh-login))
					    (-fx count 1))))))
			      (else
			       (error "hopsh"
				  (format "Illegal status code `~a'" status)
				  (read-string input-port))))))
	       :header header)))))

;*---------------------------------------------------------------------*/
;*    login->header ...                                                */
;*---------------------------------------------------------------------*/
(define (login->header login)
   (if login
       `((:authorization ,login))
       '()))

;*---------------------------------------------------------------------*/
;*    command-options ...                                              */
;*---------------------------------------------------------------------*/
(define (command-options str opts)
   (let loop ((o opts))
      (cond
	 ((null? o)
	  '())
	 ((null? (cdr o))
	  (error "command->url"
		 (format "Illegal command option `~a'" (car o))
		 str))
	 ((not (char=? (string-ref (car o) 0) #\-))
	  (cons (car o) (loop (cdr o))))
	 ((null? (cdr o))
	  (error "command->url"
		 (format "Actual value missing for option: ~a" (car o))
		 `(,str ,@opts)))
	 (else
	  (cons* (string->keyword (substring (car o) 1 (string-length (car o))))
		 (cadr o)
		 (loop (cddr o)))))))

;*---------------------------------------------------------------------*/
;*    hopsh-read-line-or-exp ...                                       */
;*---------------------------------------------------------------------*/
(define (hopsh-read-line-or-exp)
   (let ((c (peek-char)))
      (if (eq? c #\()
	  (read)
	  (read-line))))

;*---------------------------------------------------------------------*/
;*    hopsh-repl ...                                                   */
;*---------------------------------------------------------------------*/
(define (hopsh-repl)
   (let loop ()
      (hopsh-prompt)
      (let ((exp (hopsh-read-line-or-exp)))
	 (unless (eof-object? exp)
	    (print (hopsh-eval exp))
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    hopsh-prompt ...                                                 */
;*---------------------------------------------------------------------*/
(define (hopsh-prompt)
   (printf "~a@~a> " (hopsh-user) (hopsh-host)))
