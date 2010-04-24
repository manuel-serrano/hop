;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/password.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 29 10:14:56 2010                          */
;*    Last change :  Fri Apr 23 19:46:37 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Password encryption (shared by client and server code).          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_password
   
   #;@server (cond-expand ((not scheme2js) (import __hop_param)))
   
   (export (authentication-encrypt::bstring #!key (schema 'digest) (algo 'ho0) session name password path ip)
	   (basic-password-encrypt::bstring ::bstring ::bstring)
	   (digest-password-encrypt::bstring ::bstring ::bstring ::bstring)
	   (password-encrypt::bstring ::bstring ::bstring ::symbol)
	   (h0password::bstring ::bstring ::bstring)
	   (h1password::bstring ::bstring ::bstring ::int)
	   (h2password::bstring ::bstring ::bstring ::int ::bstring)))

;*---------------------------------------------------------------------*/
;*    The head                                                         */
;*---------------------------------------------------------------------*/
;; @client (<HEAD> :include "md5")

;*---------------------------------------------------------------------*/
;*    authentication-encrypt ...                                       */
;*---------------------------------------------------------------------*/
(define (authentication-encrypt #!key
				(schema 'digest)
				(algo 'ho0)
				session
				name
				password
				path
				ip)
   
   (define (encrypt-ho0-authentication m n p path)
      (let ((k (h0password (password-encrypt n p m) path)))
	 (string-append "HO0" n ":" k)))
   
   (define (encrypt-ho1-authentication m n p path)
      (if session
	  (let ((k (h1password (password-encrypt n p m) path session)))
	     (string-append "HO1" n ":" k))
	  (encrypt-ho0-authentication m n p path)))
   
   (define (encrypt-ho2-authentication m n p path ip)
      (if ip
	  (let ((k (h2password (password-encrypt n p m) path session ip)))
	     (string-append "HO2" n ":" k))
	  (encrypt-ho1-authentication m n p path)))
   
   (case algo
      ((none) password)
      ((ho0) (encrypt-ho0-authentication schema name password path))
      ((ho1) (encrypt-ho1-authentication schema name password path))
      ((ho2) (encrypt-ho2-authentication schema name password path ip))
      (else (error 'authentication-encrypt "Illegal algorithm" algo))))

;*---------------------------------------------------------------------*/
;*    basic-password-encrypt ...                                       */
;*---------------------------------------------------------------------*/
(define (basic-password-encrypt n p)
   (md5sum (string-append n " " p)))

;*---------------------------------------------------------------------*/
;*    digest-password-encrypt ...                                      */
;*---------------------------------------------------------------------*/
(define (digest-password-encrypt n p r)
   (md5sum (string-append n ":" r ":" p)))

;*---------------------------------------------------------------------*/
;*    password-encrypt ...                                             */
;*---------------------------------------------------------------------*/
(define (password-encrypt n p method)
   (if (or (eq? method 'basic) (eq? method 'url))
       (basic-password-encrypt n p)
       (digest-password-encrypt n p (hop-realm))))

;*---------------------------------------------------------------------*/
;*    h0password ...                                                   */
;*---------------------------------------------------------------------*/
(define (h0password pass path)
   (md5sum (string-append pass ":" path)))

;*---------------------------------------------------------------------*/
;*    h1password ...                                                   */
;*---------------------------------------------------------------------*/
(define (h1password pass::bstring path::bstring session::int)
   (tprint "h1password\n  path=" (string-for-read pass) "\n  path=" (string-for-read path) "\n  session=" session)
   (md5sum (string-append (integer->string session) ":" pass ":" path)))

;*---------------------------------------------------------------------*/
;*    h2password ...                                                   */
;*---------------------------------------------------------------------*/
(define (h2password pass::bstring path::bstring session::int ip::bstring)
   (md5sum (string-append (integer->string session) ":" ip ":" pass ":" path)))

