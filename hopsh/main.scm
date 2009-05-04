;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/hopsh/main.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Mon May  4 16:32:40 2009 (serrano)                */
;*    Copyright   :  2004-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPSH entry point                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh

   (library scheme2js hopscheme hop)

   (cond-expand
      (enable-threads (library pthread)))
   
   (import  hopsh_parseargs
	    hopsh_param
	    hopsh_repl)

   (main    main))

;*---------------------------------------------------------------------*/
;*    hop-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define-expander hop-verb
   (lambda (x e)
      (match-case x
	 ((?- (and (? integer?) ?level) . ?rest)
	  (let ((v (gensym)))
	     `(let ((,v ,(e level e)))
		 (if (>=fx (hop-verbose) ,v)
		     (hop-verb ,v ,@(map (lambda (x) (e x e)) rest))))))
	 (else
	  `(hop-verb ,@(map (lambda (x) (e x e)) (cdr x)))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; set the Hop cond-expand identification
   (for-each register-eval-srfi! (hop-srfis))
   ;; set the library load path
   (bigloo-library-path-set! (hop-library-path))
   ;; preload the hop library
   (eval `(library-load 'hop))
   ;; parse the command line
   (parse-args args)
   (hop-verb 1 "Starting hopsh (v" (hop-version) "):\n")
   ;; setup the hop readers
   (bigloo-load-reader-set! hop-read)
   ;; start the hop main loop
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 (exit 2))
      (unwind-protect
	 (hopsh-repl)
	 (newline))))
