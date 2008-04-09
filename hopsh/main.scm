;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/hopsh/main.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Tue Mar 25 08:16:50 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPSH entry point                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh

   (library hop)

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
   ;; set the library load path
   (let ((hop-path (make-file-path (hop-lib-directory) "hop" (hop-version))))
      (bigloo-library-path-set! (cons hop-path (bigloo-library-path))))
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
