;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/hopreplay/main.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Sat Apr 26 09:58:59 2008 (serrano)                */
;*    Copyright   :  2004-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPREPLAY entry point                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hoprp

   (library hop web pthread)
   
   (import  hoprp_parseargs
	    hoprp_param
	    hoprp_replay
	    hoprp_log)

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
   (let ((logs (append-map parse-log (parse-args args))))
      (hop-verb 1 "Starting hopreplay (v" (hop-version) "):\n")
      ;; setup the hop readers
      (bigloo-load-reader-set! hop-read)
      ;; start the hop main loop
      (replay logs)))
