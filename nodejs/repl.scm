;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/nodejs/repl.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct  6 08:22:43 2013                          */
;*    Last change :  Mon Jan 13 16:24:19 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NodeJS like REPL                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_repl

   (library hopscript js2scheme)

   (import __nodejs_require)
   
   (export (repljs)))

;*---------------------------------------------------------------------*/
;*    prompt ...                                                       */
;*---------------------------------------------------------------------*/
(define (prompt)
   (display "> ")
   (flush-output-port (current-output-port)))

;*---------------------------------------------------------------------*/
;*    repljs ...                                                       */
;*---------------------------------------------------------------------*/
(define (repljs)
   (let ((old-intrhdl (get-signal-handler sigint))
	 (mod (eval-module))
	 (console (nodejs-require "console")))
      ;; preload the hopscript header
      (call-with-input-string ""
	 (lambda (in)
	    (for-each eval!
	       (j2s-compile in :driver (j2s-plain-driver)))))
      ;; enter the read-eval-print loop
      (unwind-protect
	 (let loop ()
	    (bind-exit (re-enter-internal-repl)
	       ;; we setup ^C interupt
	       (letrec ((intrhdl (lambda (n)
				    (print "CTRL-C")
				    (notify-interrupt n)
				    ;; we flush current input port
				    (reset-console! (current-input-port))
				    ;; we restore signal handling
				    (sigsetmask 0)
				    (re-enter-internal-repl #unspecified))))
		  (signal sigint intrhdl))
	       ;; and we loop until eof
	       (newline)
	       (let luup ()
		  (with-handler
		     (lambda (e)
			(cond
			   ((isa? e JsError)
			    ((js-get console 'log) console e))
			   ((isa? e &error)
			    (error-notify e)
			    (with-access::&error e (obj)
			       (when (eof-object? obj)
				  (reset-eof (current-input-port))))))
			(sigsetmask 0)
			(luup))
		     (let liip ()
			(prompt)
			(let ((exp (jsread)))
			   (if (null? exp)
			       (quit)
			       (let ((v (jseval exp)))
				  (jsprint v console)
				  (liip))))))))
	    (loop))
	 (if (procedure? old-intrhdl)
	     (signal sigint old-intrhdl)
	     (signal sigint (lambda (n) (exit 0)))))))

;*---------------------------------------------------------------------*/
;*    jsprint ...                                                      */
;*---------------------------------------------------------------------*/
(define (jsprint exp console)
   (js-call1 (js-get console 'log) console exp))

;*---------------------------------------------------------------------*/
;*    jsread ...                                                       */
;*---------------------------------------------------------------------*/
(define (jsread)
   (j2s-compile (current-input-port)
      :parser (lambda (in) (j2s-parser in :start 'repl))
      :driver (j2s-eval-driver)))

;*---------------------------------------------------------------------*/
;*    jseval ...                                                       */
;*---------------------------------------------------------------------*/
(define (jseval exprs)
   (let ((m (eval-module)))
      (unwind-protect
	 ;; eval the compile module in the current environment
	 (let loop ((last (js-undefined))
		    (exprs exprs))
	    (if (pair? exprs)
		(loop (eval (car exprs)) (cdr exprs))
		last))
	 ;; restore the previous module
	 (eval-module-set! m))))

   
