;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/repl.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct  6 08:22:43 2013                          */
;*    Last change :  Mon Feb 13 17:25:14 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    NodeJS like REPL                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs_repl

   (include "../hopscript/stringthread.sch"
	    "nodejs.sch")
   
   (library hopscript js2scheme)

   (import __nodejs_require)
   
   (export (repljs ::JsGlobalObject ::WorkerHopThread)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    prompt ...                                                       */
;*---------------------------------------------------------------------*/
(define (prompt)
   (display "> ")
   (flush-output-port (current-output-port)))

;*---------------------------------------------------------------------*/
;*    repljs ...                                                       */
;*---------------------------------------------------------------------*/
(define (repljs %this %worker)
   (set! __js_strings (&init!))
   ;; start executing the nodejs header
   (let ((old-intrhdl (get-signal-handler sigint))
	 (mod (eval-module))
	 (module (nodejs-new-module "<repl>" (make-file-name (pwd) "repl.js")
		    %worker %this)))
      ;; force the module initialization
      (let ((exp (call-with-input-string "false"
		    (lambda (in)
		       (j2s-compile in :driver (j2s-plain-driver)
			  :driver-name "j2s-plain-driver"
			  :parser 'repl
			  :filename "repl.js"
			  :warning-global #f
			  :node-modules-directory (nodejs-node-modules-directory))))))
	 ((eval exp) %this %this %this module))
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
	       (let ((console (nodejs-require-core "console" %worker %this)))
		  (let luup ()
		     (with-handler
			(lambda (e)
			   (repl-error-handler e)
			   (luup))
			(let liip ()
			   (prompt)
			   (let ((exp (jsread-and-compile)))
			      (if (null? exp)
				  (quit)
				  (let ((v (js-worker-exec %worker "repl" #f
					      (lambda (%this)
						 (with-handler
						    repl-error-handler
						    (let ((v ((eval exp) %this %this %this module)))
						       (jsprint v console %this)))))))
				     (liip)))))))))
	    (loop))
	 (if (procedure? old-intrhdl)
	     (signal sigint old-intrhdl)
	     (signal sigint (lambda (n) (exit 0)))))))

;*---------------------------------------------------------------------*/
;*    repl-error-handler ...                                           */
;*---------------------------------------------------------------------*/
(define (repl-error-handler e)
   (cond
      ((isa? e JsError)
       (exception-notify e))
      ((isa? e &error)
       (error-notify e)
       (with-access::&error e (obj)
	  (when (eof-object? obj)
	     (reset-eof (current-input-port))))))
   (sigsetmask 0))

;*---------------------------------------------------------------------*/
;*    jsprint ...                                                      */
;*---------------------------------------------------------------------*/
(define (jsprint exp console %this)
   (let ((log (js-get console (& "log") %this)))
      (js-call1 %this log console exp)))

;*---------------------------------------------------------------------*/
;*    jsread-and-compile ...                                           */
;*---------------------------------------------------------------------*/
(define (jsread-and-compile)
   (j2s-compile (current-input-port)
      :parser 'repl
      :driver (j2s-eval-driver)
      :driver-name "j2s-eval-driver"
      :commonjs-export #f
      :warning-global #f
      :filename "repl.js"))   

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
