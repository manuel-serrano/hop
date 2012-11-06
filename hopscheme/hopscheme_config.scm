;*=====================================================================*/
;*    .../prgm/project/hop/2.4.x/hopscheme/hopscheme_config.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan 15 07:17:18 2012                          */
;*    Last change :  Tue Nov  6 07:39:55 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Default scheme2js configuration for Hop                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_config
   
   (library scheme2js)
   
   (import __hopscheme_hop_runtime
	   __dollar_scheme2js_module)
   
   (export (hopscheme-config compile-file?)
	   (init-hopscheme! #!key reader share path verbose eval hop-compile hop-register features expanders hop-library-path)
	   *hop-reader*
	   *hop-share-directory*
	   *hop-eval*))

;*---------------------------------------------------------------------*/
;*    Global constants                                                 */
;*---------------------------------------------------------------------*/
(define *module-counter*
   -1)
(define *cached-config*
   #f)
(define *module-counter-lock*
   (make-mutex))

(define *hop-reader*
   (lambda (p v) (error "hop-reader" "not implemented yet" p)))

(define *hop-share-directory*
   "/")
(define *hop-verbose*
   0)

(define *hop-eval*
   (lambda (e) (error "hop-eval" "not initialized yet" e)))
(define *hop-compile*
   (lambda (v p) (error "hop-compile" "not initialized yet" v)))
(define *hop-register*
   (lambda (v) (error "hop-register" "not initialized yet" v)))
(define *hop-library-path*
   '())

;*---------------------------------------------------------------------*/
;*    get-cached-config ...                                            */
;*---------------------------------------------------------------------*/
(define (get-cached-config)
   (or *cached-config*
       (begin
	  ;; no need to worry about multi-threading. in the worst case we
	  ;; create several lists...
	  (set! *cached-config*
	     (extend-config*
		;; on top of scheme2js's default config
		(default-scheme2js-config)
		`(;; unresolved symbols are considered to be JS variables
		  (unresolved=JS . module)
		  ;; procedures may use the 'this' variable. (This was actually
		  ;; already possible due to the 'unresolved=JS flag. But this
		  ;; is cleaner.)
		  (procedures-provide-js-this . #t)
		  ;; one can use the 'return!' form now. eg: (return! #t)
		  (return . #t)
		  ;; include-path
		  (include-paths . ,(list *hop-share-directory*))
		  ;; we are no longer using scheme2js-modules
		  (bigloo-modules . #t)
		  ;; compress the output
		  (compress . #t)
		  ;; allow $(import xyz) ...
		  (module-preprocessor . ,(dollar-modules-adder))
		  ;; hop-compile compiles HOP values.
		  (host-compiler . ,*hop-compile*)
		  ;; register values
		  (host-register . ,*hop-register*)
		  ;; library path
		  (library-path . ,*hop-library-path*)
		  ;; runtime resolver
		  (module-resolver . ,hopscheme-runtime-resolver)
		  ;; pp in debug mode
		  (pp . ,(>fx (bigloo-debug) 1)))))
	  *cached-config*)))

;*---------------------------------------------------------------------*/
;*    hopscheme-config ...                                             */
;*    -------------------------------------------------------------    */
;*    do not reuse hopscheme-configs!                                  */
;*---------------------------------------------------------------------*/
(define (hopscheme-config compile-file?)

   (define (add-suffix-clause conf)
      ;; make sure static variables (not exported, but global) do not clash
      (if compile-file?
	  conf
	  (with-lock *module-counter-lock*
	     (lambda ()
		(set! *module-counter* (+ *module-counter* 1))
		(extend-config
		 conf
		 'statics-suffix
		 (string-append "_hopM"
				(number->string *module-counter*)))))))

   (let* ((config (get-cached-config))
	  (conf-compress (if (>fx (bigloo-debug) 0)
			   (extend-config config 'compress #f)
			   config))
	  (conf-verbose (if (>fx *hop-verbose* 10)
			    (extend-config conf-compress 'verbose #t)
			    conf-compress))
	  (conf-module (add-suffix-clause conf-verbose))
	  (conf-debug (if (>fx (bigloo-debug) 0)
			  (extend-config conf-module 'debug #t)
			  conf-module)))
      conf-debug))

;*---------------------------------------------------------------------*/
;*    once-expander ...                                                */
;*---------------------------------------------------------------------*/
(define once-expander
   (lambda (x e)
      (e (expand-once x) e)))

;*---------------------------------------------------------------------*/
;*    init-hopscheme! ...                                              */
;*---------------------------------------------------------------------*/
(define (init-hopscheme! #!key reader share path verbose eval hop-compile hop-register features expanders hop-library-path)
   (set! *hop-reader* reader)
   (set! *hop-share-directory* share)
   (set! *hop-verbose* verbose)
   (set! *hop-eval* eval)
   (set! *hop-compile* hop-compile)
   (set! *hop-register* hop-register)
   (set! *hop-library-path* hop-library-path)
   (for-each srfi0-declare! features)
   (for-each (lambda (expd)
		(if (pair? expd)
		    (install-expander! (car expd) (cdr expd))
		    (install-expander! expd once-expander)))
      expanders))
