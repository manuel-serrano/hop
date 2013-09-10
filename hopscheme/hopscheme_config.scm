;*=====================================================================*/
;*    .../prgm/project/hop/2.5.x/hopscheme/hopscheme_config.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan 15 07:17:18 2012                          */
;*    Last change :  Fri Aug 23 07:16:22 2013 (serrano)                */
;*    Copyright   :  2012-13 Manuel Serrano                            */
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
	   (get-file-cached-config)
	   (init-hopscheme! #!key reader share path verbose eval hop-compile
	      hop-register features expanders hop-library-path
	      (javascript-version "1.5") (tmp-dir (os-tmp))
	      (source-map #f)
	      (arity-check #f)
	      (type-check #f)
	      (meta #f)
	      (debug #f)
	      (inlining #t)
	      (compress (=fx (bigloo-debug) 0))
	      (module-use-strict (<=fx (bigloo-debug) 2))
	      (function-use-strict (<=fx (bigloo-debug) 2)))
	   *hop-reader*
	   *hop-share-directory*
	   *hop-eval*))

;*---------------------------------------------------------------------*/
;*    *hopscheme-config-mutex* ...                                     */
;*---------------------------------------------------------------------*/
(define *hopscheme-config-mutex* (make-mutex))

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
;*    Scheme2js configuration                                          */
;*---------------------------------------------------------------------*/
;; tmp directory
(define *scheme2js-tmp-dir* (os-tmp))

;; generate let (for frames)
(define *scheme2js-javascript-let* #f)

;; add "use strict" declarations
(define *scheme2js-module-use-strict* (<=fx (bigloo-debug) 2))

;; add "use strict" declarations
(define *scheme2js-function-use-strict* (<=fx (bigloo-debug) 2))

;; generate source map
(define *scheme2js-source-map* #f)

;; generate arity check
(define *scheme2js-arity-check* #f)

;; generate type check
(define *scheme2js-type-check* #f)

;; generate meta annotation
(define *scheme2js-meta* #f)

;; generate debugging code
(define *scheme2js-debug* #f)

;; inline function calls
(define *scheme2js-inlining* #t)

;; compress the generated JS file
(define *scheme2js-compress* (=fx (bigloo-debug) 0))

;*---------------------------------------------------------------------*/
;*    get-cached-config ...                                            */
;*---------------------------------------------------------------------*/
(define (get-cached-config)
   (unless *cached-config*
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
	      (compress . ,*scheme2js-compress*)
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
	      ;; strict mode
	      (use-strict/function . ,*scheme2js-function-use-strict*)
	      ;; strict mode
	      (use-strict/module . ,*scheme2js-module-use-strict*)
	      ;; source map generation
	      (source-map . ,*scheme2js-source-map*)
	      ;; javascript1.7 let construct
	      (javascript-let . ,*scheme2js-javascript-let*)
	      ;; tmp-dir
	      (tmp-dir . ,*scheme2js-tmp-dir*)
	      ;; inlining
	      (do-inlining . ,*scheme2js-inlining*)
	      ;; arity-check
	      (arity-check . ,*scheme2js-arity-check*)
	      ;; type-check
	      (type-check . ,*scheme2js-type-check*)
	      ;; meta
	      (meta . ,*scheme2js-meta*)
	      ))))
   *cached-config*)

;*---------------------------------------------------------------------*/
;*    *cached-expression-config* ...                                   */
;*---------------------------------------------------------------------*/
(define *cached-expression-config* #f)

;*---------------------------------------------------------------------*/
;*    get-expression-cached-config ...                                 */
;*---------------------------------------------------------------------*/
(define (get-expression-cached-config)
   (synchronize *hopscheme-config-mutex*
      (unless *cached-expression-config*
	 (set! *cached-expression-config* (get-cached-config)))
      *cached-expression-config*))
   
;*---------------------------------------------------------------------*/
;*    *cached-file-config* ...                                         */
;*---------------------------------------------------------------------*/
(define *cached-file-config* #f)

;*---------------------------------------------------------------------*/
;*    get-file-cached-config ...                                       */
;*---------------------------------------------------------------------*/
(define (get-file-cached-config)
   (synchronize *hopscheme-config-mutex*
      (unless *cached-file-config*
	 (set! *cached-file-config*
	    (extend-config* (hopscheme-config #t)
	       ;; do an 'eval' on $s and 'eval' new module clause
	       `((hop-module-compilation . #t)))))
      *cached-file-config*))

;*---------------------------------------------------------------------*/
;*    hopscheme-config ...                                             */
;*    -------------------------------------------------------------    */
;*    do not reuse scheme2js-configs!                                  */
;*---------------------------------------------------------------------*/
(define (hopscheme-config compile-file?)

   (define (add-suffix-clause conf)
      ;; make sure static variables (not exported, but global) do not clash
      (if compile-file?
	  conf
	  (synchronize *module-counter-lock*
	     (set! *module-counter* (+ *module-counter* 1))
	     (extend-config
		conf
		'statics-suffix
		(string-append "_hopM"
		   (number->string *module-counter*))))))

   (let* ((config (get-expression-cached-config))
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
(define (init-hopscheme! #!key reader share path verbose eval hop-compile
	   hop-register features expanders hop-library-path
	   (javascript-version "1.5") (tmp-dir (os-tmp))
	   (source-map #f)
	   (arity-check #f)
	   (type-check #f)
	   (meta #f)
	   (debug #f)
	   (inlining #t)
	   (compress (=fx (bigloo-debug) 0))
	   (module-use-strict (<=fx (bigloo-debug) 2))
	   (function-use-strict (<=fx (bigloo-debug) 2)))
   (set! *hop-reader* reader)
   (set! *hop-share-directory* share)
   (set! *hop-verbose* verbose)
   (set! *hop-eval* eval)
   (set! *hop-compile* hop-compile)
   (set! *hop-register* hop-register)
   (set! *hop-library-path* hop-library-path)
   (set! *scheme2js-javascript-let*
      (>= (string-natural-compare3 javascript-version "1.7") 0))
   (set! *scheme2js-tmp-dir* tmp-dir)
   (set! *scheme2js-source-map* source-map)
   (set! *scheme2js-debug* debug)
   (set! *scheme2js-arity-check* arity-check)
   (set! *scheme2js-type-check* type-check)
   (set! *scheme2js-meta* meta)
   (set! *scheme2js-inlining* inlining)
   (set! *scheme2js-compress* compress)
   (set! *scheme2js-module-use-strict* module-use-strict)
   (set! *scheme2js-function-use-strict* function-use-strict)
   (for-each srfi0-declare! features)
   (for-each (lambda (expd)
		(if (pair? expd)
		    (install-expander! (car expd) (cdr expd))
		    (install-expander! expd once-expander)))
      expanders))

