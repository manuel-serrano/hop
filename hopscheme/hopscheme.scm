;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/hopscheme/hopscheme.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Wed Feb 17 18:39:39 2010                          */
;*    Last change :  Thu Oct 25 17:17:57 2012 (serrano)                */
;*    Copyright   :  2010-12 Florian Loitsch and Manuel Serrano        */
;*    -------------------------------------------------------------    */
;*    Hopscheme                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    THe module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme
   
   (library scheme2js)
   
   (import __hopscheme_config
	   __hopscheme_dollar-escape
	   __hopscheme_tilde-escape
	   __hopscheme_precompilation
	   __dollar_scheme2js_module
	   __hopscheme_hop_runtime)
   
   (export (hopscheme-compile-module clauses::pair-nil)
	   (hopscheme-compile-file file::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    hopscheme-compile-module ...                                     */
;*    -------------------------------------------------------------    */
;*    Precompiles the given clauses, so they can be used as            */
;*    "module-header" for expressions that are then compiled by        */
;*    HOPSCHEME-COMPILE-EXPRESSION. Clauses should be something like   */
;*    '(import m1), etc.                                               */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-module clauses)
   (list (precompile-headers clauses)))

;*---------------------------------------------------------------------*/
;*    hopscheme-compile-file ...                                       */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-file file env)
   (let ((abase (module-abase)))
      (unwind-protect
	 (with-output-to-string
	    (lambda ()
	       (set-abase! file)
	       (scheme2js-compile-file file              ;; input-files
		  "-"               ;; output-file
		  `(                ;; headers-overrides
		    (merge-first (import ,@(hop-runtime-modules)))
		    ,@env)
		  (extend-config (get-cached-config)
		     'module-resolver
		     (lambda (mod)
			((bigloo-module-resolver)
			 mod (module-abase))))
		  :reader *hop-reader*)))
	 (module-abase-set! abase))))

;*---------------------------------------------------------------------*/
;*    *cached-config* ...                                              */
;*---------------------------------------------------------------------*/
(define *cached-config* #f)

;*---------------------------------------------------------------------*/
;*    get-cached-config ...                                            */
;*---------------------------------------------------------------------*/
(define (get-cached-config)
   ;; no need for locks. in the worst case we create more than one list.
   (when (not *cached-config*)
      (set! *cached-config*
	    (extend-config* (hopscheme-config #t)
			    ;; do an 'eval' on $s and 'eval' new module clause
			    `((hop-module-compilation . #t)))))
   *cached-config*)

;*---------------------------------------------------------------------*/
;*    set-abase! ...                                                   */
;*---------------------------------------------------------------------*/
(define (set-abase! file)
   (let loop ((dir (dirname file)))
      (if (file-exists? (make-file-name dir ".afile"))
	  (module-abase-set! dir)
	  (let ((ndir (dirname dir)))
	     (unless (string=? dir ndir)
		(loop ndir))))))

