;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/stage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 07:48:29 2013                          */
;*    Last change :  Mon Apr 14 13:30:13 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme stage definition and execution                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_stage

   (import __js2scheme_ast)

   (use    __js2scheme_dump)
   
   (export (class J2SStage
	      (name::bstring read-only)
	      (comment::bstring read-only)
	      (proc::procedure read-only)
	      (optional::bool read-only (default #f)))

	   (generic stage-exec ::J2SStage ::J2SProgram ::bstring ::int ::obj)))

;*---------------------------------------------------------------------*/
;*    driver-exec ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (stage-exec stage::J2SStage ast::J2SProgram tmp::bstring count::int args::obj)
   (with-access::J2SStage stage (name proc comment)
      (when (>fx (bigloo-debug) 3)
	 (fprintf (current-error-port) "~3d. ~a\n" count name))
      (let ((ast (proc ast args)))
	 (when (>=fx (bigloo-debug) 1)
	    (call-with-output-file (make-file-path tmp name)
	       (lambda (p)
		  (fprint p ";; " comment)
		  (pp (j2s->list ast) p))))
	 ast)))
