;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/stage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 07:48:29 2013                          */
;*    Last change :  Mon May 26 16:58:55 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme stage definition and execution                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_stage

   (library hop)
   
   (import __js2scheme_ast)

   (use    __js2scheme_dump)
   
   (export (abstract-class J2SStage
	      (name::bstring read-only)
	      (comment::bstring read-only)
	      (optional::bool read-only (default #f)))

	   (class J2SStageProc::J2SStage
	      (proc::procedure read-only))

	   (class J2SStageUrl::J2SStage
	      (url::bstring read-only))

	   (generic stage-exec ::J2SStage ::J2SProgram ::bstring ::int ::obj)))

;*---------------------------------------------------------------------*/
;*    driver-exec ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (stage-exec stage::J2SStage ast::J2SProgram tmp::bstring count::int args::obj))

;*---------------------------------------------------------------------*/
;*    driver-debug-post ...                                            */
;*---------------------------------------------------------------------*/
(define (driver-debug-post stage tmp ast)
   (with-access::J2SStageProc stage (name proc comment)
      (when (>=fx (bigloo-debug) 1)
	 (call-with-output-file (make-file-path tmp name)
	    (lambda (p)
	       (fprint p ";; " comment)
	       (pp (j2s->list ast) p))))
      ast))

;*---------------------------------------------------------------------*/
;*    driver-exec ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (stage-exec stage::J2SStageProc ast::J2SProgram tmp::bstring count::int args::obj)
   (with-access::J2SStageProc stage (name proc comment)
      (when (>fx (bigloo-debug) 3)
	 (fprintf (current-error-port) "~3d. ~a\n" count name))
      (driver-debug-post stage tmp (proc ast args))))

;*---------------------------------------------------------------------*/
;*    stage-exec ::J2SStageUrl ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage-exec stage::J2SStageUrl ast::J2SProgram tmp::bstring count::int args::obj)
   (with-access::J2SStageUrl stage (url name)
      (when (>fx (bigloo-debug) 3)
	 (fprintf (current-error-port) "~3d. ~a\n" count name))
      (driver-debug-post stage tmp
	 (with-url url string->obj :method 'POST :body (obj->string ast)))))
	 
