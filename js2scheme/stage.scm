;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/stage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 07:48:29 2013                          */
;*    Last change :  Fri Feb  3 15:31:13 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	      (optional::bool read-only (default #f))
	      (before read-only (default #f))
	      (after read-only (default #f)))

	   (class J2SStageProc::J2SStage
	      (proc::procedure read-only))

	   (class J2SStageUrl::J2SStage
	      (url::bstring read-only))

	   (class J2SStageFile::J2SStage
	      (path::bstring read-only))

	   (generic stage-exec ::J2SStage ::J2SProgram ::bstring ::int ::obj)))

;*---------------------------------------------------------------------*/
;*    driver-exec ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (stage-exec stage::J2SStage ast::J2SProgram tmp::bstring count::int args::obj))

;*---------------------------------------------------------------------*/
;*    driver-debug-post ...                                            */
;*---------------------------------------------------------------------*/
(define (driver-debug-post stage tmp count ast args proc)

   (define (j2s-verbose)
      (let ((l (memq :verbose args)))
	 (if (pair? l)
	     (cadr l)
	     (hop-verbose))))
   
   (with-access::J2SStage stage (name comment before after)
      (when (>=fx (j2s-verbose) 2)
	 (fprintf (current-error-port) "~3d. ~a" count name))
      (when (procedure? before) (before ast))
      (let ((nast (proc ast args)))
	 (when (directory? tmp)
	    (let ((file (make-file-path tmp
			   (string-replace name (file-separator) #\_))))
	       (cond
		  ((>=fx (bigloo-debug) 1)
		   (call-with-output-file file
		      
		      (lambda (p)
			 (fprint p ";; " comment)
			 (pp (j2s->list nast) p))))
		  ((file-exists? file)
		   (delete-file file)))))
	 (when (procedure? after) (after nast))
	 (when (>=fx (j2s-verbose) 2) (newline (current-error-port)))
	 nast)))

;*---------------------------------------------------------------------*/
;*    driver-exec ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (stage-exec stage::J2SStageProc ast::J2SProgram tmp::bstring count::int args::obj)
   (with-access::J2SStageProc stage (proc)
      (driver-debug-post stage tmp count ast args proc)))

;*---------------------------------------------------------------------*/
;*    stage-exec ::J2SStageUrl ...                                     */
;*---------------------------------------------------------------------*/
(define-method (stage-exec stage::J2SStageUrl ast::J2SProgram tmp::bstring count::int args::obj)
   (with-access::J2SStageUrl stage (url)
      (driver-debug-post stage tmp count ast args
	 (lambda (ast args)
	    (call-with-output-file "/tmp/AST" 
	       (lambda (op)
		  (ast->json ast op)))
	    (with-url (string-append url "?hop-encoding=json")
	       (lambda (ast) ast)
	       :method 'POST
	       :header '((content-type: . "application/json"))
	       :connection 'close
	       :json-parser (lambda (ip ctx) (json->ast ip))
	       :body (call-with-output-string
			(lambda (op)
			   (ast->json ast op))))))))
	 
;*---------------------------------------------------------------------*/
;*    stage-exec ::J2SStageFile ...                                    */
;*---------------------------------------------------------------------*/
(define-method (stage-exec stage::J2SStageFile ast::J2SProgram tmp::bstring count args::obj)
   (with-access::J2SStageFile stage (path)
      (driver-debug-post stage tmp count ast args
	 (lambda (ast args)
	    (cond
	       ((string-suffix? ".hop" path)
		(let ((stage (hop-load path)))
		   (if (isa? stage J2SStage)
		       (stage-exec stage ast tmp count args)
		       (error "j2scheme" "Illegal plugin file" path))))
	       (else
		(error "j2sscheme" "Illegal plugin file" path)))))))
