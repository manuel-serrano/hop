;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/stage.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 29 07:48:29 2013                          */
;*    Last change :  Tue Jun 20 08:38:33 2023 (serrano)                */
;*    Copyright   :  2013-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    js2scheme stage definition and execution                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_stage

   (library hop)
   
   (import __js2scheme_ast
	   __js2scheme_utils)

   (use    __js2scheme_dump)
   
   (export (abstract-class J2SStage
	      (name::bstring read-only)
	      (footer::bstring read-only (default "\n"))
	      (comment::bstring read-only)
	      (optional read-only (default #f))
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

   (define (active? stage)
      (with-access::J2SStage stage (optional)
	 (let loop ((opt optional))
	    (or (not opt)
		(and (keyword? opt) (config-get args opt #f))
		(and (integer? opt) (>= (config-get args :optim 0) opt))
		(and (procedure? opt) (opt args))
		(and (eq? opt #t) (error "stage" "bad opt" stage))
		(and (pair? opt) (any loop opt))))))

   (with-access::J2SStage stage (name footer comment before after)
      (if (active? stage)
	  (begin
	     (when (>=fx (j2s-verbose) 2)
		(fprintf (current-error-port) "~a~3d. ~a"
		   (config-get args :verbmargin "") count name))
	     (when (procedure? before) (before ast))
	     (let ((nast (proc ast args)))
		(when (directory? tmp)
		   (let ((file (make-file-path tmp
				  (string-replace name (file-separator) #\_))))
		      (cond
			 ((config-get args :debug-stage)
			  (if (eq? (config-get args :debug-stage-format) 'json)
			      (call-with-output-file (string-append file ".json")
				 (lambda (op)
				    (ast->json ast op)))
			      (call-with-output-file file
				 (lambda (p)
				    (fprint p ";; -*-bee-*-")
				    (fprint p ";; " comment)
				    (pp/width (j2s->sexp nast) p)))))
			 ((file-exists? file)
			  (delete-file file)))))
		(when (procedure? after) (after nast))
		(when (>=fx (j2s-verbose) 2)
		   (display footer (current-error-port)))
		(values nast #t)))
	  (values ast #f))))

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
   
   (define (value->json-value v)
      (cond
	 ((eq? v #t) "true")
	 ((eq? v #f) "false")
	 ((number? v) v)
	 ((string? v) (format "~s" v))
	 ((symbol? v) (format "~s" (symbol->string v)))))
	 
   (with-access::J2SStageUrl stage (url)
      (driver-debug-post stage tmp count ast args
	 (lambda (ast args)
	    (with-url (string-append url "?hop-encoding=json")
	       (lambda (ast) ast)
	       :method 'POST
	       :header '((content-type: . "application/json")
			 (hop-serialize: . "json"))
	       :connection 'close
	       :json-parser (lambda (ip ctx)
			       (json->ast ip))
	       :x-javascript-parser (lambda (ip ctx)
				       (error "url-stage"
					  "Bad response mime-type (should be application/json)"
					  #f))
	       :body (call-with-output-string
			(lambda (op)
			   (display "{ \"ast\": " op)
			   (ast->json ast op)
			   (display ", \"config\": {" op)
			   (let loop ((args args))
			      (when (pair? args)
				 (let ((v (value->json-value (cadr args))))
				    (when v
				       (fprintf op "\"~a\": ~a" (car args) v))
				    (when (pair? (cddr args))
				       (when v
                                          (display ", " op))
				       (loop (cddr args))))))
			   (display "}}" op))))))))
	 
;*---------------------------------------------------------------------*/
;*    stage-exec ::J2SStageFile ...                                    */
;*---------------------------------------------------------------------*/
(define-method (stage-exec stage::J2SStageFile ast::J2SProgram tmp::bstring count args::obj)
   (with-access::J2SStageFile stage (path)
      (cond
	 ((string-suffix? ".hop" path)
	  (let ((stage (hop-load path)))
	     (if (isa? stage J2SStage)
		 (stage-exec stage ast tmp count args)
		 (error "js2scheme" "Illegal plugin file" path))))
	 ((or (string-suffix? ".js" path)
	      (string-suffix? ".ts" path))
	  (error "js2scheme" "JS plugin file not implemented" path))
	 (else
	  (error "js2scheme" "Illegal plugin file" path)))))
