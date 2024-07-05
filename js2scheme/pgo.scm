;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/pgo.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  5 07:51:42 2024                          */
;*    Last change :  Fri Jul  5 09:41:56 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Load profiling log for the PGO optimizations.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_pgo

   (library web)
   
   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)
   
   (export j2s-pgo-stage))

;*---------------------------------------------------------------------*/
;*    j2s-pgo-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-pgo-stage
   (instantiate::J2SStageProc
      (name "pgo")
      (comment "Profile Guided Optimization table load")
      (proc j2s-pgo)
      (optional :profile-log)))

;*---------------------------------------------------------------------*/
;*    pgo-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define (pgo-verb conf . args)
   (when (>=fx (config-get conf :verbose 0) 3)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (display "\n      ")
	    (for-each display args)))))

;*---------------------------------------------------------------------*/
;*    j2s-pgo ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-pgo this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (profiling path)
	 (let* ((log (load-profile-log conf))
		(file (config-get conf :filename))
		(path (cond
			 ((not (string? path))
			  file)
			 ((char=? (string-ref path 0) (file-separator))
			  path)
			 (else
			  (file-name-unix-canonicalize
			     (make-file-name (pwd) path))))))
	    (set! profiling
	       (filter (lambda (x) x)
		  (list
		     (get-log log 'caches file path)
		     (get-log log 'ctors file path)))))))
   this)

;*---------------------------------------------------------------------*/
;*    get-log ...                                                      */
;*---------------------------------------------------------------------*/
(define (get-log log key file path)
   
   (define (get key lst)
      (let ((c (assq key lst)))
	 (when (pair? c) (cdr c))))

   (define (point e)
      (if (and (pair? e) (pair? (car e)) (eq? (caar e) 'point))
	  (cdr (car e))
	  (error "pgo" "bad entry" e)))
   
   (define (sort-point vec)
      (sort (lambda (x y)
	       (<=fx (point x) (point y)))
	 vec))

   (let ((vec (get key log)))
      (when (vector? vec)
	  (let loop ((i (-fx (vector-length vec) 1)))
	     (when (>=fx i 0)
		(let ((filename (get 'filename (vector-ref vec i))))
		   (if (or (string=? file filename) (string=? path filename))
		       (cons key (sort-point (get key (vector-ref vec i))))
		       (loop (-fx i 1)))))))))
		    
;*---------------------------------------------------------------------*/
;*    load-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (load-profile-log conf)
   (let ((logfile (config-get conf :profile-log #f)))
      (pgo-verb conf "loading log file " (string-append "\"" logfile "\""))
      (call-with-input-file logfile
	 (lambda (ip)
	    (let ((pgo #f))
	       (json-parse ip
		  :array-alloc (lambda () (make-cell '()))
		  :array-set (lambda (a i val)
				(cell-set! a (cons val (cell-ref a))))
		  :array-return (lambda (a i)
				   (list->vector (reverse! (cell-ref a))))
		  :object-alloc (lambda ()
				   (make-cell '()))
		  :object-set (lambda (o p val)
				 (cond
				    ((string=? p "format")
				     (set! pgo (equal? val "pgo")))
				    (pgo
				     (cell-set! o
					(cons (cons (string->symbol p) val)
					   (cell-ref o))))
				    (else
				     (error "pgo" "Bad log format" logfile))))
		  :object-return (lambda (o)
				    (reverse! (cell-ref o)))
		  :parse-error (lambda (msg fname loc)
				  (error/location "pgo" "syntax error" msg
				     fname loc))))))))

