;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopc/hop.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  3 11:52:52 2022                          */
;*    Last change :  Mon Jun  6 06:28:36 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop specific utilities                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_hop

   (export (hop-module-imports file)
	   (hop-module-rebase module src dst)))

;*---------------------------------------------------------------------*/
;*    hop-module-imports ...                                           */
;*    -------------------------------------------------------------    */
;*    Find (recursively) imported hop modules                          */
;*---------------------------------------------------------------------*/
(define (hop-module-imports file)
   (module-imports file '()))

;*---------------------------------------------------------------------*/
;*    module-imports ...                                               */
;*---------------------------------------------------------------------*/
(define (module-imports file stack)
   (call-with-input-file file
      (lambda (in)
	 (let ((exp (read in))
	       (dir (dirname (file-name-canonicalize file))))
	    (match-case exp
	       ((module ?name . ?clauses)
		(let loop ((clauses clauses)
			   (stack stack))
		   (if (null? clauses)
		       stack
		       (loop (cdr clauses)
			  (imports (car clauses) dir stack)))))
	       (else
		(error/source "hop-module-imports" "wrong module"
		   file exp)))))))

;*---------------------------------------------------------------------*/
;*    imports ...                                                      */
;*---------------------------------------------------------------------*/
(define (imports clause dir stack)
   
   (define (find-module-in-path import name)
      (let ((file (symbol->string name)))
	 (let ((hop (string-append file ".hop")))
	    (if (file-exists? hop)
		hop
		(let ((scm (string-append file ".scm")))
		   (cond
		      ((file-exists? scm)
		       scm)
		      (((bigloo-module-resolver) name '() dir)
		       =>
		       (lambda (f) (car f)))
		      (else
		       (error/source "hopc"
			  "Cannot find module" name import))))))))

   (define (rebase file dir)
      (if (absolute? file)
	  file
	  (file-name-canonicalize (make-file-name dir file))))

   (match-case clause
      (((or import from) . ?imports)
       (let loop ((imports imports)
		  (stack stack))
	  (if (null? imports)
	      stack
	      (match-case (car imports)
		 ((?name ?file)
		  (let ((path (rebase file dir)))
		     (if (member path stack)
			 stack
			 (loop (cdr imports)
			    (module-imports path (cons path stack))))))
		 ((? symbol?)
		  (let* ((file (find-module-in-path clause (car imports)))
			 (path (rebase file dir)))
		     (if (member path stack)
			 stack
			 (loop (cdr imports)
			    (module-imports path (cons path stack))))))
		 (else
		  (error/source "hopc" "Illegal import"
		     (car imports) clause))))))
      (else
       stack)))

;*---------------------------------------------------------------------*/
;*    absolute? ...                                                    */
;*---------------------------------------------------------------------*/
(define (absolute? file)
   (when (>fx (string-length file) 0)
      (char=? (string-ref file 0) runtime-file-separator)))

;*---------------------------------------------------------------------*/
;*    hop-module-rebase ...                                            */
;*    -------------------------------------------------------------    */
;*    Rebase explicitly imported modules.                              */
;*---------------------------------------------------------------------*/
(define (hop-module-rebase clause src dst)
   
   (define (rebase import abase)
      (match-case import
	 ((?name (and (? string?) ?file))
	  (if (absolute? file)
	      import
	      (list name (make-file-name (dirname src) file))))
	 (else
	  (cond
	     ((not abase)
	      import)
	     (((bigloo-module-resolver) import '() abase)
	      =>
	      (lambda (files)
		 (cons import files)))
	     (else
	      import)))))

   (let* ((afile (module-load-access-file (dirname src)))
	  (abase (when (string? afile) (dirname afile))))
      (match-case clause
	 ((import . ?imports)
	  `(import ,@(map (lambda (i) (rebase i abase)) imports)))
	 ((from . ?imports)
	  `(from ,@(map (lambda (i) (rebase i abase)) imports)))
	 (else
	  clause))))
