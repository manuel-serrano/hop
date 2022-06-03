;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopc/hop.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  3 11:52:52 2022                          */
;*    Last change :  Fri Jun  3 15:34:40 2022 (serrano)                */
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
   (call-with-input-file file
      (lambda (in)
	 (let ((exp (read in)))
	    (match-case exp
	       ((module ?name . ?clauses)
		(append-map (lambda (c) (find-module-imports name c)) clauses))
	       (else
		(error/source "hop-module-imports" "wrong module" file exp)))))))

;*---------------------------------------------------------------------*/
;*    find-module-imports ...                                          */
;*---------------------------------------------------------------------*/
(define (find-module-imports module clause)
   
   (define (find-module-in-path import name)
      (let ((name (symbol->string name)))
	 (let ((hop (string-append name ".hop")))
	    (if (file-exists? hop)
		hop
		(let ((scm (string-append name ".scm")))
		   (if (file-exists? scm)
		       scm
		       (error/source "hopc"
			  "Cannot find module" name import)))))))

   (match-case clause
      (((or import from) . ?imports)
       (map (lambda (import)
	       (match-case import
		  ((?name ?file) file)
		  ((? symbol?) (find-module-in-path clause import))
		  (else (error/source "hopc" "Illegal import"
			   import clause))))
	  imports))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    hop-module-rebase ...                                            */
;*    -------------------------------------------------------------    */
;*    Rebase explicitly imported modules.                              */
;*---------------------------------------------------------------------*/
(define (hop-module-rebase clause src dst)
   
   (define (absolute? file)
      (when (>fx (string-length file) 0)
	 (char=? (string-ref file 0) runtime-file-separator)))
   
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
