;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/prefs-expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 28 07:04:20 2006                          */
;*    Last change :  Tue Apr 22 14:19:57 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The definition of the DEFINE-PREFERENCES macro.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-preferences ...                                           */
;*    -------------------------------------------------------------    */
;*    This generates three functions: id-load, id-save, and id-edit.   */
;*---------------------------------------------------------------------*/
(define (hop-define-prefs-expander x e)
   
   (define (make-load id)
      (let ((mod (eval-module)))
	 `(define (,id file)
	     (with-lock (preferences-mutex)
		(lambda ()
		   ,(if (evmodule? mod)
			`(hop-load file
		            :env (eval-find-module ',(evmodule-name mod)))
			`(hop-load file)))))))
   
   (define (make-save-clause c)
      (match-case c
	 ((?- ?type (and (? symbol?) ?param))
	  `(begin
	      ,(if (eq? type 'expr)
		   `(write (list ',(symbol-append param '-set!)
				 (list 'quote (,param))))
		   `(write (list ',(symbol-append param '-set!) (,param))))
	      (newline)))
	 ((or (? string?) (? symbol?))
	  #unspecified)
	 (((? string?) ?-)
	  #unspecified)
	 (else
	  (error 'define-preferences "Illegal clause" c))))

   (define sig ";; hop-sig: ")
   
   (define (make-save id key clauses)
      
      `(begin
	  ;; generate the save procedure
	  (define (,id file #!optional force-override)
	     
	     (define (save file)
		(let* ((str (with-output-to-string
			       (lambda ()
				  ,@(map make-save-clause clauses))))
		       (sum (md5sum str)))
		   (with-lock (preferences-mutex)
		      (lambda ()
			 (with-output-to-file file
			    (lambda ()
			       (display ,sig)
			       (print sum)
			       (display str))))))
		file)
	     
	     (define (signed? file)
		(when (file-exists? file)
		   (with-input-from-file file
		      (lambda ()
			 (let ((l (read-line)))
			    (when (substring-at? l ,sig 0)
			       (let ((sum (substring l
						     ,(string-length sig)
						     (string-length l)))
				     (rest (read-string)))
				  (string=? sum (md5sum rest)))))))))

	     (cond
		((or (not (file-exists? file)) (signed? file))
		 (save file))
		(force-override
		 (rename-file file (string-append file ".hopsave"))
		 (save file))
		(else
		 #f)))
	  ;; register the save procedure
	  (preferences-register-save! ,key ,id)))
   
   (define (make-edit-clause c)
      (match-case c
	 (((and (? string?) ?lbl) ?type (and (? symbol?) ?param))
	  (let ((get param)
		(set (symbol-append param '-set!)))
	     `(<PR> :param (list ',param ,get ,set) :type ',type ,lbl)))
	 ((? string?)
	  `(<PRLABEL> ,c))
	 (--
	  '(<PRSEP>))
	 (else
	  (error 'define-preferences "Illegal clause" c))))
   
   (define (make-edit id key clauses)
      `(define (,id #!key id)
	  (<PREFS> :id (xml-make-id id 'preferences) :lang ,key
	     ,@(map make-edit-clause clauses))))
   
   (match-case x
      ((?- (and (? symbol?) ?id) . ?clauses)
       (let* ((key (symbol->string (gensym id)))
	      (body `(begin
			,(make-load (symbol-append id '-load))
			,(make-save (symbol-append id '-save) key clauses)
			,(make-edit (symbol-append id '-edit) key clauses))))
	  (e (evepairify body x) e)))
      (else
       (error 'define-preferences "Illegal form" x))))

