;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/stdlib.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 13:49:02 2013                          */
;*    Last change :  Fri Aug  9 13:50:44 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme2js standard library                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module stdlib

   (include "mapping.sch")
   
   (import export-desc
	   tools)

   (cond-expand
      (enable-callcc
       (export *call/cc-runtime-var-mapping*
	       *call/cc-constant-runtime-var-mapping*)))

   (export *default-runtime-var-mapping*
	   *default-constant-runtime-var-mapping*

	   (add-extra-mapping! ::bstring)))

;*---------------------------------------------------------------------*/
;*    *default-constant-runtime-var-mapping* ...                       */
;*---------------------------------------------------------------------*/
(define *default-constant-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (e)
		   (let ((desc (create-Export-Desc e #f #t)))
		      (with-access::Export-Desc desc (id)
			 (hashtable-put! ht id desc))))
	 (get-exports "runtime/runtime.sch"))
      ht))

;*---------------------------------------------------------------------*/
;*    *call/cc-constant-runtime-var-mapping* ...                       */
;*---------------------------------------------------------------------*/
(define *call/cc-constant-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (e)
		   (let ((desc (create-Export-Desc e #f #t)))
		      (with-access::Export-Desc desc (id)
			 (hashtable-put! ht id desc))))
	 (get-exports "runtime/runtime-callcc.sch"))
      ht))

;*---------------------------------------------------------------------*/
;*    *default-runtime-var-mapping* ...                                */
;*---------------------------------------------------------------------*/
(define *default-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (e)
		   (let ((desc (create-Export-Desc e #f #t)))
		      (with-access::Export-Desc desc (id)
			 (hashtable-put! ht id desc))))
	 (get-exports "runtime/mod-runtime.sch"))
      ht))

;*---------------------------------------------------------------------*/
;*    *call/cc-runtime-var-mapping* ...                                */
;*---------------------------------------------------------------------*/
(define *call/cc-runtime-var-mapping*
   (let ((ht (make-eq-hashtable)))
      (for-each (lambda (e)
		   (let ((desc (create-Export-Desc e #f #t)))
		      (with-access::Export-Desc desc (id)
			 (hashtable-put! ht id desc))))
	 (get-exports "runtime/mod-runtime-callcc.sch"))
      ht))
	   
;*---------------------------------------------------------------------*/
;*    add-extra-mapping! ...                                           */
;*---------------------------------------------------------------------*/
(define (add-extra-mapping! file)
   (when (file-exists? file)
      (let* ((m-clause (with-input-from-file file read))
	     (a-list (cddr m-clause))
	     (exports (assq 'export a-list)))
	 (for-each (lambda (e)
		      (let ((desc (create-Export-Desc e #f #t)))
			 (with-access::Export-Desc desc (id)
			    (hashtable-put! *default-constant-runtime-var-mapping*
			       id desc)
			    (hashtable-put! *call/cc-constant-runtime-var-mapping*
			       id desc)
			    (hashtable-put! *default-runtime-var-mapping*
			       id desc)
			    (hashtable-put! *call/cc-runtime-var-mapping*
			       id desc))))
	    (cdr exports)))))
