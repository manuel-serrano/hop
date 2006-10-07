;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-wiki.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 07:37:32 2006                          */
;*    Last change :  Fri Oct  6 08:47:26 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The wiki markup                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki

   (include "compiler-macro.sch"
	    "xml.sch")

   (import  __hop_param
	    __hop_xml
	    __hop_read
	    __hop_cache
	    __hop_wiki-syntax)

   (export  (<WIKI> . ::obj)))

;*---------------------------------------------------------------------*/
;*    flatten ...                                                      */
;*---------------------------------------------------------------------*/
(define (flatten obj)
   (cond
      ((string? obj)
       obj)
      ((and (pair? obj) (every? string? obj))
       (apply string-append obj))
      ((pair? (car obj))
       (flatten (cons (flatten (car obj)) (cdr obj))))
      ((string? (car obj))
       (string-append (car obj) (flatten (cdr obj))))
      (else
       (error '<WIKI> "Illegal body" obj))))
       
;*---------------------------------------------------------------------*/
;*    wiki-cache->hop ...                                              */
;*---------------------------------------------------------------------*/
(define (wiki-cache->hop wiki-cache src syntax)
   (let ((cache (cache-get wiki-cache src)))
      (if (string? cache)
	  (with-input-from-file cache
	     (lambda ()
		(read-string)))
	  (let* ((wiki (wiki-file->hop src syntax))
		 (cache (cache-put! wiki-cache src wiki)))
	     (if (string? cache)
		 (with-input-from-file cache
		    (lambda () (read-string)))
		 wiki)))))
					   
;*---------------------------------------------------------------------*/
;*    <WIKI> ...                                                       */
;*---------------------------------------------------------------------*/
(define-xml-compound <WIKI> ((src #unspecified string)
			     (syntax #unspecified wiki-syntax)
			     (cache #unspecified cache)
			     body)
   (let ((syn (if (eq? syntax #unspecified)
		  #f
		  syntax)))
      (cond
	 ((eq? src #unspecified)
	  (wiki-string->hop (flatten body) syn))
	 ((not (string? src))
	  (error '<WIKI> "Illegal wiki src" src))
	 ((file-exists? src)
	  (if (cache? cache)
	      (wiki-cache->hop cache src syn)
	      (wiki-file->hop src syn))))))
