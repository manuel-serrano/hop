;*=====================================================================*/
;*    serrano/prgm/project/hop/1.11.x/runtime/hop-wiki.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 07:37:32 2006                          */
;*    Last change :  Tue Feb 24 12:53:47 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The wiki markup                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki

   (include "xml.sch")

   (import  __hop_param
	    __hop_xml
	    __hop_read
	    __hop_cache
	    __hop_wiki-syntax
	    __hop_wiki-parser)

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
(define (wiki-cache->hop wiki-cache src syntax charset)
   (let ((cache (cache-get wiki-cache src)))
      (if (string? cache)
	  (with-input-from-file cache
	     (lambda ()
		(read-string)))
	  (let* ((wiki (wiki-file->hop src :syntax syntax :charset charset))
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
			     (charset (hop-locale))
			     body)
   (let ((syn (if (eq? syntax #unspecified)
		  #f
		  syntax)))
      (cond
	 ((eq? src #unspecified)
	  (wiki-string->hop (flatten body) :syntax syn :charset charset))
	 ((not (string? src))
	  (error '<WIKI> "Illegal wiki src" src))
	 ((file-exists? src)
	  (if (cache? cache)
	      (wiki-cache->hop cache src syn charset)
	      (wiki-file->hop src :syntax syn :charset charset))))))
