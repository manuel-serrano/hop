;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/hop_wiki.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 07:37:32 2006                          */
;*    Last change :  Sat Dec 10 09:24:27 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The wiki markup                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki

   (library web)

   (include "xml.sch")

   (import  __hop_param
	    __hop_xml-types
	    __hop_xml
	    __hop_read
	    __hop_cache
	    __hop_wiki-syntax
	    __hop_wiki-parser)

   (export  (<WIKI> . ::obj)))

;*---------------------------------------------------------------------*/
;*    flatten ...                                                      */
;*---------------------------------------------------------------------*/
(define (flatten obj env i id)
   (cond
      ((string? obj)
       (values obj env i))
      ((or (boolean? obj) (eq? obj #unspecified) (null? obj))
       (values "" env i))
      ((not (pair? obj))
       (values (format ",(vector-ref ~a ~a)" id i) (cons obj env) (+fx i 1)))
      ((and (pair? obj) (every? string? obj))
       (values (html-string-decode (apply string-append obj)) env i))
      (else
       (multiple-value-bind (str env i)
	  (flatten (car obj) env i id)
	  (multiple-value-bind (str2 env2 i2)
	     (flatten (cdr obj) env i id)
	     (values (string-append (html-string-decode str)
			(html-string-decode str2)) env2 i2))))))
       
;*---------------------------------------------------------------------*/
;*    wiki-cache->hop ...                                              */
;*---------------------------------------------------------------------*/
(define (wiki-cache->hop wiki-cache src syntax charset)
   (let ((ce (cache-get wiki-cache src)))
      (if (isa? cache cache-entry)
	  (with-access::cache-entry ce (value)
	     (with-input-from-file value read-string))
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
			     (charset (hop-charset))
			     body)
   (let ((syn (if (eq? syntax #unspecified)
		  #f
		  syntax))
	 (id (gensym)))
      (cond
	 ((eq? src #unspecified)
	  (multiple-value-bind (str env i)
	     (flatten body '() 0 id)
	     (let ((venv `(,id ',(list->vector (reverse! env)))))
		(wiki-string->hop str :syntax syn :charset charset :env venv))))
	 ((not (string? src))
	  (error "<WIKI>" "Illegal wiki src" src))
	 ((file-exists? src)
	  (if (isa? cache cache)
	      (wiki-cache->hop cache src syn charset)
	      (wiki-file->hop src :syntax syn :charset charset))))))
