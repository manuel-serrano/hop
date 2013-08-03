;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/share/hop-debug.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 29 14:46:23 2013                          */
;*    Last change :  Fri Aug  2 16:18:27 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop runtime debugging support                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-debug

   (include "../scheme2js/base64_vlq.sch")

   (import __hop-exception)

   (export (hop-source-mapping-url file url))
   
   (scheme2js-pragma
      (hop-source-mapping-url (JS "hop_source_mapping_url"))))

;*---------------------------------------------------------------------*/
;*    source-file-table ...                                            */
;*---------------------------------------------------------------------*/
(define source-file-table
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    source-map-table ...                                             */
;*---------------------------------------------------------------------*/
(define source-map-table
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    hop-source-mapping-url ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-source-mapping-url file url)
   (hashtable-put! source-file-table file url))

;*---------------------------------------------------------------------*/
;*    get-source-map ...                                               */
;*---------------------------------------------------------------------*/
(define (get-source-map url)
   (or (hashtable-get source-map-table url)
       (with-hop ($(service :name "public/server-debug/source-map" (file)) url)
	  :sync #t
	  :anim #f
	  (lambda (h)
	     (hashtable-put! source-map-table url h)
	     h))))

;*---------------------------------------------------------------------*/
;*    source-map-decode-segment ...                                    */
;*---------------------------------------------------------------------*/
(define (source-map-decode-segment mappings::bstring i::int)

   (define (separator? i)
      (let ((c (string-ref mappings i)))
	 (or (char=? c #\;) (char=? c #\,))))

   (let ((len (string-length mappings)))
      (cond
	 ((or (>=fx i len) (char=? (string-ref mappings i) #\;))
	  ;; a newline
	  (values 'newline (+fx i 1) 0 0 0 0 0))
	 ((char=? (string-ref mappings i) #\,)
	  ;; a new segment
	  (source-map-decode-segment mappings (+fx i 1)))
	 (else
	  (multiple-value-bind (dstcol ni)
	     (base64-vlq-decode mappings i)
	     (if (or (=fx ni len) (separator? ni))
		 ;; only one value provided
		 (values ni 1 dstcol 0 0 0 0)
		 ;; 4 or 5 values expected
		 (multiple-value-bind (file ni)
		    (base64-vlq-decode mappings ni)
		    (multiple-value-bind (srcline ni)
		       (base64-vlq-decode mappings ni)
		       (multiple-value-bind (srccol ni)
			  (base64-vlq-decode mappings ni)
			  (if (or (=fx ni len) (separator? ni))
			      ;; 4 values segment
			      (values 4 ni dstcol file srcline srccol 0)
			      (multiple-value-bind (name ni)
				 (base64-vlq-decode mappings ni)
				 ;; 5 values segment
				 (values 5 ni dstcol file srcline srccol name))))))))))))

;*---------------------------------------------------------------------*/
;*    hop-debug-source-map-file/smap ...                               */
;*---------------------------------------------------------------------*/
(define (hop-debug-source-map-file/smap smap file line col)
   
   (define (get-line smap index line col)
      (values (vector-ref smap.sources index) line col))
   
   (let ((mappings smap.mappings)
	 (zbline (-fx line 1))
	 (zbcol (-fx col 1)))
      (let loop ((i 0)
		 (l zbline)
		 (pdstcol 0)
		 (pfindex 0)
		 (psrcline 0)
		 (psrccol 0)
		 (pname 0))
	 (multiple-value-bind (status ni dstcol findex srcline srccol name)
	    (source-map-decode-segment mappings i)
	    (if (eq? status 'newline)
		(if (=fx l 0)
		    (get-line smap pfindex psrcline psrccol)
		    ;; plain newline
		    (loop ni (-fx l 1) 0 pfindex psrcline psrccol pname))
		(let ((ndstcol (+fx dstcol pdstcol))
		      (nfindex (+fx findex pfindex))
		      (nsrcline (+fx srcline psrcline))
		      (nsrccol (+fx srccol psrccol))
		      (nname (+fx name pname)))
		   (cond
		      ((>fx l 0)
		       ;; skip the segment
		       (loop ni l ndstcol nfindex nsrcline nsrccol nname))
		      ((=fx ndstcol zbcol)
		       ;; got the line, find the best column segment
		       (get-line smap nfindex nsrcline nsrccol))
		      ((>fx ndstcol zbcol)
		       ;; got the line, find the best column segment
		       (get-line smap pfindex psrcline psrccol))
		      (else
		       (loop ni 0 ndstcol nfindex nsrcline nsrccol nname)))))))))

;*---------------------------------------------------------------------*/
;*    hop-debug-source-map-file ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-debug-source-map-file file line col)
   (let* ((i (string-index file #\?))
          (name (if i (substring file 0 i) file))
          (url (hashtable-get source-file-table name)))
      (if url
	  (let ((smap (get-source-map url)))
	     (if smap
		 (hop-debug-source-map-file/smap smap file line col)
		 (values #f #f #f)))
	  (values #f #f #f))))

;*---------------------------------------------------------------------*/
;*    hop-source-map-register! ...                                     */
;*---------------------------------------------------------------------*/
(hop-source-map-register! hop-debug-source-map-file)
