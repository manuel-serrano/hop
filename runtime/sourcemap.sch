;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/sourcemap.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 10 05:48:08 2013                          */
;*    Last change :  Fri Jul 11 16:11:05 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Source map facilities shared by servers and clients.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    source-map-decode-segment ...                                    */
;*---------------------------------------------------------------------*/
(define (source-map-decode-segment mappings::bstring i::int)

   (define (separator? i)
      (let ((c (string-ref mappings i)))
	 (or (char=? c #\;) (char=? c #\,))))

   (let luup ((i i))
      (let ((len (string-length mappings)))
	 (cond
	    ((or (>=fx i len) (char=? (string-ref mappings i) #\;))
	     ;; a newline
	     (values 'newline (+fx i 1) 0 0 0 0 0))
	    ((char=? (string-ref mappings i) #\,)
	     ;; a new segment
	     (luup (+fx i 1)))
	    (else
	     (multiple-value-bind (dstcol ni)
		(base64-vlq-decode mappings i)
		(if (or (=fx ni len) (separator? ni))
		    ;; only one value provided
		    (values 1 ni dstcol 0 0 0 0)
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
				    (values 5 ni dstcol file srcline srccol name)))))))))))))

;*---------------------------------------------------------------------*/
;*    hop-debug-source-map-file/smap ...                               */
;*---------------------------------------------------------------------*/
(define (hop-debug-source-map-file/smap mappings sources file line col)
   
   (define (get-line sources index line col)
      (values (vector-ref sources index) line col))

   (let ((zbline (-fx line 1))
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
		    (get-line sources pfindex psrcline psrccol)
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
		       (get-line sources nfindex nsrcline nsrccol))
		      ((>fx ndstcol zbcol)
		       ;; got the line, find the best column segment
		       (get-line sources pfindex psrcline psrccol))
		      (else
		       (loop ni 0 ndstcol nfindex nsrcline nsrccol nname)))))))))
