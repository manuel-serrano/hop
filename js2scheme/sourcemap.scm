;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/sourcemap.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 11 10:52:32 2014                          */
;*    Last change :  Fri Jul 11 16:09:55 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript source map generation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_sourcemap

   (include "../scheme2js/base64_vlq.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump)

   (export (generate-source-map pair-nil ::bstring ::bstring ::output-port)))

;*---------------------------------------------------------------------*/
;*    source-map-version ...                                           */
;*---------------------------------------------------------------------*/
(define (source-map-version) 3)

;*---------------------------------------------------------------------*/
;*    generate-source-map ...                                          */
;*---------------------------------------------------------------------*/
(define (generate-source-map tree in-file out-file p)
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 #f)
      (let ((nametable (make-hashtable))
	    (sourcetable (make-hashtable 4)))
	 (display "{\n" p)
	 (fprintf p "   \"version\": ~a,\n" (source-map-version))
	 (fprintf p "   \"file\": \"~a\",\n" in-file)
	 (display "   \"mappings\": \"" p)
	 (generate-source-map-mappings tree nametable sourcetable in-file out-file p)
	 (display "\",\n" p)
	 (display "   \"sources\": " p)
	 (generate-source-map-names sourcetable p)
	 (display ",\n" p)
	 (display "   \"names\": " p)
	 (generate-source-map-names nametable p)
	 (display "\n}\n" p)
	 (hashtable-map sourcetable (lambda (k v) k)))))

;*---------------------------------------------------------------------*/
;*    generate-source-map-mappings ...                                 */
;*---------------------------------------------------------------------*/
(define (generate-source-map-mappings tree::pair-nil
	   nametable sourcetable
	   src-file::bstring dst-file::bstring
	   p::output-port)
   (let ((res (cons #f '())))
      ;; generate the segments fro the ast
      (segments tree res nametable sourcetable)
      (let ((segs (sort (lambda (s1 s2) (<fx (car s1) (car s2))) (cdr res)))
	    (src-linetables (make-vector (hashtable-size sourcetable)))
	    (dst-linetable (load-file-line-table dst-file)))
	 ;; feed the source file lines table
	 (hashtable-for-each sourcetable
	    (lambda (k v)
	       (vector-set! src-linetables v (load-file-line-table k))))
	 (call-with-output-file (make-file-name "/tmp" (string-append (basename src-file) ".seg.txt"))
	    (lambda (dbg)
	       (let ((pdstline 0)
		     (pdstcol 0)
		     (psrcline 0)
		     (psrccol 0)
		     (pnindex 0)
		     (pfindex 0))
		  (let loop ((segs segs)
			     (pseg #f))
		     (when (pair? segs)
			(let* ((seg (car segs))
			       (pos (car seg))
			       (loc (cadr seg))
			       (file (hashtable-get sourcetable (cadr loc)))
			       (src-linetable (vector-ref src-linetables file)))
			   (multiple-value-bind (dstline dstbeg dstend)
			      (linetable-find dst-linetable pdstline pos dst-file)
			      (multiple-value-bind (srcline srcbeg srcend)
				 (linetable-find src-linetable psrcline (caddr loc) src-file)
				 (let ((srccol (-fx (caddr loc) srcbeg))
				       (dstcol (-fx pos dstbeg)))
				    ;; 1. zero-based starting column
				    (if (or (not pseg) (>fx dstline pdstline))
					(begin
					   ;; write as many semi-coma as we have skipped lines
					   (when (>fx dstline pdstline)
					      (display (make-string (-fx dstline pdstline) #\;) p)
					      (set! pdstline dstline))
					   (display (base64-vlq-encode dstcol) p)
					   ;; reset the dst column counter
					   (set! pdstcol dstcol))
					(begin
					   (display "," p)
					   (display (base64-vlq-encode (-fx dstcol pdstcol)) p)
					   (set! pdstcol dstcol)))
				    ;; 2. zero-based index into the sources list
				    (let* ((path (cadr loc))
					   (findex (hashtable-get sourcetable path)))
				       (display (base64-vlq-encode (-fx findex pfindex)) p)
				       (set! pfindex findex))
				    ;; 3. zero-based staring line in the src
				    (display (base64-vlq-encode (-fx srcline psrcline)) p)
				    (set! psrcline srcline)
				    ;; 4. zero-based starting column of the line
				    (display (base64-vlq-encode (-fx srccol psrccol)) p)
				    (set! psrccol srccol)
				    ;; 5. the names
				    (when (pair? (cddr seg))
				       (let* ((name (caddr seg))
					      (nindex (hashtable-get nametable name)))
					  (display (base64-vlq-encode (-fx nindex pnindex)) p)
					  (set! pnindex nindex)))
				    (let* ((file (cadr loc))
					   (findex (hashtable-get sourcetable file)))
				       (fprintf dbg "~a: dstcol=~a[~a], file=~a[~a], srcline=~a[~a], srccol=~a[~a] -> [~a~a~a~a~a"
					  dstline
					  dstcol (- dstcol pdstcol)
					  findex (- findex pfindex)
					  srcline (- srcline psrcline)
					  srccol (- psrccol psrccol)
					  (base64-vlq-encode pdstcol)
					  (base64-vlq-encode pfindex)
					  (base64-vlq-encode psrcline)
					  (base64-vlq-encode psrccol)
					  (if (pair? (cddr seg))
					      (let* ((nindex (caddr seg))
						     (nentry (hashtable-get nametable nindex)))
						 (base64-vlq-encode pnindex))
					      ""))
				       (loop (cdr segs) (car segs)))))))))))))))
   
;*---------------------------------------------------------------------*/
;*    linetable-find ...                                               */
;*    -------------------------------------------------------------    */
;*    Find the line containing the position. Returns the line number   */
;*    and the column number.                                           */
;*---------------------------------------------------------------------*/
(define (linetable-find table fromline pos file)
   (let ((len (vector-length table)))
      (let loop ((i fromline))
	 (if (<fx i len)
	     (let* ((line (vector-ref table i))
		    (b (car line))
		    (e (cdr line)))
		(cond
		   ((<fx pos b)
		    (linetable-find table 0 pos file))
		   ((>=fx e pos)
		    (values i b e))
		   (else
		    (loop (+fx i 1)))))
	     (linetable-find table 0 pos file)))))

;*---------------------------------------------------------------------*/
;*    segments ...                                                     */
;*---------------------------------------------------------------------*/
(define (segments tree res nametable sourcetable)
   ;; first element of the tree is the position offset
   (let loop ((tree (cdr tree))
	      (offset (car tree)))
      (when (pair? tree)
	 (let ((t (car tree)))
	    (cond
	       ((string? t)
		(loop (cdr tree) (+fx offset (string-length t))))
	       ((number? t)
		(loop (cdr tree) (+fx offset (string-length (number->string t)))))
	       ((symbol? t)
		(loop (cdr tree) (+fx offset (string-length (symbol->string! t)))))
	       ((isa? t J2SNode)
		(with-access::J2SNode t (loc)
		   (let ((segment (list offset loc)))
		      (set-cdr! res (cons segment (cdr res))))
		   (let* ((file (cadr loc))
			  (f (hashtable-get sourcetable (cadr loc))))
		      (unless f
			 (let ((n (hashtable-size sourcetable)))
			    (hashtable-put! sourcetable file n))))
		   (loop (cdr tree) offset)))
	       (else
		(loop (cdr tree) offset)))))))

;*---------------------------------------------------------------------*/
;*    generate-source-map-names ...                                    */
;*---------------------------------------------------------------------*/
(define (generate-source-map-names nametable p::output-port)
   (display "[" p)
   (let ((names (sort (lambda (e1 e2) (<fx (cdr e1) (cdr e2)))
		   (hashtable-map nametable cons) )))
      (if (null? names)
	  (display "]" p)
	  (let loop ((names names))
	     (if (null? (cdr names))
		 (begin
		    (display "\"" p)
		    (display (caar names) p)
		    (display "\"]" p))
		 (begin
		    (display "\"" p)
		    (display (caar names) p)
		    (display "\"," p)
		    (loop (cdr names))))))))

;*---------------------------------------------------------------------*/
;*    load-file-line-table ...                                         */
;*    -------------------------------------------------------------    */
;*    Read all the lines of a file and store the start indices         */
;*    in a table.                                                      */
;*---------------------------------------------------------------------*/
(define (load-file-line-table file::bstring)
   (call-with-input-file file
      (lambda (p)
	 (let loop ((lines '())
		    (i 0))
	    (let ((line (read-line p)))
	       (if (eof-object? line)
		   (list->vector (reverse! lines))
		   (let ((j (input-port-position p)))
		      (loop (cons (cons i j) lines) j))))))))
		   
