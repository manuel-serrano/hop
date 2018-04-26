;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/sourcemap.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 11 10:52:32 2014                          */
;*    Last change :  Thu Apr 26 18:06:19 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript source map generation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_sourcemap

   (library hop)
   
   (include "ast.sch"
	    "../runtime/sourcemap.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_stage
	   __js2scheme_dump)

   (export j2s-sourcemap-stage
	   (generate-source-map ::pair-nil ::bstring ::bstring ::output-port)))

;*---------------------------------------------------------------------*/
;*    j2s-sourcemap-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-sourcemap-stage
   (instantiate::J2SStageProc
      (optional #f)
      (name "sourcemap")
      (comment "Modify program source locations according to a source-map")
      (proc j2s-sourcemap!)))

;*---------------------------------------------------------------------*/
;*    j2s-sourcemap! ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-sourcemap! this conf)
   
   (define (assq-get k lst)
      (let ((v (assq k lst)))
	 (when (pair? v) (cdr v))))
   
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (source-map nodes path)
	 (when source-map
	    (let* ((smap (read-source-map source-map))
		   (mappings (assq-get 'mappings smap))
		   (sources (assq-get 'sources smap))
		   (root (assq-get 'sourceRoot smap))
		   (file (assq-get 'file smap)))
	       (when (and mappings sources file)
		  (let* ((positions (read-line-positions file))
			 (translations (make-vector (vector-length positions)))
			 (srcs (vector-map!
				  (lambda (f)
				     (let ((p (if (and root
						       (not (string-null? root)))
						  (make-file-name root f)
						  f)))
					(cons p (read-line-positions p))))
				  sources)))
		     (decode-mappings! mappings srcs positions translations)
;* 		     (tprint "positions=" positions " file=" file)     */
;* 		     (tprint "*** translations " translations " " path) */
		     (set! nodes
			(map! (lambda (n) (sourcemap! n translations path))
			   nodes))))))))
   this)

;*---------------------------------------------------------------------*/
;*    decode-mappings! ...                                             */
;*---------------------------------------------------------------------*/
(define (decode-mappings! mappings::bstring srcs::vector positions::vector
	   translations::vector)
   (let ((len (string-length mappings)))
      (let loop ((i 0)
		 (lnum 0)
		 (pgcol 0)
		 (pfindex 0)
		 (psline 0)
		 (pscol 0)
		 (pname 0))
	 (when (<fx i len)
	    (multiple-value-bind (status ni gcol findex sline scol name)
	       (source-map-decode-segment mappings i)
	       (if (eq? status 'newline)
		   (loop ni (+fx lnum 1) 0 pfindex psline pscol pname)
		   (let ((ngcol (+fx gcol pgcol))
			 (nfindex (+fx findex pfindex))
			 (nsline (+fx sline psline))
			 (nscol (+fx scol pscol))
			 (nname (+fx name pname)))
		      (when (<fx nfindex (vector-length srcs))
			 (let ((path (car (vector-ref srcs nfindex)))
			       (lines (cdr (vector-ref srcs nfindex)))
			       (j (+fx (vector-ref positions lnum) ngcol)))
			    (when (<fx nsline (vector-length lines))
			       (let ((pos (+fx (vector-ref lines nsline) nscol)))
				  (when (<fx j (vector-length translations))
				     (vector-set! translations j (cons path pos)))))))
		      (loop ni lnum ngcol nfindex nsline nscol nname))))))))

;*---------------------------------------------------------------------*/
;*    sourcemap! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (sourcemap! this::obj translations path)
   (if (pair? this)
       (map! (lambda (n) (sourcemap! n translations path)) this)
       this))

;*---------------------------------------------------------------------*/
;*    sourcemap! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-method (sourcemap! this::J2SNode translations path)
   (with-access::J2SNode this (loc)
      (update-node-location! loc translations path))
   (let* ((clazz (object-class this))
	  (fields (class-all-fields clazz)))
      ;; instance fields
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (info (class-field-info f)))
	       (when (and (pair? info) (member "ast" info))
		  (let ((v ((class-field-accessor f) this)))
		     (when (class-field-mutator f)
			(let ((nv (sourcemap! v translations path)))
			   ((class-field-mutator f) this nv)))))
	       (loop (-fx i 1)))))
      this))

;*---------------------------------------------------------------------*/
;*    sourcemap! ::J2SBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (sourcemap! this::J2SBlock translations path)
   (with-access::J2SBlock this (loc endloc)
      (call-next-method)
      (update-node-location! endloc translations path)
      (match-case loc
	 ((at ?- ?start)
	  (match-case endloc
	     ((at ?- ?end)
	      (when (<fx end start)
		 (set-car! (cddr endloc) start))))))
      this))

;*---------------------------------------------------------------------*/
;*    sourcemap! ::J2SDataPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (sourcemap! this::J2SDataPropertyInit translations path)

   (define (%location? %this)
      (with-access::J2SDataPropertyInit this (name val)
	 (when (isa? name J2SString)
	    (with-access::J2SString name (val)
	       (when (string=? val "%location")
		  (when (isa? val J2SObjInit)
		     (with-access::J2SObjInit val (inits)
			(when (and (list? inits) (=fx (length inits) 3))
			   (every (lambda (i) (isa? i J2SDataPropertyInit)) inits)))))))))

   (define (update-%location-init! i loc)
      (with-access::J2SDataPropertyInit i (name)
	 (when (isa? name J2SString)
	    (with-access::J2SString name (val)
	       (cond
		  ((string=? val "filename")
		   (with-access::J2SDataPropertyInit i (val)
		      (when (isa? val J2SString)
			 (with-access::J2SString val (val)
			    (set! val (cadr loc))))))
		  ((string=? val "pos")
		   (with-access::J2SDataPropertyInit i (val)
		      (when (isa? val J2SNumber)
			 (with-access::J2SNumber val (val)
			    (set! val (caddr loc)))))))))))

   (when (%location? this)
      (with-access::J2SDataPropertyInit this (val loc)
	 (when (update-node-location! loc translations path)
	    (with-access::J2SObjInit val (inits)
	       (for-each (lambda (i) (update-%location-init! i loc))
		  inits)))))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    update-node-location! ...                                        */
;*---------------------------------------------------------------------*/
(define (update-node-location! loc translations path)

   (define (find-closest-shift-position translations pos direction)
      (let loop ((pos pos)
		 (delta 0))
	 (cond
	    ((or (>=fx pos (vector-length translations)) (<fx pos 0))
	     #f)
	    ((pair? (vector-ref translations pos))
	     (cons delta (vector-ref translations pos)))
	    ((>fx direction 0)
	     (loop (+fx pos 1) (-fx delta 1)))
	    (else
	     (loop (-fx pos 1) (+fx delta 1))))))
   
   (define (find-closest-position translations pos)
      (if (pair? (vector-ref translations pos))
	  (cons 0 (vector-ref translations pos))
	  (let ((r (find-closest-shift-position translations pos 1))
		(l (find-closest-shift-position translations pos -1)))
	     (cond
		((not r) l)
		((not l) r)
		((< (abs (car r)) (abs (car l))) r)
		(else l)))))

   (match-case loc
      ((at ?fname ?pos)
;*        (tprint "path=" path " pos=" pos " trans=" (vector-length translations) */
;* 	  " test="                                                     */
;* 	  (and (equal? fname path) (<fx pos (vector-length translations)))) */
       (when (and (equal? fname path) (<fx pos (vector-length translations)))
	  (let ((p (find-closest-position translations pos)))
	     ;; (tprint "p=" p " " (+fx (car p) (cddr p)))
	     (when (and (pair? p) (>=fx (+fx (car p) (cddr p)) 0)) 
;* 		(tprint "update loc=" loc " => " p)                    */
;* 		(tprint "      nloc=" `(at ,(cadr p) ,(+fx (car p) (cddr p)) */
;* 					  ,(car p)))                   */
		(set-car! (cdr loc) (cadr p))
		(set-car! (cddr loc) (+fx (car p) (cddr p)))
		#t))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    read-source-map ...                                              */
;*---------------------------------------------------------------------*/
(define (read-source-map smap)
   (javascript->obj (call-with-input-file smap read-string)))

;*---------------------------------------------------------------------*/
;*    read-line-positions ...                                          */
;*    -------------------------------------------------------------    */
;*    Build a vector containing all the positions (character numbers)  */
;*    of the line beginings.                                           */
;*---------------------------------------------------------------------*/
(define (read-line-positions::vector path)
   
   (define (read-mmap-lnums path)
      (let* ((mmap (open-mmap path :read #t))
	     (len (mmap-length mmap))
	     (vec (make-vector len 0)))
	 (vector-set! vec 0 0)
	 (unwind-protect
	    (let loop ((i 0)
		       (line 1))
	       (when (<fx i len)
		  (if (char=? (mmap-ref mmap i) #\Newline)
		      (begin
			 (vector-set! vec line (+fx i 1))
			 (loop (+fx i 1) (+fx line 1)))
		      (loop (+fx i 1) line))))
	    (close-mmap mmap))
	 vec))
   
   (define (read-file-lnums path)
      (call-with-input-file path
	 (lambda (ip)
	    (let loop ((i 0)
		       (line 1)
		       (acc '(0)))
	       (if (eof-object? ip)
		   (list->vector (reverse! acc))
		   (if (char=? (read-char ip) #\Newline)
		       (loop (+fx i 1) (+fx line 1) (cons (+fx i 1) acc))
		       (loop (+fx i 1) line acc)))))))
   
   (if (file-exists? path)
       (read-mmap-lnums path)
       (read-file-lnums path)))
   
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
			   (multiple-value-bind (dstline dstbeg)
			      (linetable-find dst-linetable pdstline pos dst-file)
			      (multiple-value-bind (srcline srcbeg)
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
   (if (and (>=fx pos 0) (>=fx fromline 0))
       ;; guard against corrupted ast
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
			(values i b))
		       (else
			(loop (+fx i 1)))))
		 (linetable-find table 0 pos file))))
       (values 0 0)))

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
		   
