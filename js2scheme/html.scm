;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/html.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 23 17:15:52 2015                          */
;*    Last change :  Thu Sep 24 16:37:05 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    J2S Html parser                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_html

   (include "token.sch")
   
   (library hop)
   
   (import __js2scheme_parser
	    __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils)
   
   (export (html-parser ::input-port ::pair-nil #!optional tag)))

;*---------------------------------------------------------------------*/
;*    html-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (html-parser port conf::pair-nil #!optional tag)
   (if tag
      (let ((str (symbol->string! (token-value tag))))
	 (rgc-buffer-unget-char port (char->integer #\space))
	 (rgc-buffer-insert-substring! port str 0 (string-length str))
	 (read/rp xml-grammar port list *html-special-elements* #f
	    (lambda (x) x)
	    (hop-locale)
	    conf))
      (let loop ()
	 (let ((v (read/rp xml-grammar port list *html-special-elements* #f
		     (lambda (x) x)
		     (hop-locale)
		     conf)))
	    (cond
	       ((isa? v J2SString)
		(with-access::J2SString v (val)
		   (if (string-skip val " \n\t") v (loop))))
	       (else
		v))))))

;*---------------------------------------------------------------------*/
;*    special-tags ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (special-tags lst)
   ;; transforms
   ;;   '((meta) ... (p . (a ...)))
   ;; info
   ;;   '((<meta>) (<META>) ... (<p> . (<a> <A> ...)) (<p> . (<a> <A> ...)))
   
   (define (symbol-upcase val)
      (string->symbol (string-upcase (symbol->string val))))

   (define (loop entries)
      (append-map (lambda (e)
		     (match-case e
			((? symbol?)
			 `(,(symbol-append '< e '>)
			   ,(symbol-append '< (symbol-upcase e) '>)))
			((?tag)
			 `((,(symbol-append '< tag '>))
			   (,(symbol-append '< (symbol-upcase tag) '>))))
			((?tag . (and ?d (unquote ?exp)))
			 `((,(symbol-append '< tag '>) . ,d)
			   (,(symbol-append '< (symbol-upcase tag) '>) . ,d)))
			((?tag . ?list)
			 `((,(symbol-append '< tag '>)
			    . ,(loop list))
			   (,(symbol-append '< (symbol-upcase tag) '>)
			    . ,(loop list))))))
	 entries))

   (match-case lst
      ((quasiquote ?entries) (list 'quasiquote (loop entries)))
      (else (error "special-tags" "bad form" lst))))

;*---------------------------------------------------------------------*/
;*    *html-special-elements* ...                                      */
;*---------------------------------------------------------------------*/
(define *html-special-elements*
   (special-tags
      `((meta)
	(link)
	(br) (hr) (img) (input)
	(p . (a abbr acronym address big button caption del em
		i img kbd label legend 
		q s samp small span strike strong sub sup u var))
	(colgroup . (col))
	(script . ,html-parse-script))))

;*---------------------------------------------------------------------*/
;*    html-parse-script ...                                            */
;*---------------------------------------------------------------------*/
(define (html-parse-script iport)
   (let* ((sp (input-port-position iport))
	  (g (regular-grammar ()
		((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		 (let ((s (instantiate::J2SString
			     (escape '(escape))
			     (val (the-substring 1 (-fx (the-length) 1)))
			     (loc (the-coord (the-port) (+fx (the-length) 1))))))
		    (cons s (ignore))))
		((: "\'" (* (or (out #a000 #\\ #\') (: #\\ all))) "\'")
		 (let ((s (instantiate::J2SString
			     (escape '(escape))
			     (val (string-for-read
				     (the-substring 1 (-fx (the-length) 1))))
			     (loc (the-coord (the-port) (+fx (the-length) 1))))))
		    (cons s (ignore))))
		((: "//" (* all))
		 (ignore))
		((: "/*" (* (or (out #\*) (: #\* (out "/")))) "*/")
		 (ignore))
		((or (+ (out "<")) #\<)
		 (let ((s (instantiate::J2SString
			     (escape '(escape))
			     (val (the-string))
			     (loc (the-coord (the-port) (the-length))))))
		    (cons s (ignore))))
		((uncase "</script>")
		 '())  
		(else
		 (let ((char (the-failure)))
		    (raise
		       (instantiate::&io-parse-error
			  (proc "xml-parser")
			  (msg (if (eof-object? char)
				   "Premature end of file"
				   "Unclosed list"))
			  (obj (if (eof-object? char)
				   char
				   (string-append "{" (string char) "}")))
			  (fname (input-port-name iport))
			  (location (input-port-position iport)))))))))
      (read/rp g iport)))

;*---------------------------------------------------------------------*/
;*    xml-parser ...                                                   */
;*---------------------------------------------------------------------*/
(define (xml-parser::pair-nil port::input-port
	   #!key
	   (procedure list)
	   (specials '())
	   (strict #t)
	   (encoding 'UTF-8))
   (let loop ((decoder (lambda (x) x)))
      (let ((obj (read/rp xml-grammar port procedure procedure specials strict decoder encoding)))
	 (cond
	    ((eof-object? obj)
	     '())
	    ((and (pair? obj) (eq? 'xml-decl (car obj)))
	     (let ((enc (assq 'encoding (cdr obj))))
		(if enc
		    (cons obj (loop (charset-converter (cdr enc) encoding)))
		    (cons obj (loop decoder)))))
	    (else
	     (cons obj (loop decoder)))))))

;*---------------------------------------------------------------------*/
;*    special ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct special tag attributes body owner)

;*---------------------------------------------------------------------*/
;*    xml-grammar ...                                                  */
;*---------------------------------------------------------------------*/
(define xml-grammar
   (regular-grammar ((id (: (in ("azAZ") "!?") (* (in ("azAZ09") ":_-."))))
		     (lt (in #a013 #\Newline))
		     next
		     specials
		     strict
		     decoder
		     encoding
		     conf)
      
      ((+ (in " \t\n\r"))
       (instantiate::J2SString
	  (escape '(escape))
	  (val (the-string))
	  (loc (the-coord (the-port) (+fx (the-length) 1)))))
      ((+ (or (out "<$~") (: (in "$~") (out "<{"))))
       (instantiate::J2SString
	  (escape '(escape))
	  (val (decoder (the-string)))
	  (loc (the-coord (the-port) (+fx (the-length) 1)))))
      ((in "$~")
       (instantiate::J2SString
	  (escape '(escape))
	  (val (decoder (the-string)))
	  (loc (the-coord (the-port) (+fx (the-length) 1)))))      
      ((: "<!--"
	  (* (or (out "-") (: "-" (out "-")) (: "--" (out ">"))))
	  (+ "-") "->")
       (let ((tag (token 'HTML '<!--> (the-length)))
	     (data (the-substring 4 -3)))
	  (instantiate::J2SCall
	     (loc (token-loc tag))
	     (fun (j2s-tag->expr tag #t))
	     (args (list (instantiate::J2SNativeString
			    (val (decoder data))
			    (loc (the-coord (the-port) (+fx (the-length) 6)))))))))
      
      ((: "<!" (: (or (out "[-") (: "-" (out "-")))
		  (* (out ">]"))
		  (? (: "[" (* (out "]")) "]"))
		  (* (out ">"))) ">")
       (tprint "ignore [" (the-string) "]")
       (ignore))
;*        (cons 'declaration (the-string)))                            */
      ("<![CDATA["
       (cons 'cdata (read/rp cdata-grammar (the-port) decoder))
       (ignore))
      ((: "<?xml " (* (out "?>")) "?>")
       (let ((s (the-substring 6 (the-length))))
	  (string-set! s (-fx (string-length s) 2) #\space)
	  (let ((p (open-input-string s)))
	     (let loop ((attr '()))
		(let ((obj (read/rp attribute-grammar p 'xml #t decoder conf)))
		   (cond
		      ((pair? obj)
		       (loop (cons obj attr)))
		      ((eq? obj '>)
		       (cons 'xml-decl attr))))))))
      ((: "<?" (* (out ">")) ">")
       (cons 'instruction (the-string))
       (ignore))
      ((: "<" id ">")
       (let* ((t (the-substring 1 (-fx (the-length) 1)))
	      (ts (string->symbol t))
	      (tag (token 'OHTML (the-symbol) (the-length)))
	      (p (the-port)))
	  (collect-up-to ignore tag '()
	     p specials strict decoder encoding conf)))
      ((: "<" id "/>")
       (let ((tag (token 'HTML (symbol-append (the-subsymbol 0 -2) '>) (the-length))))
	  (make-dom-create tag '() '() conf)))
;* 	  (instantiate::J2SCall                                        */
;* 	     (loc (token-loc tag))                                     */
;* 	     (fun (j2s-tag->expr tag #t)))))                           */
      ((: "<" id (in " \n\t\r"))
       (let* ((t (the-substring 1 (-fx (the-length) 1)))
	      (ts (string->symbol t))
	      (tag (token 'HTML (symbol-append '< ts '>) (the-length)))
	      (p (the-port)))
	  (let loop ((attr '()))
	     (let ((obj (read/rp attribute-grammar p t strict decoder conf)))
		(cond
		   ((isa? obj J2SNode)
		    (loop (cons obj attr)))
		   ((eq? obj '>)
		    (collect-up-to ignore tag (reverse! attr) p specials strict decoder encoding
		       conf))
		   ((eq? obj '/>)
		    (make-dom-create tag attr '() conf)))))))
      ((: "</" id ">")
       (string->symbol (the-substring 2 (-fx (the-length) 1))))
      ("~{"
       (let ((str (the-string)))
	  (rgc-buffer-insert-substring! (the-port) str 0 2))
       (j2s-parser (the-port) (cons* :parser 'tilde-expression conf)))
      ("${"
       (let ((str (the-string)))
	  (rgc-buffer-insert-substring! (the-port) str 0 2))
       (j2s-parser (the-port) (cons* :parser 'dollar-expression conf)))
      (else
       (let ((c (the-failure)))
	  (cond
	     ((not (eof-object? c))
	      (xml-parse-error "Illegal character"
			       (error-line c (the-port))
			       (input-port-name (the-port))
			       (input-port-position (the-port))))
	     (else
	      c))))))

;*---------------------------------------------------------------------*/
;*    make-dom-create ...                                              */
;*---------------------------------------------------------------------*/
(define (make-dom-create tag::pair attributes body conf)

   (define (debug-init-val loc)
      (match-case loc
	 ((at ?fname ?pos)
	  (instantiate::J2SObjInit
	     (loc loc)
	     (inits (list
		       (instantiate::J2SDataPropertyInit
			  (loc loc)
			  (name (instantiate::J2SString
				   (val "filename")
				   (loc loc)))
			  (val (instantiate::J2SString
				  (val fname)
				  (loc loc))))
		       (instantiate::J2SDataPropertyInit
			  (loc loc)
			  (name (instantiate::J2SString
				   (val "pos")
				   (loc loc)))
			  (val (instantiate::J2SNumber
				  (val pos)
				  (loc loc))))))))
	 (else
	  (instantiate::J2SUndefined
	     (loc loc)))))

   (define (debug-init loc)
      (instantiate::J2SDataPropertyInit
	 (loc loc)
	 (name (instantiate::J2SString
		  (val "%location")
		  (loc loc)))
	 (val (debug-init-val loc))))

   (let ((attrs '())
	 (abody '()))
      (for-each (lambda (x)
		   (if (isa? x J2SDataPropertyInit)
		       (set! attrs (cons x attrs))
		       (set! abody (cons x abody))))
	 attributes)
      (let* ((loc (token-loc tag))
	     (inits (filter (lambda (x)
			       (isa? x J2SDataPropertyInit))
		       (reverse! attrs)))
	     (dbg (> (config-get conf :debug 0) 0))
	     (a (if (and (null? attrs) (not dbg))
		    (instantiate::J2SUndefined
		       (loc loc))
		    (instantiate::J2SObjInit
		       (loc loc)
		       (inits (if dbg
				  (cons (debug-init loc) inits)
				  inits))))))
	 (instantiate::J2SCall
	    (loc loc)
	    (fun (j2s-tag->expr tag #t))
	    (args (cons a (append (reverse! abody) body)))))))

;*---------------------------------------------------------------------*/
;*    collect-up-to ...                                                */
;*---------------------------------------------------------------------*/
(define (collect-up-to ignore tag::pair attributes port specials strict decoder encoding conf)
   
   (define (collect ignore tags)
      (let ((name (input-port-name port))
	    (po (input-port-position port)))
	 (let loop ((acc '())
		    (item (ignore)))
	    (cond
	       ((symbol? item)
		(cond
		   ((eq? item tag)
		    (make-dom-create tag attributes (reverse! acc) conf))
		   (strict
		    (xml-parse-error "Illegal closing tag"
				     (format "`~a' expected, `~a' provided"
					     tag item)
				     name po))
		   (else
		    (make-dom-create tag attributes (reverse! acc) conf))))
	       ((special? item)
		(let ((nitem (make-dom-create (special-tag item)
				(special-attributes item)
				(special-body item)
				conf)))
		   (if (memq (special-tag item) tags)
		       (loop acc nitem)
		       (begin
			  (list (make-dom-create tag attributes
				   (reverse! acc) conf) nitem)))))
	       ((eof-object? item)
		(xml-parse-error
		   (format "Premature end of HTML, expecting tag `</~a>'"
		      (let ((s (symbol->string (cdr tag))))
			 (substring s 1 (-fx (string-length s) 1))))
		   item name po))
	       (else
		(let ((po (input-port-last-token-position port)))
		   (loop (econs item acc (list 'at name po)) (ignore))))))))

   (let ((spec (assq (token-value tag) specials)))
      (cond
	 ((not spec)
	  (collect ignore '()))
	 ((null? (cdr spec))
	  (make-dom-create tag attributes '() conf))
	 ((procedure? (cdr spec))
	  (make-dom-create tag attributes ((cdr spec) port) conf))
	 ((pair? (cdr spec))
	  (let ((ignore (lambda ()
			   (read/rp xml-grammar port
				    (lambda (t a b) (special t a b tag))
				    specials strict decoder encoding
				    conf)))) 
	     (collect ignore (cdr spec))))
	 (else
	  (error "xml-parse" "Illegal special handler" spec)))))

;*---------------------------------------------------------------------*/
;*    attribute-value-grammar ...                                      */
;*---------------------------------------------------------------------*/
(define attribute-value-grammar
   (regular-grammar (strict tag conf)
      ((+ (in " \t\n\r"))
       (ignore))
      ((or (: #\" (* (or (out "~$\\\"") (: #\\ all) (: (in "~$") (out "{")))) #\")
	   (: #\' (* (or (out "~$\\'") (: #\\ all) (: (in "~$") (out "{")))) #\'))
       (instantiate::J2SString
	  (escape '())
	  (loc (the-coord (the-port) (-fx (the-length) 1)))
	  (val (the-substring 1 -1))))
      ((: (+ digit) (? (or "%" "px" "cm" "em" "mm" "inch")))
       (if strict
	   (xml-parse-error (format "Illegal `~a' attribute value" tag)
	      (the-string)
	      (input-port-name (the-port))
	      (input-port-position (the-port)))
	   (instantiate::J2SString
	      (escape '())
	      (loc (the-coord (the-port) (-fx (the-length) 1)))
	      (val (the-string)))))
      ((+ (out " ~$\t\n\r<>(){}[]@!\"'"))
       (if strict
	   (xml-parse-error (format "Illegal `~a' attribute character" tag)
	      (the-string)
	      (input-port-name (the-port))
	      (input-port-position (the-port)))
	   (instantiate::J2SString
	      (escape '())
	      (loc (the-coord (the-port) (-fx (the-length) 1)))
	      (val (the-string)))))
      ("~{"
       (let ((str (the-string)))
	  (rgc-buffer-insert-substring! (the-port) str 0 2))
       (j2s-parser (the-port) (cons* :parser 'tilde-expression conf)))
      ((or (: "\"~{" (* (or (out #\\ #\") (: #\\ all))) "}\"")
	   (: "'~{" (* (or (out #\\ #\') (: #\\ all))) "}'"))
       (let ((str (the-substring 1 -1)))
	  (call-with-input-string str
	     (lambda (ip)
		(j2s-parser ip (cons* :parser 'tilde-expression conf))))))
      ("${"
       (let ((str (the-string)))
	  (rgc-buffer-insert-substring! (the-port) str 0 2))
       (j2s-parser (the-port) (cons* :parser 'dollar-expression conf)))
      ((or (: "\"${" (* (or (out #\\ #\") (: #\\ all))) "}\"")
	   (: "'${" (* (or (out #\\ #\') (: #\\ all))) "}'"))
       (let ((str (the-substring 1 -1)))
	  (call-with-input-string str
	     (lambda (ip)
		(j2s-parser ip (cons* :parser 'dollar-expression conf))))))
      (else
       (let ((c (the-failure)))
	  (if (not (eof-object? c))
	      (if (or strict
		      (not (or (char=? c #\space)
			       (char=? c #\Newline)
			       (char=? c #\>))))
		  (xml-parse-error
		   (format "Illegal `~a' attribute character" tag)
		   (error-line c (the-port))
		   (input-port-name (the-port))
		   (input-port-position (the-port)))
		  " ")
	      (xml-parse-error
	       (format "Premature end of line for tag `~a' attribute" tag)
	       c
	       (input-port-name (the-port))
	       (-fx (input-port-position (the-port)) 1)))))))
      
;*---------------------------------------------------------------------*/
;*    attribute-grammar ...                                            */
;*---------------------------------------------------------------------*/
(define attribute-grammar
   (regular-grammar ((id (: (in ("azAZ") "_") (* (in ("azAZ09") ":_-"))))
		     tag
		     strict
		     decoder
		     conf)
      ((+ (in " \t\n\r"))
       (ignore))
      ((: id "=")
       (let* ((loc (the-coord (the-port) (+fx (the-length) 1)))
	      (key (the-substring 0 (-fx (the-length) 1)))
	      (val (read/rp attribute-value-grammar (the-port) strict tag conf))
	      (name (instantiate::J2SString
		       (loc loc)
		       (val key))))
	  (instantiate::J2SDataPropertyInit
	     (loc loc)
	     (name name)
	     (val val))))
      ((: id (+ blank) "=")
       (let* ((loc (the-coord (the-port) (+fx (the-length) 1)))
	      (key (the-substring 0 (-fx (the-length) 2)))
	      (val (read/rp attribute-value-grammar (the-port) strict tag conf)))
	  (let loop ((i (-fx (string-length key) 1)))
	     (case (string-ref key i)
		((#\space #\tab #\Newline)
		 (loop (-fx i 1)))
		(else
		 (set! key (substring key 0 (+ i 1))))))
	  (let ((name (instantiate::J2SString
			 (loc loc)
			 (val key))))
	     (instantiate::J2SDataPropertyInit
		(loc loc)
		(name name)
		(val val)))))
      ((: id)
       (let ((loc (the-coord (the-port) (+fx (the-length) 1)))
	     (key (decoder (the-string))))
	  (let ((name (instantiate::J2SString
			 (loc loc)
			 (val key)))
		(val (instantiate::J2SString
			(loc loc)
			(val key))))
	     (instantiate::J2SDataPropertyInit
		(loc loc)
		(name name)
		(val val)))))
      ((or "/>" ">")
       (the-symbol))
      ("${"
       (let ((str (the-string)))
	  (rgc-buffer-insert-substring! (the-port) str 0 2))
       (j2s-parser (the-port) (cons* :parser 'dollar-expression conf)))
      (else
       (let ((c (the-failure))
	     (loc (input-port-position (the-port))))
	  (if (not (eof-object? c))
	      (xml-parse-error "Illegal attribute character"
		 (error-line c (the-port))
		 (input-port-name (the-port))
		 loc)
	      (xml-parse-error
		 (format "Premature end of line, expecting tag `~a'" tag)
		 c
		 (input-port-name (the-port))
		 (-fx loc 1)))))))

;*---------------------------------------------------------------------*/
;*    cdata-grammar ...                                                */
;*---------------------------------------------------------------------*/
(define cdata-grammar
   (regular-grammar (decoder)
      ((* (out "]"))
       (let* ((res (decoder (the-string)))
	      (rest (ignore)))
	  (string-append res rest)))
      ("]"
       (string-append "]" (ignore)))
      ((: "]]>" (? "\n"))
       "")
      (else
       (let* ((c (the-failure))
	      (msg (if (not (eof-object? c))
		       "Illegal <![CDATA[ character"
		       "Premature end of line, expecting tag `]]>'"))
	      (loc (input-port-position (the-port))))
	  (xml-parse-error msg
	     c (input-port-name (the-port)) loc)))))

;*---------------------------------------------------------------------*/
;*    xml-parse-error ...                                              */
;*---------------------------------------------------------------------*/
(define (xml-parse-error msg obj name pos)
   (raise
    (instantiate::&io-parse-error
       (proc 'xml-parse)
       (msg msg)
       (obj obj)
       (fname name)
       (location pos))))

;*---------------------------------------------------------------------*/
;*    error-line ...                                                   */
;*---------------------------------------------------------------------*/
(define (error-line c port)
   (let ((line (read-line port)))
      (string-append "{" (string c) "}" (if (string? line) line ""))))
