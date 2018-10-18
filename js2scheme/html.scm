;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/html.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 23 17:15:52 2015                          */
;*    Last change :  Wed Dec 21 12:40:28 2016 (serrano)                */
;*    Copyright   :  2015-16 Manuel Serrano                            */
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
   (let ((lang (config-get conf :language 'hopscript)))
      (if tag
	  (let ((str (symbol->string! (token-value tag))))
	     (rgc-buffer-unget-char port (char->integer #\space))
	     (rgc-buffer-insert-substring! port str 0 (string-length str))
	     (read/rp xml-grammar port '()
		(eq? lang 'hopscript) #f
		(lambda (x) x)
		(hop-locale) lang conf))
	  (let loop ()
	     (let ((v (read/rp xml-grammar port '()
			 (eq? lang 'hopscript) #f
			 (lambda (x) x)
			 (hop-locale)
			 lang conf)))
		(cond
		   ((isa? v J2SString)
		    (with-access::J2SString v (val)
		       (if (string-skip val " \n\t") v (loop))))
		   (else
		    v)))))))

;*---------------------------------------------------------------------*/
;*    element ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct element tag attributes prop)

;*---------------------------------------------------------------------*/
;*    html-elements-properties ...                                     */
;*---------------------------------------------------------------------*/
(define (html-elements-properties)
   '((<ADDRESS> :block)
     (<AREA> :empty)
     (<BASE> :empty)
     (<BASEFONT> :empty)
     (<BLOCKQUOTE> :block)
     (<BR> :empty)
     (<COL> :empty)
     (<COLGROUP> :chidren (<COL>))
     (<DIV> :block)
     (<DL> :block :children (<DD> <DT>))
     (<DD> :item :children flow :etag-optional)
     (<DT> :item :children inline :etag-optional)
     (<FIELDSET> :block)
     (<FORM> :block)
     (<FRAME> :empty)
     (<HR> :empty :block)
     (<H1> :block)
     (<H2> :block)
     (<H3> :block)
     (<H4> :block)
     (<H5> :block)
     (<H6> :block)
     (<IMG> :empty)
     (<INPUT> :empty)
     (<ISINDEX> :empty)
     (<LI> :etag-optional)
     (<LINK> :empty)
     (<META> :empty)
     (<NOSCRIPT> :block)
     (<OL> :block)
     (<P> :block :children inline :etag-optional)
     (<PARAM> :empty)
     (<PRE> :block)
     (<SCRIPT> :script)
     (<TABLE> :block)
     (<UL> :block)))

;*---------------------------------------------------------------------*/
;*    html-element-properties ...                                      */
;*---------------------------------------------------------------------*/
(define (html-element-properties tagname)
   (let ((c (assq tagname (html-elements-properties))))
      (when (pair? c) (cdr c))))

;*---------------------------------------------------------------------*/
;*    html-property-script? ...                                        */
;*---------------------------------------------------------------------*/
(define (html-property-script? prop)
   (and prop (memq :script prop)))

;*---------------------------------------------------------------------*/
;*    html-property-empty? ...                                         */
;*---------------------------------------------------------------------*/
(define (html-property-empty? prop)
   (and prop (memq :empty prop)))

;*---------------------------------------------------------------------*/
;*    html-property-block? ...                                         */
;*---------------------------------------------------------------------*/
(define (html-property-block? prop)
   (and prop (memq :block prop)))

;*---------------------------------------------------------------------*/
;*    html-property-item? ...                                          */
;*---------------------------------------------------------------------*/
(define (html-property-item? prop)
   (and prop (memq :item prop)))

;*---------------------------------------------------------------------*/
;*    html-property-etag-optional? ...                                 */
;*---------------------------------------------------------------------*/
(define (html-property-etag-optional? prop)
   (and prop (memq :etag-optional prop)))

;*---------------------------------------------------------------------*/
;*    html-property-children ...                                       */
;*---------------------------------------------------------------------*/
(define (html-property-children prop)
   (when prop
      (let ((c (memq :children prop)))
	 (when (pair? c) (cadr c)))))

;*---------------------------------------------------------------------*/
;*    (html-empty-elements)                                            */
;*---------------------------------------------------------------------*/
(define (html-empty-elements)
   '(<AREA> <BASE> <BASEFONT> <BR> <COL> <FRAME> <HR> <IMG> <INPUT> <ISINDEX>
     <LINK> <META> <PARAM>))

;*---------------------------------------------------------------------*/
;*    html-empty-element? ...                                          */
;*---------------------------------------------------------------------*/
(define (html-empty-element? tag)
   (memq tag (html-empty-elements)))

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
;*    special ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct special tag attributes body owner)

;*---------------------------------------------------------------------*/
;*    push-ignore ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (push-ignore val)
   (let ((v (gensym 'val)))
      `(let ((,v ,val))
	  (set! stack (cons ,v stack))
	  (ignore))))

;*---------------------------------------------------------------------*/
;*    push-node-ignore ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (push-node-ignore val)
   (let ((v (gensym 'val)))
      `(let ((,v ,val))
	  (if (pair? stack)
	      (begin
		 (set! stack (cons ,v stack))
		 (ignore))
	      ,v))))

;*---------------------------------------------------------------------*/
;*    reduce-ignore ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (reduce-ignore val)
   `(begin
       (set! stack (reduce-stack-up-to stack ,val lang conf (the-port) stricttag))
       (cond
	  ((null? (cdr stack))
	   (car stack))
	  ((every (lambda (o)
		     (when (isa? o J2SString)
			(with-access::J2SString o (val)
			   (pregexp-match "[ \t\n]*" val))))
	      (cdr stack))
	   (car stack))
	  (else
	   (ignore)))))

;*---------------------------------------------------------------------*/
;*    dump-stack ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-stack stack)
   (map (lambda (o)
	   (cond
	      ((element? o)
	       (token-value (element-tag o)))
	      ((isa? o J2SString)
	       (with-access::J2SString o (val)
		  (format "~s" val)))
	      (else
	       (typeof o))))
      stack))

;*---------------------------------------------------------------------*/
;*    reduce-stack-up-to ...                                           */
;*---------------------------------------------------------------------*/
(define (reduce-stack-up-to stack ctag lang conf port stricttag)
   
   (define (tagname tag)
      (let ((s (symbol->string! tag)))
	 (substring s 1 (-fx (string-length s) 1))))

   (let loop ((stk stack)
	      (args '()))
      (cond
	 ((null? stk)
	  (xml-parse-error "Wrong closing tag"
	     (tagname ctag)
	     (input-port-name port) (input-port-position port)))
	 ((not (element? (car stk)))
	  (loop (cdr stk) (cons (car stk) args)))
	 ((eq? (token-value (element-tag (car stk))) ctag)
	  (let ((tag (element-tag (car stk)))
		(attrs (element-attributes (car stk))))
	     (cons (make-dom-create tag attrs args lang conf)
		(cdr stk))))
	 ((html-property-etag-optional?
	     (html-element-properties
		(token-value (element-tag (car stk)))))
	  (let ((tag (element-tag (car stk)))
		(attrs (element-attributes (car stk))))
	     (loop (cdr stk)
		(list (make-dom-create tag attrs args lang conf)))))
	 (stricttag
	  (xml-parse-error "Tag mismatch"
	     (format "</~a> expected, </~a> provided"
		(tagname (token-value (element-tag (car stk))))
		(tagname ctag))
	     (input-port-name port) (input-port-position port)))
	 (else
	  stack))))

;*---------------------------------------------------------------------*/
;*    reduce-stack-special ...                                         */
;*---------------------------------------------------------------------*/
(define (reduce-stack-special stack special lang conf)
   (let loop ((stack stack)
	      (args '()))
      (if (eq? (car stack) special)
	  (let ((tag (element-tag (car stack)))
		(attrs (element-attributes (car stack))))
	     (cons (make-dom-create tag attrs args lang conf)
		(cdr stack)))
	  (loop (cdr stack) (cons (car stack) args)))))

;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define debug #f)

;*---------------------------------------------------------------------*/
;*    xml-grammar ...                                                  */
;*---------------------------------------------------------------------*/
(define xml-grammar
   (regular-grammar ((id (: (in ("azAZ") "!?") (* (in ("azAZ09") ":_-."))))
		     (lt (in #a013 #\Newline))
		     stack
		     stricttag strictattr
		     decoder
		     encoding
		     lang
		     conf)
      
      (define (find-special stack)
         ;;; find the top-most special element of the stack, if any
	 (find (lambda (o)
		  (when (element? o)
		     (element-prop o)))
	    stack))
      
      (define (ident-upcase! s)
         ;;; upcase identifier
	 (let ((i (string-index-right s #\.)))
	    (if i
		(string-append (substring s 0 i)
		   "."
		   (string-upcase! (substring s (+fx i 1))))
		(string-upcase! s))))
      
      (define (open-tag type sym attributes)
         ;;; handle opening tags
	 (let ((prop (html-element-properties sym)))
	    (when debug
	       (tprint "open " sym " " (dump-stack stack)))
	    (cond
	       ((html-property-empty? prop)
		(let ((tag (token type sym (the-length))))
		   (push-node-ignore
		      (make-dom-create tag attributes '() lang conf))))
	       ((html-property-script? prop)
		(let ((tag (token type sym (the-length)))
		      (args (html-parse-script (the-port))))
		   (push-node-ignore
		      (make-dom-create tag attributes args lang conf))))
	       (else
		(let* ((tag (token type sym (the-length)))
		       (el (element tag attributes
			      (and (html-property-children prop) prop)))
		       (s (find-special stack)))
		   (when s
		      (let* ((sp (element-prop s))
			     (spchd (html-property-children sp)))
			 (cond
			    ((eq? spchd 'inline)
			     (when (html-property-block? prop)
				(set! stack
				   (reduce-stack-special stack s lang conf))
				(when debug
				   (tprint "REDUCE SPECIAL " s)
				   (tprint "            -> " (dump-stack stack)))))
			    ((eq? spchd 'flow)
			     (when (html-property-item? prop)
				(set! stack
				   (reduce-stack-special stack s lang conf))
				(when debug
				   (tprint "REDUCE SPECIAL " s)
				   (tprint "            -> " (dump-stack stack)))))
			    ((pair? spchd)
			     (unless (memq sym spchd)
				(set! stack
				   (reduce-stack-special stack s lang conf)))))))
		   (push-ignore el))))))
      
      ((+ (in " \t\n\r"))
       (push-ignore
	  (instantiate::J2SString
	     (escape '(escape))
	     (val (the-string))
	     (loc (the-coord (the-port) (+fx (the-length) 1))))))
      ((+ (or (out "<$~") (: (in "$~") (out "<{"))))
       (push-ignore
	  (instantiate::J2SString
	     (escape '(escape))
	     (val (decoder (the-string)))
	     (loc (the-coord (the-port) (+fx (the-length) 1))))))
      ((in "$~")
       (push-ignore
	  (instantiate::J2SString
	     (escape '(escape))
	     (val (decoder (the-string)))
	     (loc (the-coord (the-port) (+fx (the-length) 1))))))
      ((: "<!--"
	  (* (or (out "-") (: "-" (out "-")) (: "--" (out ">"))))
	  (+ "-") "->")
       (let ((tag (token 'HTML '<!--> (the-length)))
	     (data (the-substring 4 -3)))
	  (push-ignore
	     (instantiate::J2SCall
		(loc (token-loc tag))
		(fun (j2s-tag->expr tag #t))
		(args (list (instantiate::J2SNativeString
			       (val (decoder data))
			       (loc (the-coord (the-port) (+fx (the-length) 6))))))))))
      
      ((: "<!" (: (or (out "[-") (: "-" (out "-")))
		  (* (out ">]"))
		  (? (: "[" (* (out "]")) "]"))
		  (* (out ">"))) ">")
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
       (open-tag 'OHTML (string->symbol (ident-upcase! (the-string))) '()))
      ((: "<" id (in " \n\t\r"))
       (let* ((t (ident-upcase! (the-substring 0 (-fx (the-length) 1))))
	      (sym (string->symbol (string-append t ">"))))
	  (let loop ((attr '()))
	     (let ((obj (read/rp attribute-grammar (the-port) sym
			   strictattr decoder conf)))
		(cond
		   ((isa? obj J2SNode)
		    (loop (cons obj attr)))
		   ((eq? obj '/>)
		    (let ((tag (token 'HTML sym (the-length))))
		       (push-node-ignore
			  (make-dom-create tag (reverse! attr) '() lang conf))))
		   ((not (eq? obj '>))
		    (xml-parse-error "Illegal character"
		       obj
		       (input-port-name (the-port))
		       (input-port-position (the-port))))
		   (else
		    (open-tag 'HTML sym (reverse! attr))))))))
      ((: "<" id "/>")
       (let* ((str (string-append (ident-upcase! (the-substring 0 -2)) ">"))
	      (tag (token 'HTML (string->symbol str) (the-length))))
	  (push-node-ignore (make-dom-create tag '() '() lang conf))))
      ((: "</" id ">")
       (let ((sym (string->symbol
		     (ident-upcase!
			(string-append "<" (the-substring 2 (the-length)))))))
	  (when debug
	     (tprint "REDUCE " sym  " " (dump-stack stack)))
	  (if (html-empty-element? sym)
	      (ignore)
	      (reduce-ignore sym))))
      ("~{"
       (push-ignore
	  (let ((str (the-string)))
	     (if (eq? lang 'html)
		 (instantiate::J2SString
		    (escape '(escape))
		    (val str)
		    (loc (the-coord (the-port) (+fx (the-length) 1))))
		 (begin
		    (rgc-buffer-insert-substring! (the-port) str 0 2)
		    (j2s-parser (the-port)
		       (cons* :parser 'tilde-expression conf)))))))
      ("${"
       (push-ignore
	  (let ((str (the-string)))
	     (if (eq? lang 'html)
		 (instantiate::J2SString
		    (escape '(escape))
		    (val str)
		    (loc (the-coord (the-port) (+fx (the-length) 1))))
		 (begin
		    (rgc-buffer-insert-substring! (the-port) str 0 2)
		    (j2s-parser (the-port)
		       (cons* :parser 'dollar-expression conf)))))))
      (else
       (let ((c (the-failure)))
	  (when debug
	     (tprint "ERR: " (dump-stack stack)))
	  (if (eof-object? c)
	      (let ((oe (find element? stack)))
		 (if oe
		     (let* ((tok (element-tag oe))
			    (otag (token-value tok)))
			(match-case (token-loc tok)
			   ((at ?name ?pos)
			    (xml-parse-error "Premature EOF"
			       (format "closing ~a expected" otag)
			       name
			       pos))
			   (else
			    (xml-parse-error "Premature EOF"
			       (format "closing <~a> expected" otag)
			       (input-port-name (the-port))
			       (input-port-position (the-port))))))
		     (xml-parse-error "Premature EOF"
			""
			(input-port-name (the-port))
			(input-port-position (the-port)))))
	      (xml-parse-error "Illegal character"
		 (error-line c (the-port))
		 (input-port-name (the-port))
		 (input-port-position (the-port))))))))

;*---------------------------------------------------------------------*/
;*    make-dom-create ...                                              */
;*---------------------------------------------------------------------*/
(define (make-dom-create tag::pair attributes body lang conf)
   
   (define (debug-init-val loc name)
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
				  (loc loc))))
		       (instantiate::J2SDataPropertyInit
			  (loc loc)
			  (name (instantiate::J2SString
				   (val "name")
				   (loc loc)))
			  (val (instantiate::J2SString
				  (val (symbol->string name))
				  (loc loc))))))))
	 (else
	  (instantiate::J2SUndefined
	     (loc loc)))))
   
   (define (debug-init loc name)
      (instantiate::J2SDataPropertyInit
	 (loc loc)
	 (name (instantiate::J2SString
		  (val "%location")
		  (loc loc)))
	 (val (debug-init-val loc name))))
   
   (define (hopautohead-init loc)
      (instantiate::J2SDataPropertyInit
	 (loc loc)
	 (name (instantiate::J2SString
		  (val "hopautohead")
		  (loc loc)))
	 (val (instantiate::J2SBool
		 (val #f)
		 (loc loc)))))
   
   (define (html? tag)
      (when (symbol? (token-value tag))
	 (memq (token-value tag) '(<html> <HTML>))))
   
   (let ((attrs '())
	 (abody '()))
      (for-each (lambda (x)
		   (cond
		      ((isa? x J2SDataPropertyInit)
		       (set! attrs (cons x attrs)))
		      ((isa? x J2SExpr)
		       (with-access::J2SExpr x (loc)
			  (let ((xp (instantiate::J2SDataPropertyInit
				       (loc loc)
				       (name x)
				       (val (instantiate::J2SString
					       (loc loc)
					       (val ""))))))
			     (set! attrs (cons xp attrs)))))
		      (else
		       (let ((loc (token-loc tag)))
			  (xml-parse-error "Wrong attribute"
			     (token-value tag)
			     (cadr loc)
			     (caddr loc))))))
	 attributes)
      (let* ((loc (token-loc tag))
	     (inits (reverse! attrs))
	     (dbg (> (config-get conf :debug 0) 0))
	     (inits (if dbg
			(cons (debug-init loc (token-value tag)) inits)
			inits))
	     (inits (if (or (eq? lang 'hopscript) (not (html? tag)))
			inits
			(cons (hopautohead-init loc) inits)))
	     (a (cond
		   ((null? inits)
		    (instantiate::J2SUndefined
		       (loc loc)))
		   ((null? abody)
		    (instantiate::J2SObjInit
		       (loc loc)
		       (inits inits)))
		   (else
		    (instantiate::J2SObjInit
		       (loc loc)
		       (inits inits))))))
	 (instantiate::J2SCall
	    (loc loc)
	    (fun (j2s-tag->expr tag #t))
	    (args (cons a body))))))

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
      ((+ (out " ~$\t\n\r<>(){}[]@!\"'/"))
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
			(val ""))))
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
