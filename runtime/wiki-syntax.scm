;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/wiki-syntax.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Mon Apr 23 09:36:54 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP wiki syntax tools                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki-syntax
   
   (library web)

   (import  __hop_xml
	    __hop_read)

   (static  (class state
	       markup::symbol
	       syntax::procedure
	       (expr::pair-nil (default '()))
	       value::obj)
	    (class expr::state)
	    (class block::state
	       (is-subblock read-only (default #f))))
   
   (export  (class wiki-syntax
	       (section1::procedure (default list))
	       (section2::procedure (default list))
	       (section3::procedure (default list))
	       (section4::procedure (default list))
	       (section5::procedure (default list))
	       (h1::procedure (default <H1>))
	       (h2::procedure (default <H2>))
	       (h3::procedure (default <H3>))
	       (h4::procedure (default <H4>))
	       (h5::procedure (default <H5>))
	       (hr::procedure (default <HR>))
	       (p::procedure (default <P>))
	       (ol::procedure (default <OL>))
	       (ul::procedure (default <UL>))
	       (li::procedure (default <LI>))
	       (b::procedure (default <STRONG>))
	       (em::procedure (default <EM>))
	       (u::procedure (default <U>))
	       (del::procedure (default <DEL>))
	       (sub::procedure (default (lambda x
					   (<SUB>
					      (<SPAN>
						 :style "font-size: smaller"
						 x)))))
	       (sup::procedure (default (lambda x
					   (<SUP>
					      (<SPAN>
						 :style "font-size: smaller"
						 x)))))
	       (tt::procedure (default <TT>))
	       (code::procedure (default <CODE>))
	       (math::procedure (default <PRE>))
	       (pre::procedure (default <PRE>))
	       (table::procedure (default <TABLE>))
	       (tr::procedure (default <TR>))
	       (th::procedure (default <TH>))
	       (td::procedure (default <TD>))
	       (href::procedure (default (lambda (href node)
					    (<A> :href href node))))
	       (keyword::procedure (default (lambda (x) x)))
	       (type::procedure (default (lambda (x) x)))
	       (plugins::procedure (default (lambda (id) #f))))

	    (wiki-string->hop ::bstring #!optional syntax)
	    (wiki-file->hop ::bstring #!optional syntax)
	    (wiki-input-port->hop ::input-port #!optional syntax)))

;*---------------------------------------------------------------------*/
;*    *default-syntax* ...                                             */
;*---------------------------------------------------------------------*/
(define *default-syntax*
   (instantiate::wiki-syntax))

;*---------------------------------------------------------------------*/
;*    wiki-string->hop ...                                             */
;*---------------------------------------------------------------------*/
(define (wiki-string->hop string #!optional syntax)
   (with-input-from-string string
      (lambda ()
	 (read/rp *wiki-grammar*
		  (current-input-port)
		  (or syntax *default-syntax*)
		  '()
		  '()
		  0))))

;*---------------------------------------------------------------------*/
;*    wiki-file->hop ...                                               */
;*---------------------------------------------------------------------*/
(define (wiki-file->hop file #!optional syntax)
   (with-input-from-file file
      (lambda ()
	 (wiki-input-port->hop (current-input-port) syntax))))

;*---------------------------------------------------------------------*/
;*    wiki-input-port->hop ...                                         */
;*---------------------------------------------------------------------*/
(define (wiki-input-port->hop iport #!optional syntax)
   (read/rp *wiki-grammar*
	    iport
	    (or syntax *default-syntax*)
	    '()
	    '()
	    0))

;*---------------------------------------------------------------------*/
;*    wiki-read-error ...                                              */
;*---------------------------------------------------------------------*/
(define (wiki-read-error msg obj port)
   (raise (instantiate::&io-read-error
	     (fname (input-port-name port))
	     (location (input-port-position port))
	     (proc 'wiki-parser)
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    *wiki-grammar* ...                                               */
;*---------------------------------------------------------------------*/
(define *wiki-grammar*
   (regular-grammar ((punct (in "+*=/_-$#%"))
		     (blank (in "<>^|:~;,`'(){}[] \\\n"))
		     (letter (out "<>+^|*=/_-$#%:~;,`'(){}[] \\\n"))
		     syn state result trcount)

      ;; misc
      (define (the-html-string)
	 (html-string-encode (the-string)))
      
      (define (the-html-substring start end)
	 (html-string-encode (the-substring start end)))
      
      ;; result and expression
      (define (add-expr! str)
	 (if (pair? state)
	     (with-access::state (car state) (expr)
		(set! expr (cons str expr)))
	     (set! result (cons str result))))

      (define (the-result)
	 (reverse! result))

      ;; state management
      (define (enter-state! st fun value)
	 (let ((st (instantiate::state
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
	    (set! state (cons st state))))
      
      (define (enter-expr! st fun value)
	 (let ((st (instantiate::expr
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
	    (set! state (cons st state))))
      
      (define (enter-block! st fun value s)
	 (let ((st (instantiate::block
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value)
		      (is-subblock s))))
	    (set! state (cons st state))))
      
      (define (is-state? condition)
	 (let ((pred (cond
			((procedure? condition)
			 condition)
			((symbol? condition)
			 (lambda (s _) (eq? (state-markup s) condition)))
			((state? condition)
			 (lambda (s _) (eq? s condition)))
			(else
			 (lambda (s _) #t)))))
	    (and (pair? state) (pred (car state) (cdr state)))))

      (define (%is-in-state states condition)
	 (let ((pred (cond
			((procedure? condition)
			 condition)
			((symbol? condition)
			 (lambda (s _) (eq? (state-markup s) condition)))
			((state? condition)
			 (lambda (s _) (eq? s condition)))
			(else
			 (lambda (s _) #t)))))
	    (let loop ((state states))
	       (and (pair? state)
		    (if (pred (car state) (cdr state))
			(car state)
			(loop (cdr state)))))))
      
      (define (in-state condition)
	 (%is-in-state state condition))

      (define (in-bottom-up-state condition)
	 (%is-in-state (reverse state) condition))

      ;; pop one state
      (define (pop-state!)
	 (when (pair? state)
	    (unwind-state! (car state))))
      
      ;; unwind the state until the stack is empty or the state is found
      (define (unwind-state! s . args)
	 (let loop ((st state)
		    (el #f))
	    (if (null? st)
		(begin
		   (set! state '())
		   (when el (add-expr! el)))
		(with-access::state (car st) (markup syntax expr)
		   (let ((ar (reverse! (if el (cons el expr) expr))))
		      (if (eq? s (car st))
			  (let ((ne (apply syntax ar args)))
			     (set! state (cdr st))
			     (add-expr! ne))
			  (let ((ne (apply syntax ar)))
			     (loop (cdr st) ne))))))))

      ;; table cell
      (define (table-first-row-cell char rightp)
	 (let ((tc (if (char=? char #\^)
		       (wiki-syntax-th syn)
		       (wiki-syntax-td syn))))
	    (unless (is-state? 'table)
	       (set! trcount 0)
	       (enter-block! 'table (wiki-syntax-table syn) #f #f))
	    (enter-expr! 'tr
			 (lambda exp
			    (let ((cl (if (even? trcount)
					  "hopwiki-row-even"
					  "hopwiki-row-odd")))
			       (apply (wiki-syntax-tr syn) :class cl exp)))
			 #f)
	    (enter-expr! 'tc tc rightp)
	    (set! trcount (+fx 1 trcount))
	    (ignore)))

      (define (table-last-row-cell char leftp cs)
	 (let ((st (in-state 'tc)))
	    (if (state? st)
		(let ((align (cond
				((state-value st)
				 (if leftp
				     "text-align:center"
				     "text-align: right"))
				(leftp "text-align: left")
				(else "text-align: center"))))
		   (if (and (fixnum? cs) (>fx cs 1))
		       (unwind-state! st :colspan cs :style align)
		       (unwind-state! st :style align))
		   (pop-state!)
		   (ignore))
		(begin
		   (add-expr! (the-html-string))
		   (ignore)))))

      (define (table-cell char leftp rightp cs)
	 (let ((st (in-state 'tc)))
	    (if (state? st)
		(let ((align (cond
				((state-value st)
				 (if leftp
				     "text-align:center"
				     "text-align: right"))
				(leftp "text-align: left")
				(else "text-align: center")))
		      (tc (if (char=? char #\^)
			      (wiki-syntax-th syn)
			      (wiki-syntax-td syn))))
		   (if (>fx cs 1)
		       (unwind-state! st :colspan cs :style align)
		       (unwind-state! st :style align))
		   (enter-expr! 'tc tc rightp)
		   (ignore))
		(begin
		   (add-expr! (the-html-string))
		   (ignore)))))

      ;; continuation lines
      ((: #\\ (? #\Return) #\Newline)
       (ignore))
      
      ;; a blank line: end of expr
      ((bol (: (? #\Return) #\Newline))
       (let ((st (in-bottom-up-state (lambda (n _) (expr? n)))))
	  (when st (unwind-state! st)))
       (add-expr! (the-html-string))
       (ignore))
      ((bol (: (>= 2 (in " \t")) (? #\Return) #\Newline))
       (let ((st (in-bottom-up-state (lambda (n _) (expr? n)))))
	  (when st (unwind-state! st)))
       (add-expr! (the-html-substring 2 (the-length)))
       (ignore))

      ;; two consecutive blank lines: end of block
      ((bol (= 2 (: (? #\Return) #\Newline)))
       (let ((st (in-state (lambda (n _)
			      (and (block? n) (not (block-is-subblock n)))))))
	  (when st (unwind-state! st)))
       (add-expr! (the-html-string))
       (ignore))

      ;; three consecutive blank lines: end of everything
      ((bol (= 3 (: (? #\Return) #\Newline)))
       (unwind-state! #f)
       (add-expr! (the-html-string))
       (ignore))

      ;; simple text
      ((+ (or letter (: punct letter)))
       (add-expr! (the-html-string))
       (ignore))

      ;; paragraphs
      ((bol "~~")
       (add-expr! ((wiki-syntax-p syn)))
       (ignore))
      
      ;; sections
      ((bol (>= 2 #\=))
       (let* ((id (the-symbol))
	      (lv (-fx (the-length) 2)))
	  (if (> lv 4)
	      (begin
		 (add-expr! ((wiki-syntax-hr syn)))
		 (ignore))
	      (let* ((hx (case lv
			    ((3) (wiki-syntax-h4 syn))
			    ((2) (wiki-syntax-h3 syn))
			    ((1) (wiki-syntax-h2 syn))
			    ((0) (wiki-syntax-h1 syn))
			    (else (wiki-syntax-h5 syn))))
		     (sx (case lv
			    ((3) (wiki-syntax-section4 syn))
			    ((2) (wiki-syntax-section3 syn))
			    ((1) (wiki-syntax-section2 syn))
			    ((0) (wiki-syntax-section1 syn))
			    (else (wiki-syntax-section5 syn))))
		     (st (in-bottom-up-state
			  (lambda (s _)
			     (with-access::state s (markup value)
				(and (eq? markup 'section)
				     (>=fx value lv))))))
		     (mk (gensym)))
		 (when st (unwind-state! st))
		 (enter-state! 'section sx lv)
		 (enter-expr! '==
			      (lambda expr
				 (list (<A> :name (symbol->string mk))
				       (apply hx expr)))
			      #f)
		 (ignore)))))
      
      ((>= 2 #\=)
       (let ((st (in-state '==)))
	  (if st
	      (unwind-state! st)
	      (add-expr! (the-html-string))))
       (ignore))

      ;; verbatim mode
      ((bol (: "  " (* (in " \t")) (? (: (out "*- \t\n") (* all)))))
       (unless (is-state? 'pre)
	  (enter-block! 'pre
			(lambda expr
			   (let ((rev (reverse! expr)))
			      (if (and (pair? rev)
				       (string? (car rev))
				       (string=? (car rev) "\n"))
				  (apply (wiki-syntax-pre syn)
					 (reverse! (cdr rev)))
				  (apply (wiki-syntax-pre syn)
					 (reverse! rev)))))
			#f
			#f))
       (add-expr! (the-html-substring 2 (the-length)))
       (ignore))

      ;; itemize/enumerate
      ((bol (: "  " (* " ") (in "*-")))
       (let* ((s (the-html-substring (-fx (the-length) 1) (the-length)))
	      (c (string-ref s 0))
	      (val (the-length))
	      (id (if (char=? c #\*) 'ul 'ol))
	      (st (in-state (lambda (s n)
			       (with-access::state s (markup value)
				  (and (eq? markup 'li)
				       (=fx value val)
				       (eq? (state-markup (car n)) id)))))))
	  (if st
	      (unwind-state! st)
	      (let ((st (in-bottom-up-state
			 (lambda (s n)
			    (when (pair? n)
			       (with-access::state (car n) (markup value)
				  (when (and (eq? markup 'li)
					     (>=fx value val))
				     s)))))))
		 (when st (unwind-state! st))
		 (if (char=? c #\*)
		     (enter-block! id (wiki-syntax-ul syn) #f #f)
		     (enter-block! id (wiki-syntax-ol syn) #f #f))))
	  (enter-block! 'li (wiki-syntax-li syn) val #t))
       (ignore))

      ;; comments
      ((bol (: (or ";*" ";;") (+ all)))
       (ignore))

      ;; tables
      ((bol (in "^|"))
       (table-first-row-cell (the-character) #f))

      ((bol (: (in "^|") "  "))
       (table-first-row-cell (the-character) #t))

      ;; table cells
      ((: (+ (in "^|")) (* (in " \t")) (? #\Return) #\Newline)
       (let* ((str (the-html-string))
	      (cs (string-index str " \t\r\n")))
	  (table-last-row-cell (the-character) #f cs)))

      ((: "  " (+ (in "^|")) (* (in " \t")) (? #\Return) #\Newline)
       (let* ((str (the-html-substring 2 (the-length)))
	      (cs (string-index str " \t\r\n")))
	  (table-last-row-cell (the-character) #t cs)))

      ((+ (in "^|"))
       (table-cell (the-character) #f #f (the-length)))

      ((: (+ (in "^|")) "  ")
       (table-cell (the-character) #f #t (-fx (the-length) 2)))

      ((: "  " (+ (in "^|")) "  ")
       (table-cell (string-ref (the-html-substring 2 3) 0)
		   #t #t (-fx (the-length) 4)))

      ((: "  " (+ (in "^|")))
       (table-cell (string-ref (the-html-substring 2 3) 0)
		   #t #f (-fx (the-length) 2)))
      
      ;; font style
      ("**"
       (let ((s (in-state '**)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! '** (wiki-syntax-b syn) #f)
		 (ignore)))))
      ("//"
       (let ((s (in-state '//)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! '// (wiki-syntax-em syn) #f)
		 (ignore)))))
      ("__"
       (let ((s (in-state '__)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! '__ (wiki-syntax-u syn) #f)
		 (ignore)))))
      ("<del>"
       (enter-expr! '<del> (wiki-syntax-del syn) #f)
       (ignore))
      ("</del>"
       (let ((s (in-state '<del>)))
	  (when s (unwind-state! s))
	  (ignore)))
      
      ("<sup>"
       (enter-expr! '<sup> (wiki-syntax-sup syn) #f)
       (ignore))
      ("</sup>"
       (let ((s (in-state '<sup>)))
	  (when s (unwind-state! s))
	  (ignore)))
      
      ("<sub>"
       (enter-expr! '<sub> (wiki-syntax-sub syn) #f)
       (ignore))
      ("</sub>"
       (let ((s (in-state '<sub>)))
	  (when s (unwind-state! s))
	  (ignore)))
      
      ("%%"
       (let ((s (in-state '%%)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! '%% (wiki-syntax-math syn) #f)
		 (ignore)))))
      ("++"
       (let ((s (in-state 'tt)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! 'tt (wiki-syntax-tt syn) #f)
		 (ignore)))))
      ("--"
       (let ((s (in-state 'code)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! 'code (wiki-syntax-tt syn) #f)
		 (ignore)))))

      ;; keywords
      ((: (in " \t") #\: (out " \t\n:") (* (out " \t\n")))
       (add-expr! " ")
       (add-expr! ((wiki-syntax-keyword syn) (the-html-substring 1 (the-length))))
       (ignore))

      ((bol (: #\: (out " \t\n:") (* (out " \t\n(){}[]"))))
       (add-expr! ((wiki-syntax-keyword syn) (the-html-string)))
       (ignore))

      ;; types
      ((: "::" (+ (out " \t\n(){}[]")))
       (add-expr! ((wiki-syntax-type syn) (the-html-string)))
       (ignore))

      ;; code embedding
      ((: "##" (+ (or (out #\#) (: #\# (out #\#)))) "##")
       (let* ((name (the-substring 2 -2))
	      (path (if (substring-at? name ",(" 0)
			(let ((expr (substring name 1 (string-length name))))
			   (with-input-from-string expr
			      (lambda ()
				 (eval (read)))))
			(let ((dir (dirname (input-port-name (the-port)))))
			   (find-file/path name (list "." dir))))))
	  (cond
	     ((and (string? path) (file-exists? path))
	      (with-input-from-file path
		 (lambda ()
		    (add-expr! ((wiki-syntax-pre syn)
				(html-string-encode (read-string)))))))
	     ((string? path)
	      (warning 'wiki-parser "Can't find file" path)
	      (add-expr! ((wiki-syntax-pre syn)
			  "File not found -- " path)))
	     (else
	      (warning 'wiki-parser "Can't find file" path)
	      (add-expr! ((wiki-syntax-pre syn)
			  "Cannot find file in path -- " name))))
	  (ignore)))

      ;; links
      ((: "[[" (+ (or (out #\]) (: #\] (out #\])))) "]]")
       (let* ((s (the-substring 2 -2))
	      (i (string-index s "|"))
	      (href (wiki-syntax-href syn)))
	  (define (link-val s)
	     (cond
		((and (>fx (string-length s) 3)
		      (substring-at? s ",(" 0))
		 (with-input-from-string (substring s 1 (string-length s))
		    (lambda ()
		       (with-handler
			  (lambda (e)
			     (exception-notify e)
			     "")
			  (let ((e (eval (hop-read (current-input-port)))))
			     (values e e))))))
		((or (=fx (string-length s) 0)
		     (and (not (char=? (string-ref s 0) #\/))
			  (string-index s #\:) -1))
		 (values (string-append "#" s) s))
		(else
		 (values s s))))
	  (add-expr!
	   (if (or (not i) (=fx i -1))
	       (multiple-value-bind (h n)
		  (link-val s)
		  (href h n))
	       (let ((s2 (substring s (+fx i 1) (string-length s))))
		  (multiple-value-bind (h n)
		     (link-val (substring s 0 i))
		     (href h
			   (wiki-string->hop
			    (substring s (+fx i 1) (string-length s))
			    syn))))))
	  (ignore)))

      ;; embedded hop
      (",("
       (rgc-buffer-unget-char (the-port) (char->integer #\())
       (with-handler
	  (lambda (e)
	     (exception-notify e)
	     (add-expr! (<SPAN> "*** PARSE-ERROR:" (string (the-failure)))))
	  (let ((expr (hop-read (the-port))))
	     (with-handler
		(lambda (e)
		   (exception-notify e)
		   (add-expr! (<SPAN> "*** EVAL-ERROR:"
				      (let ((m (eval-module)))
					 (if (evmodule? m)
					     (evmodule-name m)
					     "top-level"))
				      ":"
				      (with-output-to-string
					 (lambda ()
					    (write expr))))))
		(add-expr! (eval expr)))))
       (ignore))
       
      ;; special escape markups
      ((bol (: "<" (out #\> #\/) (* (out #\>)) ">"))
       (let* ((id (the-symbol))
	      (proc ((wiki-syntax-plugins syn) id)))
	  (if (procedure? proc)
	      (let* ((/markup (string-append
			       "</" (the-substring 1 (the-length))))
		     (l/markup (string-length /markup))
		     (title (read-line (the-port)))
		     (ltitle (string-length title)))
		 (if (substring-at? title /markup (-fx ltitle l/markup))
		     (add-expr!
		      (proc (the-port)
			    (substring title 0 (-fx ltitle l/markup))
			    #f))
		     (enter-state! id
				   (lambda el (proc (the-port) title el))
				   #f)))
	      (add-expr! (the-html-string)))
	  (ignore)))

      ;; ending markup
      ((bol (: "</" (+ (out #\>)) ">"))
       (let* ((s (the-substring 2 (the-length)))
	      (id (symbol-append '< (string->symbol s)))
	      (proc ((wiki-syntax-plugins syn) id)))
	  (if (procedure? proc)
	      (let ((st (in-state id)))
		 (if (state? st)
		     (unwind-state! st)
		     (add-expr! (the-html-string))))
	      (add-expr! (the-html-string)))
	  (ignore)))
       
      ;; single escape characters
      ((or punct blank)
       (add-expr! (the-html-string))
       (ignore))

      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (begin
		 (add-expr! (unwind-state! #f))
		 (the-result))
	      (wiki-read-error "Illegal character" (string c) (the-port)))))))


   

   
