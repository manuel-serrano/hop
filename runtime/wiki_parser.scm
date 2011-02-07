;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/wiki_parser.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Sun Feb  6 19:37:10 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP wiki syntax tools                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki-parser
   
   (library web)
   
   (import  __hop_xml-types
	    __hop_html-base
	    __hop_html-img
	    __hop_mathml
	    __hop_param
	    __hop_read
	    __hop_charset
	    __hop_wiki-syntax)
   
   (static  (class state
	       markup::symbol
	       syntax::procedure
	       (expr::pair-nil (default '()))
	       value::obj)
	    (class expr::state)
	    (class block::state
	       (is-subblock read-only (default #f)))
	    (class plugin::state))

   (export  (wiki-string->hop ::bstring #!key
			      syntax
			      (charset (hop-locale))
			      env)
	    (wiki-file->hop ::bstring #!key
			    syntax
			    (charset (hop-locale))
			    env)
	    (wiki-input-port->hop ::input-port
				  #!key
				  syntax
				  (charset (hop-locale))
				  env)
	    (wiki-name::bstring ::obj)))
   
;*---------------------------------------------------------------------*/
;*    *default-syntax* ...                                             */
;*---------------------------------------------------------------------*/
(define *default-syntax*
   (instantiate::wiki-syntax))

;*---------------------------------------------------------------------*/
;*    *wiki-debug* ...                                                 */
;*---------------------------------------------------------------------*/
(define *wiki-debug* 0)

;*---------------------------------------------------------------------*/
;*    wiki-debug? ...                                                  */
;*---------------------------------------------------------------------*/
(define (wiki-debug?)
   (or (>=fx *wiki-debug* 1) (>=fx (bigloo-debug) 5)))

;*---------------------------------------------------------------------*/
;*    wiki-debug2? ...                                                 */
;*---------------------------------------------------------------------*/
(define (wiki-debug2?)
   (or (>=fx *wiki-debug* 1) (>=fx (bigloo-debug) 6)))

;*---------------------------------------------------------------------*/
;*    wiki-string->hop ...                                             */
;*---------------------------------------------------------------------*/
(define (wiki-string->hop string #!key syntax (charset (hop-locale)) env)
   (let ((ip (open-input-string string)))
      (unwind-protect
	 (wiki-input-port->hop ip :syntax syntax :charset charset :env env)
	 (close-input-port ip))))

;*---------------------------------------------------------------------*/
;*    wiki-file->hop ...                                               */
;*---------------------------------------------------------------------*/
(define (wiki-file->hop file #!key syntax (charset (hop-locale)) env)
   (with-input-from-loading-file file
      (lambda ()
	 (wiki-input-port->hop (current-input-port)
			       :syntax syntax
			       :charset charset))))

;*---------------------------------------------------------------------*/
;*    wiki-input-port->hop ...                                         */
;*---------------------------------------------------------------------*/
(define (wiki-input-port->hop iport #!key syntax (charset (hop-locale)) env)
   (let ((conv (if (procedure? charset)
		   charset
		   (charset-converter! charset (hop-charset)))))
      (cond
	 ((wiki-syntax? syntax)
	  (with-loading-file (input-port-name iport)
			     (lambda ()
				((wiki-syntax-posthook syntax)
				 (cons
				  ((wiki-syntax-prehook syntax))
				  (read/rp *wiki-grammar*
					   iport
					   syntax
					   '()
					   '()
					   0
					   0
					   conv
					   env))))))
	 ((not syntax)
	  (with-loading-file (input-port-name iport)
			     (lambda ()
				(read/rp *wiki-grammar*
					 iport
					 *default-syntax*
					 '()
					 '()
					 0
					 0
					 conv
					 env))))
	 (else
	  (error "wiki-input-port->hop" "Illegal syntax" syntax)))))

;*---------------------------------------------------------------------*/
;*    wiki-read-error ...                                              */
;*---------------------------------------------------------------------*/
(define (wiki-read-error msg obj port)
   (raise (instantiate::&io-read-error
	     (fname (input-port-name port))
	     (location (input-port-position port))
	     (proc "wiki-parser")
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    wiki-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (wiki-name expr)
   (let* ((s (with-output-to-string
		(lambda ()
		   (let loop ((e expr))
		      (cond
			 ((pair? e)
			  (for-each loop e))
			 ((xml-markup? e)
			  (loop (xml-markup-body e)))
			 (else
			  (display e)))))))
	  (s2 (pregexp-replace* "  |\n" s " ")))
      (pregexp-replace* "^ +| +$" s2 "")))

;*---------------------------------------------------------------------*/
;*    normalize-string ...                                             */
;*---------------------------------------------------------------------*/
(define (normalize-string str)
   (let ((b (string-skip str " \t"))
	 (e (string-skip-right str " \t")))
      (cond
	 ((not b)
	  "")
	 ((=fx b 0)
	  (if (=fx e (-fx (string-length str) 1))
	      str
	      (string-shrink! str (+fx 1 e))))
	 ((=fx e (-fx (string-length str) 1))
	  (substring str b))
	 (else
	  (substring str b (+fx 1 e))))))

;*---------------------------------------------------------------------*/
;*    remove-surrounding-spaces ...                                    */
;*---------------------------------------------------------------------*/
(define (remove-surrounding-spaces l)
   (let loop ((l l)
	      (mode 'all))
      (cond
	 ((string? l)
	  (case mode
	     ((all)
	      (normalize-string l))
	     ((head)
	      (let ((b (string-skip l " \t")))
		 (if b
		     (substring l b)
		     l)))
	     ((tail)
	      (let ((b (string-skip-right l " \t")))
		 (if b
		     (string-shrink! l (+fx b 1))
		     "")))
	     (else
	      l)))
	 ((pair? l)
	  (if (null? (cdr l))
	      (list (loop (car l) mode))
	      (cons (loop (car l) 'head)
		    (let liip ((l (cdr l)))
		       (if (null? (cdr l))
			   (list (loop (car l) 'tail))
			   (cons (car l) (liip (cdr l))))))))
	 (else
	  l))))

;*---------------------------------------------------------------------*/
;*    wiki-parse-ident ...                                             */
;*---------------------------------------------------------------------*/
(define (wiki-parse-ident str)
   (let ((i (string-index str #\@)))
      (cond
	 ((not i)
	  (values str #f))
	 ((=fx i 0)
	  (values #f (substring str 1 (string-length str))))
	 (else
	  (values (substring str 0 i)
		  (substring str (+fx i 1) (string-length str)))))))

;*---------------------------------------------------------------------*/
;*    *wiki-grammar* ...                                               */
;*---------------------------------------------------------------------*/
(define *wiki-grammar*
   (regular-grammar ((punct (in "+*=/_-$#%!`'"))
		     (blank (in "<>^|:~;,(){}[] \n"))
		     (letter (out "<>+^|*=/_-$#%:~;,\"`'(){}[]! \\\n"))
		     syn state result trcount dbgcount charset env)

      ;; eval-wiki
      (define (eval-wiki exp)
	 (if (pair? env)
	     (eval `(let (,env) ,exp))
	     (eval exp)))
      
      ;; misc
      (define (the-html-string)
	 (html-string-encode (charset (the-string))))
      
      (define (the-html-substring start end)
	 (html-string-encode (charset (the-substring start end))))
      
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
	 (when (wiki-debug?)
	    (set! dbgcount (+fx 1 dbgcount))
	    (fprint (current-error-port)
		    (make-string (length state) #\space) ">>> " st
		    "." dbgcount " [state] "
		    (map state-markup state)))
	 (let ((st (instantiate::state
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
	    (set! state (cons st state))))
      
      (define (enter-plugin! st fun value)
	 (when (wiki-debug?)
	    (set! dbgcount (+fx 1 dbgcount))
	    (fprint (current-error-port)
		    (make-string (length state) #\space) ">>> " st
		    "." dbgcount " [plugin] "
		    (map state-markup state)))
	 (let ((st (instantiate::plugin
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
	    (set! state (cons st state))))
      
      (define (enter-expr! st fun value)
	 (when (wiki-debug2?)
	    (set! dbgcount (+fx 1 dbgcount))
	    (fprint (current-error-port)
		    (make-string (length state) #\space) ">>> " st
		    "." dbgcount " [expr] "
		    (map state-markup state)))
	 (let ((st (instantiate::expr
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
	    (set! state (cons st state))))
      
      (define (enter-block! st fun value s)
	 (when (wiki-debug?)
	    (set! dbgcount (+fx 1 dbgcount))
	    (fprint (current-error-port)
		    (make-string (length state) #\space) ">>> " st
		    "." dbgcount " [block] "
		    (map state-markup state)))
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
	 (when (and (wiki-debug?) (or (block? s) (wiki-debug2?)))
	    (fprint (current-error-port)
		    (make-string (max 0 (- (length state) 1)) #\space) "<<< "
		    (when s (state-markup s)) " "
		    (map state-markup state)))
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
      (define (table-first-row-cell char rightp id class)
	 (let ((tc (if (char=? char #\^)
		       (wiki-syntax-th syn)
		       (wiki-syntax-td syn))))
	    (unless (is-state? 'table)
	       (set! trcount 0)
	       (enter-block! 'table
			     (if (or class id)
				 (lambda l
				    (apply (wiki-syntax-table syn)
					   :id id :class class
					   l))
				 (wiki-syntax-table syn))
			     #f #f))
	    (enter-expr! 'tr
			 (lambda exp
			    (let ((cl (if (evenfx? trcount)
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

      (define include-grammar
	 (regular-grammar (end name proc)
	    ((: "</" (+ (out #\>)) ">")
	     (if (eq? (the-symbol) end)
		 (let* ((name (apply string-append (reverse! name)))
			(path (if (substring-at? name ",(" 0)
				  (let ((e (substring name 1 (string-length name))))
				     (with-input-from-string e
					(lambda ()
					   (eval-wiki (read)))))
				  (let ((dir (dirname (input-port-name (the-port)))))
				     (find-file/path name (list "." dir))))))
		    (cond
		       ((and (string? path) (file-exists? path))
			(proc path))
		       ((string? path)
			(warning "wiki-parser" "Can't find file: " path)
			(add-expr! ((wiki-syntax-pre syn)
				    "File not found -- " path)))
		       (else
			(warning "wiki-parser" "Can't find file: " name)
			(add-expr! ((wiki-syntax-pre syn)
				    "Cannot find file in path -- " name)))))))
	    ((+ (or (out #\<) (: "<" (out "/"))))
	     (set! name (cons (the-string) name))
	     (ignore))
	    ((+ #\<)
	     (set! name (cons (the-string) name))
	     (ignore))))

      (define skip-space-grammar
	 (regular-grammar ()
	    ((+ (in " \t")) #unspecified)
	    (else #unspecified)))
      
      (define (link-val s)
	 (cond
	    ((and (>fx (string-length s) 3) (substring-at? s ",(" 0))
	     (with-input-from-string (substring s 1 (string-length s))
		(lambda ()
		   (with-handler
		      (lambda (e)
			 (exception-notify e)
			 "")
		      (eval-wiki (hop-read (current-input-port)))))))
	    ((and (string? s)
		  (>fx (string-length s) 0)
		  (not (char=? (string-ref s 0) #\/))
		  (not (char=? (string-ref s 0) #\.)))
	     (string-append (the-loading-dir) "/" s))
	    (else
	     s)))

      ;; continuation lines
      ((: #\\ (? #\Return) #\Newline)
       (ignore))
      
      ;; a blank line: end of expr
      ((bol (: (? #\Return) #\Newline))
       (let ((st (in-bottom-up-state (lambda (n _) (expr? n)))))
	  (cond
	     (st
	      (unwind-state! st))
	     ((is-state? 'p)
	      (pop-state!))))
       (add-expr! (the-html-string))
       (ignore))
      ((bol (: (>= 2 (in " \t")) (? #\Return) #\Newline))
       (let ((st (or (in-bottom-up-state (lambda (n _) (expr? n)))
		     (is-state? 'p))))
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
      ((+ (or letter (: punct letter) (: #\space letter)))
       (add-expr! (the-html-string))
       (ignore))

      ;; hyphen
      ((: #\\ #\\)
       (add-expr! ((wiki-syntax-hyphen syn) "\\\\"))
       (ignore))

      ;; paragraphs
      ((bol (: "~~" (? (: #\: (+ (out " \t\n"))))))
       (when (is-state? 'p) (pop-state!))
       (let ((args (if (=fx (the-length) 2)
		       '()
		       (multiple-value-bind (ident class)
			  (wiki-parse-ident (the-substring 3 (the-length)))
			  `(:class ,class :id ,ident)))))
	  (enter-block! 'p
			(lambda expr
			   (let ((rev (reverse! expr)))
			      (apply (wiki-syntax-p syn)
				     (append args (reverse! rev)))))
			#f
			#f))
       (read/rp skip-space-grammar (the-port))
       (ignore))
      
      ;; sections
      ((bol (: (>= 2 #\=) (? (: #\: (+ (out " \t\n"))))))
       (let* ((str (the-string))
	      (len (the-length))
	      (i (string-index str #\:))
	      (lv (if i (-fx i 2) (-fx (the-length) 2))))
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
		     (hx (if i
			     (multiple-value-bind (id cla)
				(wiki-parse-ident (substring str
							     (+fx i 1)
							     (the-length)))
				(lambda l
				   (apply hx (cons* :id id :class cla l))))
			     hx))
		     (sx (case lv
			    ((3) (wiki-syntax-section4 syn))
			    ((2) (wiki-syntax-section3 syn))
			    ((1) (wiki-syntax-section2 syn))
			    ((0) (wiki-syntax-section1 syn))
			    (else (wiki-syntax-section5 syn))))
		     (st (or (in-bottom-up-state
			      (lambda (s _)
				 (with-access::state s (markup value)
				    (and (eq? markup 'section)
					 (>=fx value lv)))))
			     (in-state
			      (lambda (s n)
				 (let loop ((s s)
					    (n n))
				    (when (pair? n)
				       (if (plugin? s)
					   (loop (car n) (cdr n))
					   (with-access::state (car n)
						 (markup value)
					      (and (eq? markup 'section)
						   (<fx value lv)))))))))))
		 (when st (unwind-state! st))
		 (enter-state! 'section sx lv)
		 (enter-expr! '==
			      (lambda expr
				 (let ((name (wiki-name expr)))
				    (when (wiki-debug?)
				       (fprint (current-error-port) ";;" name))
				    (list (<A> :name name)
					  (apply hx :name name
						 (remove-surrounding-spaces expr)))))
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
       ;; if we are in a pre, just add the entire line
       (if (is-state? 'pre)
	   (begin
	      (add-expr! (the-html-substring 2 (the-length)))
	      (add-expr! (html-string-encode (charset (read-line (the-port)))))
	      (add-expr! "\n"))
 	   (begin
              ;; this is a common mistake so we impose the paragraph ending.
	      (when (is-state? 'p) (pop-state!))
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
		 (enter-block! 'li (wiki-syntax-li syn) val #t))))
       (ignore))

      ;; comments
      ((bol (: (or ";*" ";;") (+ all) (? #\Newline)))
       (ignore))

      ;; tables
      ((bol (in "^|"))
       (table-first-row-cell (the-character) #f #f #f))
      ((bol (: (in "^|") (: ":" (+ (out " \t\r\n")))))
       (multiple-value-bind (id class)
	  (wiki-parse-ident (the-substring 2 (the-length)))
	  (table-first-row-cell (the-character) #f id class)))

      ((bol (: (in "^|") "  "))
       (table-first-row-cell (the-character) #t #f #f))
      ((bol (: (in "^|") (: ":" (+ (out " \t\r\n"))) "  "))
       (multiple-value-bind (id class)
	  (wiki-parse-ident (the-substring 2 -2))
	  (table-first-row-cell (the-character) #t id class)))

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

      ("<<"
       (enter-expr! '<< (wiki-syntax-note syn) #f)
       (ignore))
      (">>"
       (let ((s (in-state '<<)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (add-expr! (the-string))
		 (ignore)))))
      
      ((: "<" (out #\< #\> #\/) (* (out #\/ #\>)) ">")
       (let ((id (the-symbol)))
	  (case id
	     ((<del>)
	      (enter-expr! '</del> (wiki-syntax-del syn) #f)
	      (ignore))
	     ((<sup>)
	      (enter-expr! '</sup> (wiki-syntax-sup syn) #f)
	      (ignore))
	     ((<sub>)
	      (enter-expr! '</sub> (wiki-syntax-sub syn) #f)
	      (ignore))
	     ((<include>)
	      (read/rp include-grammar (the-port)
		       '</include>
		       '()
		       (lambda (path)
			  (add-expr! (wiki-file->hop path
						     :syntax syn
						     :charset charset))))
	      (ignore))
	     ((<include-pre>)
	      (read/rp include-grammar (the-port)
		       '</include-pre>
		       '()
		       (lambda (path)
			  (with-input-from-loading-file path
							(lambda ()
							   (add-expr!
							    ((wiki-syntax-pre syn)
							     (html-string-encode
							      (charset (read-string)))))))))
	      (ignore))
	     (else
	      (let* ((id (the-symbol))
		     (pproc ((wiki-syntax-plugins syn) id)))
		 (if (procedure? pproc)
		     (let* ((/markup (string-append
				      "</" (the-substring 1 (the-length))))
			    (l/markup (string-length /markup))
			    (title (read-line (the-port)))
			    (ltitle (string-length title)))
			(if (substring-at? title /markup (-fx ltitle l/markup))
			    (add-expr!
			     (pproc (the-port)
				    (normalize-string
				     (substring title 0 (-fx ltitle l/markup)))
				    #f))
			    (enter-plugin! id
					   (lambda e
					      (pproc (the-port) (normalize-string title) e))
					   #f)))
		     (let ((sproc ((wiki-syntax-verbatims syn) id)))
			(if (procedure? sproc)
			    (let* ((/markup (string-append
					     "</" (the-substring 1 (the-length))))
				   (l/markup (string-length /markup))
				   (title (read-line (the-port)))
				   (ltitle (string-length title)))
			       (if (substring-at? title /markup (-fx ltitle l/markup))
				   (add-expr!
				    (sproc (the-port)
					   (normalize-string
					    (substring title 0 (-fx ltitle l/markup)))
					   #f))
				   ;; read all the lines up to the closing tag
				   (add-expr!
				    (sproc (the-port)
					   (normalize-string title)
					   #f))))
			    (add-expr! (the-html-string))))))
	      (ignore)))))
      
      ((: "</" (+ (out #\< #\> #\/)) ">")
       (let ((id (the-symbol)))
	  (case id
	     ((</del> </sup> </sub>)
	      (let ((s (in-state id)))
		 (when s (unwind-state! s))
		 (ignore)))
	     (else
	      (let* ((s (the-substring 2 (the-length)))
		     (id (symbol-append '< (string->symbol s)))
		     (proc ((wiki-syntax-plugins syn) id)))
		 (if (procedure? proc)
		     (let ((st (in-state id)))
			(if (state? st)
			    (unwind-state! st)
			    (add-expr! (the-html-string))))
		     (add-expr! (the-html-string)))
		 (ignore))))))
      ((: "<" (+ (out #\< #\> #\/)) "/>")
       (let* ((id (string->symbol (string-append "<" (the-substring 1 -2) ">")))
	      (proc ((wiki-syntax-plugins syn) id)))
	     (if (procedure? proc)
		 (add-expr! (proc (the-port) #f #f))
		 (add-expr! (the-html-string)))
	  (ignore)))

      ;; math
      ((: "$$" (+ (or (out #\$) (: #\$ (out #\$)))) "$$")
       (add-expr! ((wiki-syntax-math syn) (the-substring 2 -2)))
       (ignore))
      
      ;; tt
      ("++"
       (let ((s (in-state 'tt)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! 'tt (wiki-syntax-tt syn) #f)
		 (ignore)))))
      ;; code
      ("%%"
       (let ((s (in-state 'code)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! 'code (wiki-syntax-code syn) #f)
		 (ignore)))))
      
      ;; strike
      ("--"
       (let ((s (in-state 'strike)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! 'strike (wiki-syntax-strike syn) #f)
		 (ignore)))))

      ;; quotes
      (#\"
       (if (in-state 'code)
	   (begin
	      (add-expr! (the-html-string))
	      (ignore))
	   (let ((s (in-state 'quotation)))
	      (if s
		  (begin
		     (unwind-state! s)
		     (ignore))
		  (begin
		     (enter-expr! 'quotation (wiki-syntax-q syn) #f)
		     (ignore))))))

      ;; keywords
      ((: (in " \t") #\: (out " \t\n:)\"'`;#") (* (out " \t\n)\"'`;#")))
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

      ;; links
      ((: "[[" (+ (or (out #\]) (: #\] (out #\])))) "]]")
       (let* ((s (the-substring 2 -2))
	      (i (string-index s "|"))
	      (href (wiki-syntax-href syn)))
	  (define (link-val s)
	     (cond
		((and (>fx (string-length s) 3) (substring-at? s ",(" 0))
		 (with-input-from-string (substring s 1 (string-length s))
		    (lambda ()
		       (with-handler
			  (lambda (e)
			     (exception-notify e)
			     "")
			  (let ((e (eval-wiki (hop-read (current-input-port)))))
			     (values e e))))))
		((or (=fx (string-length s) 0)
		     (and (not (char=? (string-ref s 0) #\/))
			  (not (string-index s #\:))))
		 (values (string-append "#" s) s))
		(else
		 (values s s))))
	  (add-expr!
	   (if (not i)
	       (multiple-value-bind (h n)
		  (link-val s)
		  (href h n))
	       (let ((s2 (substring s (+fx i 1) (string-length s))))
		  (multiple-value-bind (h n)
		     (link-val (substring s 0 i))
		     (href h
			   (wiki-string->hop
			    (substring s (+fx i 1) (string-length s))
			    :syntax syn
			    :charset charset))))))
	  (ignore)))

      ;; anchors
      ((: "##" (+ (or (out #\]) (: #\] (out #\])))) "##")
       (let* ((s (the-substring 2 -2))
	      (i (string-index s "|"))
	      (href (wiki-syntax-href syn)))
	  (add-expr!
	   (if (not i)
	       (let ((href (link-val s)))
		  (<A> :name href))
	       (let ((s2 (substring s (+fx i 1) (string-length s))))
		  (let ((href (link-val (substring s 0 i)))
			(title (html-string-encode
				(charset
				 (substring s (+fx i 1) (string-length s))))))
		     (<A> :name href :title title title)))))
	  (ignore)))

      ;; images
      ((: "{{" (+ (or (out #\}) (: #\} (out #\})))) "}}")
       (let* ((s (the-substring 2 -2))
	      (i (string-index s "|")))
	  (add-expr!
	   (if (not i)
	       (let ((path (link-val s)))
		  (<IMG> :src path :alt s))
	       (let* ((p (substring s 0 i))
		      (path (link-val p))
		      (title (html-string-encode
			      (charset
			       (substring s (+fx i 1) (string-length s))))))
		  (<IMG> :src path :alt p :title title)))))
       (ignore))

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
		(add-expr! (eval-wiki expr)))))
       (ignore))
       
      ;; single escape characters
      ((or punct blank #\\)
       (add-expr! (the-html-string))
       (ignore))

      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (begin
		 (add-expr! (unwind-state! #f))
		 (the-result))
	      (wiki-read-error "Illegal character" (string c) (the-port)))))))
