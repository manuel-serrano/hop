;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/syntax.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Mon Apr 10 10:16:12 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The HOP wiki syntax tools                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwiki_syntax
   
   (library hop)

   (static  (class state
	       markup::symbol
	       syntax::procedure
	       (expr::pair-nil (default '()))
	       value::obj)
	    (class expr::state)
	    (class block::state))
   
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
	       (p::procedure (default <P>))
	       (ol::procedure (default <OL>))
	       (ul::procedure (default <UL>))
	       (li::procedure (default <LI>))
	       (b::procedure (default <B>))
	       (em::procedure (default <EM>))
	       (u::procedure (default <U>))
	       (del::procedure (default <DEL>))
	       (tt::procedure (default <TT>))
	       (code::procedure (default <CODE>))
	       (math::procedure (default <PRE>))
	       (pre::procedure (default <PRE>))
	       (table::procedure (default <TABLE>))
	       (tr::procedure (default <TR>))
	       (th::procedure (default <TH>))
	       (td::procedure (default <TD>))
	       (specials::procedure (default (lambda (id) #f))))
	    
	    (wiki-string->hop ::bstring #!optional syntax)))

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
		  '()))))

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
   (regular-grammar ((blank (in " \t"))
		     syn state result)
      
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
      
      (define (enter-block! st fun value)
	 (let ((st (instantiate::block
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
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
		    (el '()))
	    (if (null? st)
		(begin
		   (set! state '())
		   (add-expr! el))
		(with-access::state (car st) (markup syntax expr)
		   (if (eq? s (car st))
		       (let* ((ar (reverse! (cons el expr)))
			      (ne (apply syntax ar args)))
			  (set! state (cdr st))
			  (add-expr! ne))
		       (let ((ne (syntax (reverse! (cons el expr)))))
			  (loop (cdr st) ne)))))))

      ;; table cell
      (define (table-first-row-cell char rightp)
	 (let ((tc (if (char=? char #\^)
		       (wiki-syntax-th syn)
		       (wiki-syntax-td syn))))
	    (unless (is-state? 'table)
	       (enter-block! 'table (wiki-syntax-table syn) #f))
	    (enter-expr! 'tr (wiki-syntax-tr syn) #f)
	    (enter-expr! 'tc tc rightp)
	    (ignore)))

      (define (table-last-row-cell char leftp cs)
	 (let ((st (in-state 'tc)))
	    (if (state? st)
		(let ((align (cond
				((state-value st) (if leftp "center" "right"))
				(leftp "left")
				(else "center"))))
		   (if (>fx cs 1)
		       (unwind-state! st :colspan cs :align align)
		       (unwind-state! st :align align))
		   (pop-state!)
		   (ignore))
		(begin
		   (add-expr! (the-string))
		   (ignore)))))

      (define (table-cell char leftp rightp cs)
	 (let ((st (in-state 'tc)))
	    (if (state? st)
		(let ((align (cond
				((state-value st) (if leftp "center" "right"))
				(leftp "left")
				(else "center")))
		      (tc (if (char=? char #\^)
			      (wiki-syntax-th syn)
			      (wiki-syntax-td syn))))
		   (if (>fx cs 1)
		       (unwind-state! st :colspan cs :align align)
		       (unwind-state! st :align align))
		   (enter-expr! 'tc tc rightp)
		   (ignore))
		(begin
		   (add-expr! (the-string))
		   (ignore)))))

      ;; a blank line: end of expr
      ((bol (: (? #\Return) #\Newline))
       (let ((st (in-bottom-up-state (lambda (n _) (expr? n)))))
	  (when st
	     (unwind-state! st)))
       (add-expr! (the-string))
       (ignore))

      ;; two consecutive blank lines: end of block
      ((bol (= 2 (: (? #\Return) #\Newline)))
       (let ((st (in-state (lambda (n _) (block? n)))))
	  (when st
	     (unwind-state! st)))
       (add-expr! (the-string))
       (ignore))

      ;; three consecutive blank lines: end of everything
      ((bol (= 3 (: (? #\Return) #\Newline)))
       (unwind-state! #f)
       (add-expr! (the-string))
       (ignore))

      ;; simple text
      ((+ (out "<>+^|*=/_-$#,`'() \\\n"))
       (add-expr! (the-string))
       (ignore))

      ;; single escape characters
      ((in "<>+*=/_-$#,`'() \\\n")
       (add-expr! (the-string))
       (ignore))

      ;; special escape markups
      ((bol (: "<" (out #\> #\/) (* (out #\>)) ">"))
       (let* ((id (the-symbol))
	      (proc ((wiki-syntax-specials syn) id)))
	  (if (procedure? proc)
	      (let ((title (read-line (the-port))))
		 (enter-state! id (lambda (el . _) (proc title el)) #f))
	      (add-expr! (the-string)))
	  (ignore)))

      ((bol (: "</" (+ (out #\>)) ">"))
       (let* ((s (the-substring 2 (the-length)))
	      (id (symbol-append '< (string->symbol s)))
	      (proc ((wiki-syntax-specials syn) id)))
	  (if (procedure? proc)
	      (let ((st (in-state id)))
		 (if (state? st)
		     (unwind-state! st)
		     (add-expr! (the-string))))
	      (add-expr! (the-string)))
	  (ignore)))
       
      ;; sectionning
      ((bol (>= 2 #\=))
       (let* ((id (the-symbol))
	      (lv (-fx (the-length) 2))
	      (hx (case lv
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
	      (st (in-bottom-up-state (lambda (s _)
					 (with-access::state s (markup value)
					    (and (eq? markup 'section)
						 (>=fx value lv)))))))
	  (when st (unwind-state! st))
	  (enter-state! 'section sx lv)
	  (enter-expr! '== hx #f)
	  (ignore)))
      
      ((>= 2 #\=)
       (let ((st (in-state '==)))
	  (if st
	      (unwind-state! st)
	      (add-expr! (the-string))))
       (ignore))

      ;; verbatim mode
      ((bol (: "  " (* (in " \t")) (? (: (out "*- \t\n") (* all)))))
       (unless (is-state? 'pre)
	  (enter-block! 'pre (wiki-syntax-pre syn) #f))
       (add-expr! (the-substring 2 (the-length)))
       (ignore))

      ;; itemize/enumerate
      ((bol (: "  " (* " ") (in "*-")))
       (let* ((s (the-substring (-fx (the-length) 1) (the-length)))
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
		     (enter-block! id (wiki-syntax-ul syn) #f)
		     (enter-block! id (wiki-syntax-ol syn) #f))))
	  (enter-expr! 'li (wiki-syntax-li syn) val))
       (ignore))

      ;; tables
      ((bol (in "^|"))
       (table-first-row-cell (the-character) #f))

      ((bol (: (in "^|") "  "))
       (table-first-row-cell (the-character) #t))

      ;; table cells
      ((: (+ (in "^|")) (* (in " \t")) (? #\Return) #\Newline)
       (let* ((str (the-string))
	      (cs (string-index str " \t\r\n")))
	  (table-last-row-cell (the-character) #f cs)))

      ((: "  " (+ (in "^|")) (* (in " \t")) (? #\Return) #\Newline)
       (let* ((str (the-substring 2 (the-length)))
	      (cs (string-index str " \t\r\n")))
	  (table-last-row-cell (the-character) #t cs)))

      ((+ (in "^|"))
       (table-cell (the-character) #f #f (the-length)))

      ((: (+ (in "^|")) "  ")
       (table-cell (the-character) #f #t (-fx (the-length) 2)))

      ((: "  " (+ (in "^|")) "  ")
       (table-cell (string-ref (the-substring 2 3) 0)
		   #t #t (-fx (the-length) 4)))

      ((: "  " (+ (in "^|")))
       (table-cell (string-ref (the-substring 2 3) 0)
		   #t #f (-fx (the-length) 2)))
      
      ;; standard markups
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
      ("--"
       (let ((s (in-state '--)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! '-- (wiki-syntax-del syn) #f)
		 (ignore)))))
      ("$$"
       (let ((s (in-state '$$)))
	  (if s
	      (begin
		 (unwind-state! s)
		 (ignore))
	      (begin
		 (enter-expr! '$$ (wiki-syntax-math syn) #f)
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
      ("(("
       (enter-expr! 'code (wiki-syntax-code syn) #f)
       (ignore))
      ("))"
       (let ((s (in-state 'code)))
	  (when s (unwind-state! s)))
       (ignore))

      ;; embedded hop
      (",("
       (rgc-buffer-unget-char (the-port) (char->integer #\())
       (with-handler
	  (lambda (e)
	     (exception-notify e)
	     (add-expr! (<SPAN> "***PARSE-ERROR***" (string (the-failure)))))
	  (let ((expr (hop-read (the-port))))
	     (with-handler
		(lambda (e)
		   (exception-notify e)
		   (add-expr! (<SPAN> "***EVAL-ERROR***"
				      (with-output-to-string
					 (lambda ()
					    (write expr))))))
		(add-expr! (eval expr)))))
       (ignore))
       
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (begin
		 (add-expr! (unwind-state! #f))
		 (the-result))
	      (wiki-read-error "Illegal character" (string c) (the-port)))))))


   

   
