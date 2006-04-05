;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/syntax.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Wed Apr  5 14:02:25 2006 (serrano)                */
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
	       value::obj))
   
   (export  (class wiki-syntax
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
	       (td::procedure (default <TD>)))
	    
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
      
      ;; state management
      (define (in-state? st)
	 (and (pair? state) (eq? (state-markup (car state)) st)))
      
      (define (in-deep-state st)
	 (let loop ((state state))
	    (and (pair? state)
		 (if (eq? (state-markup (car state)) st)
		     (car state)
		     (loop (cdr state))))))
      
      (define (enter-state! st fun value)
	 (let ((st (instantiate::state
		      (markup st)
		      (syntax fun)
		      (expr '())
		      (value value))))
	    (set! state (cons st state))))
      
      (define (exit-state!)
	 (with-access::state (car state) (syntax expr)
	    (let ((el (syntax (reverse! expr))))
	       (set! state (cdr state))
	       el)))

      (define (abort-states!)
	 (let loop ((st state)
		    (el '()))
	    (if (null? st)
		(begin
		   (set! state '())
		   el)
		(with-access::state (car st) (syntax expr)
		   (loop (cdr st) (syntax (reverse! (cons el expr))))))))

      (define (abort-states-until! stop . args)
	 (let loop ((st state)
		    (el '()))
	    (if (null? st)
		(begin
		   (set! state '())
		   el)
		(with-access::state (car st) (markup syntax expr)
		   (let ((nel (apply syntax (reverse! (cons el expr)) args)))
		      (if (eq? markup stop)
			  (begin
			     (set! state (cdr st))
			     nel)
			  (loop (cdr st) nel)))))))

      (define (add-expr! str)
	 (if (pair? state)
	     (with-access::state (car state) (expr)
		(set! expr (cons str expr)))
	     (add-result! str)))
      
      ;; result
      (define (add-result! res)
	 (set! result (cons res result)))
      
      (define (the-result)
	 (reverse! result))

      ;; table cell
      (define (table-first-row-cell char rightp)
	 (let ((tc (if (char=? char #\^)
		       (wiki-syntax-th syn)
		       (wiki-syntax-td syn))))
	    (unless (in-state? 'table)
	       (add-result! (abort-states!))
	       (enter-state! 'table (wiki-syntax-table syn) #f))
	    (enter-state! 'tr (wiki-syntax-tr syn) #f)
	    (enter-state! 'tc tc rightp)
	    (ignore)))
      
      (define (table-last-row-cell char leftp cs)
	 (let ((st (in-deep-state 'tc)))
	    (if (state? st)
		(let ((align (cond
				((state-value st) (if leftp "center" "right"))
				(leftp "left")
				(else "center"))))
		   (if (>fx cs 1)
		       (add-expr! (abort-states-until! 'tc
						       :colspan cs
						       :align align))
		       (add-expr! (abort-states-until! 'tc
						       :align align)))
		   (add-expr! (exit-state!))
		   (ignore))
		(begin
		   (add-expr! (the-string))
		   (ignore)))))
	 
      (define (table-cell char leftp rightp cs)
	 (let ((st (in-deep-state 'tc)))
	    (if (state? st)
		(let ((align (cond
				((state-value st) (if leftp "center" "right"))
				(leftp "left")
				(else "center")))
		      (tc (if (char=? char #\^)
			      (wiki-syntax-th syn)
			      (wiki-syntax-td syn))))
		   (if (>fx cs 1)
		       (add-expr! (abort-states-until! 'tc
						       :colspan cs
						       :align align))
		       (add-expr! (abort-states-until! 'tc
						       :align align)))
		   (enter-state! 'tc tc rightp)
		   (ignore))
		(begin
		   (add-expr! (the-string))
		   (ignore)))))
	 
      ;; continuation mark
      ((: "\\\\" (: (? #\Return) #\Newline))
       (ignore))
      
      ;; newline
      ((bol (: (? #\Return) #\Newline))
       (add-result! (abort-states!))
       (enter-state! 'p
		     (lambda (e)
			(if (null? e)
			    "\n"
			    ((wiki-syntax-p syn) e)))
		     #f)
       (ignore))
      
      ;; simple text
      ((+ (out "^|*=/_-$,`'() \\\n"))
       (add-expr! (the-string))
       (ignore))
      
      ;; sectionning
      ((bol (>= 2 #\=))
       (let ((s (the-symbol)))
	  (if (in-state? s)
	      (begin
		 (add-expr! (exit-state!))
		 (ignore))
	      (let ((constr (case (the-length)
			       ((5) (wiki-syntax-h4 syn))
			       ((4) (wiki-syntax-h3 syn))
			       ((3) (wiki-syntax-h2 syn))
			       ((2) (wiki-syntax-h1 syn))
			       (else (wiki-syntax-h5 syn)))))
		 (add-result! (abort-states!))
		 (enter-state! s constr #f)
		 (ignore)))))
      ((>= 2 #\=)
       (let ((s (the-symbol)))
	  (if (in-state? s)
	      (begin
		 (add-expr! (exit-state!))
		 (ignore))
	      ;; an error
	      (ignore))))

      ;; verbatim mode
      ((bol (: "  " (* (in " \t")) (? (: (out "*- \t") (* all)))))
       (if (in-state? 'pre)
	   (begin
	      (add-expr! (the-substring 2 (the-length)))
	      (ignore))
	   (begin
	      (add-result! (abort-states!))
	      (enter-state! 'pre (wiki-syntax-pre syn) #f)
	      (add-expr! (the-substring 2 (the-length)))
	      (ignore))))

      ;; itemize/enumerate
      ((bol (: "  " (* " ") (in "*-")))
       (let* ((s (the-substring (-fx (the-length) 1) (the-length)))
	      (c (string-ref s 0))
	      (st (if (char=? c #\*) 'ul 'ol))
	      (val (the-length)))
	  (let loop ()
	     (cond
		((or (not (in-state? 'li)) (>fx val (state-value (car state))))
		 (if (eq? st 'ul)
		     (enter-state! st (wiki-syntax-ul syn) #f)
		     (enter-state! st (wiki-syntax-ol syn) #f))
		 (enter-state! 'li (wiki-syntax-li syn) val)
		 (ignore))
		((<fx val (state-value (car state)))
		 (add-expr! (exit-state!))
		 (add-expr! (exit-state!))
		 (loop))
		(else
		 (add-expr! (exit-state!))
		 (enter-state! 'li (wiki-syntax-li syn) val)
		 (ignore))))))

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
       (if (in-state? '**)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '** (wiki-syntax-b syn) #f)
	      (ignore))))
      ("//"
       (if (in-state? '//)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '// (wiki-syntax-em syn) #f)
	      (ignore))))
      ("__"
       (if (in-state? '__)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '__ (wiki-syntax-u syn) #f)
	      (ignore))))
      ("--"
       (if (in-state? '--)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '-- (wiki-syntax-del syn) #f)
	      (ignore))))
      ("$$"
       (if (in-state? '$$)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '$$ (wiki-syntax-math syn) #f)
	      (ignore))))
      ("``"
       (enter-state! 'tt (wiki-syntax-tt syn) #f)
       (ignore))
      ("''"
       (add-expr! (abort-states-until! 'tt))
       (ignore))
      ("(("
       (enter-state! 'code (wiki-syntax-code syn) #f)
       (ignore))
      ("))"
       (add-expr! (abort-states-until! 'code))
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
       
      ;; single escape characters
      ((in "*=/_-$,`'() \\\n")
       (add-expr! (the-string))
       (ignore))

      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (begin
		 (add-result! (abort-states!))
		 (the-result))
	      (wiki-read-error "Illegal character" (string c) (the-port)))))))


   

   
