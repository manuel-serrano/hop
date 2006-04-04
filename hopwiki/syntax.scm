;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/syntax.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Tue Apr  4 07:52:22 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The HOP wiki syntax tools                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwiki_syntax
   
   (library hop)
   
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
	       (pre::procedure (default <PRE>)))
	    
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
	 (and (pair? state) (eq? (vector-ref (car state) 0) st)))
      
      (define (enter-state! st fun value)
	 (set! state (cons (vector st fun '() value) state)))
      
      (define (exit-state!)
	 (let ((st (car state)))
	    (let ((el ((vector-ref st 1) (reverse! (vector-ref st 2)))))
	       (set! state (cdr state))
	       el)))

      (define (state-value)
	 (vector-ref (car state) 3))
      
      (define (abort-states!)
	 (let loop ((st state)
		    (el '()))
	    (if (null? st)
		(begin
		   (set! state '())
		   el)
		(let* ((s (car st))
		       (kons (vector-ref s 1))
		       (els (vector-ref s 2)))
		   (loop (cdr st) (kons (reverse! (cons el els))))))))

      (define (abort-states-until! stop)
	 (let loop ((st state)
		    (el '()))
	    (if (null? st)
		(begin
		   (set! state '())
		   el)
		(let* ((s (car st))
		       (id (vector-ref s 0))
		       (kons (vector-ref s 1))
		       (els (vector-ref s 2))
		       (nel (kons (reverse! (cons el els)))))
		   (if (eq? id stop)
		       (begin
			  (set! state (cdr st))
			  nel)
		       (loop (cdr st) nel))))))

      (define (add-expr! str)
	 (if (pair? state)
	     (let ((st (car state)))
		(vector-set! st 2 (cons str (vector-ref st 2))))
	     (add-result! str)))
      
      ;; result
      (define (add-result! res)
	 (set! result (cons res result)))
      
      (define (the-result)
	 (reverse! result))
      
      ;; paragraph
      ((: "\\\\" (: (? #\Return) #\Newline))
       (add-result! (abort-states!))
       (enter-state! 'p (wiki-syntax-p syn) #f)
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
      ((+ (out "*=/_-$,`'() \\\n"))
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
		((or (not (in-state? 'li)) (>fx val (state-value)))
		 (if (eq? st 'ul)
		     (enter-state! st (wiki-syntax-ul syn) #f)
		     (enter-state! st (wiki-syntax-ol syn) #f))
		 (enter-state! 'li (wiki-syntax-li syn) val)
		 (ignore))
		((<fx val (state-value))
		 (add-expr! (exit-state!))
		 (add-expr! (exit-state!))
		 (loop))
		(else
		 (add-expr! (exit-state!))
		 (enter-state! 'li (wiki-syntax-li syn) val)
		 (ignore))))))
      
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
	     (add-expr! (<SPAN> "***PARSE-ERROR***")))
	  (add-expr! (eval (hop-read (the-port)))))
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


   

   
