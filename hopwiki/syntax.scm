;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/syntax.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Mon Apr  3 16:03:39 2006 (serrano)                */
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
	       (pre::procedure (default <PRE>))
	       )
	    
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
      
      (define (enter-state! st fun)
	 (set! state (cons (vector st fun '()) state)))
      
      (define (exit-state!)
	 (let ((st (car state)))
	    (let ((el ((vector-ref st 1) (reverse! (vector-ref st 2)))))
	       (set! state (cdr state))
	       el)))
      
      (define (abort-states!)
	 (let ((els (map (lambda (st)
			    ((vector-ref st 1) (reverse! (vector-ref st 2))))
			 state)))
	    (set! state '())
	    (reverse! els)))

      (define (add-expr! str)
	 (if (pair? state)
	     (let ((st (car state)))
		(vector-set! st 2 (cons str (vector-ref st 2))))
	     (add-result! str)))
      
      ;; result
      (define (add-result! res)
	 (set! result (cons res result)))
      
      (define (add-results! . res)
	 (set! result (append res result)))
      
      (define (the-result)
	 (reverse! result))
      
      ;; paragraph
      ((: "\\\\" (: (? #\Return) #\Newline))
       (add-results! (abort-states!))
       (enter-state! 'p (wiki-syntax-p syn))
       (ignore))
      
      ;; newline
      ((bol (: (? #\Return) #\Newline))
       (add-results! (abort-states!))
       (enter-state! 'p (lambda (e)
			   (if (null? e)
			       "\n"
			       ((wiki-syntax-p syn) e))))
       (ignore))
      
      ;; simple text
      ((+ (out "*=/_$,`'() \\\n"))
       (add-expr! (the-string))
       (ignore))
      
      ;; sectionning
      ((bol (>= 2 #\=))
       (let ((s (the-symbol)))
	  (if (in-state? s)
	      (begin
		 (add-result! (exit-state!))
		 (ignore))
	      (let ((constr (case (the-length)
			       ((5) (wiki-syntax-h4 syn))
			       ((4) (wiki-syntax-h3 syn))
			       ((3) (wiki-syntax-h2 syn))
			       ((2) (wiki-syntax-h1 syn))
			       (else (wiki-syntax-h5 syn)))))
		 (add-results! (abort-states!))
		 (enter-state! s constr)
		 (ignore)))))
      ((>= 2 #\=)
       (let ((s (the-symbol)))
	  (if (in-state? s)
	      (begin
		 (add-result! (exit-state!))
		 (ignore))
	      ;; an error
	      (ignore))))

      ;; verbatim mode
      ((bol (: "  " (+ all)))
       (if (in-state? 'pre)
	   (begin
	      (add-expr! (the-substring 2 (the-length)))
	      (ignore))
	   (begin
	      (add-results! (abort-states!))
	      (enter-state! 'pre (wiki-syntax-pre syn))
	      (add-expr! (the-substring 2 (the-length)))
	      (ignore))))
      
      ;; standard markups
      ("**"
       (if (in-state? '**)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '** (wiki-syntax-b syn))
	      (ignore))))
      ("//"
       (if (in-state? '//)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '// (wiki-syntax-em syn))
	      (ignore))))
      ("__"
       (if (in-state? '__)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '__ (wiki-syntax-u syn))
	      (ignore))))
      ("$$"
       (if (in-state? '$$)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (enter-state! '$$ (wiki-syntax-math syn))
	      (ignore))))
      ("``"
       (enter-state! 'tt (wiki-syntax-tt syn))
       (ignore))
      ("''"
       (if (in-state? 'tt)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (add-result! (exit-state!))
	      (ignore))))
      ("(("
       (enter-state! 'code (wiki-syntax-code syn))
       (ignore))
      ("))"
       (if (in-state? 'code)
	   (begin
	      (add-expr! (exit-state!))
	      (ignore))
	   (begin
	      (add-result! (exit-state!))
	      (ignore))))

      ;; embedded hop
      (",("
       (rgc-buffer-unget-char (the-port) (char->integer #\())
       (with-handler
	  (lambda (e)
	     (add-expr! (<SPAN> "***PARSE-ERROR***")))
	  (add-expr! (eval (hop-read (the-port)))))
       (ignore))
       
      ;; single escape characters
      ((in "*=/_$,`'() \\\n")
       (add-expr! (the-string))
       (ignore))

      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (begin
		 (add-results! (abort-states!))
		 (the-result))
	      (wiki-read-error "Illegal character" (string c) (the-port)))))))


   

   
