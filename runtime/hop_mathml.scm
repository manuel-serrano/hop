;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/hop_mathml.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  2 08:24:08 2007                          */
;*    Last change :  Sat Jun 19 06:40:17 2010 (serrano)                */
;*    Copyright   :  2007-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop MATHML support.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-mathml
   
   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service
	    __hop_priv)

   (static (class xml-math::xml-element))
   
   (export (<MATH> . ::obj)
	   (<MATH:MSTYLE> . ::obj)
	   (<MATH:MI> . ::obj)
	   (<MATH:MN> . ::obj)
	   (<MATH:MO> . ::obj)
	   (<MATH:MROW> . ::obj)
	   (<MATH:MUNDER> . ::obj)
	   (<MATH:MOVER> . ::obj)
	   (<MATH:MUNDEROVER> . ::obj)
	   (<MATH:MSUP> . ::obj)
	   (<MATH:MSUB> . ::obj)
	   (<MATH:MSUBSUP> . ::obj)
	   (<MATH:MFRAC> . ::obj)
	   (<MATH:MROOT> . ::obj)
	   (<MATH:MSQRT> . ::obj)
	   (<MATH:MTEXT> . ::obj)
	   (<MATH:TEX> ::bstring)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-math ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-math p backend)
   (with-access::xml-math obj (tag id attributes body)
      (display (car body) p)))

;*---------------------------------------------------------------------*/
;*    Standards MATH elements                                          */
;*---------------------------------------------------------------------*/
(define-markup <MATH> ((id #unspecified string)
		       (xmlns "http://www.w3.org/1998/Math/MathML" string)
		       (attributes)
		       body)
   (instantiate::xml-element
      (tag 'math)
      (id (xml-make-id id 'math))
      (attributes `(:xmlns ,xmlns ,@attributes))
      (body body)))

;*---------------------------------------------------------------------*/
;*    MathML markups                                                   */
;*---------------------------------------------------------------------*/
(define-xml xml-element #t <MATH:MSTYLE> :tag mstyle)
(define-xml xml-element #t <MATH:MI> :tag mi)
(define-xml xml-element #t <MATH:MN> :tag mn)
(define-xml xml-element #t <MATH:MO> :tag mo)
(define-xml xml-element #t <MATH:MROW> :tag mrow)
(define-xml xml-element #t <MATH:MUNDER> :tag munder)
(define-xml xml-element #t <MATH:MOVER> :tag mover)
(define-xml xml-element #t <MATH:MUNDEROVER> :tag munderover)
(define-xml xml-element #t <MATH:MSUP> :tag msup)
(define-xml xml-element #t <MATH:MSUB> :tag msub)
(define-xml xml-element #t <MATH:MSUBSUP> :tag msubsup)
(define-xml xml-element #t <MATH:MFRAC> :tag mfrac)
(define-xml xml-element #t <MATH:MROOT> :tag mroot)
(define-xml xml-element #t <MATH:MSQRT> :tag msqrt)
(define-xml xml-element #t <MATH:MTEXT> :tag mtext)

;*---------------------------------------------------------------------*/
;*    <MATH:TEX> ...                                                   */
;*    -------------------------------------------------------------    */
;*    Compile TeX formula representation into MathML                   */
;*---------------------------------------------------------------------*/
(define (<MATH:TEX> formula)
   (parse-tex-formula formula))
 
;*---------------------------------------------------------------------*/
;*    parse-tex-error ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-tex-error msg obj)
   (raise (instantiate::&io-parse-error
	     (proc "<MATH:TEX>")
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    parse-tex-formula ...                                            */
;*---------------------------------------------------------------------*/
(define (parse-tex-formula formula)
   (with-input-from-string formula
      (lambda ()
	 (read-expressions (current-input-port) '() 'end-of-expression))))
   
;*---------------------------------------------------------------------*/
;*    read-expressions ...                                             */
;*---------------------------------------------------------------------*/
(define (read-expressions port stack state)
   (let loop ((stack '()))
      (multiple-value-bind (exp stack)
	 (read-expression port stack state)
	 (cond
	    ((symbol? exp)
	     (cond
		((not (eq? exp state))
		 (parse-tex-error "premature end of file" exp))
		((null? stack)
		 '())
		((null? (cdr stack))
		 (car stack))
		(else
		 (<MATH:MROW> (reverse stack)))))
	    ((pair? exp)
	     (<MATH:MROW> (append (reverse stack) exp)))
	    (else
	     (loop (cons exp stack)))))))

;*---------------------------------------------------------------------*/
;*    xml-stretch! ...                                                 */
;*---------------------------------------------------------------------*/
(define (xml-stretch! e)
   (cond
      ((xml-markup-is? e 'mo)
       (with-access::xml-markup e (attributes)
	  (let ((c (plist-assq :stretchy attributes)))
	     (if c
		 (set-car! (cdr c) "true")
		 (set! attributes `(:stretchy "true" ,@attributes))))))
      ((xml-markup-is? e 'mrow)
       (for-each xml-stretch! (xml-markup-body e)))))

;*---------------------------------------------------------------------*/
;*    xml-bold! ...                                                    */
;*---------------------------------------------------------------------*/
(define (xml-bold! e)
   (cond
      ((xml-markup-is? e 'mi)
       (with-access::xml-markup e (attributes)
	  (let ((c (plist-assq :fontweight attributes)))
	     (if c
		 (set-car! (cdr c) "bold")
		 (set! attributes `(:fontweight "bold" ,@attributes))))))
      ((xml-markup-is? e 'mrow)
       (for-each xml-bold! (xml-markup-body e)))))

;*---------------------------------------------------------------------*/
;*    xml-cal! ...                                                     */
;*---------------------------------------------------------------------*/
(define (xml-cal! e)
   (cond
      ((xml-markup-is? e 'mi)
       (with-access::xml-markup e (attributes)
	  (let ((c (plist-assq :class attributes)))
	     (if c
		 (set-car! (cdr c) " cal")
		 (set! attributes `(:class "cal" ,@attributes))))))
      ((xml-markup-is? e 'mrow)
       (for-each xml-cal! (xml-markup-body e)))))

;*---------------------------------------------------------------------*/
;*    read-expression ...                                              */
;*---------------------------------------------------------------------*/
(define (read-expression port stack state)
   (let ((token (read/rp *tex-rgc* port)))
      (cond
	 ((eof-object? token)
	  (if (eq? state 'end-of-expression)
	      (values 'end-of-expression stack)
	      (parse-tex-error "premature end of file" state)))
	 ((string? token)
	  (values token stack))
	 ((char? token)
	  (case token
	     ((#\{)
	      (values (read-expressions port '() 'end-of-expression) stack))
	     ((#\})
	      (if (eq? state 'end-of-expression)
		  (values 'end-of-expression stack)
		  (parse-tex-error "Unexpected closing bracket" token)))
	     ((#\r)
	      (multiple-value-bind (exp1 _)
		 (read-expressions port '() 'of)
		 (multiple-value-bind (exp2 _)
		    (read-expression port '() 'end-of-expression)
		    (values (<MATH:MROOT> exp2 exp1) stack))))
	     ((#\o)
	      (values 'of stack))
	     ((#\b)
	      (let ((body (read-expressions port '() state)))
		 (xml-stretch! body)
		 (values state (cons body stack))))
	     ((#\B)
	      (let ((body (read-expressions port '() state)))
		 (xml-bold! body)
		 (values state (cons body stack))))
	     ((#\C)
	      (let ((body (read-expressions port '() state)))
		 (xml-cal! body)
		 (values state (cons body stack))))
	     ((#\L)
	      (multiple-value-bind (left _)
		 (read-expression port '() 'end-of-expression)
		 (xml-stretch! left)
		 (let* ((body (read-expressions port '() 'left)))
		    (multiple-value-bind (right _)
		       (read-expression port '() 'end-of-expression)
		       (xml-stretch! right)
		       (values (<MATH:MROW> left body right) stack)))))
	     ((#\R)
	      (if (eq? state 'left)
		  (values 'left stack)
		  (parse-tex-error "Unexpected \\right expression" state)))
	     ((#\!)
	      (multiple-value-bind (exp stack)
		 (read-expression port stack 'end-of-expression)
		 (let ((e (if (and (xml-markup-is? exp 'mo)
				   (pair? (xml-markup-body exp))
				   (string? (car (xml-markup-body exp)))
				   (null? (cdr (xml-markup-body exp))))
			      (let ((op (car (xml-markup-body exp))))
				 (cond
				    ((string=? op "&equiv;")
				     (<MATH:MO> "&#x2262;"))
				    ((string=? op "&eq;")
				     (<MATH:MO> "&#x2260;"))
				    ((string=? op "&isin;")
				     (<MATH:MO> "&#x2209;"))
				    (else
				     (<MATH:MROW> (<MATH:MO> "&not;") exp))))
			      (<MATH:MROW> (<MATH:MO> "&not;") exp))))
		    (values e stack))))))
	 (else
	  (case (car token)
	     ((NUMBER)
	      (values (<MATH:MN> (cdr token)) stack))
	     ((IDENT)
	      (values (<MATH:MI> (cdr token)) stack))
	     ((OPERATOR)
	      (values (<MATH:MO> (cdr token)) stack))
	     ((NONSTRETCHY)
	      (values (<MATH:MO> :stretchy "false" (cdr token)) stack))
	     ((STRETCHY)
	      (values (<MATH:MO> :stretchy "true" (cdr token)) stack))
	     ((BIGOPERATOR)
	      (if (pair? (cdr token))
		  (values (apply <MATH:MO> (cdr token)) stack)
		  (values (<MATH:MO> (cdr token)) stack)))
	     ((GREEK)
	      (values (<MATH:MI> (cdr token)) stack))
	     ((&)
	      (values "&" stack))
	     ((CR)
	      (values "cr" stack))
	     ((INFIX)
	      (if (null? stack)
		  (parse-tex-error "missing left argument" '^)
		  (let ((head (if (null? (car stack))
				  (<MATH:MROW>)
				  (car stack)))
			(tail (cdr stack)))
		     (multiple-value-bind (exp stack)
			(read-expression port '() 'end-of-expression)
			(let ((e (case (cdr token)
				    ((^)
				     (cond
					((xml-markup-is? head 'mo)
					 (<MATH:MSUP>
					    head exp))
					((xml-markup-is? head 'munder)
					 (<MATH:MUNDEROVER>
					    (xml-markup-body head) exp))
					((xml-markup-is? head 'msub)
					 (<MATH:MSUBSUP>
					    (xml-markup-body head) exp))
					(else
					 (<MATH:MSUP>
					    head exp))))
				    ((_)
				     (cond
					((xml-markup-is? head 'mo)
					 (<MATH:MUNDER>
					    head exp))
					((xml-markup-is? head 'mover)
					 (<MATH:MUNDEROVER>
					    (xml-markup-body head) exp))
					((xml-markup-is? head 'msup)
					 (let ((hbody (xml-markup-body head)))
					    (if (not (and (pair? hbody)
							  (pair? (cdr hbody))))
						(parse-tex-error
						 "Illegal subsup construction"
						 hbody)
						(<MATH:MSUBSUP>
						   (list (car hbody) exp)
						   (cadr hbody)))))
					(else
					 (<MATH:MSUB>
					    head exp)))))))
			   (values e tail))))))
	     ((PREFIX)
	      (multiple-value-bind (exp _)
		 (read-expression port '() 'end-of-expression)
		 (let ((e (case (cdr token)
			     ((sqrt)
			      (<MATH:MSQRT> exp))
			     ((underbrace)
			      (<MATH:MUNDER>
				 exp
				 (<MATH:MO> :stretchy "true" "&UnderBrace;")))
			     ((underline)
			      (<MATH:MUNDER>
				 exp
				 (<MATH:MO> :stretchy "true" "&UnderBar;")))
			     ((text)
			      (<MATH:MTEXT> exp))
			     ((pmod)
			      (<MATH:MROW>
				 (<MATH:MO> "(")
				 (<MATH:MO> "mod")
				 exp
				 (<MATH:MO> ")")))
			     (else
			      (<MATH:MOVER>
				 exp
				 (if (pair? (cdr token))
				     (apply <MATH:MO> (cdr token))
				     (<MATH:MO> (cdr token))))))))
		    (values e stack))))
	     ((PREFIXOP)
	      (multiple-value-bind (exp stack)
		 (read-expression port stack 'end-of-expression)
		 (let ((e (if (xml-markup-is? exp 'mo)
			      (apply <MATH:MO>
				     (xml-markup-body exp) (cdr token))
			      exp)))
		    (values e stack))))
	     ((PREFIX2)
	      (multiple-value-bind (exp1 _)
		 (read-expression port '() 'end-of-expression)
		 (multiple-value-bind (exp2 _)
		    (read-expression port '() 'end-of-expression)
		    (values (case (cdr token)
			       ((frac)
				(<MATH:MFRAC> exp1 exp2))
			       ((stackrel)
				(<MATH:MOVER> exp2 exp1))
			       (else
				(list exp1 exp2)))
			    stack))))
	     ((OVER)
	      (if (null? stack)
		  (parse-tex-error "missing left argument" "\\over")
		  (multiple-value-bind (exp _)
		     (read-expressions port '() state)
		     (values (list
			      (<MATH:MFRAC>
				 (<MATH:MROW> (reverse stack))
				 exp))
			     '()))))
	     ((CHOOSE)
	      (if (null? stack)
		  (parse-tex-error "missing left argument" "\\choose")
		  (multiple-value-bind (exp _)
		     (read-expressions port '() state)
		     (values (list
			      (<MATH:MO> "(")
			      (<MATH:MFRAC> :linethickness "0"
				 (<MATH:MROW> (reverse stack))
				 exp)
			      (<MATH:MO> ")"))
			     '()))))
	     ((ATOP)
	      (if (null? stack)
		  (parse-tex-error "missing left argument" "\\atop")
		  (multiple-value-bind (exp _)
		     (read-expressions port '() state)
		     (values (list
			      (<MATH:MFRAC> :linethickness "0"
				 (<MATH:MROW> (reverse stack))
				 exp))
			     '()))))
	     ((VCENTER SCRIPTSTYLE)
	      (multiple-value-bind (exp _)
		 (read-expression port '() 'end-of-expression)
		 (values exp stack)))
	     ((PMATRIX)
	      (multiple-value-bind (exp _)
		 (read-expressions port '() 'end-of-expression)
		 "pmatrix"))
	     ((ROOT)
	      (multiple-value-bind (exp _)
		 (read-expression port '() 'end-of-expression)
		 (values (<MATH:MROOT> exp (cdr token)) stack)))
	     ((DISPLAYSTYLE)
	      (multiple-value-bind (exps _)
		 (read-expressions port stack state)
		 (values (<MATH:MSTYLE> :displaystyle "true"
			    (<MATH:MROW> exps))
			 '())))
	     ((TEXTSTYLE)
	      (multiple-value-bind (exps _)
		 (read-expressions port stack state)
		 (values (<MATH:MSTYLE> :displaystyle "false"
			    (<MATH:MROW> exps))
			 '())))
	     ((ROMANSTYLE)
	      (multiple-value-bind (exp _)
		 (read-expression port '() 'end-of-expression)
		 (if (xml-markup-is? exp 'mi)
		     (with-access::xml-markup exp (attributes)
			(let ((c (plist-assq :fontstyle attributes)))
			   (if c
			       (set-car! (cdr c) "normal")
			       (set! attributes
				     `(:fontstyle "normal" ,@attributes))))))
		 (values exp stack))))))))

;*---------------------------------------------------------------------*/
;*    *latex-characters* ...                                           */
;*---------------------------------------------------------------------*/
(define *latex-characters*
   (let ((t (make-hashtable)))
      (for-each (lambda (d)
		   (if (pair? d)
		       (hashtable-put! t (car d) (cadr d))
		       (hashtable-put! t d (string-append "&" d ";"))))
		'("alpha" "beta" "gamma" "delta" "epsilon" "varepsilon"
			  "zeta" "eta" "theta" "vartheta" "iota" "kappa"
			  "lambda" "mu" "nu" "xi" "pi" "varpi" "rho" "varrho"
			  "sigma" "varsigma" "tau" "upsilon" "phi" "varphi"
			  "chi" "psi" "omega"
			  
			  "Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma"
			  "Upsilon" "Phi" "Psi" "Oema"

			  ))
      t))

;*---------------------------------------------------------------------*/
;*    *latex-identifiers* ...                                          */
;*---------------------------------------------------------------------*/
(define *latex-identifiers*
   (let ((t (make-hashtable)))
      (for-each (lambda (d)
		   (if (pair? d)
		       (hashtable-put! t (car d) (cadr d))
		       (hashtable-put! t d (string-append "&" d ";"))))
		'(
		  ("arccos" "arccos")
		  ("arcsin" "arcsin")
		  ("arctan" "arctan")
		  "arg"
		  ("bmod" "mod")
		  ("cos" "cos")
		  ("cosh" "cosh")
		  ("cot" "cot")
		  ("coth" "coth")
		  "csc"
		  "deg"
		  "det"
		  "dim"
		  ("exp" "exp")
		  ("gcd" "gcd")
		  "hom"
		  "inf"
		  "ker"
		  "lg"
		  ("lim" "lim")
		  ("liminf" "lim inf")
		  ("limsup" "lim sup")
		  "ln"
		  ("log" "log")
		  ("mod" "mod")
		  ("Pr" "Pr")
		  "sec"
		  ("sin" "sin")
		  ("sinh" "sinh")
		  ("sup" "sup")
		  ("tan" "tan")
		  ("tanh" "tanh")))
      t))

;*---------------------------------------------------------------------*/
;*    *latex-operators* ...                                            */
;*---------------------------------------------------------------------*/
(define *latex-operators*
   (let ((t (make-hashtable)))
      (for-each (lambda (d)
		   (if (pair? d)
		       (hashtable-put! t (car d) (cadr d))
		       (hashtable-put! t d (string-append "&" d ";"))))
		'(
		  ("backslash" "&bsol;")
		  ("bigcap" ("&bigcap;" :stretchy "true"))
		  ("bigcup" ("&bigcup;" :stretchy "true"))
		  "bigodot"
		  "bigotimes"
		  "bigoplus"
		  "biguplus"
		  "bigsqcup"
		  "bigvee"
		  "bigwedge"
		  
		  "bullet"
		  "cap"
		  ("cdot" "&middot;")
		  ("cdots" "&#x2026;")
		  ("circ" "&SmallCircle;")
		  "coprod"
		  "cup"
		  ("ddots" "&dtdot;")
		  ("downarrow" ("&darr;" :stretchy "false"))
		  ("Downarrow" ("&dArr;" :stretchy "false"))
		  "equiv"
		  ("neq" "&#x2260;")
		  ("exists" "&exists;")
		  "ge"
		  "gt"
		  ("iff" "&hArr;")
		  ("in" "&isin;")
		  ("infty" "&infin;")
		  "int"
		  "langle"
		  "lceil"
		  "lfloor"
		  ("ldots" "&#x2026;")
		  ("Leftarrow" "&lArr;")
		  ("leftarrow" "&larr;")
		  ("leq" "&le;")
		  ("lgroup" "\\lgroup")
		  "le"
		  ("ll" "&lt;&lt;")
		  ("lmoustache" "\\lmoustache")
		  "lt"
		  ("mapsto" "&rarr;")
		  ("max" "max")
		  ("min" "min")
		  ("mid" "&mid;")
		  ("mp" "&MinusPlus;")
		  "ne"
		  "oint"
		  ("partial" "&part;")
		  ("pm" "&PlusMinus;")
		  "prec"
		  "prod"
		  ("quad" "&emsp;&emsp;&emsp;")
		  ("qquad" "&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;")
		  "rangle"
		  "rceil"
		  "rfloor"
		  ("rgroup" "\\rgroup")
		  ("Rightarrow" "&rArr;")
		  ("rightarrow" "&rarr;")
		  ("rmoustache" "\\rmoustache")
		  "sim"
		  "simeq"
		  "sqcap"
		  "sqcup"
		  ("sqsubset" "&sqsubset;")
		  ("sqsubseteq" "&sqsubseteq;")
		  ("subset" "&sub;")
		  ("subseteq" "&sube;")
		  "sum"
		  "times"
		  ("to" "&rarr;")
		  ("uparrow" ("&uarr;" :stretchy "false"))
		  ("Uparrow" ("&uArr;" :stretchy "false"))
		  ("updownarrow" ("&updownarrow;" :stretchy "false"))
		  ("Updownarrow" ("&Updownarrow;" :stretchy "false"))
		  ("vert" "&verbar;")
		  ("Vert" "&Verbar;")

		  "vee"
		  "wedge"
		  ))
      t))
	     
;*---------------------------------------------------------------------*/
;*    *latex-mover* ...                                                */
;*---------------------------------------------------------------------*/
(define *latex-mover*
   (let ((t (make-hashtable)))
      (for-each (lambda (d)
		   (if (pair? d)
		       (hashtable-put! t (car d) (cadr d))
		       (hashtable-put! t d (string-append "&" d ";"))))
		'(
		  "acute"
		  ("bar" "&OverBar;")
		  "breve"
		  ("check" "&vee;")
		  ("dot" "&middot;")
		  ("ddot" "&middot;&middot;")
		  "grave"
		  ("hat" "&Hat;")
		  ("overbrace" ("&OverBrace;" :stretchy "true"))
		  ("overline" ("&OverBar;" :stretchy "true"))
		  ("tilde" "~")
		  ("widehat" ("&Hat;" :stretchy "true"))
		  ("vec" "&rarr;")
		  ("widehat" ("&Hat;" :stretchy "true"))
		  ("widetilde" ("~" :stretchy "true"))
		  ))
      t))
	     
;*---------------------------------------------------------------------*/
;*    *tex-rgc* ...                                                    */
;*---------------------------------------------------------------------*/
(define *tex-rgc*
   (regular-grammar ((float    (or (: (* digit) "." (+ digit))
                                   (: (+ digit) "." (* digit))))
		     (number (or (+ digit)
				 (: (or float
					(: (or float (+ digit))
					   (in "eE")
					   (? (in "+-")) (+ digit))))))
		     (ident (: alpha (* (or alpha digit #\' #\.)))))
      ;; separator
      ((+ (in " \t\n"))
       (ignore))
      ;; bra
      ((or #\{ #\})
       (the-character))
      ;; number
      (number
       (cons 'NUMBER (the-string)))
      ;; ident
      (ident
       (cons 'IDENT (the-string)))
      ;; operators
      ((+ (in "+-*/=,!:?"))
       (cons 'OPERATOR (the-string)))
      ("\\|"
       (cons 'OPERATOR "&Verbar;"))
      (#\<
       (cons 'OPERATOR "&lt;"))
      (#\>
       (cons 'OPERATOR "&gt;"))
      ;; non stretchy operators
      ((in "()[]")
       (cons 'NONSTRETCHY (the-string)))
      ("\\{"
       (cons 'NONSTRETCHY "{"))
      ("\\}"
       (cons 'NONSTRETCHY "}"))
      ("|"
       (cons 'NONSTRETCHY "|"))
      ("&"
       (list '&))
      ("\\cr"
       (list 'CR))
      ("\\!"
       (cons 'BIGOPERATOR (list "" :lspace "-0.2em")))
      ;; binops
      (#\^
       (cons 'INFIX '^))
      (#\_
       (cons 'INFIX '_))
      ;; tex commands
      ((: #\\ (+ alpha))
       (let* ((s (the-substring 1 (the-length)))
	      (c (hashtable-get *latex-characters* s)))
	  (if c
	      (cons 'GREEK c)
	      (let ((o (hashtable-get *latex-identifiers* s)))
		 (if o
		     (cons 'OPERATOR o)
		     (let ((o (hashtable-get *latex-operators* s)))
			(if o
			    (cons 'BIGOPERATOR o)
			    (let ((i (hashtable-get *latex-mover* s)))
			       (if i
				   (cons 'PREFIX i)
				   (cond
				      ((string=? s "sqrt")
				       (cons 'PREFIX 'sqrt))
				      ((string=? s "frac")
				       (cons 'PREFIX2 'frac))
				      ((string=? s "stackrel")
				       (cons 'PREFIX2 'stackrel))
				      ((string=? s "over")
				       (list 'OVER))
				      ((string=? s "choose")
				       (list 'CHOOSE))
				      ((string=? s "atop")
				       (list 'ATOP))
				      ((string=? s "bar")
				       (cons 'PREFIX 'bar))
				      ((string=? s "underline")
				       (cons 'PREFIX 'underline))
				      ((string=? s "underbrace")
				       (cons 'PREFIX 'underbrace))
				      ((string=? s "displaystyle")
				       (list 'DISPLAYSTYLE))
				      ((string=? s "scriptstyle")
				       (list 'SCRIPTSTYLE))
				      ((string=? s "textstyle")
				       (list 'TEXTSTYLE))
				      ((string=? s "vcenter")
				       (list 'VCENTER))
				      ((string=? s "rm")
				       (list 'ROMANSTYLE))
				      ((string=? s "pmatrix")
				       (list 'PMATRIX))
				      ((string=? s "matrix")
				       (list 'MATRIX))
				      ((string=? s "limits")
				       (ignore))
				      ((string=? s "root")
				       #\r)
				      ((string=? s "of")
				       #\o)
				      ((or (string=? s "Bigg")
					   (string=? s "bigg")
					   (string=? s "bigl")
					   (string=? s "bigm")
					   (string=? s "bigr"))
				       (list 'PREFIXOP :stretchy "true"))
				      ((string=? s "not")
				       #\!)
				      ((string=? s "left")
				       #\L)
				      ((string=? s "right")
				       #\R)
				      ((string=? s "big")
				       #\b)
				      ((string=? s "bf")
				       #\B)
				      ((string=? s "cal")
				       #\C)
				      ((string=? s "hbox")
				       (cons 'PREFIX 'text))
				      ((string=? s "pmod")
				       (cons 'PREFIX 'pmod))
				      (else
				       (parse-tex-error "unknown command" s))))))))))))
      ((: "\\sqrt[" (+ (out #\])) "]")
       (let ((root (<MATH:TEX> (the-substring 6 -1))))
	  (cons 'ROOT root)))
      ((or "\\ " "\\,")
       ;; tex separator
       (cons 'IDENT "&ensp;"))
      ;; tex spaces
      ("\\;"
       (cons 'IDENT "&ensp;&ensp;"))
      ;; prims
      ((+ (in "'."))
       (cons 'IDENT (the-string)))
      ;; default
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (let ((line (read-line (the-port))))
		 (raise (instantiate::&io-parse-error
			   (proc "<MATH:TEX>")
			   (msg "Illegal character")
			   (obj (string-append "{" (string c) "}"
					       (if (string? line)
						   line
						   "")))))))))))
      
