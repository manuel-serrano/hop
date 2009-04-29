(module js-lexer
   (export *JS-grammar*
	   (JS-reserved-id? id::bstring)
	   *JS-care-future-reserved*
	   *Reg-exp-grammar*))

(define (the-coord input-port)
   (list 'at (input-port-name input-port) (input-port-position input-port)))

(define (JS-reserved-id? id)
   (let ((sym (string->symbol id)))
      (or (getprop sym 'reserved)
	  (and *JS-care-future-reserved*
	       (getprop sym 'future-reserved)))))

(define *JS-care-future-reserved* #t)

(define *keyword-list*
   '("as"
     "break"
     "case"
     "catch"
     "class"
     "const"
     "continue"
     "default"
     "delete"
     "do"
     "else"
     "extends"
     "false"
     "finally"
     "for"
     "function"
     "if"
     "import"
     "in"
     "instanceof"
     "is"
     "namespace"
     "new"
     "null"
     "package"
     "private"
     "public"
     "return"
     "super"
     "switch"
     "this"
     "throw"
     "true"
     "try"
     "typeof"
     "use"
     "var"
     "void"
     "while"
     "with"))

(define *future-reserved-list*
   '("abstract"
     "debugger"
     "enum"
     "export"
     "goto"
     "implements"
     "interface"
     "native"
     "protected"
     "synchronized"
     "throws"
     "transient"
     "volatile"))

(for-each (lambda (word)
	     (putprop! (string->symbol word) 'reserved #t))
	  *keyword-list*)
(for-each (lambda (word)
	     (putprop! (string->symbol word) 'future-reserved #t))
	  *future-reserved-list*)

(define-macro (token type value)
   `(econs ,type ,value (the-coord input-port)))

(define *JS-grammar*
   (regular-grammar
	 ;; TODO: are a010 and a013 really correct?
	 ((blank        (in #\Space #\Tab #a010 #a011 #a012 #a013 #\Newline))
	  (blank_no_lt  (in #\Space #\Tab #a011 #a012))
	  (lt           (in #a013 #\Newline))
	  (nonzero-digit   (in ("19")))
	  (id_start     (or alpha #\$ #\_))
	  (id_part      (or alnum #\$ #\_))) ;; TODO: not spec-conform!


      ((+ blank_no_lt)
       (ignore))

      ((: (* blank_no_lt) lt (* blank))
       (token 'NEW_LINE #\newline))
      
      ;; LineComment
      ((:"//" (* all))
       (ignore))

      ;; multi-line comment on one line
      ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
       (ignore))

      ;; multi-line comment with LineTerminators (several lines)
      ((: "/*"
	  (* (or lt
		 (out #\*)
		 (: (+ #\*) (out #\/ #\*))))
	  (+ #\*) "/")
       (token 'NEW_LINE #\newline))

      ;; TODO: verify if this is really the correct syntax
      ((or
	;; integer constant
	#\0
	(: nonzero-digit (* digit))
	(: (uncase "0x") (+ xdigit))
	;; floating-point constant
	(: (+ digit)
	   (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
	(: (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
	   (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))))
       (token 'NUMBER (the-string)))

      (#\{   (token 'LBRACE #\{))
      (#\}   (token 'RBRACE #\}))
      (#\(   (token 'LPAREN #\())
      (#\)   (token 'RPAREN #\)))
      (#\[   (token 'LBRACKET #\[))
      (#\]   (token 'RBRACKET #\]))
      (#\.   (token 'DOT #\.))
      (#\;   (token 'SEMICOLON #\;))
      (#\,   (token 'COMMA #\,))
      (#\|   (token 'BIT_OR #\|))
      ("||"  (token 'OR "||"))
      ("|="  (token 'BIT_OR= "|="))
      ((or #\< #\> "<=" ">=" "==" "!=" "===" "!==" #\+ #\- #\* #\% "++" "--"
	   "<<" ">>" ">>>" #\& #\^ #\! #\~ "&&" #\: #\= "+=" "-="  
	   "*=" "%=" "<<=" ">>=" ">>>=" "&=" "^=" "/=" #\/ #\?)
       (token (the-symbol) (the-string)))

      ;; TODO: probably not spec-conform
      ((: #\" (* (or (out #\" #\\ #\Newline) (: #\\ all))) #\")
       (token 'STRING (the-string)))
      ((: #\' (* (or (out #\' #\\ #\Newline) (: #\\ all))) #\')
       (token 'STRING (the-string)))

      ;; Identifiers and Keywords
      ((: id_start (* id_part))
       (let* ((symbol (the-symbol)))
	  (cond
	     ((getprop symbol 'reserved)
	      (token symbol symbol))
	     ((and *JS-care-future-reserved*
		   (getprop symbol 'future-reserved))
	      (token symbol symbol))
	     (else
	      (token 'ID symbol)))))

      ;; small HACK: 0 is used to indicate that a pragma should be inserted.
      ;; doesn't get caught by lexer... (bug?)
      ;; -> will be "retreated" in else-clause
      ;(#a000 (token 'PRAGMA #unspecified))

      ;; error
      (else
       (let ((c (the-failure)))
	  (cond
	     ((eof-object? c)
	      (cons 'EOF c))
	     ((and (char? c) (char=? c #a000))
	      (token 'PRAGMA #unspecified))
	     (else
	      (token 'ERROR c)))))))

(define *Reg-exp-grammar*
   (regular-grammar
	 ;; TODO: see TODOs for JS-grammar
	 ((lt           (in #a013 #\Newline))
	  (id_part      (or alnum #\$ #\_)))
      ((: (* (or (out #\/ #\\ lt) (: #\\ (out lt)))) #\/ (* id_part))
       (cons 'RegExp (the-string)))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (cons 'EOF c)
	      (cons 'ERROR c))))))
