;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/etc/hopjs-parse.el                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  1 07:14:59 2018                          */
;*    Last change :  Sat Jun  5 06:34:07 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hopjs JavaScript/HTML parser                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'hopjs-parse)
(require 'hopjs-macro)

;*---------------------------------------------------------------------*/
;*    hopjs-parse-tokens ...                                           */
;*---------------------------------------------------------------------*/
(defvar hopjs-parse-tokens '())

;*---------------------------------------------------------------------*/
;*    rx builder ...                                                   */
;*---------------------------------------------------------------------*/
(defun rxor (rx &rest rxs)
  (concat "\\(?:" rx
	  (apply 'concat (mapcar #'(lambda (x) (concat "\\|" x)) rxs))
	  "\\)"))

(defun rx* (rx) (concat rx "*"))

(defun rx? (rx) (concat rx "?"))

(defun rx: (a &rest rxs) (concat a (apply 'concat rxs)))

(defun rxq (rx) (regexp-quote rx))

;*---------------------------------------------------------------------*/
;*    JavaScript tokens                                                */
;*---------------------------------------------------------------------*/
(defconst hopjs-parse-regexps
  (let* ((unicode_char "[[:multibyte:]]")
	 (alpha+ "[[:alpha:][:multibyte:]]")
	 (alnum+ (rxor "[[:alnum:][:multibyte:]]"))
	 (id_part_sans "[[:nonascii:]]")
	 (id_part "[[:alnum:][:multibyte:][:nonascii:]$_]")
	 (id_start "[[:alpha:][:multibyte:][:nonascii:]$_]")
	 (scmid_part "[^ (){}\"]")
	 (scmid_start "#[:]")
	 (cssid_part "[[:alnum:][:multibyte:][:nonascii:]$_#-]")
	 (cssid_start "[[:alpha:][:multibyte:][:nonascii:]$_#-]")
	 (letter "[[:alpha:][:nonascii:]]")
	 (tagid (rx: "[[:digit:]]*" "[[:alnum:]_]" (rx* "[[:alnum:]_.:]*"))))
    (list
     ;; blank
     (cons "[ \t\n]+" 'blank)
     ;; line comments
     (cons "//[^\n]*" 'eol-comment)
     ;; comments
     (cons "/[*]\\(?:[^*]\\|[*][^/]\\)*[*]+/" 'comment)
     ;; numbers
     (cons (rxor "\\(?:0[xo]\\)?[0-9]+\\>"
		 "[eE]?-?[0-9]+\\>"
		 "[+]?[0-9]+\\(?:[.][0-9]+\\)?\\>")
	   'number)
     ;; punctuation
     (cons (rxor "[{}()[.;,:?]" (rxq "]")) 'punct)
     ;; unop
     (cons (rxor (rxq "!") (rxq "~")) 'unop)
     ;; binop
     (cons (rxor "<" ">" (rxq "+") (rxq "-") (rxq "*") "%" "=" "|" "/"
		 "<<" ">>" ">>>" "[&^]") 'binop)
     (cons (rxor "&&" "||" "in" "??") 'binop)
     (cons "instanceof" 'binop)
     (cons "[?][?]" 'binop)
     (cons (rxor "<=" ">="  "!==*" "===*" "[+*%^&-]=" "<<=" ">>=" ">>>=") '=)
     ;; prefix
     (cons (rxor (rx: (rxq "+") (rxq "+")) "--") 'prefix)
     ;; arrow
     (cons "=>" '=>)
     ;; dots
     (cons "[.][.][].]" 'dots)
     ;; strings
     (cons (rxor "\"\\([^\"\\]\\|\\\\.\\)*\"" "'[^']*'" "`[^`]*`") 'string)
     ;; regexp
     (cons "/[^*/].*[^<]/[gimuy]*" 'regexp)
     (cons "/[^*/]/[gimuy]*" 'regexp)
     ;; tilde escape
     (cons "~{" 'tilde)
     ;; dollar escape
     (cons "[$]{" 'dollar)
     ;; ident
     (cons (rx: id_start (rx* id_part)) 'ident)
     ;; scheme ident
     (cons (rx: scmid_start (rx* scmid_part)) 'ident)
     ;; html-comment
     (cons "<!--\\(?:[^-]\\|-[^-]\\|--[^>]\\)[-]+->" 'html-comment)
     ;; otag
     (cons (rx: "<" tagid ">") 'otag)
     ;; ctag
     (cons (rx: "</" tagid ">") 'ctag)
     ;; ohtml
     (cons (rx: "\\(<" tagid "\\)[ \t\n]+" (rxor tagid "[$]{" ">")) 'ohtml)
     ;; chtml
     (cons "/>" 'chtml)
     ;; html
     (cons (rx: "<" tagid "/>") 'html)
     ;; end of comment
     (cons (rxq "*/") 'ecomment)
     ;; scheme mark
     (cons (rxq "#:") 'scheme)
     ;; css ident
     (cons (rx: cssid_start (rx* cssid_part)) 'cssident)
     )))

(defconst hopjs-parse-lexer
  (apply 'rxor (mapcar 'car hopjs-parse-regexps)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-token ...                                            */
;*---------------------------------------------------------------------*/
(defsubst hopjs-parse-token (type b e) (vector type b e))
(defsubst hopjs-parse-tokenp (obj) (and (vector-p obj) (= (length obj) 3)))

(defsubst hopjs-parse-token-type (tok) (aref tok 0))
(defsubst hopjs-parse-token-beginning (tok) (aref tok 1))
(defsubst hopjs-parse-token-end (tok) (aref tok 2))

(defsubst hopjs-parse-token-string (tok)
  (if (eq (hopjs-parse-token-type tok) 'bof)
      "<BOF>"
    (buffer-substring-no-properties
     (hopjs-parse-token-beginning tok)
     (hopjs-parse-token-end tok))))

(defun hopjs-parse-token-beginning-tag (tok)
  (1+ (hopjs-parse-token-beginning tok)))

(defun hopjs-parse-token-end-tag (tok)
  (if (eq (hopjs-parse-token-type tok) 'ohtml)
      (hopjs-parse-token-end tok)
    (1- (hopjs-parse-token-end tok))))

(defun hopjs-parse-token-tag (tok)
  (if (eq (hopjs-parse-token-type tok) 'ohtml)
      (buffer-substring-no-properties
       (1+ (hopjs-parse-token-beginning tok))
       (hopjs-parse-token-end tok))
      (buffer-substring-no-properties
       (1+ (hopjs-parse-token-beginning tok))
       (1- (hopjs-parse-token-end tok)))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-goto-token ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-goto-token (tok &optional shift)
  (goto-char (+ (or shift 0) (hopjs-parse-token-beginning tok))))

;*---------------------------------------------------------------------*/
;*    nullp                                                            */
;*---------------------------------------------------------------------*/
(defsubst nullp (o) (not (consp o)))

;*---------------------------------------------------------------------*/
;*    hopjs-regexp-to-token ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-regexp-to-token (tok beg)
  (case (hopjs-parse-token-type tok)
    ((punct)
     (case (char-after (hopjs-parse-token-beginning tok))
       ((?,) (aset tok 0 'comma))
       ((?\;) (aset tok 0 'semicolon))
       ((?\.) (aset tok 0 'dot))
       ((?\() (aset tok 0 'lparen))
       ((?\)) (aset tok 0 'rparen))
       ((?\[) (aset tok 0 'lbracket))
       ((?\]) (aset tok 0 'rbracket))
       ((?\{) (aset tok 0 'lbrace))
       ((?\}) (aset tok 0 'rbrace))
       ((?\:) (aset tok 0 'colon))
       ((?\?) (aset tok 0 'qmark)))
     tok)
    ((ident)
     (let ((sym (intern (hopjs-parse-token-string tok))))
       (case sym
	 ((service return try catch while if var let const else
		   new case switch for yield do default from throw)
	  (aset tok 0 sym))
	 ((function)
	  (save-excursion
	    (goto-char (hopjs-parse-token-beginning tok))
	    (if (looking-at "function[*]")
		(progn
		  (aset tok 0 'function*)
		  (aset tok 2 (match-end 0)))
	      (aset tok 0 'function))))
	 ((yield)
	  (save-excursion
	    (goto-char (hopjs-parse-token-beginning tok))
	    (if (looking-at "yield[*]")
		(progn
		  (aset tok 0 'yield*)
		  (aset tok 2 (match-end 0)))
	      (aset tok 0 'yield))))
	 ((typeof)
	  (aset tok 0 'unop)))
       tok))
    ((binop)
     (case (char-after (hopjs-parse-token-beginning tok))
       ((?=)
	(aset tok 0 '=))
       ((?>)
	(when (= (+ 1 (hopjs-parse-token-beginning tok))
		 (hopjs-parse-token-end tok))
	  (aset tok 0 '>))))
     tok)
;*     ((regexp)                                                       */
;*      (when (> (hopjs-parse-token-end tok) beg)                      */
;*        (aset tok 0 'string)                                         */
;*        (aset tok 2 (- beg 1)))                                      */
;*      tok)                                                           */
    (t
     tok)))
    
;*---------------------------------------------------------------------*/
;*    looking-at-token ...                                             */
;*---------------------------------------------------------------------*/
(defun looking-at-token ()
  (when (looking-at hopjs-parse-lexer)
    (let ((token '())
	  (i 0)
	  (l hopjs-parse-regexps)
	  (end 0)
	  (beg (+ (point) 1)))
      (while (consp l)
	(when (and (looking-at (caar l)) (> (match-end 0) end))
	  (setq token
		(hopjs-regexp-to-token
		 (vector (cdar l) (match-beginning 0) (match-end 0))
		 beg))
	  (setq end (hopjs-parse-token-end token))
	  (setq beg (hopjs-parse-token-beginning token)))
	(setq i (+ i 1))
	(setq l (cdr l)))
      token)))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-line ...                                             */
;*    -------------------------------------------------------------    */
;*    Returns the list of tokens found on the line                     */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-line (pos &optional rev)
  (interactive "d")
  (hopjs-debug 1 "hopjs-parse-line pos=%s" pos)
  (save-excursion
    (let ((tokens '()))
      (goto-char pos)
      (beginning-of-line)
      (while (< (point) pos)
	(let ((tok (looking-at-token)))
	  (cond
	   (tok
;* 	    (hopjs-debug 0 "hopjs-parse-line pos=%s point=%s tok=%s"   */
;* 			    pos (point) tok)                           */
	    (if (eq (hopjs-parse-token-type tok) 'ohtml)
		;; ohtml must be split in two parts: the tag and the attr
		(progn
		  (goto-char (match-end 1))
		  (aset tok 2 (match-end 1))
		  (setq tokens (cons tok tokens)))
	      (progn
		(goto-char (hopjs-parse-token-end tok))
		(setq tokens (cons tok tokens)))))
	   ((looking-at "[^ \t\n;<>{}()[\]]+")
	    (let ((tok (hopjs-parse-token
			'text (match-beginning 0) (match-end 0))))
	      (setq tokens (cons tok tokens))
	      (goto-char (match-end 0))))
	   (t
	    (setq tokens
		  (cons (hopjs-parse-token 'blank (point) (point))
			tokens))
	    (forward-char 1)))))
      (let ((r (cond
		((nullp tokens) (list (hopjs-parse-token 'blank pos pos)))
		(rev (reverse tokens))
		(t tokens))))
	(hopjs-debug -1 "hopjs-parse-line pos=%s -> %s" pos r)
	r))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-start ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-start (pos)
  (cond
   ((and (consp hopjs-parse-tokens)
	 (= (hopjs-parse-token-end (car (last hopjs-parse-tokens))) pos))
    (setq hopjs-parse-tokens (last hopjs-parse-tokens)))
   (t
    (setq hopjs-parse-tokens '())
    (while (nullp hopjs-parse-tokens)
      (setq hopjs-parse-tokens (hopjs-parse-line pos))
      (when (nullp hopjs-parse-tokens)
	(if (= (point) (point-min))
	    (setq hopjs-parse-tokens (list (hopjs-parse-token 'bof 0 0)))
	  (progn
	    (forward-line -1)
	    (end-of-line)
	    (setq pos (point)))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-push-token ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-push-token (tok)
  (setq hopjs-parse-tokens (cons tok hopjs-parse-tokens))
  tok)

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token-blank ...                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token-blank ()
  (hopjs-no-debug 1 "parse-peek-token-blank %s %s" (point) hopjs-parse-tokens)
  (cond
   ((nullp hopjs-parse-tokens)
    (hopjs-no-debug -1 "parse-peek-token-blank.1 %s BOF" (point))
    (hopjs-parse-token 'bof 0 0))
   ((nullp (cdr hopjs-parse-tokens))
    (let* ((tok (car hopjs-parse-tokens))
	   (b (hopjs-parse-token-beginning tok)))
      (hopjs-no-debug 0 "parse-peek-token-blank.1 %s tok=%s" (point) tok)
      (if (= b (point-min))
	  (setq hopjs-parse-tokens (list tok (hopjs-parse-token 'bof 0 0)))
	(setq hopjs-parse-tokens (cons tok (hopjs-parse-line (- b 1)))))
      (hopjs-no-debug -1 "parse-peek-token-blank.2 %s %s" (point) hopjs-parse-tokens)
      tok))
   (t
    (hopjs-no-debug -1 "parse-peek-token-blank.3 %s %s" (point) hopjs-parse-tokens)
    (car hopjs-parse-tokens))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token ()
  (hopjs-no-debug 1 "hopjs-parse-peek-token %s" (point))
  (let ((tok (hopjs-parse-peek-token-blank)))
    (while (eq (hopjs-parse-token-type tok) 'blank)
      (setq hopjs-parse-tokens (cdr hopjs-parse-tokens))
      (setq tok (hopjs-parse-peek-token-blank)))
    (hopjs-no-debug -1 "hopjs-parse-peek-token %s -> %s"
		    (point) (car hopjs-parse-tokens))
    (car hopjs-parse-tokens)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-consume-and-peek-token ...                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-consume-and-peek-token ()
  (hopjs-parse-consume-token-any)
  (hopjs-parse-peek-token))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token-type ...                                  */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token-type ()
  (hopjs-parse-token-type (hopjs-parse-peek-token)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token-string ...                                */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token-string ()
  (hopjs-parse-token-string (hopjs-parse-peek-token)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-consume-token-any ...                                */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-consume-token-any ()
  (while (eq (hopjs-parse-token-type (hopjs-parse-peek-token)) 'blank)
    (setq hopjs-parse-tokens (cdr hopjs-parse-tokens)))
  (let ((tok (car hopjs-parse-tokens)))
    (setq hopjs-parse-tokens (cdr hopjs-parse-tokens))
    tok))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-consume-tokens ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-consume-tokens (lst multilinep)
  (letn loop ((tok (hopjs-parse-consume-token-any)))
	(let ((ntok (hopjs-parse-peek-token)))
	  (if (and (or multilinep (hopjs-parse-same-linep ntok tok))
		   (memq (hopjs-parse-token-type ntok) lst))
	      (funcall loop (hopjs-parse-consume-token-any))
	    tok))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-consume-token ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-consume-token (key)
  (while (eq (hopjs-parse-token-type (hopjs-parse-peek-token)) 'blank)
    (setq hopjs-parse-tokens (cdr hopjs-parse-tokens)))
  (let ((tok (car hopjs-parse-tokens)))
    (when (eq (hopjs-parse-token-type tok) key)
      (setq hopjs-parse-tokens (cdr hopjs-parse-tokens))
      tok)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-backward-sexp ...                                    */
;*    -------------------------------------------------------------    */
;*    Jump backward one expression, if possible, and restart parsing   */
;*    on the first token composing the jumped expression.              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-backward-sexp ()
  (with-debug
   "hopjs-parse-backward-sexp point=%s" (point)
   (condition-case nil
       (progn
	 (backward-sexp 1)
	 (hopjs-debug 0 "hopjs-parse-backward-sexp starting new %s" (point))
	 (let ((pos (+ 1 (point))))
	   (hopjs-parse-start pos)
	   pos))
     (error '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-args ...                                             */
;*    -------------------------------------------------------------    */
;*    Try to parse an argument list. On success, returns the lost      */
;*    token of that list. This token is consumed                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-args (tok)
  (save-excursion
    (when (eq (hopjs-parse-token-type tok) 'rparen)
      (hopjs-parse-goto-token tok 1)
      (when (hopjs-parse-backward-sexp)
	(hopjs-parse-consume-token-any)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr ...                                             */
;*    -------------------------------------------------------------    */
;*    This function parses a complete expression (from right to left)  */
;*    and returns the last expression token (that is, the last token   */
;*    composing the expression). This token is consumed.               */
;*    When MULTILINEP is TRUE the expression parser traverses newline. */
;*    When it is FALSE it stops before the first newline.              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr (utok multilinep)
  (with-debug
   "hopjs-parse-expr utok=%s [%s]"
   utok (hopjs-parse-token-string utok)
   (let* ((atok (hopjs-parse-expr-atomic utok))
	  (tok (or (hopjs-parse-expr-simple atok multilinep) atok)))
     (hopjs-debug 0 "hopjs-parse-expr tok=%s [%s]"
		  tok (when tok (hopjs-parse-token-string tok)))
     (when tok
       (let ((ptok (hopjs-parse-peek-token)))
	 (case (hopjs-parse-token-type ptok)
	   ((colon)
	    (if (hopjs-parse-same-linep ptok tok)
		(or (hopjs-parse-expr-colon ptok multilinep) tok)
	      tok))
	   ((qmark)
	    (hopjs-parse-expr-qmark ptok multilinep))
	   (t
	    (if (eq (hopjs-parse-token-type tok) 'colon)
		(progn
		  (hopjs-parse-push-token tok)
		  '())
	      tok))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr-colon ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr-colon (utok multilinep)
  (with-debug
   "hopjs-parse-expr-colon utok=%s [%s]" 
   utok (hopjs-parse-token-string utok)
   (let* ((tok (hopjs-parse-consume-token-any))
	  (ptok (hopjs-parse-peek-token)))
     (when (or multilinep (hopjs-parse-same-linep ptok tok))
       (let ((ntok (hopjs-parse-expr ptok multilinep)))
	 (hopjs-debug 0 "hopjs-parse-expr-colon ntok=%s [%s]"
		      ntok (hopjs-parse-token-string ntok))
	 (if ntok
	     (if (and (hopjs-parse-peek-token-same-linep ntok)
		      (eq (hopjs-parse-peek-token-type) 'qmark))
		 (let ((qtok (hopjs-parse-consume-token-any)))
		   (or (hopjs-parse-expr (hopjs-parse-peek-token) nil)
		       qtok))
	       ntok)
	   tok))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr-qmark ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr-qmark (utok multilinep)
  (with-debug
   "hopjs-parse-expr-qmark utok=%s [%s]" 
   utok (hopjs-parse-token-string utok)
   (let ((tok (hopjs-parse-consume-token-any)))
     (or (hopjs-parse-expr (hopjs-parse-peek-token) nil) tok))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr-simple ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr-simple (tok multilinep)
  (with-debug
   "hopjs-parse-expr-simple tok=%s [%s]"
   tok (when tok (hopjs-parse-token-string tok))
   (cond
    ((not tok)
     '())
    (t
     (let ((ptok (hopjs-parse-peek-token)))
       (hopjs-debug 0 "hopjs-parse-expr-simple ptok=%s [%s]"
		    ptok (hopjs-parse-token-string ptok))
       (case (hopjs-parse-token-type ptok)
	 ((ident rparen rbracket)
	  (if (and (or multilinep (hopjs-parse-same-linep ptok tok))
		   (or (not (memq (hopjs-parse-token-type tok)
				  '(ident number string)))
		       (member (hopjs-parse-token-string ptok)
			       hopjs-plugins)
		       (progn (message "PAS BON tok=%s [%s] ptok=%s [%s]"
				       tok
				       (hopjs-parse-token-string tok)
				       ptok
				       (hopjs-parse-token-string ptok))
			      nil)))
	      (or (hopjs-parse-expr ptok multilinep) tok)
	    tok))
	 ((catch)
	  (let* ((ctok (hopjs-parse-consume-token-any))
		 (ptok (hopjs-parse-peek-token-type)))
	    (hopjs-parse-push-token ctok)
	    (if (eq ptok 'dot)
		(or (hopjs-parse-expr ctok multilinep) tok)
	      tok)))
	 ((dot)
	  (let* ((dtok (hopjs-parse-consume-token-any))
		 (ptok (hopjs-parse-peek-token)))
	    (hopjs-debug
	     0 "hopjs-parse-expr-simple.dot.1 ptok=%s [%s]"
	     ptok (hopjs-parse-token-string ptok))
	    (let ((ntok (hopjs-parse-expr ptok nil)))
	      (hopjs-debug
	       0 "hopjs-parse-expr-simple.dot.2 ntok=%s [%s]"
	       ntok (when ntok (hopjs-parse-token-string ntok)))
	      (or ntok dtok))))
	 ((binop = >)
	  (if (or multilinep (hopjs-parse-same-linep ptok tok))
	      (let ((btok (hopjs-parse-consume-token-any))
		    (ntok (hopjs-parse-peek-token)))
		(hopjs-debug
		 0 "hopjs-parse-expr-simple.binop ntok=%s [%s]"
		 ntok (when ntok (hopjs-parse-token-string ntok)))
		(if (or multilinep (hopjs-parse-same-linep ntok btok))
		    (hopjs-parse-expr ntok nil)
		  btok))
	    tok))
	 ((unop new yield yield*)
	  (let ((otok (hopjs-parse-consume-token-any))
		(ntok (hopjs-parse-peek-token)))
	    (hopjs-debug
	     0 "hopjs-parse-expr-simple.unary otok=%s [%s] ntok=%s"
	     otok (when otok (hopjs-parse-token-string otok)) ntok)
	    (if (or multilinep (hopjs-parse-same-linep otok ntok))
		(or (hopjs-parse-expr-simple ntok multilinep) otok)
	      otok)))
	 ((function service)
	  (hopjs-parse-consume-token-any))
	 ((scheme)
	  (hopjs-parse-consume-token-any))
	 ((=>)
	  (let ((ftok (hopjs-parse-consume-token-any)))
	    (case (hopjs-parse-peek-token-type)
	      ((ident)
	       (hopjs-parse-consume-token-any))
	      ((rparen)
	       (when (hopjs-parse-args (hopjs-parse-consume-token-any))
		 (hopjs-parse-peek-token)))
	      (t
	       tok))))
	 ((rbrace)
	  (if (memq (hopjs-parse-token-type tok) '(lparen dot))
	      (progn
		(goto-char (hopjs-parse-token-end ptok))
		(when (hopjs-parse-backward-sexp)
		  (hopjs-parse-consume-token-any)))
	    tok))
;* 	 ((lbracket lparen)                                            */
;* 	  (let ((otok (hopjs-parse-consume-token-any))                 */
;* 		(ntok (hopjs-parse-peek-token)))                       */
;* 	    (hopjs-debug                                               */
;* 	     0 "hopjs-parse-expr-simple.paren otok=%s [%s]"            */
;* 	     otok (when otok (hopjs-parse-token-string otok)))         */
;* 	    (if (or multilinep (hopjs-parse-same-linep otok ntok))     */
;* 		(hopjs-parse-expr ntok multilinep)                     */
					;* 	      otok)))                                                  */
	 (t
	  '())
	 (t
	  tok)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr-atomic ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr-atomic (utok)
  (with-debug
   "hopjs-parse-expr-atomic tok=%s [%s]"
   utok (hopjs-parse-token-string utok)
   (case (hopjs-parse-token-type utok)
     ((ident number string cssident lparen lbracket)
      (hopjs-parse-consume-token-any))
     ((rbracket rparen rbrace)
      (goto-char (hopjs-parse-token-end utok))
      (when (hopjs-parse-backward-sexp)
	(hopjs-parse-consume-token-any)))
     ((ctag)
      (hopjs-find-opening-tag (- (hopjs-parse-token-end tok) 1) 0))
     ((catch)
      (let ((tok (hopjs-parse-consume-token-any)))
	(if (eq (hopjs-parse-peek-token-type) 'dot)
	    tok
	  (progn
	    (hopjs-parse-push-token tok)
	    '()))))
     (t
      '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr-unop ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr-unop (tok multilinep)
  (let ((peek (hopjs-parse-peek-token)))
    (if (and (memq (hopjs-parse-token-type peek) '(unop))
	     (or multilinep (hopjs-parse-same-linep peek tok)))
	(hopjs-parse-consume-token-any)
      tok)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-find-opening-tok ...                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-find-opening-tok (pos depth)
  "Find opening parenthesis or tag"
  (interactive "d")
  (hopjs-parse-start pos)
  (with-debug
   "hopjs-find-opening-tok pos=%s depth=%s" pos depth
   (let ((depth depth)
	 (last '())
	 (res 'loop))
     (while (eq res 'loop)
       (let ((tok (hopjs-parse-consume-token-any)))
	 (hopjs-debug 0 "hopjs-parse-find-opening-tok tok=%s [%s] depth=%s" tok
		      (hopjs-parse-token-string tok)
		      depth)
	 (case (hopjs-parse-token-type tok)
	   ((rbracket rparen rbrace)
	    (setq depth (+ depth 1)))
	   ((ctag chtml)
	    (setq depth (+ depth 1))
	    (setq last tok))
	   ((lparen lbracket lbrace tilde dollar)
	    (if (> depth 1)
		(setq depth (- depth 1))
	      (setq res tok)))
	   ((otag)
	    (if (> depth 1)
		(progn
		  (setq depth (- depth 1))
		  (setq last tok))
	      (setq res tok)))
	   ((ohtml)
	    (cond
	     ((<= depth 1)
	      (setq res tok))
	     ((and last (eq (hopjs-parse-token-type last) 'chtml))
	      (setq depth (- depth 1))
	      (setq last tok))
	     ((memq (hopjs-parse-token-tag tok) hopjs-special-tags)
	      (setq last '()))
	     (t
	      (setq depth (- depth 1))
	      (setq last '()))))
	   (t
	    '()))))
     res)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-same-linep ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-same-linep (left right)
  (save-excursion
    (goto-char (hopjs-parse-token-end left))
    (end-of-line)
    (<= (hopjs-parse-token-end right) (point))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token-same-linep ...                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token-same-linep (tok)
  (hopjs-parse-same-linep (hopjs-parse-peek-token) tok))

 
