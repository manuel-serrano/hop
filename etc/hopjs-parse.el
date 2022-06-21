;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/etc/hopjs-parse.el                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  1 07:14:59 2018                          */
;*    Last change :  Tue Jun 21 09:37:54 2022 (serrano)                */
;*    Copyright   :  2018-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hopjs JavaScript/HTML parser                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'hopjs-parse)
(require 'hopjs-macro)

;*---------------------------------------------------------------------*/
;*    custom el indent                                                 */
;*---------------------------------------------------------------------*/
(put 'case 'lisp-indent-function 1)
(put 'letn 'lisp-indent-function 2)

;*---------------------------------------------------------------------*/
;*    constants                                                        */
;*---------------------------------------------------------------------*/
(defconst hopjs-parse-error-indent 1)
(defconst hopjs-parse-function-indent 1)
(defconst hopjs-parse-args-indent 3)
(defconst hopjs-parse-block-indent 3)
(defconst hopjs-parse-assig-indent 3)
(defconst hopjs-parse-while-indent 3)
(defconst hopjs-parse-return-indent 3)
(defconst hopjs-parse-binary-indent 3)
(defconst hopjs-parse-for-indent 3)
(defconst hopjs-parse-new-indent 3)
(defconst hopjs-parse-throw-indent 3)
(defconst hopjs-parse-switch-indent 3)
(defconst hopjs-parse-case-indent 3)
(defconst hopjs-parse-decl-indent 3)
(defconst hopjs-parse-property-indent 3)
(defconst hopjs-parse-call-indent 3)
(defconst hopjs-parse-xml-indent 2)

;*---------------------------------------------------------------------*/
;*    nullp                                                            */
;*---------------------------------------------------------------------*/
(defsubst nullp (o) (not (consp o)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-initial-context ...                                  */
;*    -------------------------------------------------------------    */
;*    This is the context of the parsing that dynamically changes      */
;*    when entering a plugin parser, e.g., the hiphop parser.          */
;*    The context is an alist of string and parser.                    */
;*---------------------------------------------------------------------*/
(defvar hopjs-parse-initial-context
  (vector
   '()	 ;; statements
   '()   ;; expressions
   nil)) ;; expression start

;*---------------------------------------------------------------------*/
;*    hopjs-parse-context-copy ...                                     */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-context-copy (ctx)
  (vector (aref ctx 0) (aref ctx 1) (aref ctx 2)))
				 
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
	 (id_start "[[:alpha:][:multibyte:][:nonascii:]$_#]")
	 (scmid_part "[^ (){}\"]")
	 (scmid_start "#[:]")
	 (cssid_part "[[:alnum:][:multibyte:][:nonascii:]$_#-]")
	 (cssid_start "\\(?:[[:alpha:][:multibyte:][:nonascii:]$_#]\\|--\\)")
	 (letter "[[:alpha:][:nonascii:]]")
	 (tagid (rx: "[[:digit:]]*" "[[:alnum:]_]" (rx* "[[:alnum:]_.:]*"))))
    (list
     ;; blank
     (cons "[ \t\n]+" 'blank)
     ;; indent line comments
     (cons "///[^\n]*" 'indent-comment)
     ;; line comments
     (cons (rxor "//\n" "//[^/][^\n]*") 'comment)
     ;; comments
     (cons "/[*]\\(?:[^*]\\|[*][^/]\\)*[*]+/" 'comment)
     ;; numbers
     (cons (rxor "\\(?:0[xo]\\)?[0-9]+\\>"
		 "[eE]-?[0-9]+\\>"
		 "[0-9]*\\(?:[.][0-9]\\)\\>"
		 "[0-9]+\\(?:[.][0-9]*\\)\\>")
	   'number)
     (cons (rxor "-Infinity" "Infinity") 'number)
     ;; punctuation
     (cons (rxor "[{}()[.;,:?]" (rxq "]")) 'punct)
     (cons "[?][.]" 'dot)
     ;; unop
     (cons (rxor (rxq "!") (rxq "~") (rxq "++") (rxq "--")) 'unop)
     ;; binop
     (cons (rxor "<=?" ">=?" (rxq "+") (rxq "-") "[*][*]?"
		 (rxq "%") "==+" "!==+" "||?" "&&?" "/"
		 "<<" ">>" ">>>" "[&^]") 'binop)
     (cons "instanceof" 'binop)
     (cons "in\\>" 'in)
     (cons (rxor "=" "[+*%^&|-]=" "<<=" ">>=" ">>>=") '=)
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
     ;; sealed classes
     (cons "@?sealed[ \t\n]+class" 'class)
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
     (cons (rx: "\\(<" tagid "\\)[ \t\n]+") 'ohtml)
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
;*    hopjs-parse-start-stmt-rx ...                                    */
;*---------------------------------------------------------------------*/
(defconst hopjs-parse-start-stmt-rx
  (let* ((unicode_char "[[:multibyte:]]")
	 (alpha+ "[[:alpha:][:multibyte:]]")
	 (alnum+ (rxor "[[:alnum:][:multibyte:]]"))
	 (id_part_sans "[[:nonascii:]]")
	 (id_part "[[:alnum:][:multibyte:][:nonascii:]$_]")
	 (id_start "[[:alpha:][:multibyte:][:nonascii:]$_#]")
	 (ident (rx: id_start (rx* id_part)))
	 (arg (rx: "[^,() \t]+"))
	 (arg_list (rx: "[(][^)]*[)]"))
	 (async "\\(?:async[ ]*\\)?")
	 (xmltag (rx: "<[[:digit:]]*" "[[:alnum:]_]" (rx* "[[:alnum:]_.:]*"))))
    (rxor
     ;; functions
     (rx: "^" async "function[*]?[ ]+[*]?")
     (rx: "^" async arg "[ ]*=>")
     (rx: "^" async arg_list "[ ]*=>")
     ;; service
     (rx: "^service[ ]+?" ident "*")
     (rx: "^service[ ]+?" ident "*" arg_list "[ ]*{")
     ;; class
     (rx: "^class[ ]+" ident "*" "[ ]*{")
     (rx: "^class[ ]+" ident "*" "[ ]*extends[ ]+" ident "[ ]*{")
     (rx: "^sealed[ \t]**class[ ]+" ident "[ ]*")
     ;; export, import
     "^\\(?:export\\|import\\)[ +]"
     ;; variable declarations or assignments
     "^\\(?:var\\|let\\|const\\)[ \t]"
     (rx: "^" ident "[ \t]*=")
     ;; top level calls
     (rx: "^" ident "\\([.]" ident "\\)*[ \t]*(")
     ;; blocks
     "^{"
     ;; xml
     (rx: "^" xmltag)
     ;; hiphop plugins
     (rx: "^module[ ]+" ident "*[ ]*" arg_list "[ ]*{")
     (rx: "^module[ ]+" ident "*[ ]*" arg_list "(?:[ ]*" "implements[ ]+" ident ")?[ ]*{")
     )))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-debug ...                                            */
;*---------------------------------------------------------------------*/
(defconst hopjs-parse-debug
  (or debug-on-error (getenv "EMACSDEBUG")))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-token ...                                            */
;*---------------------------------------------------------------------*/
(defsubst hopjs-parse-token (type b e)
  (if (or t hopjs-parse-debug)
      (vector type b e
	      (unless (memq type '(bof blank))
		(buffer-substring-no-properties b e)))
    (vector type b e)))

(defconst hopjs-parse-idents
  '(ident cssident class))

(defsubst hopjs-parse-tokenp (obj)
  (and (vectorp obj) (= (length obj) (if hopjs-parse-debug 4 3))))

(defsubst hopjs-parse-token-type (tok) (aref tok 0))
(defsubst hopjs-parse-token-beginning (tok) (aref tok 1))
(defsubst hopjs-parse-token-end (tok) (aref tok 2))

(defsubst hopjs-parse-token-column (tok &optional indent)
  (save-excursion
    (goto-char (hopjs-parse-token-beginning tok))
    (+ (or indent 0) (current-column))))

(defsubst hopjs-parse-token-lcollapsingp (tok)
  (memq (hopjs-parse-token-type tok) '(const let var return)))

(defsubst hopjs-parse-token-rcollapsing-arrow-parenp ()
  (let ((rest (cdr hopjs-parse-tokens))
	(armed nil)
	(res nil))
    (while (consp rest)
      (cond
       ((eq (hopjs-parse-token-type (car rest)) 'blank)
	(setq rest (cdr rest)))
       ((eq (hopjs-parse-token-type (car rest)) 'rparen)
	(setq armed t)
	(setq rest (cdr rest)))
       ((eq (hopjs-parse-token-type (car rest)) '=>)
	(setq res armed)
	(setq rest nil))
       ((memq (hopjs-parse-token-type (car rest)) '(ident comma))
	(if armed
	    (setq rest nil)
	  (setq rest (cdr rest))))
       (t
	(setq rest nil))))
    res))

(defsubst hopjs-parse-token-rcollapsing-arrow-identp ()
  (let ((rest (cdr hopjs-parse-tokens))
	(res nil))
    (while (consp rest)
      (cond
       ((eq (hopjs-parse-token-type (car rest)) 'blank)
	(setq rest (cdr rest)))
       ((eq (hopjs-parse-token-type (car rest)) '=>)
	(setq rest nil)
	(setq res t))
       (t
	(setq rest nil))))
    res))
       
(defsubst hopjs-parse-token-rcollapsingp (tok)
  (case (hopjs-parse-token-type tok)
   ((function function* service async class) t)
   ((lparen) (hopjs-parse-token-rcollapsing-arrow-parenp))
   ((ident) (hopjs-parse-token-rcollapsing-arrow-identp))
   (t nil)))
  
(defsubst hopjs-parse-token-in-linep (btok etok)
  (let ((beg (hopjs-parse-token-end btok))
	(end (hopjs-parse-token-beginning etok)))
    (when (<= beg end)
      (save-excursion
	(goto-char beg)
	(end-of-line)
	(let ((eol (point)))
	  (when (>= eol end)
	    (goto-char end)
	    (end-of-line)
	    (= (point) eol)))))))

(defsubst hopjs-parse-token-string (tok)
  (if (or t hopjs-parse-debug)
      (aref tok 3)
    (if (eq (hopjs-parse-token-type tok) 'bof)
	"<BOF>"
      (buffer-substring-no-properties
       (hopjs-parse-token-beginning tok)
       (hopjs-parse-token-end tok)))))

(defsubst hopjs-parse-token-string= (tok str)
  (string= (hopjs-parse-token-string tok) str))

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
  (goto-char (+ (or shift 1) (hopjs-parse-token-beginning tok))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-goto-token-end ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-goto-token-end (tok &optional shift)
  (goto-char (+ (or shift 0) (hopjs-parse-token-end tok))))

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
	 ((service return try catch finally while break if var let const else
		   new case switch for do default from as throw export import
		   await async class type extends interface declare)
	  (aset tok 0 sym))
	 ((function)
	  (save-excursion
	    (goto-char (hopjs-parse-token-beginning tok))
	    (if (looking-at "function[ ]*[*]")
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
	 ((await)
	  (save-excursion
	    (goto-char (hopjs-parse-token-beginning tok))
	    (if (looking-at "await")
		(progn
		  (aset tok 0 'await)
		  (aset tok 2 (match-end 0)))
	      (aset tok 0 'await))))
	 ((typeof)
	  (aset tok 0 'unop)))
       tok))
    ((ohtml)
     (aset tok 2 (match-end 1))
     (aset tok 3 (buffer-substring-no-properties (match-beginning 0) (match-end 1)))
     tok)
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
      (while (and (consp l) (not token))
	(when (and (looking-at (caar l)) (> (match-end 0) end))
	  (setq token
		(hopjs-regexp-to-token
		 (hopjs-parse-token (cdar l) (match-beginning 0) (match-end 0))
		 beg))
	  (setq end (hopjs-parse-token-end token))
	  (setq beg (hopjs-parse-token-beginning token)))
	(setq i (+ i 1))
	(setq l (cdr l)))
      token)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-tokenify ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the list of tokens between pos and limit                 */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-tokenify (pos limit &optional rev)
  (with-debug
   "hopjs-parse-tokenify pos=%s limit=%s"
   pos limit
   (save-excursion
     (let ((tokens '()))
       (goto-char pos)
       (beginning-of-line)
       (while (and (<= (point) limit) (< (point) (point-max)))
	 (let ((tok (looking-at-token)))
	   (hopjs-debug 0 "tok..p=%s limit=%s -> tok=%s" (point) limit tok)
	   (cond
	    (tok
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
       (setq tokens (cons (hopjs-parse-token 'eop (point) (point)) tokens))
       (let ((r (if rev tokens (reverse tokens))))
	 (hopjs-debug 0 "hopjs-parse-tokenify %s/%s -> %s" pos limit r)
	 r)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-pop-token ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-pop-token ()
  (if (nullp hopjs-parse-tokens)
      (hopjs-parse-token 'eop (point) (point))
    (let ((tok (car hopjs-parse-tokens)))
      (setq hopjs-parse-tokens (cdr hopjs-parse-tokens))
      (if (eq (hopjs-parse-token-type tok) 'blank)
	  (hopjs-parse-pop-token)
	tok))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-push-token ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-push-token (tok)
  (setq hopjs-parse-tokens (cons tok hopjs-parse-tokens)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token ()
  (if (nullp hopjs-parse-tokens)
      (hopjs-parse-token 'eop (point) (point))
    (let ((tok (car hopjs-parse-tokens)))
      (if (eq (hopjs-parse-token-type tok) 'blank)
	  (progn
	    (setq hopjs-parse-tokens (cdr hopjs-parse-tokens))
	    (hopjs-parse-peek-token))
	tok))))

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
;*    hopjs-parse-skip-tokens ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-skip-tokens (e)
  (while (and (consp hopjs-parse-tokens)
	      (<= (hopjs-parse-token-end (car hopjs-parse-tokens)) e))
    (setq hopjs-parse-tokens (cdr hopjs-parse-tokens))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-find-opening-tok ...                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-find-opening-tok (pos depth)
  "Find opening parenthesis or tag"
  (interactive "d")
  (with-debug
   "hopjs-find-opening-tok pos=%s depth=%s" pos depth
   (let ((depth depth)
	 (last '())
	 (res 'loop)
	 (limit (point)))
     (hopjs-debug 0 "hopjs-parse-find-opening-tok limit=%s" limit)
     (goto-char (hopjs-parse-find-start-pos limit))
     (setq hopjs-parse-tokens (reverse (cdr (hopjs-parse-tokenify (point) limit))))
     (while (eq res 'loop)
       (let ((tok (hopjs-parse-pop-token)))
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
;*    hopjs-parse-start-stmtp ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-start-stmtp (end)
  (or (re-search-forward hopjs-parse-start-stmt-rx end t 1)
      (and (= (length hopjs-parse-initial-context) 2)
	   (re-search-forward (aref hopjs-parse-initial-context 2) end t 1))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-find-start-pos ...                                   */
;*    -------------------------------------------------------------    */
;*    Find the pos from which the indentation will be computed.        */
;*    Returns the POINT of the first character of that line.           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-find-start-pos (pos)
  (with-debug
   "hopjs-parse-find-start-pos line=%s pos=%s" (line-number-at-pos pos) pos
   (save-excursion
     (let ((res nil))
       (while (not res)
	 (if (= (line-number-at-pos) 1)
	     ;; first buffer line
	     (progn
	       (beginning-of-line)
	       (skip-chars-forward "[ \t]*")
	       (setq res (point)))
	   (progn
	     (end-of-line)
	     (let ((end (point)))
	       (beginning-of-line)
	       (if (and (> end (point)) (hopjs-parse-start-stmtp end))
		   (setq res (point))
		 (let ((beg (point)))
		   (when (re-search-forward "//[^n]" end t 1)
		     (setq end (match-beginning 0))
		     (goto-char beg))
		   (when (hopjs-parse-start-stmtp end)
		     (let ((candidate (match-beginning 0)))
		       (hopjs-debug 0 "hopjs-parse-start-stmt-rx goto %s"
				  (1- (match-end 0)))  
		       (goto-char (1- (match-end 0)))
		       (forward-sexp 1)
		       (hopjs-debug 0 "hopjs-parse-start-stmt-rx forward %s -> %s"
				    (point) pos)
		       (if (> (point) pos)
			   (setq res candidate)
			 (goto-char beg)))))))
	     (previous-line 1))))
       res))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-at-debug ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-at-debug (pos)
  (let ((indent (hopjs-parse-at pos)))
    (when (numberp indent)
      (beginning-of-line)
      (indent-to indent))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-at ...                                               */
;*    -------------------------------------------------------------    */
;*    Return the point context (including the token) from which the    */
;*    indentation must be computed.                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-at (limit)
  (with-debug
   "hopjs-parse-at point=%s buf=[%s]"
   limit (buffer-substring-no-properties limit (min (point-max) (+ limit 10)))
   (save-excursion
     (if (and (= (current-column) 0) (looking-at "[^ \t\r\n]"))
	 nil
       (progn
	 (goto-char (hopjs-parse-find-start-pos limit))
	 (hopjs-debug 0 "hopjs-parse-at.start=%s" (point))
	 (setq hopjs-parse-tokens (hopjs-parse-tokenify (point) limit))
	 (let ((res nil))
	   (while (not (numberp res))
	     (let* ((tok (hopjs-parse-peek-token))
		    (indent (hopjs-parse-token-column tok)))
	       (case (hopjs-parse-token-type tok)
		 ((function function* service)
		  (setq res (hopjs-parse-function hopjs-parse-initial-context tok indent)))
		 ((class)
		  (setq res (hopjs-parse-class hopjs-parse-initial-context tok indent)))
		 ((async)
		  (let ((tok (hopjs-parse-pop-token)))
		    (if (memq (hopjs-parse-peek-token-type) '(function function*))
			(setq res (hopjs-parse-function hopjs-parse-initial-context tok indent))
		      -1)))
		 ((export)
		  (setq res (hopjs-parse-export hopjs-parse-initial-context tok indent)))
		 ((import)
		  (setq res (hopjs-parse-import hopjs-parse-initial-context tok indent)))
		 (t
		  (setq res (hopjs-parse-stmt hopjs-parse-initial-context tok indent))))))
	   res))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-function ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-function (ctx otok indent)
  (with-debug
   "hopjs-parse-function (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((ftok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((ident)
	(let ((itok (hopjs-parse-pop-token)))
	  (orn (hopjs-parse-args ctx ftok hopjs-parse-args-indent nil)
	       (hopjs-parse-block ctx otok indent))))
       ((lparen)
	(let ((ltok (hopjs-parse-peek-token)))
	  (orn (hopjs-parse-args ctx ltok 1 nil)
	       (hopjs-parse-block ctx otok indent))))
       (t
	-1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-class ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-class (ctx otok indent)
  (with-debug
   "hopjs-parse-class (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((ftok (hopjs-parse-pop-token))
	 (res nil))
     (while (not res)
       (case (hopjs-parse-peek-token-type)
	 ((eop)
	  (setq res (hopjs-parse-token-column otok indent)))
	 ((ident)
	  (hopjs-parse-pop-token)
	  (hopjs-debug 0 "hopjs-parse-class.indent otok=%s indent=%s ntok=%s"
		       otok indent (hopjs-parse-peek-token))
	  (case (hopjs-parse-peek-token-type)
	    ((eop)
	     (setq res (hopjs-parse-token-column otok indent)))
	    ((extends)
	     nil)
	    (t
	     (setq res (hopjs-parse-block ctx otok indent)))))
	 ((extends)
	  (hopjs-parse-pop-token)
	  (hopjs-debug 0 "hopjs-parse-class.extends otok=%s indent=%s ntok=%s"
		       otok indent (hopjs-parse-peek-token))
	  (if (eq (hopjs-parse-peek-token-type) 'eop)
	      (setq res (hopjs-parse-token-column otok (+ indent hopjs-parse-args-indent)))
	    (let ((e (hopjs-parse-expr ctx otok indent)))
	      (if (numberp e)
		  (setq res e)
		(setq res (hopjs-parse-block ctx otok indent))))))
	 (t
	  (setq res (hopjs-parse-block ctx otok indent)))))
     res)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-export ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-export (ctx otok indent)
  (with-debug
   "hopjs-parse-export (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((etok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((binop)
	(if (string= (hopjs-parse-token-string (hopjs-parse-peek-token)) "*")
	    (progn
	      (hopjs-parse-pop-token)
	      (case (hopjs-parse-peek-token-type)
		((eop)
		 (hopjs-parse-token-column otok (+ indent hopjs-parse-args-indent)))
		((from)
		 (hopjs-parse-pop-token)
		 (hopjs-debug 0 "hopjs-parse-export.from otok=%s indent=%s ntok=%s"
		       otok indent (hopjs-parse-peek-token))
		 (case (hopjs-parse-peek-token-type)
		   ((eop)
		    (hopjs-parse-token-column otok (+ indent hopjs-parse-args-indent)))
		   ((string)
		    (hopjs-parse-pop-token)
		    (case (hopjs-parse-peek-token-type)
		      ((eop)
		       (hopjs-parse-token-column otok (+ indent hopjs-parse-args-indent)))
		      ((semicolon)
		       (hopjs-parse-pop-token)
		       etok)))))))
	  -2))
       ((lbrace)
	(orn (hopjs-parse-export-vars ctx etok hopjs-parse-args-indent)
	     (case (hopjs-parse-peek-token-type)
	       ((eop)
		(hopjs-parse-token-column otok (+ indent hopjs-parse-args-indent)))
	       ((semicolon)
		(hopjs-parse-pop-token)
		etok))))
       ((function const var let)
	(hopjs-parse-function ctx otok indent))
       (t
	-2)))))
     
;*---------------------------------------------------------------------*/
;*    hopjs-parse-export-vars ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-export-vars (ctx otok indent)
  (with-debug
   "hopjs-parse-export-vars (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
  (let ((ltok (hopjs-parse-pop-token)))
    (case (hopjs-parse-token-type ltok)
      ((eop)
       (hopjs-parse-token-column otok indent))
      ((lbrace)
       (let ((res nil))
	 (while (not res)
	   (let ((ntok (hopjs-parse-peek-token)))
	     (hopjs-debug 0 "hopjs-parse-export-vars.next tok=%s indent=%s ntok=%s"
			  tok indent ntok)
	     (case (hopjs-parse-token-type ntok)
	       ((eop)
		(setq res (hopjs-parse-token-column otok indent)))
	       ((rbrace)
		(setq res (hopjs-parse-pop-token)))
	       ((ident)
		(hopjs-parse-pop-token)
		(hopjs-debug 0 "hopjs-parse-export-vars.var tok=%s indent=%s ntok=%s"
			     tok indent (hopjs-parse-peek-token))
		(case (hopjs-parse-peek-token-type)
		  ((eop)
		   (setq res (hopjs-parse-token-column otok indent)))
		  ((comma)
		   (hopjs-parse-pop-token))
		  ((rbrace)
		   (setq res (hopjs-parse-pop-token)))
		  ((as)
		   (hopjs-parse-pop-token)
		   (hopjs-debug 0 "hopjs-parse-export-vars.as tok=%s indent=%s ntok=%s"
				tok indent (hopjs-parse-peek-token))
		   (case (hopjs-parse-peek-token-type)
		     ((eop)
		      (setq res (hopjs-parse-token-column tok hopjs-parse-args-indent)))
		     ((ident)
		      (hopjs-parse-pop-token)
		      (when (eq (hopjs-parse-peek-token-type) 'eop)
			(setq res (hopjs-parse-token-column tok hopjs-parse-args-indent))))
		     (t
		      (setq res -4))))
		  (t
		   (setq res ntok))))
	       ((comma)
		(hopjs-parse-pop-token))
	       (t (setq res -3)))))
	 res))
      (t
       -100)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-import ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-import (ctx otok indent)
  (with-debug
   "hopjs-parse-import (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((etok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column etok indent))
       ((binop)
	(if (string= (hopjs-parse-token-string (hopjs-parse-peek-token)) "*")
	    (or (hopjs-parse-sequence
		 ctx otok (+ indent hopjs-parse-args-indent)
		 '(binop as ident from string semicolon))
		-201)))
       ((lbrace)
	(orn (hopjs-parse-export-vars ctx etok hopjs-parse-args-indent)
	     (case (hopjs-parse-peek-token-type)
	       ((eop)
		(hopjs-parse-token-column otok (+ indent hopjs-parse-args-indent)))
	       ((semicolon)
		(hopjs-parse-pop-token)
		etok))))
       ((ident function async class sealed)
	(hopjs-parse-pop-token)
	(hopjs-parse-stmt ctx etok hopjs-parse-args-indent))
       (t
	-200)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-stmt ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-stmt (ctx otok indent)
  (with-debug
   "hopjs-parse-stmt (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((tok (hopjs-parse-peek-token)))
     (case (hopjs-parse-token-type tok)
       ((eop) (hopjs-parse-token-column otok indent))
       ((if) (hopjs-parse-if ctx otok indent))
       ((while) (hopjs-parse-while ctx otok indent))
       ((do) (hopjs-parse-do ctx otok indent))
       ((semicolon) (hopjs-parse-pop-token))
       ((lbrace) (hopjs-parse-block ctx otok indent))
       ((var const let) (hopjs-parse-decls ctx otok indent))
       ((return) (hopjs-parse-return ctx otok indent))
       ((for) (hopjs-parse-for ctx otok indent))
       ((try) (hopjs-parse-try ctx otok indent))
       ((throw) (hopjs-parse-throw ctx otok indent))
       ((switch) (hopjs-parse-switch ctx otok indent))
       ((case) (hopjs-parse-case ctx otok indent))
       ((default) (hopjs-parse-default ctx otok indent))
       ((break) (hopjs-parse-break ctx otok indent))
       ((indent-comment) (hopjs-parse-comment-indent ctx otok indent))
       (t (hopjs-parse-plugin ctx otok indent))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-plugin ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-plugin (ctx otok indent)
  (let ((p (assoc (hopjs-parse-token-string (hopjs-parse-peek-token))
		  (aref ctx 0))))
    (if (consp p)
	(with-debug
	 "hopjs-parse-plugin (%s) otok=%s indent=%s ntok=%s"
	 (point) otok indent (hopjs-parse-peek-token)
	 (funcall (cdr p) ctx otok indent))
      (hopjs-parse-stmt-expr ctx otok indent))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-comment-indent ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-comment-indent (ctx otok indent)
  (let ((tok (hopjs-parse-pop-token)))
    (if (eq (hopjs-parse-peek-token-type) 'eop)
	(hopjs-parse-token-column tok 0)
      tok)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-block ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-block (ctx otok indent)
  (with-debug
   "hopjs-parse-block (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((btok (hopjs-parse-pop-token)))
     (case (hopjs-parse-token-type btok)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lbrace)
	(if (eq (hopjs-parse-peek-token-type) 'eop)
	    (let ((collapse (and (hopjs-parse-token-lcollapsingp otok)
				 (hopjs-parse-token-in-linep otok btok))))
	      (if collapse
		  (setq res (hopjs-parse-token-column otok indent))
		  (setq res (hopjs-parse-token-column otok (+ indent hopjs-parse-block-indent)))))
	  (let* ((res nil)
		 (collapse (and (hopjs-parse-token-lcollapsingp otok)
				(hopjs-parse-token-in-linep otok btok)))
		 (pshift (if collapse 0 hopjs-parse-block-indent))
		 (nshift (if collapse hopjs-parse-block-indent 0)))
	    (while (not res)
	      (let ((tok (hopjs-parse-peek-token)))
		(hopjs-debug
		 0 "hopjs-parse-block.next otok=%s indent=%s ntok=%s collpase=%s" otok indent tok collapse)
		(case (hopjs-parse-token-type tok)
		  ((eop)
		   (setq res (hopjs-parse-token-column otok (+ indent pshift))))
		  ((comment)
		   (hopjs-parse-pop-token))
		  ((rbrace)
		   (hopjs-parse-pop-token)
		   (if (eq (hopjs-parse-peek-token-type) 'eop)
		       (setq res (hopjs-parse-token-column otok (- indent nshift)))
		     (setq res btok)))
		  (t
		   (let ((s (hopjs-parse-stmt ctx otok (+ indent pshift))))
		     (when (numberp s)
		       (setq res s)))))))
	    res)))
       (t
	-3)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-stmt-expr ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-stmt-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-stmt-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((e (hopjs-parse-expr ctx otok indent)))
     (hopjs-debug 0 "hopjs-parse-stmt-expr.next e=%s ntok=%s"
		  e (hopjs-parse-peek-token))
     (orn e
	  (let ((tok (hopjs-parse-peek-token)))
	    (case (hopjs-parse-token-type tok)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((semicolon)
	       (hopjs-parse-pop-token)
	       otok)
	      ((comment)
	       (hopjs-parse-pop-token)
	       otok)
	      (t
	       otok)))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-if ...                                               */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-if (ctx otok indent)
  (with-debug
   "hopjs-parse-if (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((itok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-paren-expr ctx itok hopjs-parse-args-indent)
	    (let ((indent (if (eq (hopjs-parse-peek-token-type) 'lbrace)
			      indent
			    (+ indent hopjs-parse-block-indent))))
	      (hopjs-parse-stmt ctx otok indent))
	    (let ((ntok (hopjs-parse-peek-token)))
	      (hopjs-debug 0 "hopjs-parse-if.next otok=%s ntok=%s" otok ntok)
	      (case (hopjs-parse-token-type ntok)
                ((eop)
		 (hopjs-parse-token-column otok indent))
		((else)
		 (hopjs-parse-pop-token)
		 (hopjs-debug 0 "hopjs-parse-if.else otok=%s ntok=%s" otok
			      (hopjs-parse-peek-token))
		 (case (hopjs-parse-peek-token-type)
		   ((if) (orn (hopjs-parse-if ctx otok indent) itok))
		   ((lbrace) (orn (hopjs-parse-stmt ctx otok indent) itok))
		   (t (orn (hopjs-parse-stmt ctx itok 0) itok))))
		(t
		 itok))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-while ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-while (ctx otok indent)
  (with-debug
   "hopjs-parse-while (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((wtok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-paren-expr ctx wtok hopjs-parse-args-indent)
	    (let ((indent (if (eq (hopjs-parse-peek-token-type) 'lbrace)
			      indent
			    (+ indent hopjs-parse-block-indent))))
	      (hopjs-parse-stmt ctx otok indent)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-do ...                                               */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-do (ctx otok indent)
  (with-debug
   "hopjs-parse-do (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((dtok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-block ctx otok indent)
	    (case (hopjs-parse-peek-token-type)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((while)
	       (hopjs-parse-pop-token)
	       (orn (hopjs-parse-paren-expr ctx otok indent)
		    dtok))
	      (t
	       (let ((p (assoc (hopjs-parse-token-string (hopjs-parse-peek-token)) (aref ctx 0))))
		 (if (consp p)
		     (funcall (cdr p) ctx otok indent)
		   -80)))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-decls ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-decls (ctx otok indent)
  (with-debug
   "hopjs-parse-decls (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((dtok (hopjs-parse-pop-token)))
     (hopjs-debug 0 "hopjs-parse-decls.next otok=%s indent=%s ntok=%s"
		  otok indent (hopjs-parse-peek-token))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-vars ctx dtok hopjs-parse-decl-indent)
	    dtok)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-vars ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-vars (ctx otok indent)
  (with-debug
   "hopjs-parse-vars (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (if (eq (hopjs-parse-peek-token-type) 'eop)
       (hopjs-parse-token-column otok indent)
     (let ((vars (hopjs-parse-var ctx otok indent)))
       (orn vars
	    (case (hopjs-parse-peek-token-type)
	      ((semicolon)
	       (hopjs-parse-pop-token)
	       otok)
	      ((comma)
	       (let ((tok (hopjs-parse-pop-token)))
		 (hopjs-debug 0 "hopjs-parse-vars.comma= tok=%s" tok)
		 (hopjs-parse-vars ctx vars 0)))
	      ((in of)
	       otok)
	      (t
	       vars)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-var ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-var (ctx otok indent)
  (with-debug
   "hopjs-parse-var (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
     ((eop)
      (hopjs-parse-token-column otok indent))
     ((ident type as)
      (let ((tok (hopjs-parse-pop-token)))
	(hopjs-debug 0 "hopjs-parse-var.= tok=%s" (hopjs-parse-peek-token))
	(case (hopjs-parse-peek-token-type)
	  ((eop)
	   (hopjs-parse-token-column otok indent))
	  ((=)
	   (let ((etok (hopjs-parse-pop-token)))
	     (hopjs-debug 0 "hopjs-parse-var.next tok=%s" (hopjs-parse-peek-token))
	     (if (eq (hopjs-parse-peek-token-type) 'eop)
		 (hopjs-parse-token-column otok hopjs-parse-assig-indent)
	       (let ((e (hopjs-parse-assig-expr ctx otok hopjs-parse-assig-indent)))
		 (orn e
		      (if (eq (hopjs-parse-peek-token-type) 'eop)
			  (hopjs-parse-token-column otok hopjs-parse-assig-indent)
			tok))))))
	  ((semicolon comma in)
	   otok)
	  ((ident)
	   (if (hopjs-parse-token-string= (hopjs-parse-peek-token) "of")
	       otok
	     -42))
	  (t
	   -41))))
     ((lbrace lbracket)
      (hopjs-debug 0 "hopjs-parse-destructure.= tok=%s" (hopjs-parse-peek-token))
      (orn (hopjs-parse-primary ctx otok indent)
	   (case (hopjs-parse-peek-token-type)
	     ((eop)
	      (hopjs-parse-token-column otok indent))
	     ((=)
	      (let ((etok (hopjs-parse-pop-token)))
		(hopjs-debug 0 "hopjs-parse-destructure.next tok=%s" (hopjs-parse-peek-token))
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-token-column otok hopjs-parse-assig-indent)
		  (let ((e (hopjs-parse-assig-expr ctx otok hopjs-parse-assig-indent)))
		    (orn e
			 (if (eq (hopjs-parse-peek-token-type) 'eop)
			     (hopjs-parse-token-column otok hopjs-parse-assig-indent)
			   tok))))))
	     ((semicolon comma in of)
	      otok)
	     ((ident)
	      (if (hopjs-parse-token-string= (hopjs-parse-peek-token) "of")
		  otok
		-44))
	     (t
	      -43))))
     (t
      -40))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-return ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-return (ctx otok indent)
  (with-debug
   "hopjs-parse-return (%s) otok=%s ntok=%s" (point)
   otok (hopjs-parse-peek-token)
   (let ((tok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((semicolon)
	(hopjs-parse-pop-token)
	tok)
       (t
	(let ((indent (if (and (hopjs-parse-token-rcollapsingp
				(hopjs-parse-peek-token))
			       (hopjs-parse-token-in-linep
				otok (hopjs-parse-peek-token)))
			  0
			hopjs-parse-return-indent)))
	  (orn (hopjs-parse-expr ctx tok indent)
	       (if (eq (hopjs-parse-peek-token-type) 'semicolon)
		   (progn
		     (hopjs-parse-pop-token)
		     tok)
		 tok))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-for ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-for (ctx otok indent)
  (with-debug
   "hopjs-parse-for (%s) otok=%s ntok=%s" (point)
   otok (hopjs-parse-peek-token)
   (let ((ftok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lparen)
	(let ((tok (hopjs-parse-pop-token))
	      (fctx (hopjs-parse-for-in-context ctx)))
	  (orn (hopjs-parse-stmt fctx ftok hopjs-parse-args-indent)
	       (if (memq (hopjs-parse-peek-token-type) '(in of))
		   (progn
		     (hopjs-debug 0 "hopjs-parse-for.in ntok=%s"
				  (hopjs-parse-peek-token))
		     (hopjs-parse-pop-token)
		     (orn (hopjs-parse-expr ctx tok hopjs-parse-args-indent)
			  (if (eq (hopjs-parse-peek-token-type) 'rparen)
			      (progn
				(hopjs-parse-pop-token)
				(hopjs-debug 0 "hopjs-parse-for.in.block ntok=%s"
					     (hopjs-parse-peek-token))
				(hopjs-parse-stmt ctx otok indent))
			    -1)))
		 (orn (hopjs-parse-stmt ctx ftok hopjs-parse-args-indent)
		      (hopjs-parse-expr ctx ftok hopjs-parse-args-indent)
		      (if (eq (hopjs-parse-peek-token-type) 'rparen)
			  (progn
			    (hopjs-parse-pop-token)
			    (hopjs-debug 0 "hopjs-parse-for.block ntok=%s"
					 (hopjs-parse-peek-token))
			    (hopjs-parse-stmt ctx otok indent))
			-1))))))
       (t
	-1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-for-in-context ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-for-in-context (ctx)
  (let ((nctx (hopjs-parse-context-copy ctx)))
    (aset nctx 1
	  (cons (cons "in" #'(lambda (ctx o indent) nil))
		(aref ctx 1)))
    nctx))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-try ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-try (ctx otok indent)
  (with-debug
   "hopjs-parse-try (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
   (let ((ttok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (let ((e (hopjs-parse-stmt ctx otok indent)))
	 (hopjs-debug 0 "hopjs-parse-try.next e=%s ntok=%s"
		      e (hopjs-parse-peek-token))
	 (orn e
	      (case (hopjs-parse-peek-token-type)
		((eop)
		 (hopjs-parse-token-column ttok 2))
		((catch)
		 (hopjs-parse-pop-token)
		 (hopjs-debug 0 "hopjs-parse-try.catch ntok=%s"
			      (hopjs-parse-peek-token))
		 (orn (hopjs-parse-paren-expr ctx otok indent)
		      (let ((e (hopjs-parse-stmt ctx otok indent)))
			(hopjs-debug 0 "hopjs-parse-try.catch.e e=%s ntok=%s"
				     e (hopjs-parse-peek-token))
			(orn e
			     (case (hopjs-parse-peek-token-type)
			       ((eop)
				(hopjs-parse-token-column ttok 2))
			       ((finally)
				(hopjs-parse-stmt ctx otok indent))
			       (t
				e))))))
		((finally)
		 (hopjs-parse-pop-token)
		 (hopjs-debug 0 "hopjs-parse-finally.next ntok=%s"
			      (hopjs-parse-peek-token))
		 (case (hopjs-parse-peek-token-type)
		   ((eop)
		    (hopjs-parse-token-column otok indent))
		   (t
		    (hopjs-parse-stmt ctx otok indent))))
		(t
		 -90))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-throw ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-throw (ctx otok indent)
  (with-debug
   "hopjs-parse-throw (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
   (let ((ttok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-expr ctx ttok hopjs-parse-throw-indent)
	    ttok)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-switch ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-switch (ctx otok indent)
  (with-debug
   "hopjs-parse-switch (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
   (let ((stok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-paren-expr ctx stok hopjs-parse-args-indent)
	    (hopjs-parse-stmt ctx otok indent)
	    stok)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-case ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-case (ctx otok indent)
  (with-debug
   "hopjs-parse-case (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
   (let ((ctok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-expr ctx otok indent)
	    (case (hopjs-parse-peek-token-type)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((colon)
	       (hopjs-parse-case-block ctx ctok hopjs-parse-switch-indent))
	      (t
	       -100)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-default ...                                          */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-default (ctx otok indent)
  (with-debug
   "hopjs-parse-default (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
   (let ((ctok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (case (hopjs-parse-peek-token-type)
	 ((eop)
	  (hopjs-parse-token-column otok indent))
	 ((colon)
	  (hopjs-parse-case-block ctx ctok hopjs-parse-switch-indent))
	 (t
	  -100))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-break ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-break (ctx otok indent)
  (with-debug
   "hopjs-parse-break (%s) otok=%s indent=%s ntok=%s" (point)
   otok indent (hopjs-parse-peek-token)
   (let ((btok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       btok))))
	 
;*---------------------------------------------------------------------*/
;*    hopjs-parse-case-block ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-case-block (ctx otok indent)
  (with-debug
   "hopjs-parse-case-block (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
  (let ((btok (hopjs-parse-pop-token)))
    (case (hopjs-parse-token-type btok)
      ((eop)
       (hopjs-parse-token-column otok hopjs-parse-block-indent))
      ((colon)
       (let ((res nil)
	     (ltok otok)
	     (lindent hopjs-parse-block-indent))
	 (while (not res)
	   (let ((tok (hopjs-parse-peek-token)))
	     (hopjs-debug
	      0 "hopjs-parse-case-block.next otok=%s ntok=%s ltok=%s lindent=%s"
	      otok tok ltok lindent)
	     (case (hopjs-parse-token-type tok)
	       ((eop)
		(setq res (hopjs-parse-token-column ltok lindent)))
	       ((rbrace case default)
		(setq res tok))
	       ((comment)
		(hopjs-parse-pop-token))
	       (t
		(let ((s (hopjs-parse-stmt ctx ltok lindent)))
		  (cond
		   ((numberp s)
		    (setq res s))
		   ((or (eq s otok) (eq (hopjs-parse-token-type s) 'semicolon))
		    nil)
		   (t
		    (setq ltok s)
		    (setq lindent 0))))))))
	 res))
      (t
       -1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-args ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-args (ctx otok indent callp)
  (with-debug
   "hopjs-parse-args (%s) otok=%s indent=%s ntok=%s callp=%s" (point)
   otok indent (hopjs-parse-peek-token) callp
  (let ((ltok (hopjs-parse-pop-token)))
    (case (hopjs-parse-token-type ltok)
      ((eop)
       (hopjs-parse-token-column otok indent))
      ((lparen)
       (let ((res nil)
	     (aindent (if (and (hopjs-parse-token-lcollapsingp otok)
			       (hopjs-parse-token-in-linep otok ltok))
			 indent
			 (+ hopjs-parse-args-indent indent)))
	     (atok otok)
	     (armed callp))
	 (while (not res)
	   (let ((ntok (hopjs-parse-peek-token)))
	     (hopjs-debug 0 "hopjs-parse-args.next tok=%s indent=%s ntok=%s"
			  atok aindent ntok)
	     (case (hopjs-parse-token-type ntok)
	       ((eop)
		(setq res (hopjs-parse-token-column atok aindent)))
	       ((rparen)
		(setq res (hopjs-parse-pop-token)))
	       ((comment)
		(hopjs-parse-pop-token)
		nil)
	       (t
		(hopjs-debug 0 "hopjs-parse-args.collapse ltok=%s ntok=%s rcollapse=%s inline=%s"
			     ltok (hopjs-parse-peek-token)
			     (hopjs-parse-token-rcollapsingp (hopjs-parse-peek-token))
			     (hopjs-parse-token-in-linep ltok (hopjs-parse-peek-token)))
		(let ((e (if (and (hopjs-parse-token-rcollapsingp (hopjs-parse-peek-token))
				  (hopjs-parse-token-in-linep ltok (hopjs-parse-peek-token)))
			     (hopjs-parse-assig-expr ctx otok indent)
			   (hopjs-parse-assig-expr ctx atok aindent))))
		  (hopjs-debug 0 "hopjs-parse-args.e indent=%s e=%s ntok=%s"
			       aindent e (hopjs-parse-peek-token))
		  (if (numberp e)
		      (setq res e)
		    (let ((ntok (hopjs-parse-peek-token)))
		      (case (hopjs-parse-token-type ntok)
			((eop)
			 (setq res (hopjs-parse-token-column tok 0)))
			((comma)
			 (hopjs-parse-pop-token)
			 (unless armed
			   (setq armed t)
			   (setq aindent 0)
			   (setq atok e)))
			((rparen)
			 (setq res (hopjs-parse-pop-token)))
			(t
			 (setq res e))))))))))
	 res))
      (t
       -100)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-paren-expr ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-paren-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-paren-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((tok (hopjs-parse-pop-token)))
     (case (hopjs-parse-token-type tok)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lparen)
	(let ((e (hopjs-parse-expr ctx otok indent)))
	  (orn e
	       (let ((rtok (hopjs-parse-pop-token)))
		 (if (eq (hopjs-parse-token-type rtok) 'rparen)
		     tok
		   -1)))))
       (t
	-1)))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-expr ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((a (hopjs-parse-assig-expr ctx otok indent)))
     (orn a
	  (if (eq (hopjs-parse-peek-token-type) 'comma)
	      (progn
		(hopjs-parse-pop-token)
		(hopjs-parse-expr ctx otok indent))
	    a)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-assig-expr ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-assig-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-assig-expr (%s) otok=%s indent=%s ntok=%s" 
   (point) otok indent (hopjs-parse-peek-token)
   (let ((l (hopjs-parse-cond-expr ctx otok indent)))
     (hopjs-debug 0 "hopjs-parse-assig-expr.next otok=%s indent=%s ntok=%s l=%s"
		  otok indent (hopjs-parse-peek-token) l)
     (orn l
	  (case (hopjs-parse-peek-token-type)
	    ((eop)
	     (hopjs-parse-token-column otok indent))
	    ((=)
	     (let* ((etok (hopjs-parse-pop-token))
		    (tok (hopjs-parse-peek-token)))
	       (if (eq (hopjs-parse-token-type tok) 'eop)
		   (hopjs-parse-token-column otok hopjs-parse-assig-indent)
		 (let ((eindent (if (hopjs-parse-token-in-linep etok tok)
				    0
				  hopjs-parse-assig-indent)))
		   (hopjs-parse-assig-expr ctx otok (+ indent eindent))))))
	    (t
	     l))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-cond-expr ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-cond-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-cond-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((b (hopjs-parse-binary-expr ctx otok indent)))
     (hopjs-debug 0 "hopjs-parse-cond-expr.next b=%s ntok=%s"
		  b (hopjs-parse-peek-token))
     (orn b
	  (if (eq (hopjs-parse-peek-token-type) 'qmark)
	      (let ((tok (hopjs-parse-pop-token)))
		(hopjs-debug 0 "hopjs-parse-cond-expr.qmark b=%s ntok=%s"
			     b (hopjs-parse-peek-token))
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-token-column otok indent)
		  (let ((e (hopjs-parse-assig-expr ctx otok hopjs-parse-binary-indent)))
		    (hopjs-debug 0 "hopjs-parse-cond-expr.colon e=%s ntok=%s"
				 e (hopjs-parse-peek-token))
		    (orn e
			 (if (eq (hopjs-parse-peek-token-type) 'colon)
			     (let ((tok (hopjs-parse-pop-token)))
			       (if (eq (hopjs-parse-peek-token-type) 'eop)
				   (hopjs-parse-token-column otok indent)
				 (hopjs-parse-assig-expr ctx tok 0)))
			   b))))))
	  b))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-binary-expr ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-binary-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-binary-expr (%s) tok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (if (eq (hopjs-parse-token-type otok) 'eop)
       (hopjs-parse-token-column otok indent)
     (let ((e (hopjs-parse-unary-expr ctx otok indent)))
       (orn e
	    (case (hopjs-parse-peek-token-type)
	      ((binop)
	       (let ((tok (hopjs-parse-pop-token)))
		 (orn (hopjs-parse-binary-expr ctx otok indent) e)))
	      ((in)
	       (let ((p (assoc "in" (aref ctx 1))))
		 (if p
		     (orn (funcall (cdr p) ctx otok indent) e)
		   (let ((tok (hopjs-parse-pop-token)))
		     (orn (hopjs-parse-binary-expr ctx otok indent) e)))))
	      (t
	       e)))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-unary-expr ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-unary-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-unary-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
     ((eop)
      (hopjs-parse-token-column otok indent))
     ((unop)
      (let ((tok (hopjs-parse-pop-token)))
	(hopjs-parse-unary-expr ctx otok indent)))
     (t
      (if (and (eq (hopjs-parse-peek-token-type) 'binop)
	       (let ((str (hopjs-parse-token-string (hopjs-parse-peek-token))))
		 (or (string= str "+") (string= str "-"))))
	  (let ((tok (hopjs-parse-pop-token)))
	    (hopjs-parse-unary-expr ctx otok indent))
	(let ((e (hopjs-parse-lhs ctx otok indent)))
	  (hopjs-debug
	   0 "hopjs-parse-unary.next e=%s ntok=%s"
	   e (hopjs-parse-peek-token))
	  (orn e
	       (if (eq (hopjs-parse-peek-token-type) 'unop)
		   (hopjs-parse-pop-token)
		 e))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-lhs ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-lhs (ctx otok indent)
  (with-debug
   "hopjs-parse-lhs (%s) tok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (let ((e (hopjs-parse-new-expr ctx otok indent)))
     (orn e
	  (hopjs-parse-access-or-call ctx otok indent e)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-new-expr ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-new-expr (ctx otok indent)
  (with-debug
   "hopjs-parse-new-expr (%s) tok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
      ((new)
       (hopjs-parse-pop-token)
       (hopjs-parse-expr ctx otok indent))
      ((yield await)
       (hopjs-parse-pop-token)
       (if (eq (hopjs-parse-peek-token-type) 'eop)
	   (hopjs-parse-token-column otok indent)
	 (hopjs-parse-assig-expr ctx tok indent)))
      ((comment)
       (hopjs-parse-pop-token)
       (hopjs-parse-new-expr ctx otok indent))
      (t
       (let ((p (hopjs-parse-primary ctx otok indent)))
	 (orn p
	      (hopjs-parse-access-or-call ctx otok indent p)))))))
	  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-access-or-call ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-access-or-call (ctx otok indent e)
  (with-debug
   "hopjs-parse-access-or-call (%s) otok=%s indent=%s e=%s ntok=%s"
   (point) otok indent e (hopjs-parse-peek-token)
     
   (letn axs ()
     (let ((tok (hopjs-parse-peek-token)))
       (case (hopjs-parse-token-type tok)
	 ((eop)
	  (hopjs-parse-token-column otok indent))
	 ((lparen)
	  (hopjs-debug 0 "hopjs-parse-access-or-call.lparen otok=%s indent=%s ntok=%s"
		       otok indent tok)
	  (orn (hopjs-parse-args ctx otok indent t)
	       (funcall axs)))
	 ((dot)
	  (let ((tok (hopjs-parse-pop-token)))
	    (hopjs-debug 0 "hopjs-parse-access-or-call.dot otok=%s indent=%s e=%s tok=%s ntok=%s"
			 otok indent e tok (hopjs-parse-peek-token))
	    (unless (or (hopjs-parse-token-in-linep otok e)
			(hopjs-parse-token-in-linep e tok))
	      (setq otok e)
	      (setq indent hopjs-parse-args-indent)
	      (hopjs-debug 0 "hopjs-parse-access-or-call.dot.newotok otok=%s indent=%s tok=%s ntok=%s"
			   otok indent tok (hopjs-parse-peek-token)))
	    (case (hopjs-parse-peek-token-type)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((ident type catch declare interface class)
	       (hopjs-parse-pop-token)
	       (funcall axs))
	      (t
	       -900))))
	 ((lbracket)
	  (let* ((tok (hopjs-parse-pop-token))
		 (e (hopjs-parse-expr ctx tok hopjs-parse-args-indent)))
	    (hopjs-debug 0 "hopjs-parse-access-or-call.lbracket otok=%s indent=%s tok=%s ne=%s ntok=%s"
			 otok indent tok e (hopjs-parse-peek-token))
	    
	    (orn e
		 (case (hopjs-parse-peek-token-type)
		   ((eop)
		    (hopjs-parse-token-column otok indent))
		   ((rbracket)
		    (hopjs-parse-pop-token)
		    (funcall axs))
		   (t
		    -901)))))
	 (t
	  e))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-primary ...                                          */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-primary (ctx otok indent)
  (with-debug
   "hopjs-parse-primary (%s) tok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
     ((eop)
      (hopjs-parse-token-column otok indent))
     ((comment)
      (hopjs-parse-pop-token))
     ((indent-comment)
      (hopjs-parse-token-column (hopjs-parse-pop-token) 0))
     ((number string boolean regexp catch)
      (hopjs-parse-pop-token))
     ((ident type declare interface)
      (let ((p (assoc (hopjs-parse-token-string (hopjs-parse-peek-token)) (aref ctx 1))))
	(if (consp p)
	    (funcall (cdr p) ctx otok indent)
	  (let ((tok (hopjs-parse-pop-token)))
	    (case (hopjs-parse-peek-token-type)
	      ((=>) (hopjs-parse-arrow ctx otok tok indent))
	      (t tok))))))
     ((dots)
      (hopjs-parse-pop-token)
      (hopjs-parse-primary ctx otok indent))
     ((function function* service)
      (hopjs-parse-function ctx otok indent))
     ((async)
      (let ((tok (hopjs-parse-pop-token)))
	(hopjs-parse-primary ctx otok indent)))
     ((class)
      (hopjs-parse-class ctx otok indent))
     ((lparen)
      (let ((tok (hopjs-parse-pop-token)))
	(case (hopjs-parse-peek-token-type)
	  ((eop)
	   (hopjs-parse-token-column otok indent))
	  ((rparen)
	   (let ((tok (hopjs-parse-pop-token)))
	     (case (hopjs-parse-peek-token-type)
	       ((eop) (hopjs-parse-token-column otok indent))
	       ((=>) (hopjs-parse-arrow ctx otok tok indent))
	       (t tok))))
	  (t
	   (let ((e (hopjs-parse-expr ctx tok 1)))
	     (hopjs-debug 0 "hopjs-parse-primary.next tok=%s e=%s"
			  (hopjs-parse-peek-token) e)
	     (orn e
		  (case (hopjs-parse-peek-token-type)
		    ((eop)
		     (hopjs-parse-token-column e))
		    ((rparen)
		     (hopjs-parse-pop-token)
		     (case (hopjs-parse-peek-token-type)
		       ((eop) tok)
		       ((=>) (hopjs-parse-arrow ctx otok tok indent))
		       (t tok)))
		    ((comma)
		     (hopjs-parse-expr ctx tok 1))
		    (t -1001))))))))
     ((lbracket)
      (hopjs-parse-array ctx otok indent))
     ((lbrace)
      (hopjs-parse-object ctx otok indent))
     ((ohtml otag)
      (hopjs-parse-xml ctx otok indent))
     ((dollar)
      (hopjs-parse-dollar ctx otok indent))
     ((tilde)
      (hopjs-parse-tilde ctx otok indent))
     (t
      (let ((p (assoc (hopjs-parse-token-string (hopjs-parse-peek-token)) (aref ctx 1))))
	(if (consp p)
	    (funcall (cdr p) ctx otok indent)
	  -1000))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-arrow ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-arrow (ctx otok tok indent)
  (with-debug
   "hopjs-parse-arrow (%s) otok=%s indent=%s tok=%s ntok=%s"
   (point) otok indent tok (hopjs-parse-peek-token)
   (let* ((atok (hopjs-parse-pop-token))
	  (collapse (hopjs-parse-token-in-linep otok atok)))
     (hopjs-debug 0 "hopjs-parse-arrow.next otok=%s atok=%s ntok=%s collapse=%s"
		  otok atok (hopjs-parse-peek-token) collapse)
     (let ((aindent (if (and (hopjs-parse-token-rcollapsingp (hopjs-parse-peek-token))
			     (hopjs-parse-token-in-linep atok (hopjs-parse-peek-token)))
			0
		      hopjs-parse-block-indent)))
       (case (hopjs-parse-peek-token-type)
	 ((eop)
	  (if collapse
	      (hopjs-parse-token-column otok indent)
	    (hopjs-parse-token-column otok (+ hopjs-parse-block-indent indent))))
	 ((lbrace)
	  (let ((collapse2 (hopjs-parse-token-in-linep atok (hopjs-parse-peek-token)))
		(collapse3 (hopjs-parse-token-in-linep tok atok)))
	    (hopjs-debug 0 "hopjs-parse-arrow.lbrace otok=%s atok=%s ntok=%s collapse=%s collapse2=%s collapse3=%s"
			 otok atok (hopjs-parse-peek-token)
			 collapse collapse2 collapse3)
	    (let ((tok (hopjs-parse-pop-token)))
	      (cond
	       ((and collapse collapse2)
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-block ctx otok hopjs-parse-block-indent)
		  (progn
		    (hopjs-parse-push-token tok)
		    (hopjs-parse-block ctx otok indent))))
	       (collapse
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-block ctx otok hopjs-parse-block-indent)
		  (progn
		    (hopjs-parse-push-token tok)
		    (hopjs-parse-block ctx otok (+ hopjs-parse-block-indent indent)))))
	       (collapse3
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-block ctx otok hopjs-parse-block-indent)
		  (progn
		    (hopjs-parse-push-token tok)
		    (hopjs-parse-block ctx tok 0))))
	       (collapse2
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-block ctx otok hopjs-parse-block-indent)
		  (progn
		    (hopjs-parse-push-token tok)
		    (hopjs-parse-block ctx atok 0))))
	       (t
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (hopjs-parse-block ctx otok hopjs-parse-block-indent)
		  (progn
		    (hopjs-parse-push-token tok)
		    (hopjs-parse-block ctx atok 0))))))))
	 (t
	  (hopjs-parse-expr ctx otok (+ indent aindent))))))))
	
;*---------------------------------------------------------------------*/
;*    hopjs-parse-array ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-array (ctx otok indent)
  (with-debug
   "hopjs-parse-array (%s) tok=%s ntok=%s indent=%s"
   (point) otok (hopjs-parse-peek-token) indent
   (let ((ltok (hopjs-parse-pop-token)))
     (case (hopjs-parse-token-type ltok)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lbracket)
	(let ((res nil))
	  (while (not res)
	    (let ((tok (hopjs-parse-peek-token)))
	      (case (hopjs-parse-token-type tok)
		((eop)
		 (setq res (hopjs-parse-token-column ltok lindent)))
		((rbracket)
		 (setq res (hopjs-parse-pop-token)))
		((comma)
		 (let ((tok (hopjs-parse-pop-token)))
		   (when (eq (hopjs-parse-peek-token-type) 'eop)
		     (setq res tok))))
		(t
		 (let ((e (hopjs-parse-assig-expr ctx otok indent)))
		   (hopjs-debug 0 "hopjs-parse-array.e e=%s" e)
		   (if (numberp e)
		       (setq res e)
		     (let ((tok (hopjs-parse-peek-token)))
		       (hopjs-debug 0 "hopjs-parse-array.next tok=%s" tok)
		       (case (hopjs-parse-token-type tok)
			 ((eop)
			  (setq res (hopjs-parse-token-column otok indent)))
			 ((comma)
			  (hopjs-parse-pop-token))
			 ((rbracket)
			  (hopjs-parse-pop-token)
			  (setq res ltok))
			 (t
			  (setq res e))))))))))
	  res))
       (t
	-1010)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-object ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-object (ctx otok indent)
  (with-debug
   "hopjs-parse-object (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((ltok (hopjs-parse-pop-token)))
     (case (hopjs-parse-token-type ltok)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lbrace)
	(let* ((res nil)
	       (collapse (and (or (hopjs-parse-token-lcollapsingp otok)
				  (memq (hopjs-parse-token-type otok) hopjs-parse-idents))
			      (hopjs-parse-token-in-linep otok ltok)))
	       (pshift (if collapse 0 hopjs-parse-args-indent))
	       (nshift (if collapse hopjs-parse-args-indent 0)))
	  (while (not res)
	    (let ((tok (hopjs-parse-peek-token)))
	      (hopjs-debug 0 "hopjs-parse-object.lbrace otok=%s ltok=%s ntok=%s collapse=%s"
			   otok ltok tok collapse)
	      (case (hopjs-parse-token-type tok)
                ((eop)
		 (setq res (hopjs-parse-token-column otok indent)))
		((rbrace)
		 (let ((tok (hopjs-parse-pop-token)))
		   (if (eq (hopjs-parse-peek-token-type) 'eop)
		       (setq res (hopjs-parse-token-column otok (- indent nshift)))
		     (setq res tok))))
		((comment)
		 (let ((tok (hopjs-parse-pop-token)))
		   (when (eq (hopjs-parse-peek-token-type) 'eop)
		     (setq res (hopjs-parse-token-column otok (+ indent pshift))))))
		((dots)
		 (let ((tok (hopjs-parse-pop-token)))
		   (when (eq (hopjs-parse-peek-token-type) 'eop)
		     (setq rest tok))))
		((ident type as string)
		 (let ((tok (hopjs-parse-pop-token)))
		   (hopjs-debug 0 "hopjs-parse-object.lbrace.ident tok=%s ntok=%s"
				tok (hopjs-parse-peek-token))
		   (case (hopjs-parse-peek-token-type)
		     ((eop)
		      (setq res (hopjs-parse-token-column otok (+ indent pshift)))
		      ;; (setq res (hopjs-parse-token-column otok indent))
		      )
		     ((colon)
		      (let ((ctok (hopjs-parse-pop-token)))
			(hopjs-debug 0 "hopjs-parse-object.lbrace.ident.colon ctok=%s ntok=%s"
				     ctok (hopjs-parse-peek-token))
			(if (eq (hopjs-parse-peek-token-type) 'eop)
			    (setq res (hopjs-parse-token-column tok hopjs-parse-args-indent))
			  (let* ((eindent (if (and (hopjs-parse-token-rcollapsingp
						    (hopjs-parse-peek-token))
						   (hopjs-parse-token-in-linep
						    tok
						    (hopjs-parse-peek-token)))
					      0
					    hopjs-parse-property-indent))
				 (e (hopjs-parse-assig-expr ctx tok eindent)))
			    (hopjs-debug 0 "hopjs-parse-object.lbrace.ident.colon.e e=%s" e)
			    (if (numberp e)
				(setq res e)
			      (let ((ntok (hopjs-parse-peek-token)))
				(hopjs-debug 0 "hopjs-parse-object.lbrace.ident.colon.next tok=%s" ntok)
				(case (hopjs-parse-token-type ntok)
				  ((eop)
				   (setq res (hopjs-parse-token-column otok (+ hopjs-parse-args-indent indent)))
				   (setq res (hopjs-parse-token-column otok indent)))
				  ((comma)
				   (hopjs-parse-pop-token))
				  ((rbrace)
				   (hopjs-parse-pop-token)
				   (if (eq (hopjs-parse-peek-token-type) 'eop)
				       (setq res (hopjs-parse-token-column otok (- indent nshift)))
				     (setq res ltok)))
				  (t
				   (setq res e)))))))))
		     ((comma)
		      (hopjs-parse-pop-token)))))
		((lbracket)
		 (let ((tok (hopjs-parse-pop-token)))
		   (if (eq (hopjs-parse-peek-token-type) 'eop)
		       (hopjs-parse-token-column otok indent)
		     (orn (hopjs-parse-expr ctx otok indent)
			  (case (hopjs-parse-peek-token-type)
			    ((eop)
			     (setq res (hopjs-parse-token-column otok indent)))
			    ((rbracket)
			     (hopjs-parse-pop-token)
			     (if (eq (hopjs-parse-peek-token-type) 'eop)
				 (setq res (hopjs-parse-token-column otok 0))
			       (setq res ltok)))
			    (t
			     (setq res -1022)))))))
		(t
		 (setq res -1021)))))
	    res))
       (t
	-1020)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-xml ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-xml (ctx otok indent)
  (with-debug
   "hopjs-parse-xml (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
     ((ohtml)
      (hopjs-parse-ohtml ctx otok indent))
     ((otag)
      (hopjs-parse-otag ctx otok indent))
     ((html)
      (let ((tok (hopjs-parse-pop-token)))
	(if (eq (hopjs-parse-peek-token-type) 'eop)
	    (hopjs-parse-token-column otok indent)
	  tok)))
     (t
      -2000))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-ohtml ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-ohtml (ctx otok indent)
  (with-debug
   "hopjs-parse-ohtml (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((xtok (hopjs-parse-pop-token)))
     (hopjs-debug 0 "hopjs-parse-othml.next ntok=%s"
		  (hopjs-parse-peek-token))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((ident cssident class)
	(orn (hopjs-parse-xml-attrs ctx xtok hopjs-parse-args-indent)
	     (hopjs-parse-xml-body ctx otok indent xtok)))
       ((otag)
	(hopjs-parse-xml-body ctx otok indent xtok))
       ((ohtml)
	(orn (cond
	      ((hopjs-parse-token-in-linep otok xtok)
	       (hopjs-parse-xml ctx otok indent))
	      ((not (hopjs-parse-token-in-linep xtok ntok))
	       (hopjs-parse-xml ctx xtok hopjs-parse-xml-indent))
	      (t
	       (hopjs-parse-xml ctx otok indent)))
	     (hopjs-parse-xml-body ctx otok indent xtok)))
       ((chtml ctag)
	(hopjs-parse-pop-token)
	(if (eq (hopjs-parse-peek-token-type) 'eop)
	    (hopjs-parse-token-column otok indent)
	  xtok))
       ((binop)
	(if (string= (hopjs-parse-peek-token-string) ">")
	    xtok
	  -2002))
       (t
	-2001)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-otag ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-otag (ctx otok indent)
  (with-debug
   "hopjs-parse-otag (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((xtok (hopjs-parse-pop-token)))
     (hopjs-debug 0 "hopjs-parse-otag.next ntok=%s"
		  (hopjs-parse-peek-token))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((otag)
	(hopjs-parse-xml-body ctx otok indent xtok))
       ((ohtml)
	(orn (cond
	      ((hopjs-parse-token-in-linep otok xtok)
	       (hopjs-parse-xml ctx otok indent))
	      ((not (hopjs-parse-token-in-linep xtok ntok))
	       (hopjs-parse-xml ctx xtok hopjs-parse-xml-indent))
	      (t
	       (hopjs-parse-xml ctx otok indent)))
	     (hopjs-parse-xml-body ctx otok indent xtok)))
       ((chtml ctag)
	(hopjs-parse-pop-token)
	(if (eq (hopjs-parse-peek-token-type) 'eop)
	    (hopjs-parse-token-column otok indent)
	  xtok))
       ((binop)
	(if (string= (hopjs-parse-peek-token-string) ">")
	    xtok
	  -2004))
       (t
	(orn (hopjs-parse-xml-body ctx otok indent xtok)
	     xtok))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-xml-attrs ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-xml-attrs (ctx otok indent)
  (with-debug
   "hopjs-parse-xml-attrs (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((res nil))
     (while (not res)
       (let ((tok (hopjs-parse-peek-token)))
	 (hopjs-debug 0 "hopjs-parse-xml-attrs.next ntok=%s" tok)
	 (case (hopjs-parse-token-type tok)
           ((eop)
	    (setq res (hopjs-parse-token-column otok indent)))
	   ((ident cssident class)
	    (let ((itok (hopjs-parse-xml-attr-ident)))
	      (case (hopjs-parse-peek-token-type)
		((eop)
		 (setq res (hopjs-parse-token-column otok hopjs-parse-args-indent)))
		((=)
		 (hopjs-parse-pop-token)
		 (let ((e (hopjs-parse-primary ctx itok hopjs-parse-args-indent)))
		   (if (numberp e)
		       (setq res e)
		     (case (hopjs-parse-peek-token-type)
		       ((eop)
			(setq res e))
		       ((ident cssident class)
			nil)
		       ((chtml)
			;(hopjs-parse-pop-token)
			(setq res tok))
		       ((binop)
			(if (string= (hopjs-parse-peek-token-string) ">")
			    (progn
			      (hopjs-parse-pop-token)
			      (setq res tok))
			  (setq res -2003)))
		       (t
			(setq res -2002))))))
		((chtml)
		 ;(hopjs-parse-pop-token)
		 (setq res tok))
		((binop)
		 (if (string= (hopjs-parse-peek-token-string) ">")
		     (progn
		       (hopjs-parse-pop-token)
		       (setq res tok))
		   (setq res -2003)))
		(t
		 (setq res -2004)))))
	   (t
	    (setq res -2005)))))
     res)))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-xml-attr-ident ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-xml-attr-ident ()
  (with-debug
   "hopjs-parse-xml-attr-ident (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((res (hopjs-parse-pop-token)))
     (while (or (memq (hopjs-parse-peek-token-type) hopjs-parse-idents)
		(and (eq (hopjs-parse-peek-token-type) 'binop)
		     (string= (hopjs-parse-peek-token-string) "-")))
       (hopjs-parse-pop-token))
     res)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-xml-body ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-xml-body (ctx otok indent xtok)
  (with-debug
   "hopjs-parse-xml-body (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (if (eq (hopjs-parse-peek-token-type) 'chtml)
       (let ((tok (hopjs-parse-pop-token)))
	 (if (eq (hopjs-parse-peek-token-type) 'eop)
	     (hopjs-parse-token-column otok indent)
	   tok))
     (let ((res nil))
       (while (not res)
	 (let ((ntok (hopjs-parse-peek-token)))
	   (hopjs-debug 0 "hopjs-parse-xml.while otok=%s xtok=%s ntok=%s"
			otok xtok ntok)
	   (case (hopjs-parse-token-type ntok)
	     ((eop)
	      (if (hopjs-parse-token-in-linep otok xtok)
		  (setq res (hopjs-parse-token-column otok hopjs-parse-xml-indent))
		(setq res (hopjs-parse-token-column xtok hopjs-parse-xml-indent))))
	     ((html)
	      (let ((tok (hopjs-parse-pop-token)))
		(when (eq (hopjs-parse-peek-token-type) 'eop)
		  (setq res (hopjs-parse-token-column xtok hopjs-parse-xml-indent)))))
	     ((ohtml otag)
	      (let ((x (cond
			((hopjs-parse-token-in-linep otok xtok)
			 (hopjs-parse-xml ctx otok indent))
			((not (hopjs-parse-token-in-linep xtok ntok))
			 (hopjs-parse-xml ctx xtok hopjs-parse-xml-indent))
			(t
			 (hopjs-parse-xml ctx otok indent)))))
		(when (numberp x)
		  (setq res x))))
	     ((chtml ctag)
	      (let ((ctok (hopjs-parse-pop-token)))
		(setq res
		      (if (eq (hopjs-parse-peek-token-type) 'eop)
			  (cond
			   ((hopjs-parse-token-in-linep otok xtok)
			    (hopjs-parse-xml ctx otok 0))
			   ((hopjs-parse-token-in-linep xtok ctok)
			    (hopjs-parse-token-column xtok 0))
			   (t
			    (hopjs-parse-token-column xtok 0)))
			xtok))))
;* 		    ((binop)                                           */
;* 		     (if (string= (hopjs-parse-peek-token-string) ">") */
;* 			 (let ((ctok (hopjs-parse-pop-token)))         */
;* 			   (setq res                                   */
;* 				 (if (eq (hopjs-parse-peek-token-type) 'eop) */
;* 				     (cond                             */
;* 				      ((hopjs-parse-token-in-linep otok xtok) */
;* 				       (hopjs-parse-xml ctx otok 0))   */
;* 				                                       */
;* 				      ((hopjs-parse-token-in-linep xtok ctok) */
;* 				       (hopjs-parse-token-column xtok 0)) */
;* 				      ((hopjs-parse-token-in-linep otok xtok) */
;* 				       (hopjs-parse-xml ctx otok (+ indent hopjs-parse-xml-indent))) */
;* 				      (t                               */
;* 				       (hopjs-parse-token-column xtok 0))) */
;* 				   xtok)))                             */
;* 		       (setq res -2003)))                              */
	     ((dollar)
	      (hopjs-parse-dollar ctx otok indent))
	     (t
	      (let ((x (hopjs-parse-xml-text ctx otok indent)))
		(cond
		 ((numberp x)
		  (setq res x))
		 ((eq (hopjs-parse-peek-token-type) 'eop)
		  ;; don't indent texts
		  (setq res -1))))))))
       res))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-xml-text ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-xml-text (ctx otok indent)
  (with-debug
   "hopjs-parse-xml-text (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (save-excursion
     (goto-char (hopjs-parse-token-beginning (hopjs-parse-peek-token)))
     (let* ((b (point))
	    (m (re-search-forward "\\(?:[$]{\\|/>\\|<\\)" (point-max) t 1)))
       (if m
	   (let ((e (1- (point))))
	     (hopjs-parse-skip-tokens e)
	     (hopjs-parse-token 'string b e))
	 -2100)))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-dollar ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-dollar (ctx otok indent)
  (with-debug
   "hopjs-parse-dollar (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((dtok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-expr ctx otok indent)
	    (case (hopjs-parse-peek-token-type)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((rbrace)
	       (hopjs-parse-pop-token)
	       dtok)
	      (t
	       -8005)))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-tilde ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-tilde (ctx otok indent)
  (with-debug
   "hopjs-parse-tilde (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((dtok (hopjs-parse-pop-token)))
     (if (eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent)
       (orn (hopjs-parse-stmt ctx otok indent)
	    (case (hopjs-parse-peek-token-type)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((rbrace)
	       (hopjs-parse-pop-token)
	       dtok)
	      (t
	       -9005)))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-sequence ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-sequence (ctx otok indent seq)
  (with-debug
   "hopjs-parse-sequence (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((res nil))
     (while (consp seq)
       (hopjs-debug 0 "hopjs-parse-sequence ntok=%s seq=%s"
		    (hopjs-parse-peek-token) seq)
       (cond
	((eq (hopjs-parse-peek-token-type) 'eop)
	 (hopjs-parse-token-column otok indent))
	((eq (hopjs-parse-peek-token-type) (car seq))
	 (setq seq (cdr seq))
	 (setq res (hopjs-parse-pop-token)))
	(t
	 (setq seq nil))))
     res)))
	
