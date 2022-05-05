;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/etc/hopjs-parse.el                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  1 07:14:59 2018                          */
;*    Last change :  Fri Apr 29 08:34:49 2022 (serrano)                */
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
;*    constants                                                        */
;*---------------------------------------------------------------------*/
(defconst hopjs-parse-error-indent 1)
(defconst hopjs-parse-function-indent 1)
(defconst hopjs-parse-args-indent 1)
(defconst hopjs-parse-block-indent 3)
(defconst hopjs-parse-assig-indent 3)
(defconst hopjs-parse-if-indent 3)
(defconst hopjs-parse-while-indent 3)
(defconst hopjs-parse-return-indent 3)
(defconst hopjs-parse-binary-indent 3)
(defconst hopjs-parse-for-indent 3)
(defconst hopjs-parse-new-indent 3)

;*---------------------------------------------------------------------*/
;*    nullp                                                            */
;*---------------------------------------------------------------------*/
(defsubst nullp (o) (not (consp o)))

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
	 (cssid_start "[[:alpha:][:multibyte:][:nonascii:]$_#-]")
	 (letter "[[:alpha:][:nonascii:]]")
	 (tagid (rx: "[[:digit:]]*" "[[:alnum:]_]" (rx* "[[:alnum:]_.:]*"))))
    (list
     ;; blank
     (cons "[ \t\n]+" 'blank)
     ;; indent line comments
     (cons "///[^\n]*" 'indent-comment)
     ;; line comments
     (cons "//[^/\n]*" 'eol-comment)
     ;; comments
     (cons "/[*]\\(?:[^*]\\|[*][^/]\\)*[*]+/" 'comment)
     ;; numbers
     (cons (rxor "\\(?:0[xo]\\)?[0-9]+\\>"
		 "[eE]?-?[0-9]+\\>"
		 "[+]?[0-9]+\\(?:[.][0-9]+\\)?\\>")
	   'number)
     ;; punctuation
     (cons (rxor "[{}()[.;,:?]" (rxq "]")) 'punct)
     (cons "[?]." 'dot)
     ;; unop
     (cons (rxor (rxq "!") (rxq "~") (rxq "++") (rxq "--")) 'unop)
     ;; binop
     (cons (rxor "<" ">" (rxq "+") (rxq "-") (rxq "*") "%" "=" "|" "/"
		 "<<" ">>" ">>>" "[&^]") 'binop)
     (cons (rxor "&&" "||" "??") 'binop)
     (cons "instanceof" 'binop)
     (cons "[?][?]" 'binop)
     (cons "[*][*]" 'binop)
     (cons "in" 'in)
     (cons (rxor "<=" ">="  "!==*" "===*" "[+*%^&-|]=" "<<=" ">>=" ">>>=") '=)
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
;*    hopjs-start-stmt-rx ...                                          */
;*---------------------------------------------------------------------*/
(defconst hopjs-start-stmt-rx
  (let* ((unicode_char "[[:multibyte:]]")
	 (alpha+ "[[:alpha:][:multibyte:]]")
	 (alnum+ (rxor "[[:alnum:][:multibyte:]]"))
	 (id_part_sans "[[:nonascii:]]")
	 (id_part "[[:alnum:][:multibyte:][:nonascii:]$_]")
	 (id_start "[[:alpha:][:multibyte:][:nonascii:]$_#]")
	 (ident (rx: id_start (rx* id_part)))
	 (arg (rx: "[^,() \t]+"))
	 (arg_list (rx: "[(][^)]*[)]"))
	 (async "(?:async[ ]*)?"))
    (rxor
     ;; functions
     (rx: async "function[*]?[ ]+[*]?" ident "*" arg_list "[ ]*{")
     (rx: async arg "[ ]*=>")
     (rx: async arg_list "[ ]*=>")
     ;; service
     (rx: async "service[ ]+?" ident "*" arg_list "[ ]*{")
     ;; class
     (rx: "class[ ]+" ident "*" "[ ]*{")
     (rx: "class[ ]+" ident "*" "extends[ ]+" ident "[ ]*{")
     ;; export, import
     "(?:export|import)[ +]"
     ;; hiphop plugins
     (rx: "module[ ]+" ident "*[ ]*" arg_list "[ ]*{")
     (rx: "module[ ]+" ident "*[ ]*" arg_list "(?:[ ]*" "implements[ ]+" ident ")?[ ]*{")
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
  (if hopjs-parse-debug
      (vector type b e
	      (unless (memq type '(bof blank))
		(buffer-substring-no-properties b e)))
    (vector type b e)))

(defsubst hopjs-parse-tokenp (obj)
  (and (vectorp obj) (= (length obj) (if hopjs-parse-debug 4 3))))

(defsubst hopjs-parse-token-type (tok) (aref tok 0))
(defsubst hopjs-parse-token-beginning (tok) (aref tok 1))
(defsubst hopjs-parse-token-end (tok) (aref tok 2))

(defsubst hopjs-parse-token-column (tok &optional indent)
  (save-excursion
    (goto-char (hopjs-parse-token-beginning tok))
    (+ (or indent 0) (current-column))))

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
	 ((service return try catch while break if var let const else
		   new case switch for do default from throw export
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
	  (aset tok 0 'unop))
	 ((hiphop out inout run)
	  (aset tok 0 sym)))
       tok))
    ((binop in)
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
  (save-excursion
    (let ((tokens '()))
      (goto-char pos)
      (beginning-of-line)
      (while (<= (point) limit)
	(let ((tok (looking-at-token)))
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
	r))))

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
  (let ((tok (hopjs-parse-pop-token)))
    (setq hopjs-parse-tokens (cons tok hopjs-parse-tokens))
    tok))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-peek-token-type ...                                  */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-peek-token-type ()
  (hopjs-parse-token-type (hopjs-parse-peek-token)))

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
	       (skip-chars-forward "[ \t]*")
	       (if (and (= (current-column) 0) (> end (point)))
		   (setq res (point))
		 (let ((beg (point)))
		   (when (re-search-forward "//[^n]" end t 1)
		     (setq end (match-beginning 0)))
		   (when (re-search-forward hopjs-start-stmt-rx end t 1)
		     (let ((candidate (match-beginning 0)))
		       (hopjs-debug 0 "hopjs-start-stmt-rx goto %s"
				  (1- (match-end 0)))  
		       (goto-char (1- (match-end 0)))
		       (forward-sexp 1)
		       (hopjs-debug 0 "hopjs-start-stmt-rx forward %s => %s"
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
   "hopjs-parse-at (%s)" limit
   (save-excursion
     (goto-char (hopjs-parse-find-start-pos limit))
     (hopjs-debug 0 "hopjs-parse-at.start=%s" (point))
     (setq hopjs-parse-tokens (hopjs-parse-tokenify (point) limit))
     (let ((res nil))
       (while (not (numberp res))
	 (let ((tok (hopjs-parse-peek-token)))
	   (case (hopjs-parse-token-type tok)
	     ((function function*)
	      (setq res (hopjs-parse-function tok 0)))
	     ((async)
	      (let ((tok (hopjs-parse-pop-token)))
		(if (memq (hopjs-parse-peek-token-type) '(function function*))
		    (setq res (hopjs-parse-function tok 0))
		  -1)))
	     (t
	      (setq res -1)))))
       res))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-function ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-function (otok indent)
  (with-debug
   "hopjs-parse-function (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (let ((ftok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((ident)
	(orn (hopjs-parse-args (hopjs-parse-pop-token) hopjs-parse-args-indent)
	     (hopjs-parse-block otok hopjs-parse-function-indent)))
       ((lparen)
	(orn (hopjs-parse-args otok hopjs-parse-args-indent)
	     (hopjs-parse-block otok hopjs-parse-function-indent)))
       (t
	-1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-block ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-block (otok indent)
  (with-debug
   "hopjs-parse-block (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
  (let ((tok (hopjs-parse-pop-token)))
    (case (hopjs-parse-token-type tok)
      ((eop)
       (hopjs-parse-token-column otok hopjs-parse-block-indent))
      ((lbrace)
       (let ((res nil)
	     (ltok otok)
	     (lindent hopjs-parse-block-indent))
	 (while (not res)
	   (let ((tok (hopjs-parse-peek-token)))
	     (hopjs-debug
	      0 "hopjs-parse-block.next tok=%s ltok=%s lindent=%s"
	      tok ltok lindent)
	     (case (hopjs-parse-token-type tok)
	       ((eop)
		(setq res (hopjs-parse-token-column ltok lindent)))
	       ((rbrace)
		(hopjs-parse-pop-token)
		(if (eq (hopjs-parse-peek-token-type) 'eop)
		    (setq res (hopjs-parse-token-column otok 0))
		  (setq res tok)))
	       (t
		(let ((s (hopjs-parse-stmt otok hopjs-parse-block-indent)))
		  (cond
		   ((numberp s)
		    (setq res s))
		   ((eq s otok)
		    nil)
		   (t
		    (setq ltok s)
		    (setq lindent 0))))))))
	 res))
      (t
       -1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-stmt ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-stmt (otok indent)
  (with-debug
   "hopjs-parse-stmt (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((tok (hopjs-parse-peek-token)))
     (case (hopjs-parse-token-type tok)
       ((eop) (hopjs-parse-token-column otok indent))
       ((if) (hopjs-parse-if otok indent))
       ((while) (hopjs-parse-while otok indent))
       ((semicolon) (hopjs-parse-pop-token))
       ((lbrace) (hopjs-parse-block otok indent))
       ((var const let) (hopjs-parse-decls otok indent))
       ((return) (hopjs-parse-return otok indent))
       ((for) (hopjs-parse-for otok indent))
       (t (hopjs-parse-stmt-expr otok indent))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-stmt-expr ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-stmt-expr (otok indent)
  (with-debug
   "hopjs-parse-stmt-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((e (hopjs-parse-expr otok indent)))
     (orn e
	  (let ((tok (hopjs-parse-pop-token)))
	    (case (hopjs-parse-token-type tok)
	      ((eop)
	       (hopjs-parse-token-column otok indent))
	      ((semicolon)
	       otok)
	      (t
	       -1)))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-if ...                                               */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-if (otok indent)
  (with-debug
   "hopjs-parse-if (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (let ((itok (hopjs-parse-pop-token)))
     (orn (hopjs-parse-paren-expr otok indent)
	  (let ((s (hopjs-parse-stmt itok hopjs-parse-if-indent)))
	    (orn s
		 (let ((tok (hopjs-parse-pop-token)))
		   (hopjs-debug 0 "hopjs-parse-if.else tok=%s" tok)
		   (case (hopjs-parse-token-type tok)
		     ((eop)
		      (hopjs-parse-token-column itok 0))
		     ((else)
		      (hopjs-parse-stmt itok hopjs-parse-if-indent))
		     (t
		      tok)))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-while ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-while (otok indent)
  (with-debug
   "hopjs-parse-while (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (let ((wtok (hopjs-parse-pop-token)))
     (orn (hopjs-parse-paren-expr otok indent)
	  (hopjs-parse-stmt wtok hopjs-parse-while-indent)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-decls ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-decls (otok indent)
  (with-debug
   "hopjs-parse-decls (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (if (eq (hopjs-parse-peek-token-type) 'eop)
       (hopjs-parse-token-column otok indent)
     (let ((dtok (hopjs-parse-pop-token)))
       (orn (hopjs-parse-vars otok indent)
	    dtok)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-vars ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-vars (otok indent)
  (with-debug
   "hopjs-parse-vars (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (if (eq (hopjs-parse-peek-token-type) 'eop)
       (hopjs-parse-token-column otok indent)
     (let ((vars (hopjs-parse-var otok indent)))
       (orn vars
	    (case (hopjs-parse-peek-token-type)
	      ((semicolon)
	       (hopjs-parse-pop-token)
	       otok)
	      ((comma)
	       (let ((tok (hopjs-parse-pop-token)))
		 (hopjs-debug 0 "hopjs-parse-vars.comma= tok=%s" tok)
		 (hopjs-parse-vars vars 0)))
	      ((in of)
	       otok)
	      (t
	       vars)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-var ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-var (otok indent)
  (with-debug
   "hopjs-parse-var (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
     ((eop)
      (hopjs-parse-token-column otok indent))
     ((ident)
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
	       (let ((e (hopjs-parse-assig-expr otok hopjs-parse-assig-indent)))
		 (orn e
		      (if (eq (hopjs-parse-peek-token-type) 'eop)
			  (hopjs-parse-token-column otok hopjs-parse-assig-indent)
			tok))))))
	  ((semicolon comma)
	   otok)
	  (t
	   -1))))
     (t
      -1))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-return ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-return (otok indent)
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
	(orn (hopjs-parse-expr tok hopjs-parse-return-indent)
	     (if (eq (hopjs-parse-peek-token-type) 'semicolon)
		 (progn
		   (hopjs-parse-pop-token)
		   tok)
	       tok)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-for ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-for (otok indent)
  (with-debug
   "hopjs-parse-for (%s) otok=%s ntok=%s" (point)
   otok (hopjs-parse-peek-token)
   (let ((ftok (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lparen)
	(let ((tok (hopjs-parse-pop-token)))
	  (orn (hopjs-parse-stmt tok hopjs-parse-args-indent)
	       (if (memq (hopjs-parse-peek-token-type) '(in of))
		   (progn
		     (hopjs-debug 0 "hopjs-args-for.in ntok=%s"
					 (hopjs-parse-peek-token))
		     (hopjs-parse-pop-token)
		     (orn (hopjs-parse-expr tok hopjs-parse-args-indent)
			  (if (eq (hopjs-parse-peek-token-type) 'rparen)
			      (progn
				(hopjs-parse-pop-token)
				(hopjs-debug 0 "hopjs-args-for.in.block ntok=%s"
					     (hopjs-parse-peek-token))
				(hopjs-parse-stmt ftok hopjs-parse-for-indent))
			    -1)))
		 (orn (hopjs-parse-stmt tok hopjs-parse-args-indent)
		      (hopjs-parse-expr tok hopjs-parse-args-indent)
		      (if (eq (hopjs-parse-peek-token-type) 'rparen)
			  (progn
			    (hopjs-parse-pop-token)
			    (hopjs-debug 0 "hopjs-args-for.block ntok=%s"
					 (hopjs-parse-peek-token))
			    (hopjs-parse-stmt ftok hopjs-parse-for-indent))
			-1))))))
       (t
	-1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-args ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-args (otok indent)
  (with-debug
   "hopjs-parse-args (%s) otok=%s ntok=%s indent=%s" (point)
   otok (hopjs-parse-peek-token) indent
  (let ((ltok (hopjs-parse-pop-token)))
    (case (hopjs-parse-token-type ltok)
      ((eop)
       (hopjs-parse-token-column otok indent))
      ((lparen)
       (let ((res nil)
	     (ltok otok)
	     (lindent indent))
	 (while (not res)
	   (let ((tok (hopjs-parse-peek-token)))
	     (case (hopjs-parse-token-type tok)
	       ((eop)
		(setq res (hopjs-parse-token-column ltok lindent)))
	       ((rparen)
		(setq res (hopjs-parse-pop-token)))
	       (t
		(let ((e (hopjs-parse-assig-expr ltok lindent)))
		  (hopjs-debug 0 "hopjs-args-args.e e=%s" e)
		  (orn e
		       (let ((tok (hopjs-parse-peek-token)))
			 (hopjs-debug 0 "hopjs-parse-args.next tok=%s" tok)
			 (case (hopjs-parse-token-type tok)
			   ((comma)
			    (hopjs-parse-pop-token)
			    (setq lindent 0)
			    (setq ltok e))
			   ((rparen)
			    (setq res (hopjs-parse-pop-token)))
			   ((eop)
			    (setq res (hopjs-parse-token-column ltok lindent)))
			   (t
			    (setq res e))))))))))
	 res))
      (t
       -1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-paren-expr ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-paren-expr (otok indent)
  (with-debug
   "hopjs-parse-paren-expr (%s) otok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (let ((tok (hopjs-parse-pop-token)))
     (case (hopjs-parse-token-type tok)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lparen)
	(let ((e (hopjs-parse-expr otok indent)))
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
(defun hopjs-parse-expr (otok indent)
  (with-debug
   "hopjs-parse-expr (%s) otok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (let ((a (hopjs-parse-assig-expr otok indent)))
     (orn a
	  (if (eq (hopjs-parse-peek-token-type) 'comma)
	      (progn
		(hopjs-parse-pop-token)
		(hopjs-parse-expr otok indent))
	    a)))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-assig-expr ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-assig-expr (otok indent)
  (with-debug
   "hopjs-parse-assig-expr (%s) tok=%s ntok=%s" 
   (point) otok (hopjs-parse-peek-token)
   (let ((l (hopjs-parse-cond-expr otok indent)))
     (hopjs-debug 0 "hopjs-parse-assig-expr.next tok=%s" (hopjs-parse-peek-token))
     (orn l
	  (case (hopjs-parse-peek-token-type)
	    ((eop)
	     (hopjs-parse-token-column otok hopjs-parse-assig-indent))
	    ((=)
	     (let* ((etok (hopjs-parse-pop-token))
		    (tok (hopjs-parse-pop-token)))
	       (if (eq (hopjs-parse-token-type tok) 'eop)
		   (hopjs-parse-token-column otok hopjs-parse-assig-indent)
		 (hopjs-parse-assig-expr tok 0))))
	    (t
	     l))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-cond-expr ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-cond-expr (otok indent)
  (with-debug
   "hopjs-parse-cond-expr (%s) tok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (let ((b (hopjs-parse-binary-expr otok indent)))
     (orn b
	  (if (eq (hopjs-parse-peek-token-type) 'qmark)
	      (let ((tok (hopjs-parse-pop-token)))
		(let ((e (hopjs-parse-assig-expr tok 0)))
		  (orn e
		       (if (eq (hopjs-parse-peek-token-type) 'colon)
			   (let ((tok (hopjs-parse-pop-token)))
			     (hopjs-parse-assig-expr tok 0)))
		       e))))
	  b))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-binary-expr ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-binary-expr (otok indent)
  (with-debug
   "hopjs-parse-binary-expr (%s) tok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (if (eq (hopjs-parse-token-type otok) 'eop)
       (hopjs-parse-token-column otok indent)
     (let ((e (hopjs-parse-unary-expr otok indent)))
       (orn e
	    (if (eq (hopjs-parse-peek-token-type) 'binop)
		(let ((tok (hopjs-parse-pop-token)))
		  (hopjs-parse-binary-expr e hopjs-parse-binary-indent))
	      e))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-unary-expr ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-unary-expr (otok indent)
  (with-debug
   "hopjs-parse-unary-expr (%s) tok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (case (hopjs-parse-token-type otok)
     ((eop)
      (hopjs-parse-token-column otok indent))
     ((unop)
      (let ((tok (hopjs-parse-pop-token)))
	(hopjs-parse-expr tok 1)))
     (t
      (let ((e (hopjs-parse-lhs otok indent)))
	(hopjs-debug
	 0 "hopjs-parse-unary.next e=%s ntok=%s"
	 e (hopjs-parse-peek-token))
	(orn e
	     (if (eq (hopjs-parse-peek-token-type) 'unop)
		 (hopjs-parse-pop-token)
	       e)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-lhs ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-lhs (otok indent)
  (with-debug
   "hopjs-parse-lhs (%s) tok=%s ntok=%s"
   (point) otok (hopjs-parse-peek-token)
   (hopjs-parse-new-expr otok indent)))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-new-expr ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-new-expr (otok indent)
  (with-debug
   "hopjs-parse-new-expr (%s) tok=%s indent=%s ntok=%s"
   (point) otok indent (hopjs-parse-peek-token)
   (case (hopjs-parse-peek-token-type)
     ((new)
      (let ((tok (hopjs-parse-pop-token)))
	(orn (hopjs-parse-expr otok indent)
	     (if (eq (hopjs-parse-peek-token-type) 'dot)
		 (hopjs-parse-primary tok indent)
	       (hopjs-parse-new-expr tok indent)))))
     ((yield await)
      (let ((tok (hopjs-parse-pop-token)))
	(orn tok
	     (hopjs-parse-assig-expr tok indent))))
     (t
      (hopjs-parse-access-or-call otok indent)))))
	  
;*---------------------------------------------------------------------*/
;*    hopjs-parse-access-or-call ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-access-or-call (otok indent)
  (with-debug
   "hopjs-parse-access-or-call (%s) tok=%s ntok=%s indent=%s"
   (point) otok (hopjs-parse-peek-token) indent
   (let ((e (hopjs-parse-primary otok indent)))
     (orn e
	  (let ((tok (hopjs-parse-peek-token)))
	    (hopjs-debug 0 "hopjs-parse-access-or-call.next tok=%s e=%s" tok otok)
	    (case (hopjs-parse-token-type tok)
	      ((lparen)
	       (orn (hopjs-parse-args otok indent)
		    otok))
	      ((dot)
	       (let ((tok (hopjs-parse-pop-token)))
		 (if (eq (hopjs-parse-peek-token-type) 'ident)
		     (hopjs-parse-access-or-call e 1)
		   -1)))
	      (t
	       e)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-primary ...                                          */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-primary (otok indent)
  (with-debug
   "hopjs-parse-primary (%s) tok=%s ntok=%s indent=%s"
   (point) otok (hopjs-parse-peek-token) indent
   (case (hopjs-parse-peek-token-type)
     ((eop)
      (hopjs-parse-token-column otok indent))
     ((number string boolean regexp)
      (hopjs-parse-pop-token))
     ((ident)
      (let ((tok (hopjs-parse-pop-token)))
	(case (hopjs-parse-peek-token-type)
	  ((=>) (hopjs-parse-arrow otok indent))
	  (t tok))))
     ((function function*)
      (hopjs-parse-function otok indent))
     ((lparen)
      (let ((tok (hopjs-parse-pop-token)))
	(case (hopjs-parse-peek-token-type)
	  ((eop)
	   (hopjs-parse-token-column (hopjs-parse-pop-token) indent))
	  ((rparen)
	   (let ((tok (hopjs-parse-pop-token)))
	     (case (hopjs-parse-peek-token-type)
	       ((eop) (hopjs-parse-token-column otok indent))
	       ((=>) (hopjs-parse-arrow otok indent))
	       (t tok))))
	  (t
	   (let ((e (hopjs-parse-expr tok 1)))
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
		       ((=>) (hopjs-parse-arrow otok indent))
		       (t tok)))
		    ((comma)
		     (hopjs-parse-expr tok 1))
		    (t -1))))))))
     (t
      -1))))

;*---------------------------------------------------------------------*/
;*    hopjs-parse-arrow ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-parse-arrow (otok indent)
  (with-debug
   "hopjs-parse-arrow (%s) tok=%s ntok=%s indent=%s"
   (point) otok (hopjs-parse-peek-token) indent
   (let ((arrow (hopjs-parse-pop-token)))
     (case (hopjs-parse-peek-token-type)
       ((eop)
	(hopjs-parse-token-column otok indent))
       ((lbrace)
	(hopjs-parse-block otok indent))
       (t
	(hopjs-parse-expr otok hopjs-parse-function-indent))))))
	
