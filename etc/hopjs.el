;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/etc/hopjs.el                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 25 13:05:16 2014                          */
;*    Last change :  Thu Aug  6 18:41:33 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOPJS customization of the standard js-mode                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'hopjs)
(require 'js)

(defcustom hopjs-indent-level-html 2
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :safe 'integerp
  :group 'js)

;*---------------------------------------------------------------------*/
;*    hopjs-mode-hook ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-mode-hook ()
  ;; key bindings
  (hopjs-key-bindings)
  ;; font lock
  (font-lock-add-keywords nil hopjs-font-lock-keywords)
  ;; user hooks
  (run-hooks 'hopjs-mode-hook))

;*---------------------------------------------------------------------*/
;*    font-lock ...                                                    */
;*---------------------------------------------------------------------*/
(defconst hopjs-font-lock-keywords
  (list (list "^\\s-*\\(service\\)\\(?:\\s-+\\|(\\)" 1 'font-lock-keyword-face)
	(cons ".post" 'font-lock-face-2)
	(cons "</?[a-zA-Z0-9_.]+[ ]*>\\|[ ]*/>\\|<[^ /]*/>" 'font-lock-face-9)
	(list "\\(</?[a-zA-Z0-9_.]+\\)[ ]+[a-zA-Z0-9_]" 1 'font-lock-face-9)
	(list "[}\"][ ]*\\(>\\)" 1 'font-lock-face-9)
	(cons "$\{[^ \t\r\n{}]*\}" 'font-lock-face-2)
	(list "\\([$]\\){" 1 'font-lock-face-2)
	(list "\\([~]\\){" 1 'font-lock-face-3)
	(cons "[0-9a-zA-Z_-]*:" 'font-lock-face-10)
	(list (concat "^\\s-*\\(?:service\\)\\s-+\\(" js--name-re "\\)") 1 'font-lock-function-name-face)))

;*---------------------------------------------------------------------*/
;*    hopjs-key-bindings ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-key-bindings ()
  (let ((map (current-local-map)))
    (define-key map "\C-m" 'hopjs-return)
    (define-key map "\e\C-m" 'newline)
    (local-unset-key "\ee")
    (define-key map "\e\C-q" 'hopjs-indent-statement)
    (local-unset-key "}")
    (define-key map "}" 'hopjs-electric-brace)
    (local-unset-key ")")
    (define-key map ")" 'hopjs-electric-paren)
    (local-unset-key ">")
    (define-key map ">" 'hopjs-electric-abra)))

;*---------------------------------------------------------------------*/
;*    hopjs-re-open-tag ...                                            */
;*---------------------------------------------------------------------*/
(defconst hopjs-re-open-tag "<[a-zA-Z_$][a-zA-Z_$0-9.]*[^<>]*\\([^/]>\\|[ \n]\\)")
(defconst hopjs-re-close-tag "</[a-zA-Z_$][a-zA-Z_$0-9.]*[ ]*>")
(defconst hopjs-re-tag
  (concat hopjs-re-open-tag "\\|" hopjs-re-close-tag "\\|[$~]{"))

(defconst hopjs-re-entering-html
  "[a-zA-Z_$][.0-9a-zA-Z_$]+[ \t]*[(= ]?[ \t]*<\\([^>]\\|[^/]>\\)")

;*---------------------------------------------------------------------*/
;*    debugging                                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-debug (fmt &rest l)
  (apply 'message fmt l))

(defconst hopjs-debug t)

;*---------------------------------------------------------------------*/
;*    hopjs-electric-brace ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-electric-brace ()
  "Insert and indent line."
  (interactive)
  (insert "}")
  (indent-for-tab-command))

;*---------------------------------------------------------------------*/
;*    hopjs-electric-paren ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-electric-paren ()
  "Insert and indent line."
  (interactive)
  (insert ")")
  (indent-for-tab-command))

;*---------------------------------------------------------------------*/
;*    hopjs-electric-abra ...                                          */
;*---------------------------------------------------------------------*/
(defun hopjs-electric-abra ()
  "Insert and indent line."
  (interactive)
  (insert ">")
  (indent-for-tab-command))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-statement ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-statement ()
  "Indent curent statement."
  (interactive)
  (save-excursion
    (c-beginning-of-statement
     0
     (save-excursion
       (beginning-of-defun)
       (point))
     nil)
    (let ((start (point)))
      (c-forward-sexp)
      (let ((end (point)))
	(indent-region start end)))))

;*---------------------------------------------------------------------*/
;*    hopjs-return ...                                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-return (&optional dummy)
   "On indent automatiquement sur un RET.
usage: (js-return)  -- [RET]"
   (interactive)
   (if (= (point) 1)
       (newline)
     (newline-and-indent)))

;*---------------------------------------------------------------------*/
;*    hopjs-beginning-of-defun ...                                     */
;*---------------------------------------------------------------------*/
(defun hopjs-beginning-of-defun (pos)
  (interactive "d")
  (let ((res 'loop))
    (while (eq res 'loop)
      (beginning-of-defun)
      (let ((defpos (point)))
	(cond
	 ((<= defpos (point-min))
	  (setq res nil))
	 ((search-forward "{" pos t)
	  (forward-char -1)
	  (condition-case nil
	      (progn
		(forward-sexp 1)
		(if (> (point) pos)
		    (progn
		      (goto-char defpos)
		      (setq res t))
		  (goto-char defpos)))
	    (error
	     (progn
	       (goto-char defpos)
	       (setq res t)
	       nil))))
	 (t
	  (setq res nil)))))
    res))
	      
;*---------------------------------------------------------------------*/
;*    hopjs--indent-operator-re ...                                    */
;*---------------------------------------------------------------------*/
(defconst hopjs--indent-operator-re 
  (concat "[-+*/%=&^|?:.]\\([^-+*/]\\|$\\)\\|^<$\\|^>$\\|"
          (js--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")

(defun js--looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at hopjs--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?")))))))

(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
	  ((js--html-statement-indentation))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (cond (same-indent-p
                                  (current-column))
                                 (continued-expr-p
                                  (+ (current-column) (* 2 js-indent-level)
                                     js-expr-indent-offset))
                                 (t
                                  (+ (current-column) js-indent-level
                                     (pcase (char-after (nth 1 parse-status))
                                       (?\( js-paren-indent-offset)
                                       (?\[ js-square-indent-offset)
                                       (?\{ js-curly-indent-offset)))))))
                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

;*---------------------------------------------------------------------*/
;*    hopjs-html-p ...                                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-html-p (pos)
  (interactive "d")
  (hopjs-debug ">>> hopjs-html-p pos=%d" pos)
  (save-excursion
    (let ((loop 'loop)
	  (open 0)
	  (hopjs-debug '())
	  (be (progn (hopjs-beginning-of-defun (point)) (point))))
      (save-excursion
	(let ((p (nth 9 (parse-partial-sexp pos be))))
	  (while (and (consp p) (eq loop 'loop))
	    (when (> (car p) (point-min))
	      (goto-char (- (car p) 1))
	      (if (looking-at "[~$]{")
		  (setq loop nil)
		(setq p (cdr p)))))))
      (hopjs-debug "~~~ hopjs-html-p, loop=%s be=%d" loop be)
      (when (eq loop 'loop)
	(goto-char be)
	(while (eq loop 'loop)
	  (if (re-search-forward hopjs-re-tag pos t)
	      (let ((next (match-end 0)))
		(hopjs-debug "--- hopjs-html-p, open=%d next=%d %s" open next
			 (buffer-substring
			  (match-beginning 0) (match-end 0)))
		(goto-char (match-beginning 0))
		(cond
		 ((memq (get-text-property (point) 'face)
			'(font-lock-comment-face font-lock-string-face))
		  (goto-char next))
		 ((looking-at hopjs-re-open-tag)
		  (setq open (+ open 1))
		  (when hopjs-debug
		    (setq hopjs-debug (cons (buffer-substring
				     (match-beginning 0) (match-end 0))
				    hopjs-debug)))
		  (goto-char next))
		 ((looking-at hopjs-re-close-tag)
		  (setq hopjs-debug (cdr hopjs-debug))
		  (setq open (- open 1))
		  (goto-char next))
		 (t
		  (forward-char 1)
		  (forward-sexp 1)
		  (hopjs-debug "!!! hopjs-html-p forward=%s" (point)))))
	    (setq loop nil))))
      (hopjs-debug "<<< hopjs-html-p pos=%d open=%s %s -> %s" pos open hopjs-debug
		   (> open 0))
      (> open 0))))

;*---------------------------------------------------------------------*/
;*    hopjs-html-previous-line-indent ...                              */
;*---------------------------------------------------------------------*/
(defun hopjs-html-previous-line-indent (point-min)
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (back-to-indentation)
    (when (> (point) point-min)
      (cond
       ((looking-at hopjs-re-entering-html)
	;; entering HTML mode
	(hopjs-debug "hopjs-html-previous-line-indent: entering HTML")
	(+ (current-column) hopjs-indent-level-html))
       ((looking-at "<[^>/]*/>")
	;; standlone tag
	(hopjs-debug "hopjs-html-previous-line-indent: standalong tag")
	(current-column))
       ((looking-at "<[a-zA-Z_$][.0-9a-zA-Z_$]*[ ]+\\([^>]\\)[^>]*$")
	;; open tag + attribute
	(goto-char (match-beginning (if ending 0 1)))
	(hopjs-debug "hopjs-html-previous-line-indent: open + attr")
	(current-column))
       ((looking-at "[^<>]+=[^<>]+/>$")
	;; attribute + closing
	(hopjs-debug "hopjs-html-previous-line-indent: attr + close")
	(- (hopjs-html-previous-line-indent point-min) hopjs-indent-level-html))
       ((looking-at "[^<>]+=[^<>]")
	;; attribute
	(hopjs-debug "hopjs-html-previous-line-indent: attr")
	(current-column))
       ((looking-at "</[^>]*>$")
	;; closing
	(hopjs-debug "hopjs-html-previous-line-indent: close")
	(current-column))
       ((looking-at "<\\([a-zA-Z_$][.0-9a-zA-Z_$]*\\).*</\\1>$")
	;; single line open/closing
	(hopjs-debug "hopjs-html-previous-line-indent: open-close")
	(current-column))
       ((looking-at "<[^/<>]+\\(>$\\| \\)")
	;; opening
	(hopjs-debug "hopjs-html-previous-line-indent: opening")
	(+ (current-column) hopjs-indent-level-html))
       (t
	(hopjs-debug "hopjs-html-previous-line-indent: ?")
	(current-column))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-html-line-type ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-html-line-type ()
  (save-excursion
    (beginning-of-line)
    (let ((p (point)))
      (back-to-indentation)
      (cond
       ((or (= (point) p) (looking-at "[ \t]*$"))
	'blank)
       ((looking-at "<[^>]*/>")
	'tag)
       ((looking-at "<\\([a-zA-Z_$][.0-9a-zA-Z_$]*\\).*</\\1>$")
	'tag)
       ((looking-at "<[^>/ ]*\\(>[ \t]*$\\| \\)")
	'otag)
       ((looking-at "</[^>]*>$")
	'ctag)
       ((looking-at "[$~]{")
	'hop)
       ((looking-at "[^<>]+=[^<>]+/>$")
	(if (memq (progn (previous-line) (beginning-of-line) (hopjs-html-line-type))
		  '(blank attr 'otag))
	    'otag
	  'text))
       (t
	'text)))))

;*---------------------------------------------------------------------*/
;*    js--html-statement-indentation ...                               */
;*---------------------------------------------------------------------*/
(defun js--html-statement-indentation ()
  (hopjs-debug ">>> js--html-statement-indentation %s" (point))
  (when (hopjs-html-p (point))
    (hopjs-debug "--- js--html-statement-indentation... in HTML %s" (point))
    (save-excursion
      (let ((ltype (hopjs-html-line-type)))
	(hopjs-debug "--- ltype=%s" ltype)
	(cond
	 ((eq ltype 'text)
	  ;; plain text, no indent
	  (back-to-indentation)
	  (hopjs-debug "<<<  text=%s" (current-column))
	  (current-column))
	 ((and (not (hopjs-html-p (save-excursion (previous-line) (beginning-of-line) (point))))
	       (save-excursion
		 (previous-line)
		 (back-to-indentation)
		 (looking-at hopjs-re-entering-html)))
	  (goto-char (match-beginning 0))
	  ;; entering HTML mode
	  (hopjs-debug "<<<  entering=%s" (+ (current-column) hopjs-indent-level-html))
	  (+ (current-column) hopjs-indent-level-html))
	 (t
	  (let ((pindent (hopjs-html-previous-line-indent 0)))
	    (hopjs-debug "<<<  pindent=%s" pindent)
	    (if (eq ltype 'ctag)
		(max (- pindent hopjs-indent-level-html) 0)
	      pindent))))))))

;*---------------------------------------------------------------------*/
;*    init                                                             */
;*---------------------------------------------------------------------*/
;; (autoload 'hopjs-mode-hook "hopjs" "Hop.js javascript mode hook" t)
;; (add-hook 'js-mode-hook 'hopjs-mode-hook)
