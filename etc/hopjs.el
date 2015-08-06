;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/etc/hopjs.el                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 25 13:05:16 2014                          */
;*    Last change :  Wed Aug  5 19:44:13 2015 (serrano)                */
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
  ;; indent function
  (setq js-mode-custom-indent 'hopjs-indent-html)
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
	(cons "</?[a-zA-Z0-9_.]+[ ]*>\\|[ ]*/>" 'font-lock-face-9)
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
(defvar hopjs-re-open-tag "<[a-zA-Z_$][a-zA-Z_$0-9.]*[ >]")
(defvar hopjs-re-close-tag "</[a-zA-Z_$][a-zA-Z_$0-9.]*[ ]*>")
(defvar hopjs-re-tag
  (concat hopjs-re-open-tag "\\|" hopjs-re-close-tag "\\|[$]{"))

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

;*---------------------------------------------------------------------*/
;*    hopjs-in-html-p ...                                              */
;*---------------------------------------------------------------------*/
(defun hopjs-in-html-p ()
  (interactive)
  (save-excursion
    (let ((cp (point))
	  (res 'loop)
	  (be (progn (beginning-of-defun) (point))))
      (save-excursion
	(let ((p (nth 9 (parse-partial-sexp cp be))))
	  (while (and (consp p) (eq res 'loop))
	    (when (> (car p) (point-min))
	      (goto-char (- (car p) 1))
	      (if (looking-at "[$]{")
		  (setq res nil)
		(setq p (cdr p))))))
	(when (eq res 'loop)
	  (goto-char be))
	(let ((open 0))
	  (while (eq res 'loop)
	    (if (re-search-forward hopjs-re-tag cp t)
		(let ((next (match-end 0)))
		  (goto-char (match-beginning 0))
		  (cond
		   ((looking-at hopjs-re-open-tag)
		    (setq open (+ open 1))
		    (goto-char next))
		   ((looking-at hopjs-re-close-tag)
		    (setq open (- open 1))
		    (goto-char next))
		   (t
		    (goto-char (+ 1 (match-beginning 0)))
		    (forward-sexp 1)))
		  (setq res (> open 0)))
	      (setq res nil))))
	res))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-html-previous-line ...                              */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-html-previous-line (point-min ending)
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (back-to-indentation)
    (when (> (point) point-min)
      (cond
       ((looking-at "[a-zA-Z_$][.0-9a-zA-Z_$]*[ \t*][(= ]?<\\([^>]\\|[^/]>\\)")
	(message "<<< html.1 %s" (+ hopjs-indent-level-html (current-column)))
	(+ (current-column) hopjs-indent-level-html))
       ((looking-at "<[^>]*/>")
	(message "<<< html.2 %s" (current-column))
	(current-column))
       ((looking-at "<[a-zA-Z_$][.0-9a-zA-Z_$]*[ ]+\\([^>]\\)[^>]*$")
	(save-excursion
	  (goto-char (match-beginning (if ending 0 1)))
	  (message "<<< html.7 %s" (current-column))
	  (current-column)))
       ((looking-at "[^<>]+=[^<>]+/>$")
	(message "<<< html.8...")
	(hopjs-indent-html-previous-line point-min t))
       ((looking-at "[^<>]+=[^<>]+>$")
	(message "<<< html.8...")
	(+ (hopjs-indent-html-previous-line point-min t) hopjs-indent-level-html))
       ((looking-at "<[^>/]*>$")
	(message "<<< html.3 %s" (+ hopjs-indent-level-html (current-column)))
	(+ (current-column) hopjs-indent-level-html))
       ((looking-at "</[^>]*>$")
	(message "<<< html.4 %s" (current-column))
	(current-column))
       ((looking-at "<\\([a-zA-Z_$][.0-9a-zA-Z_$]*\\).*</\\1>$")
	(message "<<< html.5 %s" (+ hopjs-indent-level-html (current-column)))
	(current-column))
       (t
	(hopjs-indent-html-previous-line point-min nil))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-html ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-html ()
  (message ">>> html %s in HTML=%s" (point) (hopjs-in-html-p))
  (let ((r (save-excursion
	     (when (not (eq (point-at-bol) (point-min)))
	       (when (hopjs-in-html-p)
		 (let ((c (hopjs-indent-html-previous-line
			   (save-excursion (beginning-of-defun) (point))
			   nil)))
		   (when c
		     (beginning-of-line)
		     (back-to-indentation)
		     (cond
		      ((looking-at "</") (- c hopjs-indent-level-html))
		      (t c)))))))))
    (message "<<< html %s" r)
    r))

;*---------------------------------------------------------------------*/
;*    init                                                             */
;*---------------------------------------------------------------------*/
;; (autoload 'hopjs-mode-hook "hopjs" "Hop.js javascript mode hook" t)
;; (add-hook 'js-mode-hook 'hopjs-mode-hook)
