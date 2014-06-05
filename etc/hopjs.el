;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/etc/hopjs.el                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 25 13:05:16 2014                          */
;*    Last change :  Thu Jun  5 11:43:40 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOPJS customization of the standard js-mode                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'hopjs)
(require 'js)

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
  (list (list "^\\s-*\\(service\\|tag\\)\\(?:\\s-+\\|(\\)" 1 'font-lock-keyword-face)
	(cons ".post" 'font-lock-face-2)
	(cons "</?[a-zA-Z0-9_]*>" 'font-lock-face-9)
	(cons "$\{[^ \t\r\n{}]*\}" 'font-lock-face-2)
	(list "\\([$]\\){" 1 'font-lock-face-2)
	(list "\\([~]\\){" 1 'font-lock-face-3)
	(cons "[0-9a-zA-Z_-]*:" 'font-lock-face-10)
	(list (concat "^\\s-*\\(?:service\\|tag\\)\\s-+\\(" js--name-re "\\)") 1 'font-lock-function-name-face)))

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
    (define-key map ")" 'hopjs-electric-paren)))

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
;*    init                                                             */
;*---------------------------------------------------------------------*/
;; (autoload 'hopjs-mode-hook "hopjs" "Hop.js javascript mode hook" t)
;; (add-hook 'js-mode-hook 'hopjs-mode-hook)
