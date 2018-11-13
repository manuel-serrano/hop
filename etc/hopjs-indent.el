;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/etc/hopjs-indent.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov  2 09:45:39 2018                          */
;*    Last change :  Mon Nov 12 19:05:51 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hopjs indent                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'hopjs-indent)
(require 'hopjs-macro)
(require 'hopjs-parse)
(require 'hopjs-config)

;*---------------------------------------------------------------------*/
;*    hopjs-indent ...                                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-indent (pos)
  (interactive "d\npoint: ")
  (with-debug
   "hopjs-indent line=%s pos=%s" (line-number-at-pos pos) pos
   (cond
    ((= (point-min) pos) 0)
    ((hopjs-blank-line-p pos) (hopjs-indent-new pos))
    (t (hopjs-indent-old pos)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new ...                                             */
;*    -------------------------------------------------------------    */
;*    This function indent the newly created line according to         */
;*    what's found in the previous line.                               */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new (pos)
  (interactive "d")
  (with-debug
   "hopjs-indent-new pos=%s" pos
   (if (<= pos (point-min))
       0
     (save-excursion
       (goto-char pos)
       (forward-line -1)
       (end-of-line)
       (hopjs-parse-start (point))
       (let ((tok (hopjs-parse-consume-token-any)))
	 (hopjs-debug 0 "hopjs-indent-new tok=%s [%s]" tok
		      (hopjs-parse-token-string tok))
	 (let ((col (case (hopjs-parse-token-type tok)
		      ((lbrace) (hopjs-indent-new-lbrace tok))
		      ((rbrace) (hopjs-indent-new-rbrace tok))
		      ((lparen) (hopjs-indent-new-lparen tok))
		      ((rparen) (hopjs-indent-new-rparen tok))
		      ((comma) (hopjs-indent-new-comma tok))
		      ((semicolon) (hopjs-indent-new-semicolon tok))
		      ((colon) (hopjs-indent-new-colon tok))
		      ((otag) (hopjs-indent-new-otag tok))
		      ((>) (hopjs-indent-new-etag tok))
		      ((ctag) (hopjs-indent-new-ctag tok))
		      ((chtml) (hopjs-indent-new-chtml tok))
		      ((tilde dollar) (hopjs-indent-new-tilde tok))
		      ((number string ident) (hopjs-indent-new-literal tok))
		      ((=) (hopjs-indent-new-= tok))
		      ((binop) (hopjs-indent-new-binop tok))
		      ((return) (hopjs-indent-new-return tok))
		      ((comment) (hopjs-indent-new-comment tok))
		      ((qmark) (hopjs-indent-new-qmark tok)))))
	   (or col 0)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old ...                                             */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old (pos)
  (with-debug
   "hopjs-indent-old pos=%s" pos
   (save-excursion
     (goto-char pos)
     (beginning-of-line)
     (cond
      ((looking-at "[ \t]*</")
       (or (hopjs-indent-old-ctag (- (match-end 0) 2)) 0))
      ((looking-at "[ \t]*<[^ \t]")
       (hopjs-indent-old-otag (- (match-end 0) 2)))
      ((looking-at "[ \t]*\\(?:case\\|default\\)")
       (hopjs-indent-old-case (match-end 0)))
      ((looking-at "[ \t]*}")
       (or (hopjs-indent-old-rbrace (- (match-end 0) 2)) 0))
      ((looking-at "[ \t]*[.]")
       (let ((col (hopjs-indent-new pos)))
	 (if col (+ col hopjs-indent-level) 0)))
      ((looking-at "[ \t]*[?]")
       (hopjs-indent-old-qmark (match-end 0)))
      (t
       (or (hopjs-indent-new pos) 0))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lbrace (tok)
  (with-debug
   "hopjs-indent-new-lbrace...%s" tok
   (cond
    ((hopjs-parse-args (hopjs-parse-peek-token))
     ;; (args) {
     (hopjs-debug 0 "hopjs-indent-new-lbrace.ARGS...%s"
		  (hopjs-parse-peek-token))
     (case (hopjs-parse-peek-token-type)
       ((ident)
	;; ident (args) {
	(hopjs-debug 0 "IDENT.1..%s" (hopjs-parse-peek-token))
	(hopjs-parse-consume-token-any)
	(when (memq (hopjs-parse-peek-token-type) '(function function* service))
	  (let ((tok (hopjs-parse-consume-token-any)))
	    (hopjs-debug 0 "IDENT.2.. %s %s" tok (hopjs-parse-peek-token-type))
	    (if (memq (hopjs-parse-peek-token-type) '(= colon))
		;; lhs = function ident (args) {
		(let ((etok (hopjs-parse-expr (hopjs-parse-consume-token-any))))
		  (hopjs-debug 0 "IDENT.3..%s" etok)
		  (if etok
		      (hopjs-indent-column-token etok hopjs-indent-level)
		    0))
	      (hopjs-indent-column-token tok hopjs-indent-level)))))
       ((function)
	;; function (args) {
	(let ((tok (hopjs-parse-consume-token-any)))
	  (hopjs-debug 0 "FUNCTION.1..%s" (hopjs-parse-peek-token))
	  (case (hopjs-parse-peek-token-type)
	    ((=)
	     ;; lhs = function ident (args) {
	     (let ((tok (hopjs-parse-lhs (hopjs-parse-consume-token-any))))
	       (if tok
		   (let ((dtok (hopjs-parse-peek-token)))
		     (if (memq (hopjs-parse-token-type dtok) '(var let const))
			 (hopjs-indent-column-token dtok hopjs-indent-level)
		       (hopjs-indent-column-token tok hopjs-indent-level)))
		 0)))
	    ((colon)
	     ;; lhs : function ident (args) {
	     (let ((tok (hopjs-parse-expr (hopjs-parse-consume-token-any))))
	       (if tok
		   (let ((dtok (hopjs-parse-peek-token)))
		     (hopjs-indent-column-token tok hopjs-indent-level))
		 0)))
	    (t
	     (hopjs-indent-column-token tok hopjs-indent-level)))))
       ((if while switch for)
	;; if (args) {
	(hopjs-debug 0 "IF/WHILE/SWITCH.1..%s" (hopjs-parse-peek-token))
	(hopjs-indent-column-token
	 (hopjs-parse-consume-token-any) hopjs-indent-level))
       (t
	'())))
    ((eq (hopjs-parse-peek-token-type) '=>)
     (hopjs-parse-consume-token-any)
     (let ((tok (hopjs-parse-consume-token-any)))
       (hopjs-debug 0 "hopjs-indent-new-lbrace =>...%s" tok)
       (case (hopjs-parse-token-type tok)
	 ((ident)
	  (hopjs-indent-column-token tok hopjs-indent-level))
	 ((rparen)
	  (hopjs-parse-goto-token tok 1)
	  (when (hopjs-parse-backward-sexp)
	    (+ (current-column) hopjs-indent-level)))
	 (t
	  0))))
    ((eq (hopjs-parse-peek-token-type) 'try)
     (hopjs-indent-column-token
      (hopjs-parse-consume-token-any) hopjs-indent-level))
    ((eq (hopjs-parse-peek-token-type) 'else)
     (let ((tok (hopjs-parse-consume-token-any)))
       (if (eq (hopjs-parse-peek-token-type) 'rbrace)
	   (hopjs-indent-column-token
	    (hopjs-parse-peek-token) hopjs-indent-level)
	 (hopjs-indent-column-token
	  (hopjs-parse-consume-token-any) hopjs-indent-level))))
    ((eq (hopjs-parse-peek-token-type) '=)
     (let ((tok (hopjs-parse-consume-token-any)))
       (hopjs-debug 0 "hopjs-indent-new-lbrace =...%s" tok)
       (if (eq (hopjs-parse-peek-token-type) 'ident)
	   (let ((tok (hopjs-parse-consume-token-any)))
	     (if (memq (hopjs-parse-peek-token-type) '(let const var))
		 (hopjs-indent-column-token
		  (hopjs-parse-consume-token-any) hopjs-indent-level)
	       (hopjs-indent-column-token tok hopjs-indent-level)))
	 (hopjs-indent-column-token tok 0))))
    ((eq (hopjs-parse-peek-token-type) 'colon)
     (let ((tok (hopjs-parse-consume-token-any)))
       (if (eq (hopjs-parse-peek-token-type) 'ident)
	   (let ((tok (hopjs-parse-consume-token-any)))
	     (if (memq (hopjs-parse-peek-token-type) '(let const var))
		 (hopjs-indent-column-token
		  (hopjs-parse-consume-token-any) hopjs-indent-level)
	       (hopjs-indent-column-token tok hopjs-indent-level)))
	 (hopjs-indent-column-token tok 0))))
    (t
     '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-tilde ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-tilde (tok)
  (with-debug
   "hopjs-indent-new-tilde...%s" tok
   (progn
     (hopjs-debug 0 "hopjs-indent-new-tilde...%s" (hopjs-parse-peek-token))
     (when (eq (hopjs-parse-peek-token-type) '=)
       (hopjs-parse-consume-token-any)
       (hopjs-debug 0 "hopjs-indent-new-tilde...%s" (hopjs-parse-peek-token))
       (when (eq (hopjs-parse-peek-token-type) 'ident)
	 (hopjs-indent-column-token
	  (hopjs-parse-consume-token-any)
	  hopjs-indent-level))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-literal ...                                     */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-literal (tok)
  (with-debug
   "hopjs-indent-new-literal...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-push-token tok))
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-literal...etok=%s peek=%s"
		      etok (hopjs-parse-peek-token))
	 (case (hopjs-parse-peek-token-type)
	   ((rbrace)
	    (hopjs-indent-column-token (hopjs-parse-peek-token) 0))
	   ((number string)
	    (hopjs-indent-new-literal (hopjs-parse-consume-token-any)))
	   ((return var)
	    (let ((rtok (hopjs-parse-peek-token)))
	      (if (hopjs-indent-same-linep rtok etok) 
		  (hopjs-indent-column-token rtok hopjs-indent-level)
		(hopjs-indent-column-token etok hopjs-indent-level))))
	   ((ohtml)
	    (goto-char (+ 1 (hopjs-parse-token-end (hopjs-parse-peek-token))))
	    (current-column))
	   (t
	    '()))))
    (t
     '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-= ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-= (tok)
  (with-debug
   "hopjs-indent-new-= %s" tok
   (if (hopjs-parse-expr (hopjs-parse-peek-token))
       (hopjs-indent-column-token
	(hopjs-parse-consume-token-any)
	hopjs-indent-level)
     (hopjs-indent-column-token tok (- hopjs-indent-level)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-binop ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-binop (tok)
  (with-debug
   "hopjs-indent-new-binop %s [%s]" tok (hopjs-parse-token-string tok)
   (let ((tok (hopjs-parse-expr (hopjs-parse-peek-token))))
     (if tok
	 (progn
	   (hopjs-debug 0
			"hopjs-indent-new-binop %s [%s] peek=%s"
			tok (hopjs-parse-token-string tok)
			(hopjs-parse-peek-token-type))
	   (case (hopjs-parse-peek-token-type)
	     ((return var)
	      (hopjs-indent-column-token
	       (hopjs-parse-consume-token-any) (+ hopjs-indent-level)))
	     (t (hopjs-indent-column-token tok 0))))
       (hopjs-indent-column-token tok (- hopjs-indent-level))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-return ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-return (tok)
  (with-debug
   "hopjs-indent-new-binop %s [%s]" tok (hopjs-parse-token-string tok)
   (hopjs-indent-column-token tok hopjs-indent-level)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-comment ...                                     */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-comment (tok)
  (with-debug
   "hopjs-indent-new-comment %s [%s]" tok (hopjs-parse-token-string tok)
   (progn
     (hopjs-indent-new (- (hopjs-parse-token-beginning tok) 1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-qmark ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-qmark (tok)
  (with-debug
   "hopjs-indent-new-qmark %s [%s]" tok (hopjs-parse-token-string tok)
   (hopjs-parse-goto-token (hopjs-parse-consume-token-any))
   (+ (hopjs-indent-new (point)) hopjs-indent-level)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-qmark ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-qmark (pos)
  (with-debug
   "hopjs-indent-old-qmark %s" pos
   (progn
     (hopjs-parse-start (- pos 1))
     (hopjs-debug 0 "hopjs-indent-old-qmark tok=%s" (hopjs-parse-peek-token))
     (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token))))
       (hopjs-debug 0 "hopjs-indent-old-qmark etok=%s" etok)
       (hopjs-indent-column-token etok hopjs-indent-level)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-rbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-rbrace (tok)
  (with-debug
   "hopjs-indent-new-rbrace...%s" tok
   (progn
     (hopjs-parse-goto-token tok 1)
     (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.1 -> %s" (point))
     (hopjs-parse-backward-sexp)
     (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.2 -> %s %c" (point) (char-after (- (point) 1)))
     (if (memq (char-after (- (point) 1)) '(?~ ?$))
	 (progn
	   (hopjs-parse-start (point))
	   (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.3 -> %s"
			(hopjs-parse-peek-token-type))
	   (when (memq (hopjs-parse-peek-token-type) '(tilde dollar))
	     (let ((tok (hopjs-parse-consume-token-any)))
	       (cond
		((eq (hopjs-parse-peek-token-type) '=)
		 (hopjs-parse-consume-token-any)
		 (when (eq (hopjs-parse-peek-token-type) 'ident)
		   (hopjs-indent-column-token
		    (hopjs-parse-consume-token-any) 0)))
		(t
		 (hopjs-indent-column-token tok 0))))))
       (progn
	 (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.4 %s %s" (point) tok)
	 (hopjs-parse-start (point))
	 (let ((lpos (hopjs-indent-new-lbrace tok)))
	   (when lpos
	     (- lpos hopjs-indent-level))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-rbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-rbrace (pos)
  (with-debug
   "hopjs-indent-new-rbrace...%s" pos
   (progn
     (hopjs-parse-start (+ 2 pos))
     (hopjs-indent-new-rbrace (hopjs-parse-consume-token-any)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lparen ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lparen (tok)
  (with-debug
   "hopjs-indent-new-lparen...%s" tok
   (let ((tok (hopjs-parse-consume-token-any)))
     (hopjs-debug 0 "hopjs-indent-new-lparen...next=%s" tok)
     (hopjs-indent-new-literal tok))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-rparen ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-rparen (tok)
  (with-debug
   "hopjs-indent-new-rparen...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-push-token tok))
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-rparen...expr.etok=%s"
		      (hopjs-parse-peek-token))
	 (case (hopjs-parse-peek-token-type)
	   ((=)
	    (if (hopjs-parse-lhs (hopjs-parse-consume-token-any))
		(progn
		  (hopjs-debug 0 "hopjs-indent-new-rparen...expr.%s"
			       (hopjs-parse-peek-token-type))
		  (case (hopjs-parse-peek-token-type)
		    ((var return)
		     (save-excursion
		       (hopjs-parse-goto-token (hopjs-parse-peek-token))
		       (back-to-indentation)
		       (current-column)))
		    (t
		     (hopjs-indent-column-token
		      (hopjs-parse-consume-token-any) 0))))
	      (hopjs-indent-column-token etok 0)))
	   ((rparen)
	    (hopjs-debug 0 "hopjs-indent-new-rparen...rparen %s"
			 (hopjs-parse-peek-token))
	    (hopjs-indent-new-rparen (hopjs-parse-consume-token-any)))
	   ((return if)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any) hopjs-indent-level))
	   (t 
	    (hopjs-indent-column-token etok 0)))))
    (t
     0))))
   
;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-comma ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-comma (tok)
  (with-debug
   "hopjs-indent-new-comma...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-peek-token))
     =>
     #'(lambda (etok)
	 (letn loop ((etok etok))
	       (hopjs-debug 0 "hopjs-indent-new-comma %s [%s] -> %s"
			    etok
			    (hopjs-parse-token-string etok)
			    (hopjs-parse-peek-token))
	       (case (hopjs-parse-peek-token-type)
		 ((comma)
		  (hopjs-parse-consume-token-any)
		  (funcall loop (hopjs-parse-expr (hopjs-parse-peek-token))))
		 ((lparen)
		  (hopjs-indent-column-token etok 0))
		 ((colon)
		  (let ((tok (hopjs-parse-consume-token-any)))
		       (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token))))
			 (if etok
			     (hopjs-indent-column-token etok 0)
			   (hopjs-indent-column-token tok 0)))))
		 ((lbracket)
		  (hopjs-indent-column-token etok 0))
		 (t
		  0)))))
    (t
     0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-semicolon ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-semicolon (tok)
  (with-debug
   "hopjs-indent-new-semicolon...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-peek-token))
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-semicolon...expr.tok=%s [%s]"
		      (hopjs-parse-peek-token)
		      (hopjs-parse-token-string (hopjs-parse-peek-token)))
	 (case (hopjs-parse-peek-token-type)
	   ((=)
	    (let ((lhstok (hopjs-parse-lhs (hopjs-parse-consume-token-any))))
	      (hopjs-debug 0 "hopjs-indent-new-semicolon...lhs=%s" lhstok)
	      (if lhstok
		  (progn
		    (hopjs-debug 0 "hopjs-indent-new-semicolon...lhstok %s" 
				 (hopjs-parse-peek-token))
		    (case (hopjs-parse-peek-token-type)
		      ((var let const return)
		       (save-excursion
			 (hopjs-parse-goto-token (hopjs-parse-peek-token))
			 (back-to-indentation)
			 (current-column)))
		      ((lbrace semicolon)
		       (save-excursion
			 (hopjs-parse-goto-token lhstok)
			 (back-to-indentation)
			 (current-column)))
		      (t
		       (hopjs-indent-column-token
			(hopjs-parse-consume-token-any) 0))))
		(hopjs-indent-column-token etok 0))))
	   ((rparen)
	    (hopjs-indent-new (hopjs-parse-token-end (hopjs-parse-peek-token))))
	   ((comma)
	    (hopjs-indent-new-semicolon
	     (hopjs-parse-token-beginning (hopjs-parse-consume-token-any))))
	   ((var const let)
	    (let* ((tok (hopjs-parse-consume-token-any))
		   (b (hopjs-parse-token-beginning tok)))
	      (hopjs-debug 0
			   "hopjs-indent-new-semicolon var=%s first=%s b=%s"
			   tok (hopjs-indent-first-on-linep b) b)
	      (if (hopjs-indent-first-on-linep b)
		  (hopjs-indent-column-token tok 0)
		(hopjs-indent-new (hopjs-parse-token-beginning tok)))))
;* 		(hopjs-indent-new-semicolon (hopjs-parse-consume-token-any))))) */
	   ((return new)
	    (hopjs-indent-column-token (hopjs-parse-consume-token-any) 0))
	   (t 
	    (hopjs-indent-column-token etok 0)))))
    (t
     '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-colon ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-colon (tok)
  (with-debug
   "hopjs-indent-new-colon...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-peek-token))
     =>
     #'(lambda (etok)
	 (case (hopjs-parse-peek-token-type)
	   ((case)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any)
	     hopjs-indent-level-html))
	   (t
	    (hopjs-indent-column-token tok hopjs-indent-level-html)))))
    (t
     '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-case ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-case (pos)
  (with-debug
   "hopjs-indent-old-case %s" pos
   (save-excursion
     (forward-line -1)
     (end-of-line)
     (hopjs-parse-start (point))
     (letn loop ()
	   (let ((tok (hopjs-parse-consume-token-any)))
	     (hopjs-debug 0 "hopjs-indent-old-case peek=%s" tok)
	     (case (hopjs-parse-token-type tok)
	       ((case)
		(hopjs-indent-column-token tok 0))
	       ((switch)
		(hopjs-indent-column-token tok hopjs-indent-level))
	       (t
		(funcall loop))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-otag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-otag (tok)
  (with-debug
   "hopjs-indent-new-otag...%s [%s] next=%s"
   tok (hopjs-parse-token-string tok) (hopjs-parse-peek-token-type)
   (letn loop ()
	 (hopjs-debug 0 "hopjs-indent-new-otag peek=%s %s"
		      (hopjs-parse-peek-token-type)
		      (hopjs-parse-token-string (hopjs-parse-peek-token)))
	 (case (hopjs-parse-peek-token-type)
	   ((=)
	    ;; lhs = <otag>
	    (if (hopjs-parse-lhs (hopjs-parse-consume-token-any))
		(hopjs-indent-column-token
		 (hopjs-parse-consume-token-any)
		 hopjs-indent-level-html)
	      0))
	   ((ident text)
	    (hopjs-parse-consume-token-any)
	    (funcall loop))
	   ((return)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any)
	     hopjs-indent-level-html))
	   (t
	    (hopjs-indent-column-token tok hopjs-indent-level-html))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-otag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-otag (pos)
  (with-debug
   "hopjs-indent-old-otag %s" pos
   (save-excursion
     (forward-line -1)
     (end-of-line)
     (hopjs-parse-start (point))
     (let ((tok (hopjs-parse-peek-token)))
       (if (memq (hopjs-parse-token-type tok) '(ident text dots))
	 (let ((tok tok))
	   (letn loop ()
		 (hopjs-debug 0 "hopjs-indent-old-otag peek=%s %s"
			      (hopjs-parse-peek-token-type)
			      (hopjs-parse-token-string (hopjs-parse-peek-token)))
		 (case (hopjs-parse-peek-token-type)
		   ((ident text dots)
		    (hopjs-parse-consume-token-any)
		    (funcall loop))
		   ((otag)
		    (hopjs-indent-column-token
		     (hopjs-parse-consume-token-any)
		     hopjs-indent-level-html))
		   ((ctag)
		    (hopjs-indent-column-token
		     (hopjs-parse-consume-token-any)
		     0))
		   ((>)
		    (hopjs-indent-new-etag (hopjs-parse-consume-token-any)))
		   (t
		    (hopjs-indent-column-token tok hopjs-indent-level-html)))))
	 (progn
	   (next-line 1)
	   (beginning-of-line)
	   (hopjs-indent-new (point))))))))
     
;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-etag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-etag (tok)
  (with-debug
   "hopjs-indent-new-etag %s" pos
   (letn loop ()
	 (hopjs-debug 0 "hopjs-indent-new-etag peek=%s [%s]"
		      (hopjs-parse-peek-token)
		      (hopjs-parse-token-string (hopjs-parse-peek-token)))
	 (let ((tok (hopjs-parse-consume-token-any)))
	   (case (hopjs-parse-token-type tok)
;* 	    ((ohtml)                                                   */
;* 	     (hopjs-indent-column-token tok hopjs-indent-level))       */
;* 	    ((otag ohtml)                                              */
;* 	     (hopjs-indent-new-otag (hopjs-parse-consume-token-any)))  */
	    ((otag ohtml)
	     (hopjs-indent-new-otag tok))
	    ((number string)
	     (when (hopjs-indent-new-literal tok)
	       (funcall loop)))
	    ((ident)
	     (funcall loop))
	    ((rbrace)
	     (when (hopjs-indent-new-rbrace tok)
	       (hopjs-debug 0 "hopjs-indent-new-etag rbrace -> point=%s %s"
			    (point) (hopjs-parse-peek-token))
	       (funcall loop)))
	    (t
	     '()))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-ctag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-ctag (tok)
  (with-debug
   "hopjs-indent-new-ctag...%s %s" tok (hopjs-parse-token-string tok)
   (let ((otag (hopjs-find-opening-tag (- (hopjs-parse-token-end tok) 1))))
     (if otag
	 (let ((b (match-beginning 0))
	       (e (match-end 0)))
	   (hopjs-debug 0 "hopjs-indent-new-ctag...find-opening=[%s]"
			(buffer-substring-no-properties b e))
	   (hopjs-parse-start b)
	   (let ((tok (hopjs-parse-peek-token)))
	     (hopjs-debug 0 "hopjs-indent-new-ctag opening %s %s [%s] otag=%s peek=%s"
			  b e (buffer-substring-no-properties b e)
			  otag tok)
	     (if (memq (hopjs-parse-token-type tok) '(return var))
		 (hopjs-indent-column-token tok 0)
	       (progn
		 (goto-char b)
		 (current-column)))))
       0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-ctag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-ctag (pos)
  (with-debug
   "hopjs-indent-old-ctag...%s" pos
   (let ((otag (hopjs-find-opening-tag pos)))
     (if otag
	 (let ((b (match-beginning 0))
	       (e (match-end 0)))
	   (hopjs-debug 0 "hopjs-indent-old-ctag opening.1 b=%s e=%s" b e)
	   (hopjs-parse-start e)
	   (let ((col (hopjs-indent-new-otag (hopjs-parse-consume-token-any))))
	     (hopjs-debug 0 "hopjs-indent-old-ctag opening %s > %s" pos col)
	     (- col hopjs-indent-level-html)))
       0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-chtml ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-chtml (tok)
  (with-debug
   "hopjs-indent-new-chtml...%s %s" tok (hopjs-parse-token-string tok)
   (letn loop ()
	 (let ((tok (hopjs-parse-consume-token-any)))
	   (hopjs-debug 0 "hopjs-indent-new-chtml tok=%s" tok)
	   (cond
	    ((eq (hopjs-parse-token-type tok) 'ohtml)
	     (hopjs-indent-column-token tok 0))
	    ((eq (hopjs-parse-token-type tok) 'bof)
	     0)
	    (t
	     (funcall loop)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-column-token ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-column-token (token indent)
  (save-excursion
    (goto-char (hopjs-parse-token-beginning token))
    (+ (current-column) indent)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-find-otag ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-find-otag (tok)
  (when (hopjs-find-opening-tag (- (hopjs-parse-token-end tok) 1))
    (cons (match-beginning 0) (match-end 0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-first-on-linep ...                                  */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-first-on-linep (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (when (looking-at "[ \t]*[^ \t]")
      (= (match-end 0) (+ pos 1)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-pos-column ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-pos-column (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-same-linep ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-same-linep (left right)
  (save-excursion
    (goto-char (hopjs-parse-token-end left))
    (end-of-line)
    (<= (hopjs-parse-token-end right) (point))))

