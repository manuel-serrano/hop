;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/etc/hopjs-indent.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov  2 09:45:39 2018                          */
;*    Last change :  Fri Nov 16 13:05:10 2018 (serrano)                */
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
    ((hopjs-indent-in-commentp pos) nil)
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
		      ((lbracket) (hopjs-indent-new-lbracket tok))
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
		      ((qmark) (hopjs-indent-new-qmark tok))
		      ((=>) (hopjs-indent-new-=> tok)))))
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
       (hopjs-debug 0 "hopjs-indent-old ctag...")
       (or (hopjs-indent-old-ctag (- (match-end 0) 2)) 0))
      ((looking-at "[ \t]*<[^ \t]")
       (hopjs-debug 0 "hopjs-indent-old otag...")
       (hopjs-indent-old-otag (- (match-end 0) 2)))
      ((looking-at "[ \t]*\\(?:case\\|default\\)")
       (hopjs-debug 0 "hopjs-indent-old case...")
       (hopjs-indent-old-case (match-end 0)))
      ((looking-at "[ \t]*}")
       (hopjs-debug 0 "hopjs-indent-old rbrace...")
       (or (hopjs-indent-old-rbrace (- (match-end 0) 2)) 0))
      ((looking-at "[ \t]*]")
       (hopjs-debug 0 "hopjs-indent-old rbracket...")
       (or (hopjs-indent-old-rbracket (- (match-end 0) 2)) 0))
      ((looking-at "[ \t]*[.]")
       (hopjs-debug 0 "hopjs-indent-old dot...")
       (or (hopjs-indent-new pos) 0))
      ((looking-at "[ \t]*[?]")
       (hopjs-debug 0 "hopjs-indent-old qmark...")
       (hopjs-indent-old-qmark (match-end 0)))
      ((looking-at "[ \t]*\\(?:[+*|&^%-]\\|/[^/*]\\)")
       (hopjs-debug 0 "hopjs-indent-old binop...")
       (hopjs-indent-old-binop (match-end 0)))
      (t
       (hopjs-debug 0 "hopjs-indent-old default...")
       (or (hopjs-indent-new pos) 0))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lbrace (tok)
  (with-debug
   "hopjs-indent-new-lbrace...%s peek=%s" tok (hopjs-parse-peek-token)
   (cond
    ((hopjs-parse-args (hopjs-parse-peek-token))
     ;; (args) {
     (hopjs-debug 0 "hopjs-indent-new-lbrace.expr...%s"
		  (hopjs-parse-peek-token))
     (case (hopjs-parse-peek-token-type)
       ((ident)
	;; ident (args) {
	(hopjs-debug 0 "hopjs-indent-new-lbrace IDENT.1..%s [%s]"
		     (hopjs-parse-peek-token)
		     (hopjs-parse-token-string (hopjs-parse-peek-token)))
	(let ((itok (hopjs-parse-consume-token-any)))
	  (case (hopjs-parse-peek-token-type)
	    ((function function* service)
	     (let ((tok (hopjs-parse-consume-tokens
			 '(function function* service ident))))
	       (hopjs-debug 0 "hopjs-indent-new-lbrace IDENT.2.. %s %s"
			    tok (hopjs-parse-peek-token-type))
	       (case (hopjs-parse-peek-token-type)
		 ((= colon)
		  ;; lhs = function ident (args) {
		  (let ((etok (hopjs-parse-expr (hopjs-parse-consume-token-any))))
		    (hopjs-debug 0 "hopjs-indent-new-lbrace IDENT.3..%s" etok)
		    (if etok
			(hopjs-indent-column-token etok hopjs-indent-level)
		      (hopjs-indent-column-token itok hopjs-indent-level))))
		 ((return)
		  (hopjs-indent-column-token
		   (hopjs-parse-peek-token) hopjs-indent-level))
		 (t
		  (hopjs-indent-column-token tok hopjs-indent-level)))))
	    ((ident)
	     (hopjs-indent-new-idents hopjs-indent-level))
	    ((return)
	     (hopjs-indent-column-token
	      (hopjs-parse-peek-token) hopjs-indent-level))
	    (t
	     (hopjs-debug 0 "hopjs-indent-new-lbrace IDENT.4..%s [%s]"
			  itok (hopjs-parse-token-string itok))
	     (hopjs-indent-column-token itok hopjs-indent-level)))))
       ((function)
	;; function (args) {
	(let ((tok (hopjs-parse-consume-token-any)))
	  (hopjs-debug 0 "hopjs-indent-new-lbrace FUNCTION..%s"
		       (hopjs-parse-peek-token))
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
	    ((lparen)
	     ;; lhs( function ident (args) {...
	     ;; indent with respect to this line indentation
	     (or (hopjs-indent-column-line hopjs-indent-level)
		 (hopjs-indent-column-token tok hopjs-indent-level)))
	    ((return)
	     (hopjs-indent-column-token
	      (hopjs-parse-peek-token) hopjs-indent-level))
	    (t
	     (hopjs-indent-column-token tok hopjs-indent-level)))))
       ((if while switch for)
	;; if (args) {
	(hopjs-debug 0 "hopjs-indent-new-lbrace IF/WHILE/SWITCH.1..%s"
		     (hopjs-parse-peek-token))
	(hopjs-indent-column-token
	 (hopjs-parse-consume-token-any) hopjs-indent-level))
       (t
	(hopjs-debug 0 "hopjs-indent-new-lbrace DEFAULT..%s"
		     (hopjs-parse-peek-token))
	'())))
    (t
     (case (hopjs-parse-peek-token-type)
       ((=>)
	(hopjs-indent-new-=> (hopjs-parse-consume-token-any)))
       ((try catch)
	(hopjs-indent-column-token
	 (hopjs-parse-consume-token-any) hopjs-indent-level))
       ((else)
	(hopjs-debug 0 "hopjs-indent-new-lbrace else...%s" tok)
	(let ((tok (hopjs-parse-consume-token-any)))
	  (if (eq (hopjs-parse-peek-token-type) 'rbrace)
	      (hopjs-indent-column-token
	       (hopjs-parse-peek-token) hopjs-indent-level)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any) hopjs-indent-level))))
       ((=)
	(let ((tok (hopjs-parse-consume-token-any)))
	  (hopjs-debug 0 "hopjs-indent-new-lbrace =...%s" tok)
	  (if (eq (hopjs-parse-peek-token-type) 'ident)
	      (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token))))
		(hopjs-debug 0 "hopjs-indent-new-lbrace.2 =...%s" etok)
		(cond
		 ((not etok)
		  (hopjs-indent-column-token tok hopjs-indent-level))
		 ((memq (hopjs-parse-peek-token-type) '(let const var))
		  (hopjs-indent-column-token
		   (hopjs-parse-consume-token-any) hopjs-indent-level))
		 (t
		  (hopjs-indent-column-token etok hopjs-indent-level))))
	    (hopjs-indent-column-token tok 0))))
       ((colon)
	(let ((tok (hopjs-parse-consume-token-any)))
	  (if (memq (hopjs-parse-peek-token-type) '(ident string number))
	      (let ((tok (hopjs-parse-consume-token-any)))
		(if (memq (hopjs-parse-peek-token-type) '(let const var))
		    (hopjs-indent-column-token
		     (hopjs-parse-consume-token-any) hopjs-indent-level)
		  (hopjs-indent-column-token tok hopjs-indent-level)))
	    (hopjs-indent-column-token tok 0))))
       ((semicolon)
	(hopjs-indent-column-token tok hopjs-indent-level))
       ((ident)
	(let ((itok (hopjs-parse-consume-token-any)))
	  (case (hopjs-parse-peek-token-type)
	    ((rbrace)
	      (hopjs-indent-column-token
	       (hopjs-parse-peek-token) hopjs-indent-level))
	    ((ident text)
	     hopjs-indent-level)
	    (t
	     (hopjs-indent-column-token itok hopjs-indent-level)))))
       (t
	(hopjs-indent-column-token tok hopjs-indent-level)))))))

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
	      (if (hopjs-parse-same-linep rtok etok) 
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
	     ((return var let const)
	      (hopjs-indent-column-token
	       (hopjs-parse-consume-token-any) (+ hopjs-indent-level)))
	     (t (hopjs-indent-column-token tok 0))))
       (hopjs-indent-column-token tok (- hopjs-indent-level))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-binop ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-binop (pos)
  (with-debug
   "hopjs-indent-old-binop %s" pos
   (progn
     (hopjs-parse-start (- pos 1))
     (let ((tok (hopjs-parse-expr (hopjs-parse-peek-token))))
       (if tok
	   (progn
	     (hopjs-debug 0
			  "hopjs-indent-old-binop %s [%s] peek=%s"
			  tok (hopjs-parse-token-string tok)
			  (hopjs-parse-peek-token-type))
	     (case (hopjs-parse-peek-token-type)
	       ((return var let const)
		(hopjs-indent-column-token
		 (hopjs-parse-consume-token-any) (+ hopjs-indent-level)))
	       (t (hopjs-indent-column-token tok 0))))
	 0)))))

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
   (hopjs-indent-new (- (hopjs-parse-token-beginning tok) 1))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-qmark ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-qmark (tok)
  (with-debug
   "hopjs-indent-new-qmark %s [%s]" tok (hopjs-parse-token-string tok)
   (progn
     (hopjs-parse-goto-token (hopjs-parse-consume-token-any))
     (+ (hopjs-indent-new (point)) hopjs-indent-level))))

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
;*    hopjs-indent-new-=> ...                                          */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-=> (tok)
  (with-debug
   "hopjs-indent-new-=> %s [%s]" tok (hopjs-parse-token-string tok)
   (let ((tok (hopjs-parse-consume-token-any)))
     (hopjs-debug 0 "hopjs-indent-new-lbrace =>...%s" tok)
     (case (hopjs-parse-token-type tok)
       ((ident)
	(hopjs-indent-function-body tok hopjs-indent-level))
       ((rparen)
	(hopjs-parse-goto-token tok 1)
	(let ((pos (hopjs-parse-backward-sexp)))
	  (if pos
	      (progn
		(hopjs-parse-start (+ 1 pos))
		(hopjs-indent-function-body
		 (hopjs-parse-consume-token-any) hopjs-indent-level))
	    (hopjs-indent-column-token tok hopjs-indent-level))))
       (t
	(hopjs-indent-column-token tok hopjs-indent-level))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-rbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-rbrace (tok)
  (with-debug
   "hopjs-indent-new-rbrace...%s" tok
   (progn
     (hopjs-parse-goto-token tok 1)
     (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.1 -> %s" (point))
     (let ((pos (hopjs-parse-backward-sexp)))
       (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.2 -> %s %c" (point) (char-after (- (point) 1)))
       (cond
	((memq (char-after (- (point) 1)) '(?~ ?$))
	 (hopjs-parse-start (point))
	 (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.3 -> %s"
		      (hopjs-parse-peek-token-type))
	 (when (memq (hopjs-parse-peek-token-type) '(tilde dollar))
	   (let ((tok (hopjs-parse-consume-token-any)))
	     (cond
	      ((eq (hopjs-parse-peek-token-type) '=)
	       (hopjs-parse-consume-token-any)
	       (hopjs-indent-new-idents 0 t))
;* 	       (when (eq (hopjs-parse-peek-token-type) 'ident)         */
;* 		 (hopjs-indent-column-token                            */
;* 		  (hopjs-parse-consume-token-any) 0)))                 */
	      (t
	       (hopjs-indent-column-token tok 0))))))
	((memq (char-after (- (point) 1)) '(?\( ?{))
	 (hopjs-parse-start (+ 1 pos))
	 (hopjs-indent-column-token (hopjs-parse-peek-token) 0))
	(t
	 (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.4 %s %s"
		      (point) tok)
	 (hopjs-parse-start (+ (point) 1))
	 (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.5 %s"
		      (hopjs-parse-peek-token))
	 (let ((col (hopjs-indent-new-lbrace (hopjs-parse-consume-token-any))))
	   (if col (- col hopjs-indent-level) 0))))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-rbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-rbrace (pos)
  (with-debug
   "hopjs-indent-old-rbrace...%s" pos
   (progn
     (hopjs-parse-start (+ 2 pos))
     (hopjs-indent-new-rbrace (hopjs-parse-consume-token-any)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lparen ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lparen (tok)
  (with-debug
   "hopjs-indent-new-lparen...%s" tok
   (mcond
    ((hopjs-parse-expr tok)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-lparen etok=%s [%s]" etok
		      (hopjs-parse-token-string etok))
	 (cond
	  ((eq (hopjs-parse-token-type etok) 'new)
	   (hopjs-indent-column-token etok hopjs-indent-level))
	  ((memq (hopjs-parse-peek-token-type) '(return var let const ident))
	   (hopjs-indent-column-token
	    (hopjs-parse-peek-token) hopjs-indent-level))
	  (etok
	   (hopjs-indent-column-token etok hopjs-indent-level))
	  (t
	   0))))
    (t
     0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lbracket ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lbracket (tok)
  (with-debug
   "hopjs-indent-new-lbracket...%s" tok
   (mcond
    ((hopjs-parse-expr tok)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-lbracket etok=%s" etok)
	 (cond
	  ((eq (hopjs-parse-token-type etok) 'new)
	   (hopjs-indent-column-token etok hopjs-indent-level))
	  ((memq (hopjs-parse-peek-token-type) '(return var let const ident))
	   (hopjs-indent-column-token
	    (hopjs-parse-peek-token) hopjs-indent-level))
	  (etok
	   (hopjs-indent-column-token etok hopjs-indent-level))
	  (t
	   0))))
    ((eq (hopjs-parse-peek-token-type) 'colon)
     (let ((tok (hopjs-parse-consume-token-any)))
       (if (memq (hopjs-parse-peek-token-type) '(ident string number))
	   (let ((tok (hopjs-parse-consume-token-any)))
	     (if (memq (hopjs-parse-peek-token-type) '(let const var))
		 (hopjs-indent-column-token
		  (hopjs-parse-consume-token-any) hopjs-indent-level)
	       (hopjs-indent-column-token tok hopjs-indent-level)))
	 (hopjs-indent-column-token tok 0))))    
    (t
     0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-rbracket ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-rbracket (tok)
  (with-debug
   "hopjs-indent-new-rbracket...%s" tok
   (progn
     (hopjs-parse-goto-token tok 1)
     (hopjs-debug 0 "hopjs-indent-new-rbracket sexp.1 -> %s" (point))
     (let ((pos (hopjs-parse-backward-sexp)))
       (hopjs-debug 0 "hopjs-indent-new-rbracket sexp.2 -> %s %c" (point) (char-after (- (point) 1)))
       (cond
	((memq (char-after (- (point) 1)) '(?\[))
	 (hopjs-parse-start (+ 1 pos))
	 (hopjs-indent-column-token (hopjs-parse-peek-token) 0))
	(t
	 (hopjs-debug 0 "hopjs-indent-new-rbracket sexp.4 %s %s"
		      (point) tok)
	 (hopjs-parse-start (+ (point) 1))
	 (hopjs-debug 0 "hopjs-indent-new-rbracket sexp.5 %s"
		      (hopjs-parse-peek-token))
	 (let ((col (hopjs-indent-new-lbrace (hopjs-parse-consume-token-any))))
	   (if col (- col hopjs-indent-level) 0))))))))
  
;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-rbracket ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-rbracket (pos)
  (with-debug
   "hopjs-indent-old-rbracket...%s" pos
   (hopjs-parse-start (+ 2 pos))
     (hopjs-indent-new-rbracket (hopjs-parse-consume-token-any))))

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
	   ((return if let const var)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any) hopjs-indent-level))
	   (t 
	    (hopjs-indent-column-token etok hopjs-indent-level)))))
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
	       (hopjs-debug 0 "hopjs-indent-new-comma etok=%s [%s] -> peek=%s"
			    etok
			    (hopjs-parse-token-string etok)
			    (hopjs-parse-peek-token))
	       (case (hopjs-parse-peek-token-type)
		 ((comma)
		  (if (hopjs-parse-same-linep (hopjs-parse-peek-token) etok)
		      (progn
			(hopjs-parse-consume-token-any)
			(funcall loop (hopjs-parse-expr (hopjs-parse-peek-token))))
		    (hopjs-indent-column-token etok 0)))
		 ((lparen)
		  (if (hopjs-parse-same-linep (hopjs-parse-peek-token) etok)
		      (hopjs-indent-new-lparen (hopjs-parse-consume-token-any))
		    (hopjs-indent-column-token etok 0)))
		 ((colon)
		  (let ((tok (hopjs-parse-consume-token-any)))
		       (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token))))
			 (if etok
			     (hopjs-indent-column-token etok 0)
			   (hopjs-indent-column-token tok 0)))))
		 ((lbrace)
		  (hopjs-indent-column-token (hopjs-parse-peek-token) 0))
		 ((lbracket)
		  (if (hopjs-parse-same-linep (hopjs-parse-peek-token) etok)
		      (hopjs-indent-column-token etok 0)
		    (hopjs-indent-new-lbracket (hopjs-parse-consume-token-any))))
		 ((var let const)
		  (hopjs-indent-column-token
		   (hopjs-parse-peek-token) hopjs-indent-level))
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
	   ((semicolon comma)
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
	   ((ident)
	    (hopjs-indent-new-idents 0))
	   ((dollar)
	    (let ((tok (hopjs-parse-consume-token-any)))
	      (if (memq (hopjs-parse-peek-token-type)
			'(return let var const ident))
		  (hopjs-indent-column-token (hopjs-parse-peek-token) 0)
		(hopjs-indent-column-token tok 0))))
	   ((else)
	    (hopjs-debug 0 "hopjs-indent-new-semicolon ELSE pos=%s first=%s"
			 (hopjs-parse-token-beginning
			  (hopjs-parse-peek-token))
			 (hopjs-indent-first-on-linep
			  (hopjs-parse-token-beginning
			   (hopjs-parse-peek-token))))
	    (if (hopjs-indent-first-on-linep
		 (hopjs-parse-token-beginning
		  (hopjs-parse-peek-token)))
		(hopjs-indent-column-token etok 0)
		(let ((pos (hopjs-parse-backward-sexp)))
		  (hopjs-debug 0 "hopjs-indent-new-semicolon ELSE pos=%s" pos)
		  (if pos
		      (hopjs-indent-new (+ 1 pos))
		    (hopjs-indent-column-token etok 0)))))
	   ((colon)
	    (hopjs-indent-new-idents 0))
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
	     hopjs-indent-level))
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
   (let* ((col (hopjs-indent-column-token tok 0))
	  (otag (hopjs-find-opening-tag (- (hopjs-parse-token-end tok) 1))))
     (if otag
	 (let ((b (match-beginning 0))
	       (e (match-end 0)))
	   (hopjs-debug 0 "hopjs-indent-new-ctag...find-opening=[%s]"
			(buffer-substring-no-properties b e))
	   (hopjs-parse-start b)
	   (let ((otok (hopjs-parse-peek-token)))
	     (hopjs-debug 0 "hopjs-indent-new-ctag opening %s %s [%s] otag=%s peek=%s"
			  b e (buffer-substring-no-properties b e)
			  otag otok)
	     (if (memq (hopjs-parse-token-type otok) '(return var))
		 (hopjs-indent-column-token otok 0)
	       (progn
		 (goto-char b)
		 (hopjs-debug 0 "hopjs-indent-new-ctag closing cur=%s col=%s"
			      (current-column) col)
		 (if (> (current-column) col) col (current-column))))))
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
;*    hopjs-indent-new-idents ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-idents (level &optional strict)
  (with-debug
   "hopjs-indent-new-idents...%s" (hopjs-parse-peek-token)
   (letn loop ((tok (hopjs-parse-consume-token-any)))
	 (let ((next (hopjs-parse-peek-token)))
	   (hopjs-debug 0 "hopjs-indent-new-idents...next=%s" next)
	   (cond
	    ((memq (hopjs-parse-token-type next) '(ident text binop))
	     (hopjs-parse-consume-token-any)
	     (funcall loop next))
	    ((memq (hopjs-parse-token-type next) '(return var let const))
	     (hopjs-indent-column-token next level))
	    ((and (not strict) (eq (hopjs-parse-token-type next) 'rbrace))
	     (hopjs-indent-column-token next level))
	    (t
	     (hopjs-indent-column-token tok level)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-function-body ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-function-body (tok level)
  (with-debug
   "hopjs-indent-function-body tok=%s" tok
   (if (not (hopjs-parse-same-linep (hopjs-parse-peek-token) tok))
       (hopjs-indent-column-token tok level)
     (case (hopjs-parse-peek-token-type)
       ((lparen)
	(let ((etok (hopjs-parse-expr (hopjs-parse-consume-token-any))))
	  (hopjs-debug 0 "hopjs-indent-function-body etok=%s [%s]" etok
		       (hopjs-parse-token-string etok))
	  (case (hopjs-parse-peek-token-type)
	    ((return)
	     (hopjs-indent-column-token (hopjs-parse-peek-token) level))
	    ((=)
	     (let ((tok (hopjs-parse-lhs (hopjs-parse-consume-token-any))))
	       (if tok
		   (let ((dtok (hopjs-parse-peek-token)))
		     (if (memq (hopjs-parse-token-type dtok) '(var let const))
			 (hopjs-indent-column-token dtok hopjs-indent-level)
		       (hopjs-indent-column-token tok hopjs-indent-level)))
		 (hopjs-indent-column-token tok level))))
	    (t
	     (hopjs-indent-column-token
	      (or etok tok)
	      (* hopjs-indent-function-body-alignment level))))))
       (t
	(hopjs-indent-column-token tok level))))))
     
;*---------------------------------------------------------------------*/
;*    hopjs-indent-column-token ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-column-token (token indent)
  (save-excursion
    (goto-char (hopjs-parse-token-beginning token))
    (+ (current-column) indent)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-column-line ...                                     */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-column-line (indent)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[ \t]*")
      (goto-char (match-end 0))
      (+ (current-column) indent))))

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
;*    hopjs-indent-in-commentp ...                                     */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-in-commentp (pos)
  (and (memq (get-text-property pos 'face)
	     '(font-lock-comment-face font-lock-string-face))
       (not (eq (char-after pos) ?\"))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-test ...                                            */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-test ()
  (interactive)
  (let ((loop t)
	(line 0)
	(mod (buffer-modified-p))
	(pos (point)))
    (unwind-protect
	(progn
	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (while loop
	    (beginning-of-line)
	    (condition-case nil
		(indent-for-tab-command)
	      (error
	       (message "indentation error line %s... stopping" line)
	       (setq loop nil)))
	    (cond
	     ((buffer-modified-p)
	      (setq loop nil))
	     ((> (forward-line 1) 0)
	      (setq loop nil))
	     (t
	      (setq line (+ line 1)))))
	  (if (buffer-modified-p)
	      (message "Tests failed line: %s" line)
	    (progn
	      (message "All tests passed")
	      (goto-char pos))))
      (when mod
	(set-buffer-modified-p t)))))
