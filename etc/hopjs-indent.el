;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/etc/hopjs-indent.el                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov  2 09:45:39 2018                          */
;*    Last change :  Fri Dec 24 06:30:51 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
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
    ((hopjs-indent-in-commentp pos) (hopjs-indent-comment pos))
    ((= (point-min) pos) 0)
    ((hopjs-blank-line-p pos) (hopjs-indent-new pos))
    (t (hopjs-indent-old pos)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-comment ...                                         */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-comment (pos)
  (interactive "d")
  (with-debug
   "hopjs-indent-comment pos=%s" pos
   (save-excursion
     (hopjs-parse-start (point))
     (let ((tok (hopjs-parse-consume-token-any)))
       (if (eq (hopjs-parse-token-type tok) 'eol-comment)
	   (hopjs-indent-new-token tok)
	 '())))))
  
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
       (hopjs-indent-new-token (hopjs-parse-consume-token-any))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-token ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-token (tok)
  (with-debug
   "hopjs-indent-new-token tok=%s [%s]" tok (hopjs-parse-token-string tok)
   (case (hopjs-parse-token-type tok)
     ((lbrace) (hopjs-indent-new-lbrace tok))
     ((rbrace) (hopjs-indent-new-rbrace tok))
     ((lparen) (hopjs-indent-new-lparen tok))
     ((rparen rbracket) (hopjs-indent-new-rparen tok))
     ((lbracket) (hopjs-indent-new-lbracket tok))
     ((comma) (hopjs-indent-new-comma tok))
     ((semicolon) (hopjs-indent-new-semicolon tok))
     ((colon) (hopjs-indent-new-colon tok))
     ((otag) (hopjs-indent-new-otag tok))
     ((>) (hopjs-indent-new-etag tok))
     ((ctag) (hopjs-indent-new-ctag tok))
     ((chtml) (hopjs-indent-new-chtml tok))
     ((tilde dollar) (hopjs-indent-new-tilde tok))
     ((number string regexp) (hopjs-indent-new-literal tok))
     ((ident) (hopjs-indent-new-ident tok))
     ((=) (hopjs-indent-new-= tok))
     ((binop) (hopjs-indent-new-binop tok))
     ((return throw) (hopjs-indent-new-return tok))
     ((eol-comment) (hopjs-indent-new-eol-comment tok))
     ((comment) (hopjs-indent-new-comment tok))
     ((qmark) (hopjs-indent-new-qmark tok))
     ((=>) (hopjs-indent-new-=> tok))
     (t (hopjs-indent-new-as-previous tok)))))

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
       (or (hopjs-indent-old-ctag (match-end 0)) 0))
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
;*    hopjs-indent-new-as-previous ...                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-as-previous (tok)
  (with-debug
   "hopjs-indent-new-as-previous tok=%s" tok
   (progn
     (goto-char (hopjs-parse-token-beginning tok))
     (skip-chars-forward " \t")
     (current-column))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lbrace ...                                      */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lbrace (tok)
  (with-debug
   "hopjs-indent-new-lbrace tok=%s peek=%s" tok (hopjs-parse-peek-token)
   (cond
    ((hopjs-parse-args (hopjs-parse-peek-token))
     ;; (args) {
     (hopjs-debug 0 "hopjs-indent-new-lbrace.expr...%s"
		  (hopjs-parse-peek-token))
     (case (hopjs-parse-peek-token-type)
       ((ident cssident)
	;; ident (args) {
	(hopjs-debug 0 "hopjs-indent-new-lbrace args.1..peek-%s [%s]"
		     (hopjs-parse-peek-token)
		     (hopjs-parse-peek-token-string))
	(let ((itok (hopjs-parse-consume-tokens '(ident dot) t)))
	  (hopjs-debug 0 "hopjs-indent-new-lbrace args.2..peek=%s [%s]"
		       (hopjs-parse-peek-token)
		       (hopjs-parse-peek-token-string))
	  (case (hopjs-parse-peek-token-type)
	    ((function function* service)
	     (let ((tok (hopjs-parse-consume-tokens
			 '(function function* service ident)
			 t)))
	       (hopjs-debug 0 "hopjs-indent-new-lbrace args.3.. %s %s"
			    tok (hopjs-parse-peek-token-type))
	       (case (hopjs-parse-peek-token-type)
		 ((export)
		  ;; export function foo() { ... }
		  (hopjs-indent-column-token
		   (hopjs-parse-peek-token) hopjs-indent-level))
		 ((= colon)
		  ;; lhs = function ident (args) {
		  (let ((etok (hopjs-parse-expr (hopjs-parse-consume-and-peek-token) t)))
		    (hopjs-debug 0 "hopjs-indent-new-lbrace args.4..%s" etok)
		    (if etok
			(hopjs-indent-column-token etok hopjs-indent-level)
		      (hopjs-indent-column-token itok hopjs-indent-level))))
		 ((return throw async)
		  (hopjs-indent-column-token
		   (hopjs-parse-peek-token) hopjs-indent-level))
		 (t
		  (hopjs-indent-column-token tok hopjs-indent-level)))))
	    ((ident dot)
	     (hopjs-indent-new-idents hopjs-indent-level))
	    ((return throw)
	     (hopjs-debug 0 "hopjs-indent-new-lbrace args.5..%s [%s]"
			  itok (hopjs-parse-token-string itok))
	     (hopjs-indent-column-token
	      (hopjs-parse-peek-token) hopjs-indent-level))
	    ((= colon)
	     (let ((etok (hopjs-parse-expr (hopjs-parse-consume-and-peek-token) t)))
	       (hopjs-debug 0 "hopjs-indent-new-lbrace args.6..%s [%s] -> %s [%s]"
			    itok (hopjs-parse-token-string itok)
			    etok (hopjs-parse-token-string etok))
	       (cond
		((not etok)
		 (hopjs-indent-column-token tok hopjs-indent-level))
		((not (eq (hopjs-parse-token-type etok) 'ident))
		 (hopjs-indent-column-token etok hopjs-indent-level))
		((memq (hopjs-parse-peek-token-type) '(const let var))
		 (let ((tok (hopjs-parse-peek-token)))
		   (if (eq (hopjs-parse-peek-token-type) 'export)
		       (hopjs-indent-column-token (hopjs-parse-peek-token) hopjs-indent-level)
		     (hopjs-indent-column-token tok hopjs-indent-level))))
		(t
		 (hopjs-indent-column-token etok hopjs-indent-level)))))
	    ((export)
	     (hopjs-indent-column-token
	      (hopjs-parse-peek-token) hopjs-indent-level))
	    (t
	     (hopjs-debug 0 "hopjs-indent-new-lbrace args.7..%s [%s]"
			  itok (hopjs-parse-token-string itok))
	     (hopjs-indent-column-token itok hopjs-indent-level)))))
       ((function function*)
	;; function (args) {
	(let ((tok (hopjs-parse-consume-token-any)))
	  (hopjs-debug 0 "hopjs-indent-new-lbrace FUNCTION..%s"
		       (hopjs-parse-peek-token))
	  (case (hopjs-parse-peek-token-type)
	    ((=)
	     ;; lhs = function ident (args) {
	     (if (hopjs-parse-same-linep (hopjs-parse-peek-token) tok)
		 (let ((tok (hopjs-parse-expr (hopjs-parse-consume-and-peek-token) t)))
		   (if tok
		       (let ((dtok (hopjs-parse-peek-token)))
			 (if (memq (hopjs-parse-token-type dtok) '(var let const))
			     (hopjs-indent-column-token dtok hopjs-indent-level)
			   (hopjs-indent-column-token tok hopjs-indent-level)))
		     0))
	       (hopjs-indent-column-token tok hopjs-indent-level)))
	    ((colon)
	     ;; lhs : function ident (args) {
	     (let ((tok (hopjs-parse-expr (hopjs-parse-consume-and-peek-token) t)))
	       (if tok
		   (let ((dtok (hopjs-parse-peek-token)))
		     (hopjs-indent-column-token tok hopjs-indent-level))
		 0)))
	    ((lparen)
	     ;; lhs( function ident (args) {...
	     ;; indent with respect to this line indentation
	     (or (hopjs-indent-column-line hopjs-indent-level)
		 (hopjs-indent-column-token tok hopjs-indent-level)))
	    ((return throw async)
	     (hopjs-indent-column-token
	      (hopjs-parse-peek-token) hopjs-indent-level))
	    ((comma)
	     (hopjs-indent-function-body
	      tok (hopjs-parse-consume-token-any) hopjs-indent-level))
	    (t
	     (hopjs-indent-column-token tok hopjs-indent-level)))))
       ((if)
	;; if (args) {
	(let ((tok (hopjs-parse-consume-token-any))
	      (peek (hopjs-parse-peek-token)))
	  (hopjs-debug 0 "hopjs-indent-new-lbrace IF.1..%s peek=%s [%s]" tok
		       peek (hopjs-parse-token-string peek))
	  (if (and (eq (hopjs-parse-token-type peek) 'else)
		   (hopjs-parse-same-linep peek tok))
	      (let ((ntok (hopjs-parse-consume-token-any))
		    (npeek (hopjs-parse-peek-token)))
		(if (and (eq (hopjs-parse-token-type npeek) 'rbrace)
			 (hopjs-parse-same-linep npeek peek))
		    (hopjs-indent-column-token npeek hopjs-indent-level)
		  (hopjs-indent-column-token ntok hopjs-indent-level)))
	    (hopjs-indent-column-token tok hopjs-indent-level))))
       ((while switch for do)
	;; if (args) {
	(hopjs-debug 0 "hopjs-indent-new-lbrace WHILE/SWITCH/FOR.1..%s"
		     (hopjs-parse-peek-token))
	(hopjs-indent-column-token
	 (hopjs-parse-consume-token-any) hopjs-indent-level))
       ((catch)
	;; try { } catch( e ) {
	(hopjs-debug 0 "hopjs-indent-new-lbrace CATCH..%s"
		     (hopjs-parse-peek-token))
	(let* ((ctok (hopjs-parse-consume-token-any))
	       (peek (hopjs-parse-peek-token)))
	  (if (and (eq (hopjs-parse-token-type peek) 'rbrace)
		   (hopjs-parse-same-linep peek ctok))
	      (hopjs-indent-column-token peek hopjs-indent-level)
	    (hopjs-indent-column-token ctok hopjs-indent-level))))
       (t
	(hopjs-debug 0 "hopjs-indent-new-lbrace DEFAULT..%s"
		     (hopjs-parse-peek-token))
	'())))
    (t
     (case (hopjs-parse-peek-token-type)
       ((=>)
	(hopjs-indent-new-=> (hopjs-parse-consume-token-any)))
       ((try catch return throw do)
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
	(let ((r (hopjs-indent-new-= (hopjs-parse-consume-token-any))))
	  (if (eq (hopjs-parse-peek-token-type) 'export)
	      (hopjs-indent-column-token
	       (hopjs-parse-peek-token) hopjs-indent-level)
	    r)))
       ((colon)
	(let ((tok (hopjs-parse-consume-token-any)))
	  (if (memq (hopjs-parse-peek-token-type) '(ident string regexp number))
	      (let ((tok (hopjs-parse-consume-token-any)))
		(if (memq (hopjs-parse-peek-token-type) '(let const var))
		    (hopjs-indent-column-token
		     (hopjs-parse-consume-token-any) hopjs-indent-level)
		  (hopjs-indent-column-token tok hopjs-indent-level)))
	    (hopjs-indent-column-token tok 0))))
       ((semicolon)
	(hopjs-indent-column-token tok hopjs-indent-level))
       ((comma)
	(cond
	 ((hopjs-parse-same-linep (hopjs-parse-peek-token) tok)
	  (+ (hopjs-indent-new-comma tok) hopjs-indent-level))
	 ((hopjs-indent-first-on-linep tok)
	  (hopjs-indent-column-token tok (+ hopjs-indent-level)))
	 (t
	  (hopjs-indent-column-token tok (- hopjs-indent-level)))))
       ((ident cssident)
	(let ((itok (hopjs-parse-consume-tokens '(ident dot text) t)))
	  (hopjs-debug 0 "hopjs-indent-new-lbrace IDENT.1 peek=%s [%s]"
		       (hopjs-parse-peek-token)
		       (hopjs-parse-peek-token-string))
	  (case (hopjs-parse-peek-token-type)
	    ((rbrace)
	      (hopjs-indent-column-token
	       (hopjs-parse-peek-token) hopjs-indent-level))
	    ((rbracket)
	     (+ (hopjs-indent-current-line-column) hopjs-indent-level))
	    ((ident text dot cssident)
	     (let ((itok (hopjs-parse-consume-tokens '(ident text dot cssident) t)))
	       (hopjs-indent-column-token itok hopjs-indent-level)))
	    ((=)
	     (hopjs-indent-new-= (hopjs-parse-consume-and-peek-token)))
	    ((colon)
	     (let* ((ctok (hopjs-parse-consume-token-any))
		    (etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
	       (hopjs-indent-column-token (or etok ctok) hopjs-indent-level)))
	    (t
	     (hopjs-indent-column-token itok hopjs-indent-level)))))
       ((text)
	(beginning-of-line)
	(skip-chars-forward "\t ")
	(+ (current-column) hopjs-indent-level))
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
    ((hopjs-parse-expr (hopjs-parse-push-token tok) t)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-literal...etok=%s peek=%s"
		      etok (hopjs-parse-peek-token))
	 (case (hopjs-parse-peek-token-type)
	   ((rbrace)
	    (hopjs-indent-column-token (hopjs-parse-peek-token) 0))
	   ((number string regexp)
	    (hopjs-indent-new-literal (hopjs-parse-consume-token-any)))
	   ((return throw var)
	    (let ((rtok (hopjs-parse-peek-token)))
	      (if (hopjs-parse-same-linep rtok etok) 
		  (hopjs-indent-column-token rtok hopjs-indent-level)
		(hopjs-indent-column-token etok hopjs-indent-level))))
	   ((ohtml)
	    (goto-char (+ 1 (hopjs-parse-token-end (hopjs-parse-peek-token))))
	    (current-column))
	   ((otag)
	    (if (eq (hopjs-parse-token-type tok) 'ident)
		0
	      (hopjs-indent-column-token etok 0)))
	   (t
	    (if (eq (hopjs-parse-token-type tok) 'ident)
		(hopjs-indent-column-token etok 3)
	      (hopjs-indent-column-token etok 0))))))
    (t
     '()))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-ident ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-ident (tok)
  (with-debug
   "hopjs-indent-new-ident...%s" tok
   (hopjs-indent-new-literal tok)))
;*    (letn loop ((tok tok)                                            */
;* 	       (count 0))                                              */
;* 	 (let ((ntok (hopjs-parse-consume-token-any)))                 */
;* 	   (hopjs-debug 0 "hopjs-indent-new-ident ntok=%s [%s]"        */
;* 			ntok (hopjs-parse-token-string ntok))          */
;* 	   (cond                                                       */
;* 	    ((not (hopjs-parse-same-linep ntok tok))                   */
;* 	     (if (>= count 2)                                          */
;* 		 0                                                     */
;* 		 (hopjs-indent-column-token tok hopjs-indent-level)))  */
;* 	    ((eq (hopjs-parse-token-type ntok) 'ident)                 */
;* 	     (funcall loop ntok (1+ count)))                           */
;* 	    ((eq (hopjs-parse-token-type ntok) 'dot)                   */
;* 	     (funcall loop ntok 0))                                    */
;* 	    (t                                                         */
;* 	     (hopjs-indent-column-token tok hopjs-indent-level)))))))  */

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-= ...                                           */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-= (tok)
  (with-debug
   "hopjs-indent-new-= %s" tok
   (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
     (cond
      ((not etok)
       (hopjs-indent-column-token tok hopjs-indent-level))
      ((memq (hopjs-parse-peek-token-type) '(var let const))
       (let ((tok (hopjs-parse-consume-token-any)))
	 (hopjs-indent-column-token tok hopjs-indent-level)))
      (t
       (hopjs-indent-column-token etok hopjs-indent-level))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-binop ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-binop (tok)
  (with-debug
   "hopjs-indent-new-binop %s [%s]" tok (hopjs-parse-token-string tok)
   (let ((tok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
     (if tok
	 (progn
	   (hopjs-debug 0 "hopjs-indent-new-binop %s [%s] peek=%s"
			tok (hopjs-parse-token-string tok)
			(hopjs-parse-peek-token-type))
	   (case (hopjs-parse-peek-token-type)
	     ((return throw var let const)
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
     (let ((tok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
       (if tok
	   (progn
	     (hopjs-debug 0 "hopjs-indent-old-binop %s [%s] peek=[%s]"
			  tok (hopjs-parse-token-string tok)
			  (hopjs-parse-peek-token-type))
	     (case (hopjs-parse-peek-token-type)
	       ((return throw var let const)
		(hopjs-indent-column-token
		 (hopjs-parse-consume-token-any) (+ hopjs-indent-level)))
	       ((binop)
		(hopjs-indent-column-token
		 (hopjs-parse-consume-token-any) 0))
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
   (hopjs-indent-new-token (hopjs-parse-consume-token-any))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-eol-comment ...                                 */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-eol-comment (tok)
  (with-debug
   "hopjs-indent-new-comment %s [%s]" tok (hopjs-parse-token-string tok)
   (if (hopjs-indent-first-on-linep tok)
       (hopjs-indent-column-token tok 0)
     (hopjs-indent-new-token (hopjs-parse-consume-token-any)))))

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
     (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
       (hopjs-debug 0 "hopjs-indent-old-qmark etok=%s" etok)
       (if (and (eq (hopjs-parse-token-type etok) 'ident)
		(memq (hopjs-parse-peek-token-type) '(let var const)))
	   (hopjs-indent-column-token (hopjs-parse-peek-token) hopjs-indent-level)
	 (hopjs-indent-column-token etok hopjs-indent-level))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-=> ...                                          */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-=> (tok)
  (with-debug
   "hopjs-indent-new-=> %s [%s]" tok (hopjs-parse-token-string tok)
   (let ((tok (hopjs-parse-consume-token-any)))
     (hopjs-debug 0 "hopjs-indent-new-=> %s [%s]" tok
		  (hopjs-parse-token-string tok))
     (case (hopjs-parse-token-type tok)
       ((ident)
	(hopjs-indent-function-body
	 tok (hopjs-parse-consume-token-any) hopjs-indent-level))
       ((rparen)
	(let ((atok (hopjs-parse-args tok)))
	  (hopjs-debug 0 "hopjs-indent-new-=> args=%s [%s]" atok
		       (hopjs-parse-token-string atok))
	  (if atok
	      (let ((ntok (hopjs-parse-consume-token-any)))
		(hopjs-indent-function-body atok ntok hopjs-indent-level))
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
       (hopjs-parse-start (+ (point) 1))
       (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.2 -> %s c=%c c-1=%c peek=%s"
		    (point) (char-after (point)) (char-after (- (point) 1))
		    (hopjs-parse-peek-token))
       (cond
	((memq (hopjs-parse-peek-token-type) '(tilde dollar))
	 (let ((tok (hopjs-parse-consume-token-any)))
	   (cond
	    ((eq (hopjs-parse-peek-token-type) '=)
	     (hopjs-parse-consume-token-any)
	     (hopjs-indent-new-idents 0 t))
	    (t
	     (hopjs-indent-column-token tok 0)))))
	(t
	 (hopjs-parse-start (+ (point) 1))
	 (hopjs-debug 0 "hopjs-indent-new-rbrace sexp.4 %s"
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
    ((hopjs-parse-expr tok nil)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-lparen etok=%s [%s] peek=%s" etok
		      (hopjs-parse-token-string etok)
		      (hopjs-parse-peek-token))
	 (let ((indent-level (if (hopjs-parse-same-linep etok tok)
				 hopjs-indent-level
			       (* 2 hopjs-indent-level))))
	   (cond
	    ((memq (hopjs-parse-peek-token-type)
		   '(return throw var let const ident))
	     (if (hopjs-parse-same-linep (hopjs-parse-peek-token) etok)
		 (hopjs-indent-column-token
		  (hopjs-parse-peek-token) indent-level)
	       (hopjs-indent-column-token etok indent-level)))
	    ((eq (hopjs-parse-peek-token-type) '=)
	     (if (hopjs-parse-same-linep (hopjs-parse-peek-token) etok)
		 (hopjs-indent-new-token (hopjs-parse-consume-token-any))
	       (hopjs-indent-column-token etok indent-level)))
	    (etok
	     (hopjs-indent-column-token etok indent-level))
	    (t
	     0)))))
    (t
     0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-lbracket ...                                    */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-lbracket (tok)
  (with-debug
   "hopjs-indent-new-lbracket...%s" tok
   (mcond
    ((hopjs-parse-expr tok t)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-lbracket.1 etok=%s" etok)
	 (cond
	  ((eq (hopjs-parse-token-type etok) 'new)
	   (hopjs-indent-column-token etok hopjs-indent-level))
	  ((memq (hopjs-parse-peek-token-type)
		 '(return throw var let const ident))
	   (hopjs-indent-column-token
	    (hopjs-parse-peek-token) hopjs-indent-level))
	  (etok
	   (hopjs-indent-column-token etok hopjs-indent-level))
	  (t
	   0))))
    ((eq (hopjs-parse-peek-token-type) 'colon)
     (hopjs-debug 0 "hopjs-indent-new-lbracket.2 ptok=%s" 
		  (hopjs-parse-peek-token-type))
     (let ((tok (hopjs-parse-consume-token-any)))
       (if (memq (hopjs-parse-peek-token-type) '(ident string regexp number))
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
	((memq (char-after (- (point) 2)) '(?\[))
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
    ((hopjs-parse-expr (hopjs-parse-push-token tok) t)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-rparen.expr.etok=%s peek=%s"
		      etok
		      (hopjs-parse-peek-token))
	 (case (hopjs-parse-peek-token-type)
	   ((=)
	    (if (hopjs-parse-expr (hopjs-parse-consume-token-any) t)
		(progn
		  (hopjs-debug 0 "hopjs-indent-new-rparen.expr..=.%s"
			       (hopjs-parse-peek-token-type))
		  (case (hopjs-parse-peek-token-type)
		    ((var return throw)
		     (save-excursion
		       (hopjs-parse-goto-token (hopjs-parse-peek-token))
		       (back-to-indentation)
		       (current-column)))
		    (t
		     (hopjs-indent-column-token
		      (hopjs-parse-consume-token-any) 0))))
	      (hopjs-indent-column-token etok 0)))
	   ((rparen)
	    (hopjs-debug 0 "hopjs-indent-new-rparen.expr.rparen %s"
			 (hopjs-parse-peek-token))
	    (hopjs-indent-new-rparen (hopjs-parse-consume-token-any)))
	   ((return throw if let const var)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any) hopjs-indent-level))
	   ((qmark)
	    (hopjs-indent-column-token (hopjs-parse-peek-token) 0))
	   ((while)
	    (hopjs-parse-consume-token-any)
	    (hopjs-debug 0 "hopjs-indent-new-rparen..expr.while %s"
			 (hopjs-parse-peek-token))
	    (if (eq (hopjs-parse-peek-token-type) 'rbrace)
		;; might be a do/while loop
		(let ((pos (progn
			     (goto-char (hopjs-parse-token-end (hopjs-parse-peek-token)))
			     (hopjs-parse-backward-sexp))))
		  (goto-char pos)
		  (hopjs-parse-start (1- (point)))
		  (hopjs-debug 0 "hopjs-indent-new-rparen..expr.while.2 %s"
			     (hopjs-parse-peek-token))
		  (if (eq (hopjs-parse-peek-token-type) 'do)
		      (hopjs-indent-column-token (hopjs-parse-peek-token) 0)
		    (hopjs-indent-column-token etok hopjs-indent-level)))
	      (hopjs-indent-column-token etok hopjs-indent-level)))
	   (t 
	    (hopjs-indent-column-token etok hopjs-indent-level)))))
    (t
     0))))
   
;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-comma ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-comma (tok)
  (with-debug
   "hopjs-indent-new-comma...%s peek=%s" tok
   (hopjs-parse-peek-token)
   (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
     (if etok
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
			(funcall loop (hopjs-parse-expr (hopjs-parse-peek-token) t)))
		    (hopjs-indent-column-token etok 0)))
		 ((var let const)
		  (hopjs-indent-column-token
		   (hopjs-parse-peek-token) hopjs-indent-level))
		 ((lparen)
		  (hopjs-indent-new-token (hopjs-parse-consume-token-any)))
		 ((colon)
		  (hopjs-parse-consume-token-any)
		  (let ((tok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
		    (if tok
			(hopjs-indent-column-token tok 0)
		      (hopjs-indent-column-token etok 0))))
		 (t
		  (hopjs-indent-column-token etok 0))))
       (hopjs-indent-current-line-column)))))

(defun hopjs-indent-new-comma-old (tok)
  (with-debug
   "hopjs-indent-new-comma...%s peek=%s" tok
   (hopjs-parse-peek-token)
   (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
     (if etok
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
			(funcall loop (hopjs-parse-expr (hopjs-parse-peek-token) t)))
		    (hopjs-indent-column-token etok 0)))
		 ((lparen)
		  (hopjs-indent-new-token (hopjs-parse-consume-token-any)))
		 ((colon)
		  (let ((tok (hopjs-parse-consume-token-any)))
		    (hopjs-debug 0 "hopjs-indent-new-comma expr.colon tok=%s peek=%s"
				 tok
				 (hopjs-parse-peek-token))
		    (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
		      (hopjs-debug 0 "hopjs-indent-new-comma expr.colon.expr etok=%s peek=%s" etok (hopjs-parse-peek-token))
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
		 ((rparen)
		  (if (hopjs-parse-same-linep etok tok)
		      (hopjs-indent-column-token etok 0)
		    (hopjs-indent-column-token etok hopjs-indent-level)))
		 (t
		  (goto-char (hopjs-parse-token-beginning tok))
		  (hopjs-indent-current-line-column))))
       (hopjs-indent-current-line-column)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-semicolon ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-semicolon (tok)
  (with-debug
   "hopjs-indent-new-semicolon...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-peek-token) t)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-semicolon...expr etok=%s ptok=%s [%s]"
		      etok
		      (hopjs-parse-peek-token)
		      (hopjs-parse-token-string (hopjs-parse-peek-token)))
	 (case (hopjs-parse-peek-token-type)
	   ((=)
	    (let ((lhstok (hopjs-parse-expr (hopjs-parse-consume-and-peek-token) t)))
	      (hopjs-debug 0 "hopjs-indent-new-semicolon...lhs=%s" lhstok)
	      (if lhstok
		  (progn
		    (hopjs-debug 0 "hopjs-indent-new-semicolon...lhstok %s" 
				 (hopjs-parse-peek-token))
		    (case (hopjs-parse-peek-token-type)
		      ((var let const return throw)
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
			   "hopjs-indent-new-semicolon.var=%s first=%s b=%s"
			   tok (hopjs-indent-first-on-linep b) b)
  	      (cond
	       ((hopjs-indent-first-on-linep b)
		(hopjs-indent-column-token tok 0))
	       ((eq (hopjs-parse-peek-token-type) 'export)
		(hopjs-indent-column-token (hopjs-parse-peek-token-type) 0))
	       (t
		(hopjs-indent-new (hopjs-parse-token-beginning tok))))))
	   ((return throw new await)
	    (let ((rtok (hopjs-parse-consume-token-any)))
	      (hopjs-debug 0
			   "hopjs-indent-new-semicolon.new rtok=%s [%s]"
			   rtok (hopjs-parse-token-string rtok))
	      (cond
	       ((hopjs-parse-same-linep (hopjs-parse-peek-token) rtok)
		(save-excursion
		  (goto-char (hopjs-parse-token-beginning rtok))
		  (hopjs-indent-current-line-column)))
	       ((eq (hopjs-parse-peek-token-type) 'rparen)
		(goto-char (hopjs-parse-token-beginning (hopjs-parse-peek-token)))
		(hopjs-indent-current-line-column))
	       (t
		(hopjs-indent-column-token rtok 0)))))
	   ((ident)
	    (hopjs-indent-new-idents 0))
	   ((dollar)
	    (let ((tok (hopjs-parse-consume-token-any)))
	      (if (memq (hopjs-parse-peek-token-type)
			'(return throw let var const ident))
		  (let ((tok (hopjs-parse-peek-token-type)))
		    (if (eq (hopjs-parse-peek-token-type) 'export)
			(hopjs-indent-column-token (hopjs-parse-peek-token) 0)
		      (hopjs-indent-column-token tok 0)))
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
		      (hopjs-indent-new pos)
		    (hopjs-indent-column-token etok 0)))))
	   ((colon)
	    (if (hopjs-parse-same-linep (hopjs-parse-peek-token) etok)
		(hopjs-indent-new-idents 0)
	      (hopjs-indent-column-token etok 0)))
	   ((from)
	    (hopjs-indent-current-line-column))
	   (t
	    (hopjs-indent-column-token etok 0)))))
    (t
     (hopjs-indent-current-line-column)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-colon ...                                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-colon (tok)
  (with-debug
   "hopjs-indent-new-colon...%s" tok
   (mcond
    ((hopjs-parse-expr (hopjs-parse-peek-token) t)
     =>
     #'(lambda (etok)
	 (hopjs-debug 0 "hopjs-indent-new-colon etok=%s [%s] peek=%s"
		      etok (hopjs-parse-token-string etok)
		      (hopjs-parse-peek-token))
	 (case (hopjs-parse-peek-token-type)
	   ((case lbrace)
	    (hopjs-indent-column-token
	     (hopjs-parse-consume-token-any)
	     hopjs-indent-level))
	   
	   ((comma)
	    (if (hopjs-indent-first-on-linep etok)
		(hopjs-indent-column-token etok hopjs-indent-level)
	      (hopjs-indent-column-token tok hopjs-indent-level)))
	   (t
	    (hopjs-indent-column-token tok hopjs-indent-level)))))
    ((eq (hopjs-parse-peek-token-type) 'default)
     (hopjs-indent-column-token (hopjs-parse-peek-token) hopjs-indent-level))
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
     (let ((r '()))
       (while (not r)
	 (let ((tok (hopjs-parse-consume-token-any)))
	   (hopjs-debug 0 "hopjs-indent-old-case peek=%s" tok)
	   (case (hopjs-parse-token-type tok)
	     ((rbrace)
	      (goto-char (hopjs-parse-token-end tok))
	      (hopjs-parse-start (1- (hopjs-parse-backward-sexp)))
	      (if (eq (hopjs-parse-peek-token-type) 'rparen)
		  (let ((tok (hopjs-parse-consume-token-any)))
		    (goto-char (hopjs-parse-token-end tok))
		    (hopjs-parse-start (1- (hopjs-parse-backward-sexp)))
		    (message "glop %s" (hopjs-parse-peek-token))
		    (when (eq (hopjs-parse-peek-token-type) 'switch)
		      (message "ICI...")
		      (hopjs-parse-consume-token-any)))))
	     ((case)
	      (setq r (hopjs-indent-column-token tok 0)))
	     ((switch)
	      (setq r (hopjs-indent-column-token tok hopjs-indent-level)))
	     ((bof)
	      (setq r 0)))))
       r))))

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
	    (let ((tok (hopjs-parse-consume-token-any)))
	      (if (hopjs-parse-expr (hopjs-parse-peek-token) t)
		  (hopjs-indent-column-token
		   (hopjs-parse-consume-token-any)
		   hopjs-indent-level-html)
		tok)))
	   ((ident text)
	    (hopjs-parse-consume-token-any)
	    (funcall loop))
	   ((return throw)
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
   "hopjs-indent-old-otag pos=%s" pos
   (save-excursion
     (forward-line -1)
     (end-of-line)
     (letn loop ()
	   (hopjs-parse-start (point))
	   (let ((tok (hopjs-parse-peek-token)))
	     (hopjs-debug 0 "hopjs-indent-old-tag tok=%s [%s]"
			  tok (hopjs-parse-token-string tok))
	     (cond
	      ((memq (hopjs-parse-token-type tok) '(ident text dots))
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
			  (hopjs-indent-column-token tok hopjs-indent-level-html))))))
	      ((eq (hopjs-parse-token-type tok) 'ecomment)
	       (goto-char (hopjs-parse-token-end tok))
	       (backward-sexp 1)
	       (funcall loop))
	      ((eq (hopjs-parse-token-type tok) 'html)
	       (hopjs-indent-column-token tok 0))
	      (t
	       (progn
		 (next-line 1)
		 (beginning-of-line)
		 (hopjs-indent-new (point))))))))))
     
;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-etag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-etag (tok)
  (with-debug
   "hopjs-indent-new-etag %s" pos
   (let ((res 'loop))
     (while (eq res 'loop)
       (hopjs-debug 0 "hopjs-indent-new-etag peek=%s [%s]"
		    (hopjs-parse-peek-token)
		    (hopjs-parse-token-string (hopjs-parse-peek-token)))
       (let ((tok (hopjs-parse-consume-token-any)))
	 (case (hopjs-parse-token-type tok)
	   ((otag ohtml)
	    (setq res (hopjs-indent-new-otag tok)))
	   ((number string regexp)
	    (unless (hopjs-indent-new-literal tok)
	      (setq res '())))
	   ((ident)
	    'loop)
	   ((rbrace)
	    (if (hopjs-indent-new-rbrace tok)
		(hopjs-debug 0 "hopjs-indent-new-etag rbrace -> point=%s %s"
			     (point) (hopjs-parse-peek-token))
	      (setq res '())))
	   (t
	    (setq res '())))))
     res)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-new-ctag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-new-ctag (tok)
  (with-debug
   "hopjs-indent-new-ctag...%s %s" tok (hopjs-parse-token-string tok)
   (let* ((col (hopjs-indent-column-token tok 0))
	  (otag (hopjs-find-opening-tag (- (hopjs-parse-token-end tok) 1) 0)))
     (if otag
	 (let ((otok (hopjs-parse-peek-token)))
	   (hopjs-debug 0 "hopjs-indent-new-ctag...find-opening=[%s] peek=%s"
			(hopjs-parse-token-string otag)
			otok)
	   (if (memq (hopjs-parse-token-type otok) '(return throw var))
	       (hopjs-indent-column-token otok 0)
	     (progn
	       (goto-char (hopjs-parse-token-beginning otag))
	       (hopjs-debug 0 "hopjs-indent-new-ctag closing cur=%s col=%s"
			    (current-column) col)
	       (if (> (current-column) col) col (current-column)))))
       0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-old-ctag ...                                        */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-old-ctag (pos)
  (with-debug
   "hopjs-indent-old-ctag...%s" pos
   (let ((otag (hopjs-find-opening-tag pos 0)))
     (hopjs-debug 0 "hopjs-indent-old-ctag otag=%s [%s]" otag
		  (if otag (hopjs-parse-token-string otag)))
     (if otag
	 (let ((b (hopjs-parse-token-beginning otag))
	       (e (hopjs-parse-token-end otag)))
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
	    ((memq (hopjs-parse-token-type next) '(ident cssident text dot))
	     (hopjs-parse-consume-token-any)
	     (funcall loop next))
	    ((memq (hopjs-parse-token-type next) '(return throw var let const))
	     (hopjs-indent-column-token next level))
	    ((and (not strict) (eq (hopjs-parse-token-type next) 'rbrace))
	     (hopjs-indent-column-token next level))
	    ((eq (hopjs-parse-token-type next) 'colon)
	     (hopjs-parse-consume-token-any)
	     (hopjs-indent-new-idents 0))
	    (t
	     (hopjs-indent-column-token tok level)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-function-body ...                                   */
;*    -------------------------------------------------------------    */
;*    This function is invoked after the last token composing the      */
;*    argument list has been parsed. TOK is then the next (left-most)  */
;*    token preceeding the function declaration.                       */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-function-body (ftok tok level)
  (with-debug
   "hopjs-indent-function-body tok=%s [%s] ftok=%s [%s]"
   tok (hopjs-parse-token-string tok)
   ftok (hopjs-parse-token-string ftok)
   (cond
    ((not (hopjs-parse-same-linep tok ftok))
     (hopjs-debug 0 "hopjs-indent-function-body not same line")
     (hopjs-indent-column-token ftok level))
    ((eq (hopjs-parse-token-type tok) 'comma)
     (hopjs-debug 0 "hopjs-indent-function-body comma")
     (hopjs-indent-first-char-column tok hopjs-indent-level))
    ((not (hopjs-parse-same-linep (hopjs-parse-peek-token) tok))
     (hopjs-indent-column-token tok level))
    ((memq (hopjs-parse-token-type tok) '(= colon))
     (hopjs-debug 0 "hopjs-indent-function-body =|colon=%s %s"
		  (hopjs-parse-peek-token)
		  (hopjs-parse-peek-token-string))
     (let ((etok (hopjs-parse-expr (hopjs-parse-peek-token) t)))
       (if etok
	   (if (memq (hopjs-parse-peek-token-type) '(var let const))
	       (hopjs-indent-column-token (hopjs-parse-peek-token) level)
	       (hopjs-indent-column-token etok level))
	 (hopjs-indent-column-token tok level))))
    ((eq (hopjs-parse-token-type tok) 'lparen)
     (hopjs-debug 0 "hopjs-indent-function-body strictness=%s lparen=%s peek=%s"
		  hopjs-indent-function-body-strictness-level
		  (hopjs-parse-peek-token)
		  (hopjs-parse-peek-token-string))
     (case hopjs-indent-function-body-strictness-level
       ((0) (hopjs-indent-column-token ftok level))
       ((1) (hopjs-indent-function-expression tok (* 2 level)))
       (t (hopjs-indent-function-expression tok level))))
    (t
     (hopjs-debug 0 "hopjs-indent-function-body peek=%s %s"
		  (hopjs-parse-peek-token)
		  (hopjs-parse-peek-token-string))
     (case (hopjs-parse-peek-token-type)
       ((lparen)
	(hopjs-indent-function-expression
	 (hopjs-parse-consume-token-any) level))
       (t
	(hopjs-indent-column-token tok level)))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-function-expression ...                             */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-function-expression (etok level)
  (with-debug
   "hopjs-indent-function-expression etok=%s [%s] -> peek=%s"
   etok
   (hopjs-parse-token-string etok)
   (hopjs-parse-peek-token)
   (letn loop ((etok (hopjs-parse-peek-token)))
	 (case (hopjs-parse-peek-token-type)
	   ((ident)
	    (hopjs-debug 0 "hopjs-indent-function-expression.ident")
	    (funcall loop (hopjs-parse-expr etok nil)))
	   ((return throw var let const)
	    (hopjs-debug 0 "hopjs-indent-function-expression.return")
	    (hopjs-indent-column-token etok level))
	   ((=)
	    (hopjs-debug 0 "hopjs-indent-function-expression.=")
	    (let ((tok (hopjs-parse-expr (hopjs-parse-consume-token-any) t)))
	      (if tok
		  (funcall loop tok)
		(hopjs-indent-column-token tok level))))
	   (t
	    (hopjs-debug 0 "hopjs-indent-function-expression.else")
	    (hopjs-indent-column-token
	     (or etok tok)
	     (* hopjs-indent-function-body-alignment level)))))))

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
  (when (hopjs-find-opening-tag (- (hopjs-parse-token-end tok) 1) 0)
    (cons (match-beginning 0) (match-end 0))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-first-on-linep ...                                  */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-first-on-linep (pos-or-tok)
  (save-excursion
    (let ((pos (if (numberp pos-or-tok)
		   pos-or-tok
		 (hopjs-parse-token-beginning pos-or-tok))))
      (goto-char pos)
      (beginning-of-line)
      (when (looking-at "[ \t]*[^ \t]")
	(= (match-end 0) (+ pos 1))))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-last-on-linep ...                                   */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-last-on-linep (pos-or-tok)
  (save-excursion
    (let ((pos (if (numberp pos-or-tok)
		   pos-or-tok
		 (hopjs-parse-token-end pos-or-tok))))
      (goto-char pos)
      (looking-at "[ \t]*$"))))

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
	  (while (eq loop t)
	    (beginning-of-line)
	    (condition-case nil
		(indent-for-tab-command)
	      (error
	       (message "indentation error line %s... stopping" line)
	       (setq loop 'error)))
	    (cond
	     ((buffer-modified-p)
	      (setq loop nil))
	     ((> (forward-line 1) 0)
	      (setq loop nil))
	     (t
	      (setq line (+ line 1)))))
	  (if (or (eq loop 'error) (buffer-modified-p))
	      (message "Tests failed line: %s" line)
	    (progn
	      (message "All tests passed")
	      (goto-char pos))))
      (when mod
	(set-buffer-modified-p t)))))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-first-char-column ...                               */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-first-char-column (tok level)
  (goto-char (hopjs-parse-token-beginning tok))
  (beginning-of-line)
  (skip-chars-forward " \t")
  (+ (current-column) level))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-current-line-column ...                             */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-current-line-column ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

;*---------------------------------------------------------------------*/
;*    hopjs-indent-previous-line-column ...                            */
;*---------------------------------------------------------------------*/
(defun hopjs-indent-previous-line-column ()
  (save-excursion
    (forward-line -1)
    (hopjs-indent-current-line-column)))

