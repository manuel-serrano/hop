;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/tools/ident.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 27 07:21:08 2024                          */
;*    Last change :  Thu Mar 28 18:19:24 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Ident mangling (shared by hop2js and mkjsast).                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    ident-table ...                                                  */
;*---------------------------------------------------------------------*/
(define var-ident-table (create-hashtable :weak 'open-string))
(define prop-ident-table (create-hashtable :weak 'open-string))

(hashtable-put! var-ident-table "arguments" "$$arguments")
(hashtable-put! var-ident-table "await" "$$await")
(hashtable-put! var-ident-table "break" "$$break")
(hashtable-put! var-ident-table "catch" "$$catch")
(hashtable-put! var-ident-table "continue" "$$continue")
(hashtable-put! var-ident-table "do" "$$do")
(hashtable-put! var-ident-table "export" "$$export")
(hashtable-put! var-ident-table "else" "$$else")
(hashtable-put! var-ident-table "eval" "$$eval")
(hashtable-put! var-ident-table "extends" "$$extends")
(hashtable-put! var-ident-table "for" "$$for")
(hashtable-put! var-ident-table "finally" "$$finally")
(hashtable-put! var-ident-table "function" "$$function")
(hashtable-put! var-ident-table "import" "$$import")
(hashtable-put! var-ident-table "interface" "$$interface")
(hashtable-put! var-ident-table "method" "$$method")
(hashtable-put! var-ident-table "return" "$$return")
(hashtable-put! var-ident-table "private" "$$private")
(hashtable-put! var-ident-table "static" "$$static")
(hashtable-put! var-ident-table "super" "$$super")
(hashtable-put! var-ident-table "switch" "$$switch")
(hashtable-put! var-ident-table "this" "$$this")
(hashtable-put! var-ident-table "throw" "$$throw")
(hashtable-put! var-ident-table "var" "$$var")
(hashtable-put! var-ident-table "with" "$$with")
(hashtable-put! var-ident-table "while" "$$while")

;*---------------------------------------------------------------------*/
;*    caml-case ...                                                    */
;*---------------------------------------------------------------------*/
(define (caml-case s)
   (let* ((s0 (if (string-index s #\!) (string-replace s #\! #\$) s))
	  (s1 (if (string-index s0 #\?) (string-replace s0 #\? #\p) s0))
	  (s2 (if (string-index s1 #\*) (string-replace s1 #\* #\_) s1))
	  (s3 (if (string-index s2 #\/) (string-replace s2 #\/ #\_) s2))
	  (s4 (if (string-index s3 #\&) (string-replace s3 #\& #\$) s3))
	  (s5 (if (string-index s4 #\=) (string-replace s4 #\= #\$) s4))
	  (s6 (if (string-index s5 #\%) (string-replace s5 #\% #\$) s5))
	  (s7 (pregexp-replace* "->" s6 "to")))
      (if (string-index s7 #\-)
	  (let ((l (string-split s7 "-")))
	     (apply string-append (car l)
		(map string-capitalize (cdr l))))
	  s7)))

;*---------------------------------------------------------------------*/
;*    var-ident ...                                                    */
;*---------------------------------------------------------------------*/
(define (var-ident::bstring ident)
   
   (define (need-mangling? name)
      (string-index name "!?*/&=%-"))
   
   (let* ((name (if (string? ident) ident (symbol->string! ident)))
	  (old (hashtable-get var-ident-table name)))
      (or old
	  (if (need-mangling? name)
	      (let ((mangled (caml-case name)))
		 (hashtable-put! var-ident-table name mangled)
		 mangled)
	      name))))

;*---------------------------------------------------------------------*/
;*    prop-ident ...                                                   */
;*---------------------------------------------------------------------*/
(define (prop-ident::bstring ident)
   
   (define (need-mangling? name)
      (string-index name "!?*/&=%-"))
   
   (let* ((name (if (string? ident) ident (symbol->string! ident)))
	  (old (hashtable-get prop-ident-table name)))
      (or old
	  (if (need-mangling? name)
	      (let ((mangled (caml-case name)))
		 (hashtable-put! prop-ident-table name mangled)
		 mangled)
	      name))))


