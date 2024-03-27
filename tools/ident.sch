;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/tools/ident.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 27 07:21:08 2024                          */
;*    Last change :  Wed Mar 27 09:24:03 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Ident mangling (shared by hop2js and mkjsast).                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    ident-table ...                                                  */
;*---------------------------------------------------------------------*/
(define ident-table (create-hashtable :weak 'open-string))

(hashtable-put! ident-table "arguments" "$$arguments")
(hashtable-put! ident-table "await" "$$await")
(hashtable-put! ident-table "break" "$$break")
(hashtable-put! ident-table "catch" "$$catch")
(hashtable-put! ident-table "continue" "$$continue")
(hashtable-put! ident-table "do" "$$do")
(hashtable-put! ident-table "export" "$$export")
(hashtable-put! ident-table "else" "$$else")
(hashtable-put! ident-table "eval" "$$eval")
(hashtable-put! ident-table "extends" "$$extends")
(hashtable-put! ident-table "for" "$$for")
(hashtable-put! ident-table "finally" "$$finally")
(hashtable-put! ident-table "function" "$$function")
(hashtable-put! ident-table "import" "$$import")
(hashtable-put! ident-table "interface" "$$interface")
(hashtable-put! ident-table "method" "$$method")
(hashtable-put! ident-table "return" "$$return")
(hashtable-put! ident-table "private" "$$private")
(hashtable-put! ident-table "static" "$$static")
(hashtable-put! ident-table "super" "$$super")
(hashtable-put! ident-table "switch" "$$switch")
(hashtable-put! ident-table "this" "$$this")
(hashtable-put! ident-table "throw" "$$throw")
(hashtable-put! ident-table "var" "$$var")
(hashtable-put! ident-table "with" "$$with")
(hashtable-put! ident-table "while" "$$while")

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
;*    ident ...                                                        */
;*---------------------------------------------------------------------*/
(define (ident::bstring ident)
   
   (define (need-mangling? name)
      (string-index name "!?*/&=%-"))
   
   (let* ((name (if (string? ident) ident (symbol->string! ident)))
	  (old (hashtable-get ident-table name)))
      (or old
	  (if (need-mangling? name)
	      (let ((mangled (caml-case name)))
		 (hashtable-put! ident-table name mangled)
		 mangled)
	      name))))


