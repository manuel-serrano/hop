;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/js-lib.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 15:55:02 2005                          */
;*    Last change :  Tue Jun 12 09:35:12 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple JS lib                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_js-lib

   (import  __hop_param
	    __hop_types
	    __hop_xml)

   (export  (generic hop->json ::obj)
	    (json->hop ::input-port)))

;*---------------------------------------------------------------------*/
;*    list->arguments ...                                              */
;*---------------------------------------------------------------------*/
(define (list->arguments lst)
   (let loop ((lst lst)
	      (res '()))
      (if (null? lst)
	  (apply string-append (reverse! res))
	  (loop (cdr lst)
		(cons* (if (pair? (cdr lst)) "," ")")
		       (hop->json (car lst))
		       res)))))

;*---------------------------------------------------------------------*/
;*    list->array ...                                                  */
;*---------------------------------------------------------------------*/
(define (list->array lst)
   (string-append "new Array(" (list->arguments lst)))

;*---------------------------------------------------------------------*/
;*    vector->json ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector->json vec)
   (let ((len (vector-length vec)))
      (case len
	 ((0)
	  "[]")
	 ((1)
	  (string-append "[" (hop->json (vector-ref vec 0)) "]"))
	 (else
	  (let loop ((i (-fx len 2))
		     (strs (list (hop->json (vector-ref vec (-fx len 1)))
				 "]")))
	     (if (=fx i -1)
		 (apply string-append "[" strs)
		 (loop (-fx i 1)
		       (cons* (hop->json (vector-ref vec i)) ", " strs))))))))
	 
;*---------------------------------------------------------------------*/
;*    hop->json ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (hop->json obj)
   (cond
      ((string? obj)
       (string-append "\"" (string-for-read obj) "\""))
      ((number? obj)
       (number->string obj))
      ((symbol? obj)
       (string-append "'" (symbol->string obj)  "'"))
      ((eq? obj #t)
       "true")
      ((eq? obj #f)
       "false")
      ((null? obj)
       "null")
      ((pair? obj)
       (let ((car (hop->json (car obj)))
	     (cdr (hop->json (cdr obj))))
	  (format "new sc_Pair( ~a, ~a )" car cdr)))
      ((vector? obj)
       (vector->json obj))
      ((eq? obj #unspecified)
       "undefined")
      ((procedure? obj)
       (error 'hop->json
	      "Illegal procedure in JavaScript conversion"
	      obj))
      ((date? obj)
       (format "new Date( ~a000 )" (date->seconds obj)))
      (else
       (error 'javascript "Illegal Javascript value" obj))))

;*---------------------------------------------------------------------*/
;*    hop->json ::object ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hop->json obj::object)
   (define (list->block lst)
      (let loop ((lst lst)
		 (res '()))
	 (if (null? lst)
	     (apply string-append (reverse! res))
	     (loop (cdr lst)
		   (cons* (if (pair? (cdr lst)) "," ")")
			  (if (symbol? (car lst))
			      (symbol->string (car lst))
			      (car lst))
			  res)))))
   (let* ((klass (object-class obj))
	  (kname (symbol->string! (class-name klass)))
	  (name (if (bigloo-need-mangling? kname)
		    (bigloo-mangle kname)
		    kname))
	  (fields (class-all-fields klass)))
      (format "function ~a( ~a { ~a }; new ~a( ~a"
	      name
	      (list->block (map class-field-name fields))
	      (apply string-append
		     (map (lambda (f)
			     (let ((n (class-field-name f)))
				(format "this.~a = ~a;" n n)))
			  fields))
	      name
	      (list->arguments (map (lambda (f) ((class-field-accessor f) obj))
				    fields)))))
   
;*---------------------------------------------------------------------*/
;*    hop->json ::xml ...                                              */
;*---------------------------------------------------------------------*/
(define-method (hop->json obj::xml)
   (error 'hop->json "Cannot translate xml element" xml))

;*---------------------------------------------------------------------*/
;*    hop->json ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hop->json obj::xml-element)
   (format "document.getElementById( \"~a\" )" (xml-element-id obj)))

;*---------------------------------------------------------------------*/
;*    hop->json ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (hop->json obj::hop-service)
   (hop-service-javascript obj))

;*---------------------------------------------------------------------*/
;*    *json-lexer* ...                                                 */
;*---------------------------------------------------------------------*/
(define *json-lexer*
   
   (regular-grammar ()
      
      ;; blank
      ((+ (in #\space #\newline #\tab #a012))
       (ignore))
      
      ;; comment
      ((or (: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
	   (: "//" (* all)))
       (ignore))
      
      ;; commas
      (#\.
       (list 'DOT))
      (#\,
       (list 'COMMA))
      (#\;
       (list 'SEMI-COMMA))
      (#\=
       (list '=))
      
      ;; angles
      (#\[
       (list 'ANGLE-OPEN))
      (#\]
       (list 'ANGLE-CLO))
      
      ;; parenthesis
      (#\(
       (list 'PAR-OPEN))
      (#\)
       (list 'PAR-CLO))
      
      ;; brackets
      (#\{
       (list 'BRA-OPEN))
      (#\}
       (list 'BRA-CLO))
      
      ;; integer constant
      ((: (+ digit))
       (list 'CONSTANT (the-fixnum)))
      
      ;; floating-point constant
      ((or (: (+ digit)
	      (: (in #\e #\E) (? (in #\- #\+)) (+ digit))
	      (? (in #\f #\F #\l #\L)))
	   (: (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
	      (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
	      (? (in #\f #\F #\l #\L))))
       (list 'CONSTANT (the-flonum)))
      
      ;; symbols constant
      ((: #\' (+ all) #\')
       (list 'CONSTANT (the-symbol)))
      
      ;; string constant
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-substring 0 (-fx (the-length) 1))))
	  (list 'CONSTANT (escape-C-string str))))
      
      ;; identifier
      ((: (or #\_ alpha) (* (or #\_ alpha digit)))
       (case (the-symbol)
	  ((null) (list 'CONSTANT '()))
	  ((undefined) (list 'CONSTANT #unspecified))
	  ((true) (list 'CONSTANT #t))
	  ((false) (list 'CONSTANT #f))
	  ((new) (list 'NEW))
	  ((function) (list 'FUNCTION))
	  ((return) (list 'RETURN))
	  ((sc_Pair) (list 'CONS))
	  ((Date) (list 'DATE))
	  (else (list 'IDENTIFIER (the-symbol)))))
      
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (list 'ERROR c))))))

;*---------------------------------------------------------------------*/
;*    *json-parser* ...                                                */
;*---------------------------------------------------------------------*/
(define *json-parser*
   
   (lalr-grammar
      
      ;; tokens
      (CONSTANT PAR-OPEN PAR-CLO BRA-OPEN BRA-CLO ANGLE-OPEN ANGLE-CLO
       COMMA SEMI-COMMA DOT =
       IDENTIFIER ERROR NEW CONS DATE FUNCTION RETURN)
      
      ;; initial rule
      (start
       (() '())
       ((expression) expression))
      
      ;; expression
      (expression
       ((CONSTANT)
	(car CONSTANT))
       ((NEW CONS PAR-OPEN expression@a COMMA expression@d PAR-CLO)
	(cons a d))
       ((NEW DATE PAR-OPEN CONSTANT PAR-CLO)
	(let ((sec (car CONSTANT)))
	   (if (integer? sec)
	       (seconds->date (/fx sec 1000))
	       (error 'json->hop "Illegal `date' construction" sec))))
       ((ANGLE-OPEN ANGLE-CLO)
	'#())
       ((ANGLE-OPEN expression ANGLE-CLO)
	(vector expression))
       ((ANGLE-OPEN array-elements ANGLE-CLO)
	(list->vector array-elements))
       ((object)
	object)
       ((get-element-by-id)
	get-element-by-id)
       ((service)
	service))
      
      ;; array
      (array-elements
       ((COMMA expression)
	expression)
       ((expression array-elements)
	(cons expression array-elements)))

      ;; object
      (object
       ((FUNCTION IDENTIFIER PAR-OPEN arguments PAR-CLO
		  BRA-OPEN sets BRA-CLO SEMI-COMMA
		  NEW IDENTIFIER@klass PAR-OPEN vals PAR-CLO)
	(let ((c (find-class klass)))
	   (if (not (class? c))
	       (error 'json->hop "Can't find class" c)
	       (let* ((constr (class-constructor c))
		      (create (class-creator c))
		      (ins (apply constr vals)))
		  (when (procedure? create) (create ins))
		  ins)))))

      (arguments
       (()
	'())
       ((IDENTIFIER)
	(list (car IDENTIFIER)))
       ((IDENTIFIER COMMA arguments)
	(cons (car IDENTIFIER) arguments)))

      (sets
       (()
	'())
       ((set SEMI-COMMA sets)
	(cons set sets)))

      (set
       ((IDENTIFIER DOT IDENTIFIER = expression)
	'_))

      (vals
       (()
	'())
       ((expression COMMA vals)
	(cons expression vals)))

      ;; get-element-by-id
      (get-element-by-id
       ((IDENTIFIER@obj DOT IDENTIFIER@field PAR-OPEN CONSTANT PAR-CLO)
	;; we don't have a document at hand, so this is getElementById
	;; is meaningless
	#f))

      ;; service
      (service
       ((FUNCTION PAR-OPEN PAR-CLO BRA-OPEN RETURN IDENTIFIER
		  PAR-OPEN arguments PAR-CLO BRA-CLO)
	(error 'json->hop "Service cannot be transmitted" IDENTIFIER)))))

;*---------------------------------------------------------------------*/
;*    json->hop ...                                                    */
;*---------------------------------------------------------------------*/
(define (json->hop ip)
   (read/lalrp *json-parser* *json-lexer* ip))
