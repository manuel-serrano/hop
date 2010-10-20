;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/js_lib.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 15:55:02 2005                          */
;*    Last change :  Wed Oct 20 09:30:31 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple JS lib                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_js-lib

   (library web)
	    
   (import  __hop_param
	    __hop_types
	    __hop_hop-inline
	    __hop_xml-types
	    __hop_xml
	    __hop_service
	    __hop_charset
	    __hop_clientc
	    __hop_read-js)

   (export  (generic obj->javascript ::obj ::output-port ::obj)
	    (json->hop ::input-port)
	    (hop->js-callback ::obj)))

;*---------------------------------------------------------------------*/
;*    obj->javascript ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (obj->javascript obj op::output-port isrep)
   (cond
      ((procedure? obj)
       (if (service? obj)
	   (obj->javascript (procedure-attr obj) op isrep)
	   (error "obj->javascript"
		  "Illegal procedure in JavaScript conversion"
		  obj)))
      ((date? obj)
       (fprintf op "new Date( ~a000 )" (date->seconds obj)))
      (else
       (let ((comp (hop-clientc))
	     (foreign-out (lambda (obj op)
			     (obj->javascript obj op isrep))))
	  ((clientc-valuec comp) obj op foreign-out #f)))))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::object ...                                     */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::object op isrep)

   (define (display-seq lst op proc)
      (when (pair? lst)
	 (proc (car lst) op)
	 (let loop ((lst (cdr lst)))
	    (when (pair? lst)
	       (display "," op)
	       (proc (car lst) op)
	       (loop (cdr lst))))))

   (define (display-list lst op proc)
      (display "(" op)
      (display-seq lst op proc)
      (display ")" op))
   
   (define (display-field-init fields op)
      (display "{" op)
      (for-each (lambda (f)
		   (let ((n (class-field-name f)))
		      (fprintf op "this.~a = ~a;" n n)))
		fields)
      (display "}" op))

   (let* ((klass (object-class obj))
	  (kname (symbol->string! (class-name klass)))
	  (name (if (bigloo-need-mangling? kname)
		    (bigloo-mangle kname)
		    kname))
	  (hash (class-hash klass))
	  (fields (class-all-fields klass)))
      (fprintf op "(function() {var ~a=function" name)
      (display-list fields op (lambda (o op) (display (class-field-name o) op)))
      (display-field-init fields op)
      (fprintf op ";~a.prototype.hop_bigloo_serialize = hop_bigloo_serialize_object; " name)
      (fprintf op "~a.prototype.hop_classname = '~a'; " name name)
      (fprintf op "~a.prototype.hop_classhash = ~a;" name hash)
      (fprintf op "~a.prototype.hop_classfields = [" name)
      (display-seq fields op (lambda (f op)
				(display "'" op)
				(display (class-field-name f) op)
				(display "'" op)))
      (display "]; " op)
      (fprintf op "return new ~a" name)
      (display-list fields op (lambda (f op)
				 (obj->javascript
				  ((class-field-accessor f) obj) op isrep)))
      (display ";})()" op))
   #t)

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml ...                                        */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml op isrep)
   (error "obj->javascript" "Cannot translate xml element" xml))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml-markup ...                                 */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml-markup op isrep)
   (display "hop_create_encoded_element(\"" op)
   (let ((s (url-path-encode
	     (call-with-output-string
	      (lambda (op) (xml-write obj op (hop-xml-backend)))))))
      (display s op))
   (display "\")" op)
   #t)

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml-element ...                                */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml-element op isrep)
   (if isrep
       (call-next-method)
       (fprintf op "document.getElementById( '~a' )" (xml-element-id obj)))
   #t)

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml-tilde ...                                  */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml-tilde op isrep)
   (display (xml-tilde->expression obj) op)
   #t)

;*---------------------------------------------------------------------*/
;*    obj->javascript ::hop-service ...                                */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::hop-service op isrep)
   (display (hop-service-javascript obj) op)
   #t)

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
      (#\:
       (list 'COLON))
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
       (list 'CONSTANT (the-integer)))
      
      ;; floating-point constant
      ((or (: (? #\-) (+ digit)
	      (: (in #\e #\E) (? (in #\- #\+)) (+ digit))
	      (? (in #\f #\F #\l #\L)))
	   (: (? #\-) (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
	      (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
	      (? (in #\f #\F #\l #\L))))
       (list 'CONSTANT (the-flonum)))
      
      ;; symbols constant
      ((: #\' (* (or (out #\\ #\') (: #\\ all))) #\')
       (list 'CONSTANT (the-symbol)))
      
      ;; string constant
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (ucs2->utf8 (the-substring 1 (-fx (the-length) 1)) 0)))
	  (list 'CONSTANT (string-as-read str))))
      
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
	  ((var) (list 'VAR))
	  (else (list 'IDENTIFIER (the-symbol)))))
      
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (list 'ERROR c))))))

;*---------------------------------------------------------------------*/
;*    ucs2->utf8 ...                                                   */
;*---------------------------------------------------------------------*/
(define (ucs2->utf8 str start)

   (define (hex n)
      (cond
	 ((and (char>=? n #\0) (char<=? n #\9))
	  (-fx (char->integer n) (char->integer #\0)))
	 ((and (char>=? n #\a) (char<=? n #\f))
	  (+fx 10 (-fx (char->integer n) (char->integer #\a))))
	 ((and (char>=? n #\A) (char<=? n #\F))
	  (+fx 10 (-fx (char->integer n) (char->integer #\A))))
	 (else
	  0)))
	 
   (define (utf8 str i)
      (let* ((c0 (hex (string-ref str i)))
	     (c1 (hex (string-ref str (+fx i 1))))
	     (c2 (hex (string-ref str (+fx i 2))))
	     (c3 (hex (string-ref str (+fx i 3))))
	     (n (+fx (bit-lsh (+fx (*fx c0 16) c1) 8)
		     (+fx (*fx c2 16) c3)))
	     (u (integer->ucs2 n)))
	 (ucs2-string->utf8-string (make-ucs2-string 1 u))))
   
   (let ((len (string-length str)))
      (let loop ((i start))
	 (cond
	    ((=fx i len)
	     (if (=fx start 0)
		 str
		 (substring str start len)))
	    ((and (char=? (string-ref str i) #\\)
		  (<= (+fx i 6) len)
		  (char=? (string-ref str (+fx i 1)) #\u))
	     (if (> i start)
		 (string-append (substring str start i)
				(utf8 str (+fx i 2))
				(ucs2->utf8 str (+fx i 6)))
		 (string-append (substring str start i)
				(utf8 str (+fx i 2))
				(ucs2->utf8 str (+fx i 6)))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    *json-parser* ...                                                */
;*---------------------------------------------------------------------*/
(define *json-parser*
   
   (lalr-grammar
      
      ;; tokens
      (CONSTANT PAR-OPEN PAR-CLO BRA-OPEN BRA-CLO ANGLE-OPEN ANGLE-CLO
       COMMA SEMI-COMMA COLON DOT =
       IDENTIFIER ERROR NEW CONS DATE FUNCTION RETURN VAR)
      
      ;; initial rule
      (start
       (() '())
       ((expression) expression))
      
      ;; expression
      (expression
       ((CONSTANT)
	(car CONSTANT))
       ((IDENTIFIER)
	(car IDENTIFIER))
       ((new)
	new)
       ((IDENTIFIER PAR-OPEN vals PAR-CLO)
	(case (car IDENTIFIER)
	   ((sc_consStart) (apply cons* vals))
	   ((sc_Pair) (cons (car vals) (cadr vals)))
	   ((sc_list) vals)
	   ((sc_jsstring2symbol) (string->symbol (car vals)))
	   ((sc_jsstring2keyword) (string->keyword (car vals)))
	   (else (error "json->hop" "Unknown function" (car IDENTIFIER)))))
       ((ANGLE-OPEN ANGLE-CLO)
	'#())
       ((ANGLE-OPEN array-elements ANGLE-CLO)
	(list->vector array-elements))
       ((BRA-OPEN BRA-CLO)
	'())
       ((BRA-OPEN hash-elements BRA-CLO)
	hash-elements)
       ((object)
	object)
       ((get-element-by-id)
	get-element-by-id)
       ((service)
	service)
       ((PAR-OPEN FUNCTION PAR-OPEN PAR-CLO BRA-OPEN
		  VAR set SEMI-COMMA
		  set+
		  RETURN new SEMI-COMMA
		  BRA-CLO PAR-CLO PAR-OPEN PAR-CLO)
	new)
       ((constructor)
	#unspecified))

      ;; new
      (new
       ((NEW IDENTIFIER PAR-OPEN vals PAR-CLO)
	(case (car IDENTIFIER)
	   ((sc_Pair)
	    (match-case vals
	       ((?a ?d)
		(cons a d))
	       (else
		(error "json->hop" "Illegal `cons' construction" vals))))
	   ((Date)
	    (match-case vals
	       ((and ?sec (? integer?))
		(seconds->date (/fx sec 1000)))
	       (else
		(error "json->hop" "Illegal `date' construction" vals))))
	   (else
	    (let ((c (find-class (car IDENTIFIER))))
	       (if (not (class? c))
		   (error "json->hop" "Can't find class" c)
		   (let* ((constr (class-constructor c))
			  (create (class-creator c))
			  (ins (apply create vals)))
		      (when (procedure? constr) (constr ins))
		      ins)))))))
      
      ;; array
      (array-elements
       ((expression)
	(list expression))
       ((expression COMMA array-elements)
	(cons expression array-elements)))

      ;; hash
      (hash-elements
       ((hash-element)
	(list hash-element))
       ((hash-element COMMA hash-elements)
	(cons hash-element hash-elements)))

      (hash-element
       ((IDENTIFIER COLON expression)
	(list (symbol->keyword (car IDENTIFIER)) expression))
       ((CONSTANT COLON expression)
	(let ((v (car CONSTANT)))
	   (if (string? v)
	       (list (string->keyword v) expression)
	       (list v expression)))))

      ;; object
      (object
       ((FUNCTION IDENTIFIER PAR-OPEN argument+ PAR-CLO
		  BRA-OPEN set+ BRA-CLO SEMI-COMMA
		  BRA-OPEN proto BRA-CLO SEMI-COMMA
		  NEW IDENTIFIER@klass PAR-OPEN vals PAR-CLO)
	(let ((c (find-class klass)))
	   (if (not (class? c))
	       (error "json->hop" "Can't find class" c)
	       (let* ((constr (class-constructor c))
		      (create (class-creator c))
		      (ins (apply constr vals)))
		  (when (procedure? create) (create ins))
		  ins)))))

      (argument*
       (()
	'())
       ((IDENTIFIER)
	(list (car IDENTIFIER)))
       ((IDENTIFIER COMMA argument*)
	(cons (car IDENTIFIER) argument*)))

      (argument+
       ((IDENTIFIER)
	(list (car IDENTIFIER)))
       ((IDENTIFIER COMMA argument+)
	(cons (car IDENTIFIER) argument+)))

      (set+
       ((set SEMI-COMMA set*)
	(cons set set*)))

      (set*
       (() '())
       ((set SEMI-COMMA set*)
	(cons set set*)))

      (set
       ((IDENTIFIER = expression)
	'_)
       ((IDENTIFIER DOT IDENTIFIER = expression)
	'_)
       ((IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER = expression)
	'_))

      (proto
       ((IDENTIFIER@c DOT IDENTIFIER@p DOT IDENTIFIER@f = IDENTIFIER@fun)
	(list c p f fun)))

      (vals
       (()
	'())
       ((expression)
	(list expression))
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
		  PAR-OPEN argument* PAR-CLO BRA-CLO)
	(error "json->hop" "Service cannot be transmitted" IDENTIFIER)))

      ;; constructor
      (constructor
       ((FUNCTION PAR-OPEN argument+ PAR-CLO
		  BRA-OPEN set+ BRA-CLO)
	#unspecified))))


;*---------------------------------------------------------------------*/
;*    json->hop ...                                                    */
;*---------------------------------------------------------------------*/
(define (json->hop ip)
   (read/lalrp *json-parser* *json-lexer* ip))

;*---------------------------------------------------------------------*/
;*    hop->js-callback ...                                             */
;*---------------------------------------------------------------------*/
(define (hop->js-callback obj)
   (cond
      ((xml-tilde? obj)
       (format "function( event ) { ~a }" (xml-tilde->return obj)))
      ((string? obj)
       (format "function( event ) { ~a }" obj))
      (else
       "false")))
