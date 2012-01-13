;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/js_comp.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 15:55:02 2005                          */
;*    Last change :  Fri Jan 13 18:30:27 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JS compilation tools                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_js-comp

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

   (export  (obj->javascript-attr ::obj ::output-port)
	    (obj->javascript-expr ::obj ::output-port)
	    (generic obj->javascript ::obj ::output-port ::bool)
	    (javascript->obj ::obj)
	    (hop->js-callback ::obj)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::xml-markup ...                               */
;*---------------------------------------------------------------------*/
(register-class-serialization! xml-markup
   (lambda (o)
      (let ((p (open-output-string)))
	 (obj->javascript-expr o p)
	 (close-output-port p)))
   (lambda (o)
      #unspecified))

;*---------------------------------------------------------------------*/
;*    obj->javascript-attr ...                                         */
;*---------------------------------------------------------------------*/
(define (obj->javascript-attr obj op)
   (obj->javascript obj op #f))

;*---------------------------------------------------------------------*/
;*    obj->javascript-expr ...                                         */
;*---------------------------------------------------------------------*/
(define (obj->javascript-expr obj op)
   (obj->javascript obj op #t))

;*---------------------------------------------------------------------*/
;*    obj->javascript ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (obj->javascript obj op::output-port isexpr)
   (cond
      ((eq? obj #t)
       (display (cached-clientc-true) op))
      ((eq? obj #f)
       (display (cached-clientc-false) op))
      ((eq? obj #unspecified)
       (display (cached-clientc-unspecified) op))
      ((procedure? obj)
       (if (service? obj)
	   (obj->javascript (procedure-attr obj) op isexpr)
	   (error "obj->javascript"
	      "Illegal procedure in JavaScript conversion"
	      obj)))
      (else
       (clientc-compile obj op isexpr))))

;*---------------------------------------------------------------------*/
;*    clientc-compile ...                                              */
;*---------------------------------------------------------------------*/
(define (clientc-compile obj op isexpr)
   (let ((foreign-out (lambda (obj2 op)
			 (obj->javascript obj2 op isexpr))))
      (with-access::clientc (hop-clientc) (valuec)
	 (valuec obj op foreign-out #f))))

;*---------------------------------------------------------------------*/
;*    *clientc-true* ...                                               */
;*---------------------------------------------------------------------*/
(define *clientc-true* #f)
(define *clientc-false* #f)
(define *clientc-unspecified* #f)

;*---------------------------------------------------------------------*/
;*    cached-clientc-true ...                                          */
;*---------------------------------------------------------------------*/
(define (cached-clientc-true)
   (if *clientc-true*
       *clientc-true*
       (let ((op (open-output-string)))
	  (clientc-compile #t op #f)
	  (set! *clientc-true* (close-output-port op))
	  *clientc-true*)))
       
;*---------------------------------------------------------------------*/
;*    cached-clientc-false ...                                         */
;*---------------------------------------------------------------------*/
(define (cached-clientc-false)
   (if *clientc-false*
       *clientc-false*
       (let ((op (open-output-string)))
	  (clientc-compile #f op #f)
	  (set! *clientc-false* (close-output-port op))
	  *clientc-false*)))
       
;*---------------------------------------------------------------------*/
;*    cached-clientc-unspecified ...                                   */
;*---------------------------------------------------------------------*/
(define (cached-clientc-unspecified)
   (if *clientc-unspecified*
       *clientc-unspecified*
       (let ((op (open-output-string)))
	  (clientc-compile #unspecified op #f)
	  (set! *clientc-unspecified* (close-output-port op))
	  *clientc-unspecified*)))
       
;*---------------------------------------------------------------------*/
;*    obj->javascript ::object ...                                     */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::object op isexpr)
   
   (define (display-seq vec op proc)
      (let ((len (vector-length vec)))
	 (when (>fx len 0)
	    (proc (vector-ref vec 0) op)
	    (let loop ((i 1))
	       (when (<fx i len)
		  (display "," op)
		  (proc (vector-ref vec i) op)
		  (loop (+fx i 1)))))))
   
   (define (display-fields fields op)
      (display "{ " op)
      (display-seq fields op
	 (lambda (f op)
	    (display "'" op)
	    (display (class-field-name f) op)
	    (display "': " op)
	    (obj->javascript
	       ((class-field-accessor f) obj) op isexpr)))
      (display "}" op))
   
   (let ((klass (object-class obj)))
      (if (nil? obj)
	  (begin
	     (display "sc_class_nil(sc_class_exists(sc_string2symbol(sc_jsstring2string(\"" op)
	     (display (class-name klass) op)
	     (display "\"))))" op))
	  (let ((fields (class-all-fields klass)))
	     (display "hop_js_to_object(\""  op)
	     (display (class-name klass) op)
	     (display "\", " op)
	     (display (class-hash klass) op)
	     (display ", " op)
	     (display-fields fields op)
	     (display ")" op)))))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml ...                                        */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml op isexpr)
   (error "obj->javascript" "Cannot translate xml element" xml))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml-markup ...                                 */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml-markup op isexpr)
   (display "hop_create_encoded_element(\"" op)
   (let ((s (url-path-encode
	     (call-with-output-string
	      (lambda (op) (xml-write obj op (hop-xml-backend)))))))
      (display s op))
   (display "\")" op))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml-element ...                                */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml-element op isexpr)
   (if isexpr
       (call-next-method)
       (with-access::xml-element obj (id)
	  (fprintf op "document.getElementById( '~a' )" id))))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::xml-tilde ...                                  */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::xml-tilde op isexpr)
   (display (xml-tilde->expression obj) op))

;*---------------------------------------------------------------------*/
;*    obj->javascript ::hop-service ...                                */
;*---------------------------------------------------------------------*/
(define-method (obj->javascript obj::hop-service op isexpr)
   (with-access::hop-service obj (javascript)
      (display javascript op)))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (return key . val)
   `(list ,key
	  ,(if (pair? val) (car val) '(the-string))
	  (input-port-name (the-port))
	  (input-port-position (the-port))))

;*---------------------------------------------------------------------*/
;*    *javascript-lexer* ...                                           */
;*---------------------------------------------------------------------*/
(define *javascript-lexer*
   
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
       (return 'DOT))
      (#\,
       (return 'COMMA))
      (#\;
       (return 'SEMI-COMMA))
      (#\:
       (return 'COLON))
      (#\=
       (return '=))
      
      ;; angles
      (#\[
       (return 'ANGLE-OPEN))
      (#\]
       (return 'ANGLE-CLO))
      
      ;; parenthesis
      (#\(
       (return 'PAR-OPEN))
      (#\)
       (return 'PAR-CLO))
      
      ;; brackets
      (#\{
       (return 'BRA-OPEN))
      (#\}
       (return 'BRA-CLO))
      
      ;; integer constant
      ((: (? (in "+-")) (+ digit))
       (return 'CONSTANT (the-integer)))
      
      ;; floating-point constant
      ((or (: (? #\-) (+ digit)
	      (: (in #\e #\E) (? (in #\- #\+)) (+ digit))
	      (? (in #\f #\F #\l #\L)))
	   (: (? #\-) (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
	      (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
	      (? (in #\f #\F #\l #\L))))
       (return 'CONSTANT (the-flonum)))
      
      ;; symbols constant
      ((: #\' (* (or (out #\\ #\') (: #\\ all))) #\')
       (return 'CONSTANT (the-symbol)))
      
      ;; string constant
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (ucs2->utf8 (the-substring 1 (-fx (the-length) 1)) 0)))
	  (cond
	     ;; see scheme2js/runtime/immutable.js
	     ((string-prefix? "\356\256\254" str)
	      (return 'CONSTANT (string->symbol (substring str 3))))
	     ((string-prefix? "\356\256\255" str)
	      (return 'CONSTANT (string->keyword (substring str 3))))
	     (else
	      (return 'CONSTANT (string-as-read str))))))
      
      ;; identifier
      ((: (or #\_ alpha) (* (or #\_ alpha digit)))
       (case (the-symbol)
	  ((null) (return 'CONSTANT '()))
	  ((undefined) (return 'CONSTANT #unspecified))
	  ((true) (return 'CONSTANT #t))
	  ((false) (return 'CONSTANT #f))
	  ((new) (return 'NEW))
	  ((function) (return 'FUNCTION))
	  ((return) (return 'RETURN))
	  ((var) (return 'VAR))
	  (else (return 'IDENTIFIER (the-symbol)))))
      
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (return 'ERROR c))))))

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
;*    *javascript-parser* ...                                          */
;*---------------------------------------------------------------------*/
(define *javascript-parser*
   
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
       ((IDENTIFIER PAR-OPEN expressions* PAR-CLO)
	(case (car IDENTIFIER)
	   ((sc_consStart) (apply cons* expressions*))
	   ((sc_Pair) (cons (car expressions*) (cadr expressions*)))
	   ((sc_list) expressions*)
	   ((sc_jsstring2symbol) (string->symbol (car expressions*)))
	   ((sc_jsstring2keyword) (string->keyword (car expressions*)))
	   ((hop_js_to_object) (javascript->object expressions*))
	   (else (error "javascript->obj" "Unknown function" (car IDENTIFIER)))))
       ((ANGLE-OPEN ANGLE-CLO)
	'#())
       ((ANGLE-OPEN array-elements ANGLE-CLO)
	(list->vector array-elements))
       ((BRA-OPEN BRA-CLO)
	'())
       ((BRA-OPEN hash-elements BRA-CLO)
	;; see json compilation
	(if (and (=fx (length hash-elements) 3)
		 (eq? (car (car hash-elements)) __uuid:)
		 (equal? (cadr (car hash-elements)) "pair"))
	    (cons (cadr (cadr hash-elements)) (cadr (caddr hash-elements)))
	    hash-elements))
;*        ((object)                                                    */
;* 	object)                                                        */
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
       ((PAR-OPEN expressions* PAR-CLO)
	(if (null? (cdr expressions*))
	    (car expressions*)
	    expressions*))
       ((constructor)
	#unspecified))

      (expressions*
       (()
	'())
       ((expression)
	(list expression))
       ((expression COMMA expressions*)
	(cons expression expressions*)))

      ;; new
      (new
       ((NEW IDENTIFIER PAR-OPEN expressions* PAR-CLO)
	(case (car IDENTIFIER)
	   ((sc_Pair)
	    (match-case expressions*
	       ((?a ?d)
		(cons a d))
	       (else
		(error "javascript->obj" "Illegal `cons' construction" expressions*))))
	   ((Date)
	    (match-case expressions*
	       ((and ?sec (? integer?))
		(seconds->date (/fx sec 1000)))
	       (else
		(error "javascript->obj" "Illegal `date' construction" expressions*))))
	   (else
	    (error "javascript->obj" "Illegal `new' form" expressions*)))))
;* 	    (let* ((mangle (symbol->string (car IDENTIFIER)))          */
;* 		   (name (if (bigloo-mangled? mangle)                  */
;* 			     (bigloo-demangle mangle)                  */
;* 			     mangle))                                  */
;* 		   (clazz (find-class (string->symbol name))))         */
;* 	       (if (not (class? clazz))                                */
;* 		   (error "javascript->obj" "Can't find class" clazz)        */
;* 		   (let* ((constr (class-constructor clazz))           */
;* 			  (create (class-creator clazz))               */
;* 			  (ins (apply create expressions*)))           */
;* 		      (when (procedure? constr) (constr ins))          */
;* 		      ins)))))))                                       */
      
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
;*       (object                                                       */
;*        ((FUNCTION IDENTIFIER PAR-OPEN expressions* PAR-CLO          */
;* 		  BRA-OPEN set+ BRA-CLO SEMI-COMMA                     */
;* 		  BRA-OPEN proto BRA-CLO SEMI-COMMA                    */
;* 		  NEW IDENTIFIER@klass PAR-OPEN expressions* PAR-CLO)  */
;* 	(let ((c (find-class klass)))                                  */
;* 	   (if (not (class? c))                                        */
;* 	       (error "javascript->obj" "Can't find class" c)                */
;* 	       (let* ((constr (class-constructor c))                   */
;* 		      (create (class-creator c))                       */
;* 		      (ins (apply constr expressions*)))               */
;* 		  (when (procedure? create) (create ins))              */
;* 		  ins)))))                                             */

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
	(error "javascript->obj" "Service cannot be transmitted" IDENTIFIER)))

      ;; constructor
      (constructor
       ((FUNCTION PAR-OPEN argument+ PAR-CLO BRA-OPEN set+ BRA-CLO)
	#unspecified))))

;*---------------------------------------------------------------------*/
;*    javascript->object ...                                           */
;*---------------------------------------------------------------------*/
(define (javascript->object expressions*)
   (let* ((mangle (car expressions*))
	  (hash (cadr expressions*))
	  (fields (caddr expressions*))
	  (name (if (bigloo-mangled? mangle) (bigloo-demangle mangle) mangle))
	  (clazz (find-class (string->symbol name))))
      (if (=fx hash (class-hash clazz))
	  (let* ((create (class-creator clazz))
		 (constr (class-constructor clazz))
		 (o (apply create (map cadr fields))))
	     (when (procedure? constr) (constr o))
	     o)
	  (error "javascript->obj" "corrupted class" name))))

;*---------------------------------------------------------------------*/
;*    javascript->obj ...                                              */
;*---------------------------------------------------------------------*/
(define (javascript->obj o)
   
   (define (parse-javascript ip)
      (with-handler
	 (lambda (e)
	    (if (not (isa? e &io-parse-error))
		(raise e)
		(with-access::&io-parse-error e (obj)
		   (match-case obj
		      ((?token ?val ?file ?pos)
		       (raise (duplicate::&io-parse-error e
				 (obj (format "~a (~a)" token val))
				 (fname file)
				 (location pos))))
		      (else
		       (raise e))))))
	 (read/lalrp *javascript-parser* *javascript-lexer* ip)))

   (cond
      ((input-port? o) (parse-javascript o))
      ((string? o) (call-with-input-string o parse-javascript))
      (else (error "javascript->obj" "Illegal argument" o))))

;*---------------------------------------------------------------------*/
;*    hop->js-callback ...                                             */
;*---------------------------------------------------------------------*/
(define (hop->js-callback obj)
   (cond
      ((isa? obj xml-tilde)
       (format "function( event ) { ~a }" (xml-tilde->return obj)))
      ((string? obj)
       (format "function( event ) { ~a }" obj))
      (else
       "false")))
