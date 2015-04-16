;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/json.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 19 11:52:55 2010                          */
;*    Last change :  Thu Apr 16 07:32:01 2015 (serrano)                */
;*    Copyright   :  2010-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JSON lib.                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_json
   
   (import  __hop_param
	    __hop_types
	    __hop_hop-inline
	    __hop_dom
	    __hop_xml-types
	    __hop_service
	    __hop_charset
	    __hop_clientc
	    __hop_js-comp)
   
   (export  (generic obj->json ::obj ::output-port)
            (byte-array->json ::bstring ::output-port)
	    (javascript->obj ::obj)
	    (generic javascript-class-all-fields ::object)))

;*---------------------------------------------------------------------*/
;*    obj->json ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (obj->json obj op::output-port)
   (cond
      ((vector? obj)
       (vector->json obj op))
      ((pair? obj)
       (pair->json obj op))
      ((procedure? obj)
       (if (service? obj)
	   (obj->json (procedure-attr obj) op)
	   (error "obj->json"
	      "Illegal procedure in JSON conversion"
	      obj)))
      (else
       (obj->javascript-expr obj op))))

;*---------------------------------------------------------------------*/
;*    byte-array->json ...                                             */
;*---------------------------------------------------------------------*/
(define (byte-array->json str::bstring op::output-port)
   (let ((len (string-length str)))
      (case len
	 ((0)
	  (display "[]" op))
	 ((1)
	  (display "[" op)
	  (display-fixnum (char->integer (string-ref-ur str 0)) op)
	  (display "]" op))
	 (else
	  (display "[" op)
	  (display-fixnum (char->integer (string-ref-ur str 0)) op)
	  (let loop ((i 1))
	     (if (=fx i len)
		 (display "]" op)
		 (begin
		    (display "," op)
		    (display-fixnum (char->integer (string-ref-ur str i)) op)
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    vector->json ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector->json vec op::output-port)
   (let ((len (vector-length vec)))
      (case len
	 ((0)
	  (display "[]" op))
	 ((1)
	  (display "[" op)
	  (obj->json (vector-ref vec 0) op)
	  (display "]" op))
	 (else
	  (display "[" op)
	  (obj->json (vector-ref vec 0) op)
	  (let loop ((i 1))
	     (if (=fx i len)
		 (display "]" op)
		 (begin
		    (display "," op)
		    (obj->json (vector-ref vec i) op)
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    pair->json ...                                                   */
;*---------------------------------------------------------------------*/
(define (pair->json pair op::output-port)
   (display "{" op)
   (display "\"__uuid\":" op) (display "\"pair\"," op)
   (display "\"car\":" op) (obj->json (car pair) op)
   (display ",\"cdr\":" op) (obj->json (cdr pair) op)
   (display "}" op))
   
;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for i from to . Lbody)
   (let ((loop (gensym 'loop))
	 (to-tmp (gensym 'to)))
      `(let ((,to-tmp ,to))
	  (let ,loop ((,i ,from))
	       (when (<fx ,i ,to-tmp)
		  ,@Lbody
		  (,loop (+fx ,i 1)))))))

;*---------------------------------------------------------------------*/
;*    javascript-parser ...                                            */
;*---------------------------------------------------------------------*/
(define (javascript-parser ip)
   (with-handler
      (lambda (e)
	 (if (not (isa? e &io-parse-error))
	     (raise e)
	     (with-access::&io-parse-error e (obj)
		(match-case obj
		   ((?token ?val (and (? string?) ?file) ?pos)
		    (raise (duplicate::&io-parse-error e
			      (obj (format "~a (~a)" token val))
			      (fname file)
			      (location pos))))
		   ((?token ?val ?- (and (? integer?) ?pos))
		    (raise (duplicate::&io-parse-error e
			      (obj (format "~a (~a)" token val))
			      (fname "-")
			      (location pos))))
		   (else
		    (raise e))))))
      (let ((lalr (javascript-lalr-parser)))
	 (read/lalrp lalr *javascript-lexer* ip))))

;*---------------------------------------------------------------------*/
;*    javascript->obj ...                                              */
;*---------------------------------------------------------------------*/
(define (javascript->obj o)
   (cond
      ((input-port? o)
       (javascript-parser o))
      ((string? o)
       (call-with-input-string o
	  (lambda (ip)
	     (javascript-parser ip))))
      (else
       (error "javascript->obj" "Illegal argument" o))))

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
      ((+ (in #\space #\newline #\tab #a012 #\return))
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
;*    javascript-lalr-parser ...                                       */
;*---------------------------------------------------------------------*/
(define (javascript-lalr-parser)
   
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
	 ((RETURN expression)
	  expression)
	 ((IDENTIFIER PAR-OPEN expressions* PAR-CLO)
	  (case (car IDENTIFIER)
	     ((sc_consStart) (apply cons* expressions*))
	     ((sc_Pair) (cons (car expressions*) (cadr expressions*)))
	     ((sc_list) expressions*)
	     ((sc_jsstring2symbol) (string->symbol (car expressions*)))
	     ((sc_jsstring2keyword) (string->keyword (car expressions*)))
	     ((hop_js_to_object) (javascript->object expressions*))
	     ((sc_circle) (javascript-circle->obj expressions*))
	     ((sc_circle_ref) `(sc_circle_ref ,@expressions*))
	     ((sc_circle_def) `(sc_circle_def ,@expressions*))
	     ((sc_vector2array) (car expressions*))
	     ((hop_buffer) (car expressions*))
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
	  constructor))
      
      (expressions*
	 (()
	  '())
	 ((expression)
	  (list expression))
	 ((expression COMMA expressions*)
	  (cons expression expressions*))
	 ((expression SEMI-COMMA expressions*)
	  (cons expression expressions*)))
      
      ;; new
      (new
       ;;; new expr	    
       ((NEW IDENTIFIER PAR-OPEN expressions* PAR-CLO)
	(case (car IDENTIFIER)
	   ((sc_Pair)
	    (match-case expressions*
	       ((?a ?d)
		(cons a d))
	       (else
		(error "javascript->obj" "Illegal `cons' construction"
		   expressions*))))
	   ((Date)
	    (match-case expressions*
	       (((and ?nsec (? llong?)))
		(nanoseconds->date nsec))
	       (((and ?sec (? flonum?)))
		(nanoseconds->date (*llong #l1000000 (flonum->llong sec))))
	       (((and ?sec (? integer?)))
		(seconds->date sec))
	       (else
		(error "javascript->obj" "Illegal `date' construction"
		   expressions*))))
	   ((Number)
	    (match-case expressions*
	       (((and ?val (? number?)))
		val)
	       (else
		(error "javascript->obj" "Illegal `number' construction"
		   expressions*))))
	   ((Boolean)
	    (match-case expressions*
	       (((and ?val (? boolean?)))
		val)
	       (else
		(error "javascript->obj" "Illegal `boolean' construction"
		   expressions*))))
	   ((String)
	    (match-case expressions*
	       ((?val)
		val)
	       (else
		(error "javascript->obj" "Illegal `string' construction"
		   expressions*))))
	   (else
	    (error "javascript->obj" "Illegal `new' form" expressions*)))))
      
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
	  (cons (car IDENTIFIER) expression))
	 ((CONSTANT COLON expression)
	  (let ((v (car CONSTANT)))
	     (if (string? v)
		 (cons (string->symbol v) expression)
		 (cons v expression)))))
      
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
      
      ;; function definition
      (constructor
	 ((FUNCTION PAR-OPEN argument+ PAR-CLO BRA-OPEN expressions* BRA-CLO)
	  `(lambda ,argument+ ,@expressions*)))))

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
		 (o (apply create (map cdr fields))))
	     (when (procedure? constr) (constr o))
	     o)
	  (error "javascript->obj" "corrupted class" name))))

;*---------------------------------------------------------------------*/
;*    javascript-circle->obj ...                                       */
;*---------------------------------------------------------------------*/
(define (javascript-circle->obj expressions)
   (match-case expressions
      (((and (? integer?) ?n) (lambda (cache) ?expr) #t)
       expr)
      (((and (? integer?) ?n) (lambda (cache) ?expr) #f)
       (let ((cache (make-vector n #f)))
	  (let loop ((obj expr))
	     (match-case obj
		((sc_circle_ref ?- ?n)
		 (vector-ref cache n))
		((sc_circle_def ?- ?n ?v)
		 (vector-set! cache n v)
		 (loop v)
		 v)
		((?a . ?d)
		 (set-car! obj (loop a))
		 (set-cdr! obj (loop d))
		 obj)
		((? vector?)
		 (for i 0 (vector-length obj)
		    (vector-set-ur! obj i (loop (vector-ref-ur obj i))))
		 obj)
		((? object?)
		 (let ((fields (javascript-class-all-fields obj)))
		    (for i 0 (vector-length fields)
		       (let ((f (vector-ref-ur fields i)))
			  ((class-field-mutator f)
			   obj (loop ((class-field-accessor f) obj))))))
		 obj)
		(else
		 obj)))))
      (else
       (error "javascript-circle->obj" "illegal expression" expressions))))

;*---------------------------------------------------------------------*/
;*    javascript-class-all-fields ::object ...                         */
;*---------------------------------------------------------------------*/
(define-generic (javascript-class-all-fields obj::object)
   (class-all-fields (object-class obj)))

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

