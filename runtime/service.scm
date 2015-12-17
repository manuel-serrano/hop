;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/service.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:29:08 2006                          */
;*    Last change :  Wed Dec 16 21:19:05 2015 (serrano)                */
;*    Copyright   :  2006-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP services                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_service
   
   (include "service.sch"
            "verbose.sch"
	    "param.sch")
   
   (library web)
   
   (import  __hop_configure
	    __hop_param
	    __hop_types
	    __hop_misc
	    __hop_read
	    __hop_http-error
	    __hop_http-response
	    __hop_xml-types
	    __hop_xml
	    __hop_html-base
	    __hop_html-head
	    __hop_preferences
	    __hop_xdomain
	    __hop_js-comp
	    __hop_user
	    __hop_weblets
	    __hop_hop
	    __hop_json
	    __hop_http-lib)
   
   (export  (init-hop-services!)
	    (hop-object->plist::pair ::obj)
	    (hop-plist->object::object ::pair)
	    (generic service?::bool ::obj)
	    (generic service->hop-service::hop-service ::obj)
	    (service-path::bstring ::obj)
	    (service-resource::bstring ::obj #!optional file)
	    (generic service-pack-cgi-arguments ::obj ::hop-service ::pair-nil)
	    (generic post-multipart->obj ::obj ::obj ::bstring)
	    (generic service-base-url::bstring ::obj ::http-request)
	    (get-service::hop-service ::bstring)
	    (service-exists? ::bstring)
	    (get-all-services ::http-request)
	    (gen-service-url::bstring #!key (prefix "") (public #f))
	    (hop-service-path? ::bstring)
	    (hop-apply-nice-url::bstring ::bstring ::pair-nil)
	    (hop-apply-url::bstring ::bstring ::pair-nil)
	    (hop-apply-service-url::bstring ::hop-service ::pair-nil)
	    (hop-request-service-name::bstring ::http-request)
	    (procedure->service::procedure ::procedure)
	    (service-filter ::http-request)
	    (register-service!::hop-service ::hop-service)
	    (unregister-service! ::hop-service)
	    (expired-service-path?::bool ::bstring)
	    (service-etc-path-table-fill! ::bstring)
	    (etc-path->service ::bstring)))

;*---------------------------------------------------------------------*/
;*    builtin class serializers                                        */
;*---------------------------------------------------------------------*/
(register-class-serialization! object
   (lambda (obj mode)
      (if (eq? mode 'hop-client)
	  (hop-object->plist obj)
	  obj))
   (lambda (obj ctx)
      (if (pair? obj)
	  (hop-plist->object obj)
	  obj)))

(register-class-serialization! hop-service
   (lambda (obj) obj)
   (lambda (obj) obj))

;*---------------------------------------------------------------------*/
;*    procedure-serializer ...                                         */
;*---------------------------------------------------------------------*/
(register-procedure-serialization! 
   (lambda (p)
      (let ((attr (procedure-attr p)))
	 (if attr
	     attr
	     (error "obj->string" "cannot serialize procedure" p))))
   (lambda (o)
      (if (isa? o hop-service)
	  (with-access::hop-service o (args id wid path timeout ttl args)
	     (eval `(service :path ,path :id ,id :url ,path
		       :timeout ,timeout :ttl ,ttl ,args)))
	  o)))

;*---------------------------------------------------------------------*/
;*    for  ....                                                        */
;*---------------------------------------------------------------------*/
(define-macro (for var min max . body)
   (let ((loop (gensym 'for)))
      `(let ,loop ((,var ,min))
	    (when (<fx ,var ,max)
	       ,@body
	       (,loop (+fx ,var 1))))))

;*---------------------------------------------------------------------*/
;*    hop-object->plist ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-object->plist obj)
   (let* ((args '())
	  (klass (object-class obj))
	  (fields (class-all-fields klass))
	  (len (vector-length fields)))
      (for i 0 len
	 (let* ((f (vector-ref-ur fields i))
		(iv (class-field-info f)))
	    (let ((v (when (pair? iv) (memq :serialize iv))))
	       (when (or (not v) (cadr v))
		  (let ((val (cond
				((and (pair? iv) (memq :client iv)) => cadr)
				(else ((class-field-accessor f) obj))))
			(key (symbol->keyword (class-field-name f))))
		     (set! args (cons* key val args)))))))
      (cons* ':__class__ (string->symbol (typeof obj)) args)))

;*---------------------------------------------------------------------*/
;*    hop-plist->object ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-plist->object plist)
   (let ((c (memq ':__class__ plist)))
      (if (pair? c)
	  (let* ((obj (allocate-instance (cadr c)))
		 (klass (object-class obj))
		 (fields (class-all-fields klass))
		 (len (vector-length fields)))
	     (for i 0 len
		(let* ((f (vector-ref-ur fields i))
		       (iv (class-field-info f)))
		   (cond
		      ((and (pair? iv) (memq :client iv))
		       (let ((d (class-field-default-value f)))
			  ((class-field-mutator f) obj d)))
		      ((memq (symbol->keyword (class-field-name f)) plist)
		       =>
		       (lambda (v)
			  ((class-field-mutator f) obj (cadr v))))
		      (else
		       (let ((d (class-field-default-value f)))
			  ((class-field-mutator f) obj d))))))
	     obj)
	  (bigloo-type-error "hop-plist->object" "plist" plist))))

;*---------------------------------------------------------------------*/
;*    service? ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (service? obj)
   (and (procedure? obj) (isa? (procedure-attr obj) hop-service)))

;*---------------------------------------------------------------------*/
;*    service->hop-service ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (service->hop-service obj)
   (if (procedure? obj)
       (procedure-attr obj)
       (bigloo-type-error "service->hop-service" "service" obj)))

;*---------------------------------------------------------------------*/
;*    service-path ...                                                 */
;*---------------------------------------------------------------------*/
(define (service-path svc)
   (with-access::hop-service (service->hop-service svc) (path)
      path))

;*---------------------------------------------------------------------*/
;*    service-resource ...                                             */
;*---------------------------------------------------------------------*/
(define (service-resource svc #!optional file)
   (with-access::hop-service (service->hop-service svc) (resource)
      (if (string? file)
	  (string-append resource "/" file)
	  resource)))

;*---------------------------------------------------------------------*/
;*    service-base-url ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (service-base-url svc req)
   (with-access::http-request req (scheme host port)
      (let ((path (service-resource svc)))
	 (format (if (and (>fx (string-length path) 0)
			  (not (char=? (string-ref path 0) #\/)))
		     "~a://~a:~a/~a/"
		     "~a://~a:~a~a/")
		 (if (eq? scheme '*) "http" scheme) host port
		 path))))

;*---------------------------------------------------------------------*/
;*    mutexes ...                                                      */
;*---------------------------------------------------------------------*/
(define *service-mutex* (make-mutex "service"))
(define *expiration-mutex* (make-mutex "expiration"))
(define *service-table-mutex* (make-mutex "hop-service-table"))
(define *service-etc-table-mutex* (make-mutex "hop-service-etc-table"))
(define *service-serialize-mutex* (make-mutex "hop-service-serialize"))

;*---------------------------------------------------------------------*/
;*    *service-table-count* ...                                        */
;*---------------------------------------------------------------------*/
(define *service-table-count* 1)

;*---------------------------------------------------------------------*/
;*    *service-table*                                                  */
;*---------------------------------------------------------------------*/
(define *service-table*
   (make-hashtable #unspecified #unspecified equal-path? hash-path))

;*---------------------------------------------------------------------*/
;*    *service-source-table* ...                                       */
;*---------------------------------------------------------------------*/
(define *service-source-table*
   (make-hashtable 4))

;*---------------------------------------------------------------------*/
;*    get-all-services ...                                             */
;*---------------------------------------------------------------------*/
(define (get-all-services req)
   (synchronize *service-table-mutex*
      (delete-duplicates!
	 (filter (lambda (svc)
		    (with-access::hop-service svc (id wid)
		       (or (authorized-service? req wid)
			   (authorized-service? req id))))
	    (hashtable->list *service-table*)))))

;*---------------------------------------------------------------------*/
;*    init-hop-services! ...                                           */
;*    -------------------------------------------------------------    */
;*    Create the first HOP services.                                   */
;*---------------------------------------------------------------------*/
(define (init-hop-services!)
   (init-hop-prefs-services!)
   (init-hop-xdomain-service!))

;*---------------------------------------------------------------------*/
;*    gen-service-url ...                                              */
;*---------------------------------------------------------------------*/
(define (gen-service-url #!key (prefix "") (public #f))
   (synchronize *service-table-mutex*
      (set! *service-table-count* (+fx 1 *service-table-count*))
      (format (if public "public/~a/~a~a" "~a/~a~a")
	 (hop-service-weblet-name)
	 prefix
	 *service-table-count*)))

;*---------------------------------------------------------------------*/
;*    hop-service-path? ...                                            */
;*---------------------------------------------------------------------*/
(define (hop-service-path? path)
   (when (substring-at? path (hop-service-base) 0)
      (let ((l1 (string-length (hop-service-base)))
	    (lp (string-length path)))
	 (or (=fx lp l1)
	     (and (>fx lp l1) (char=? (string-ref path l1) #\/))))))
   
;*---------------------------------------------------------------------*/
;*    hop-request-service-name ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-request-service-name req)
   (with-access::http-request req (path)
      (let ((len (string-length path)))
	 (let loop ((i 1))
	    (cond
	       ((=fx i len)
		path)
	       ((char=? (string-ref path i) #\?)
		(substring path 0 i))
	       (else
		(loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    hop-apply-nice-url ...                                           */
;*    -------------------------------------------------------------    */
;*    This is used for fix arity functions in order to build nice URLS.*/
;*    When at least one argument is not a string, it falls back to     */
;*    hop-apply-url.                                                   */
;*---------------------------------------------------------------------*/
(define (hop-apply-nice-url base vals)
   
   (define (all-keyword-string? vals)
      (cond
	 ((null? vals)
	  #t)
	 ((null? (cdr vals))
	  #f)
	 ((and (keyword? (car vals)) (string? (cadr vals)))
	  (all-keyword-string? (cddr vals)))
	 (else
	  #f)))

   (cond
      ((null? vals)
       base)
      ((all-keyword-string? vals)
       (let loop ((vals vals)
		  (strs '()))
	  (if (null? vals)
	      (apply string-append base strs)
	      (loop (cddr vals)
		    (let ((str (string-append
				(if (pair? (cddr vals)) "&" "?")
				(keyword->string (car vals))
				"="
				(url-path-encode (cadr vals)))))
		       (cons str strs))))))
      (else
       (hop-apply-url base vals))))
       
;*---------------------------------------------------------------------*/
;*    hop-apply-url ...                                                */
;*---------------------------------------------------------------------*/
(define (hop-apply-url base vals)
   (let ((o (if (vector? vals) (vector->list vals) vals)))
      (string-append base
	 "?hop-encoding=hop"
	 "&vals=" (url-path-encode (obj->string o 'hop-to-hop)))))

;*---------------------------------------------------------------------*/
;*    hop-apply-service-url ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-apply-service-url svc vals)
   (with-access::hop-service svc (path)
      (hop-apply-url path vals)))

;*---------------------------------------------------------------------*/
;*    service-pack-cgi-arguments ::obj ...                             */
;*---------------------------------------------------------------------*/
(define-generic (service-pack-cgi-arguments ctx::obj svc alist)
   alist)

;*---------------------------------------------------------------------*/
;*    service-parse-request-get-args ...                               */
;*---------------------------------------------------------------------*/
(define (service-parse-request-get-args args)
   
   (define (normalize l)
      ;; pack values of arguments occurring more than once
      ;; (normalize '((a 1) (a 2) (b 3)) => ((b 3) (a (1 2))))
      (let loop ((l l)
		 (res '()))
	 (if (null? l)
	     res
	     (let ((a (assq (caar l) res)))
		(if (pair? a)
		    (begin
		       (if (pair? (cadr a))
			   (append! (cadr a) (list (cadar l)))
			   (set-car! (cdr a) (list (cadr a) (cadar l))))
		       (loop (cdr l) res))
		    (loop (cdr l) (cons (car l) res)))))))
   
   ;; replace the arguments name with keyword and replace
   ;; cons cells with lists
   (for-each (lambda (a)
		(set-car! a (string->keyword (car a)))
		(set-cdr! a (cons (cdr a) '())))
      args)
   ;; pack the multiple arguments occurrences
   (normalize args)
   ;; build the arguments list
   (apply append args))

;*---------------------------------------------------------------------*/
;*    dsssl-service? ...                                               */
;*---------------------------------------------------------------------*/
(define (dsssl-service? svc)
   (with-access::hop-service svc (args)
      (and (pair? args) (eq? (car args) #!key))))

;*---------------------------------------------------------------------*/
;*    service-parse-request-get ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (service-parse-request-get svc::hop-service req::http-request)
   (with-access::http-request req (query path)
      (with-access::hop-service svc (id ctx)
	 (if (string? query)
	     (let ((args (cgi-args->list query)))
		(match-case args
		   ((("hop-encoding" . "hop") ("vals" . ?vals))
		    (string->obj vals #f ctx))
		   (else
		    (cond
		       ((and ctx (not (dsssl-service? svc)))
			(service-pack-cgi-arguments ctx svc args))
		       ((every string? args)
			(if (dsssl-service? svc)
			    (error id "bad arguments" path)
			    (service-parse-request-get-args args)))
		       ((every pair? args)
			(if (dsssl-service? svc)
			    (service-parse-request-get-args args)
			    (error id "bad arguments" path)))
		       (else
			(error id "bad arguments" path))))))
	     (service-pack-cgi-arguments ctx svc '())))))

;*---------------------------------------------------------------------*/
;*    service-parse-request-put ...                                    */
;*---------------------------------------------------------------------*/
(define (service-parse-request-put svc::hop-service req::http-request)
   (with-access::http-request req (query abspath)
      (with-access::hop-service svc (ctx)
	 (if (string? query)
	     (let ((args (cgi-args->list query)))
		(match-case args
		   ((("hop-encoding" . "hop") ("vals" . ?vals))
		    (string->obj vals #f ctx))
		   (else
		    (service-pack-cgi-arguments ctx svc
		       (service-parse-request-get-args args)))))
	     (service-pack-cgi-arguments ctx svc '())))))

;*---------------------------------------------------------------------*/
;*    post-multipart->obj ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (post-multipart->obj ctx val enc)
   (cond
      ((string=? enc "string") val)
      ((string=? enc "file") val)
      ((string=? enc "integer") (string->integer val))
      ((string=? enc "keyword") (string->keyword val))
      (else (string->obj val #f ctx))))

;*---------------------------------------------------------------------*/
;*    service-parse-request-post ...                                   */
;*---------------------------------------------------------------------*/
(define (service-parse-request-post svc::hop-service req::http-request)
   
   (define (multipart-value! v)
      (with-access::hop-service svc (ctx)
	 (cond
	    ((memq :data v)
	     =>
	     (lambda (data)
		(let ((val (cadr data))
		      (header (memq :header v)))
		   (cond
		      ((not header)
		       (post-multipart->obj ctx val "string"))
		      ((memq :hop-encoding (cadr header))
		       =>
		       (lambda (hop-enc)
			  (post-multipart->obj ctx val (cadr hop-enc))))
		      (else
		       (post-multipart->obj ctx val "string"))))))
	    ((memq :file v)
	     =>
	     (lambda (file)
		(post-multipart->obj ctx (cadr file) "file")))
	    (else
	     (error "service-parse-request-post" "unknown error" v)))))

   (define (multipart-named-value v)
      (with-access::hop-service svc (ctx)
	 (cond
	    ((memq :data v)
	     =>
	     (lambda (data) (cons (car v) (cadr data))))
	    ((memq :file v)
	     =>
	     (lambda (file) (cons (car v) (cadr file))))
	    (else
	     (error "service-parse-request-post" "unknown error" v)))))
   
   (define (multipart-dsssl-arg-value v)
      (cond
	 ((memq :data v)
	  =>
	  (lambda (data)
	     (list (string->keyword (car v)) (cadr data))))
	 ((memq :file v)
	  =>
	  (lambda (file)
	     (let ((val (cadr file)))
		(list (string->keyword (car v)) val))))
	 (else
	  (error "service-parse-request-post" "unknown error" v))))
   
   (define (multipart-arg-value v)
      (cond
	 ((memq :data v)
	  =>
	  (lambda (data)
	     (cadr data)))
	 ((memq :file v)
	  =>
	  (lambda (file)
	     (let ((val (cadr file)))
		(list (string->keyword (car v)) val))))
	 (else
	  (error "service-parse-request-post" "unknown error" v))))
   
   (define (multipart-dir)
      (let ((dir (make-file-path (hop-cache-directory)
		    (integer->string (hop-port))
		    (hop-upload-directory))))
	 (unless (directory? dir) (make-directories dir))
	 dir))
      
   (define (multipart->list pi content-length boundary transfer-encoding)
      (if (eq? transfer-encoding 'chunked)
	  (let ((pic (http-chunks->port pi)))
	     (unwind-protect
		(cgi-multipart->list multipart-dir pic content-length boundary)
		(close-input-port pic)))
	  (cgi-multipart->list multipart-dir pi content-length boundary)))
   
   (define (hop-multipart? args)
      (match-case args
	 ((((? string?) :data ?- :header (:hop-encoding ?-)) . ?-)
	  #t)
	 ((((? string?) :data ?- :header (and (? pair?) ?val)) . ?-)
	  (memq :hop-encoding val))
	 (else #f)))

   (define (multipart-boundary ctype)
      (when (and (string? ctype)
		 (substring-ci-at? ctype "multipart/form-data; boundary=" 0))
	 (substring ctype (string-length "multipart/form-data; boundary=")
	    (string-length ctype))))
   
   (with-access::http-request req (content-length header socket transfer-encoding)
      (let* ((pi (socket-input socket))
	     (ctype (http-header-field header content-type:)))
	 (cond
	    ((multipart-boundary ctype)
	     =>
	     (lambda (boundary)
		(with-access::hop-service svc (ctx)
		   (let ((args (multipart->list
				  pi content-length boundary
				  transfer-encoding)))
		      (cond
			 ((hop-multipart? args)
			  ;; hop-multipart is used for fix arity services.
			  ;; arguments are unamed.
			  (cond
			     ((dsssl-service? svc)
			      (multipart-value! (car args)))
			     (else
			      (map! multipart-value! args))))
			 ((dsssl-service? svc)
			  (append-map multipart-dsssl-arg-value args))
			 (ctx
			  ;; standard post call, named string arguments
			  (service-pack-cgi-arguments ctx svc
			     (map multipart-named-value args)))
			 (else
			  (map multipart-arg-value args)))))))
	     ((not (string? ctype))
	      '())
	     ((string=? ctype "application/json")
	      (with-access::hop-service svc (ctx)
		 (json->obj ctx pi)))
	     ((string=? ctype "application/x-www-form-urlencoded")
	      (let ((body (read-chars (elong->fixnum content-length) pi)))
		 (with-access::hop-service svc (ctx)
		    (if (and ctx (not (dsssl-service? svc)))
			(service-pack-cgi-arguments ctx svc
			   (cgi-args->list body))
			(service-parse-request-get-args
			   (cgi-args->list body))))))
	     (else
	      (with-access::hop-service svc (id)
		 (error "service-parse-request"
		    (format "Illegal HTTP POST request type (~a)" id)
		    ctype)))))))

;*---------------------------------------------------------------------*/
;*    service-parse-request ...                                        */
;*    -------------------------------------------------------------    */
;*    Hop uses various service call protocols. They mainly depends     */
;*    on the HTTP verb (PUT, POST, GET) and additionally, for some     */
;*    of these verbs, different encoding can be used by the caller.    */
;*---------------------------------------------------------------------*/
(define (service-parse-request svc::hop-service req::http-request)
   (with-trace 2 'service-parse-request
      (with-access::http-request req (method path abspath query)
	 (trace-item "path=" path)
	 (trace-item "abspath=" (string-for-read abspath))
	 (trace-item "method=" method)
	 (trace-item "query=" (when (string? query) (string-for-read query)))
	 (with-access::http-request req (method)
	    (case method
	       ((PUT)
		(service-parse-request-put svc req))
	       ((GET)
		(service-parse-request-get svc req))
	       ((POST)
		(service-parse-request-post svc req))
	       (else
		(with-access::hop-service svc (id)
		   (error "service-parse-request"
		      (format "Illegal HTTP method (~a)" id)
		      method))))))))
   
;*---------------------------------------------------------------------*/
;*    service-handler ...                                              */
;*---------------------------------------------------------------------*/
(define (service-handler svc req)
   
   (define (invoke-trace req id vals)
      (hop-verb 2 (hop-color req req " INVOKE.svc")
	 " "
	 (with-output-to-string
	    (lambda ()
	       (write-circle (cons id vals))))
	 "\n"))
   
   (define (invoke vals)
      (with-access::hop-service svc (id proc args ctx)
	 (invoke-trace req id vals)
	 (cond
	    ((not vals)
	     (error id "Illegal service arguments encoding" `(,id)))
	    ((or (pair? vals) (null? vals))
	     (if (correct-arity? proc (+fx 1 (length vals)))
		 (let ((env (current-dynamic-env))
		       (name id))
		    ($env-push-trace env name #f)
		    (let ((aux (apply proc req vals)))
		       ($env-pop-trace env)
		       aux))
		 (error id
		    (format "Wrong number of arguments (~a/~a)" (length vals)
		       (-fx (procedure-arity proc) 1))
		    `(,id ,@vals))))
	    ((correct-arity? proc 2)
	     (let ((env (current-dynamic-env))
		   (name id))
		($env-push-trace env name #f)
		(let ((aux (proc req vals)))
		   ($env-pop-trace env)
		   aux)))
	    (else
	     (error id
		(format "Wrong number of arguments (1/~a)"
		   (-fx (procedure-arity proc) 1))
		`(,id ,vals))))))
   
   (invoke (service-parse-request svc req)))

;*---------------------------------------------------------------------*/
;*    procedure->service ...                                           */
;*---------------------------------------------------------------------*/
(define (procedure->service::procedure proc::procedure)
   (let ((arity (procedure-arity proc)))
      (case arity
	 ((0)
	  (service () (proc)))
	 ((1)
	  (service (a0) (proc a0)))
	 ((2)
	  (service (a0 a1) (proc a0 a1)))
	 ((3)
	  (service (a0 a1 a2) (proc a0 a1 a2)))
	 ((4)
	  (service (a0 a1 a2 a3) (proc a0 a1 a2 a3)))
	 ((5)
	  (service (a0 a1 a2 a3 a4) (proc a0 a1 a2 a3 a4)))
	 ((6)
	  (service (a0 a1 a2 a3 a4 a5) (proc a0 a1 a2 a3 a4 a5)))
	 ((7)
	  (service (a0 a1 a2 a3 a4 a5 a6) (proc a0 a1 a2 a3 a4 a5 a6)))
	 ((8)
	  (service (a0 a1 a2 a3 a4 a5 a6 a7) (proc a0 a1 a2 a3 a4 a5 a6 a7)))
	 (else
	  (error "procedure->service" "arity not supported" proc)))))

;*---------------------------------------------------------------------*/
;*    exp->eval-string ...                                             */
;*    -------------------------------------------------------------    */
;*    JS eval does not accept \n inside quotes!                        */
;*---------------------------------------------------------------------*/
(define (exp->eval-string exp)
   (cond
      ((string? exp)
       (string-replace exp #\Newline #\space))
      ((isa? exp xml-tilde)
       (let ((body (xml-tilde->expression exp)))
	  (let ((l (string-length body)))
	     (if (substring-at? body ";\n" (-fx l 2))
		 (string-replace (substring body 0 (-fx l 2)) #\Newline #\space)
		 (string-replace body #\Newline #\space)))))
      ((list? exp)
       (apply string-append (map exp->eval-string exp)))
      (else
       exp)))

;*---------------------------------------------------------------------*/
;*    exp-list->eval-string ...                                        */
;*---------------------------------------------------------------------*/
(define (exp-list->eval-string exp)
   (if (null? exp)
       "[]"
       (let loop ((exp exp)
		  (res '()))
	  (cond
	     ((null? (cdr exp))
	      (apply string-append "["
		 (reverse! (cons* "]" (exp->eval-string (car exp)) res))))
	     (else
	      (loop (cdr exp)
		    (cons* "," (exp->eval-string (car exp)) res)))))))

;*---------------------------------------------------------------------*/
;*    equal-path? ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes that p1 and p2 shares (hop-service-base)   */
;*    as prefix.                                                       */
;*---------------------------------------------------------------------*/
(define (equal-path? p1 p2)
   (let ((l1 (string-length p1))
	 (l2 (string-length p2)))
      (and (>=fx l2 l1)
	   (let loop ((i (+fx 1 (string-length (hop-service-base)))))
	      (cond
		 ((=fx i l1)
		  (or (=fx i l2) (char=? (string-ref p2 i) #\?)))
		 ((char=? (string-ref p1 i) (string-ref p2 i))
		  (loop (+fx i 1)))
		 (else
		  #f))))))

;*---------------------------------------------------------------------*/
;*    hash-path ...                                                    */
;*---------------------------------------------------------------------*/
(define (hash-path::long p)
   (let ((l (string-length p)))
      (let loop ((i (+fx 1 (string-length (hop-service-base))))
		 (r::long 0))
	 (if (>=fx i l)
	     (bit-and r (-fx (bit-lsh 1 29) 1))
	     (let ((c (string-ref p i)))
		(if (char=? c #\?)
		    (bit-and r (-fx (bit-lsh 1 29) 1))
		    (loop (+fx i 1)
			  (+fx r (+fx (bit-lsh r 3) (char->integer c))))))))))

;*---------------------------------------------------------------------*/
;*    get-service ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-service abspath)
   (or (synchronize *service-mutex*
	  (hashtable-get *service-table* abspath))
       (let ((req (instantiate::http-server-request
		     (abspath abspath)
		     (method 'GET))))
	  (autoload-filter req))))
   
;*---------------------------------------------------------------------*/
;*    service-exists? ...                                              */
;*---------------------------------------------------------------------*/
(define (service-exists? svc)
   (with-handler
      (lambda (e) #f)
      (let ((url (string-append (hop-service-base) "/" svc)))
	 (isa? (get-service url) hop-service))))

;*---------------------------------------------------------------------*/
;*    service-filter ...                                               */
;*---------------------------------------------------------------------*/
(define (service-filter req)
   (when (isa? req http-server-request)
      (with-access::http-server-request req (abspath service method)
	 (when (hop-service-path? abspath)
	    (let loop ((svc (synchronize *service-mutex*
			       (hashtable-get *service-table* abspath))))
	       (cond
		  ((isa? svc hop-service)
		   (when (hop-force-reload-service)
		      (set! svc (force-reload-service svc)))
		   (set! service svc)
		   (with-access::hop-service svc (ttl path id wid)
		      (cond
			 ((service-expired? svc)
			  (mark-service-path-expired! path)
			  (http-invalidated-service-error req))
			 ((or (authorized-service? req wid)
			      (authorized-service? req id))
			  (cond
			     ((eq? method 'HEAD)
			      (instantiate::http-response-string))
			     ((>fx ttl 0)
			      (unwind-protect
				 (scheme->response
				    (service-handler svc req) req)
				 (if (=fx ttl 1)
				     (unregister-service! svc)
				     (set! ttl (-fx ttl 1)))))
			     (else
			      (scheme->response
				 (service-handler svc req) req))))
			 (else
			  (service-denied req id)))))
		  (else
		   (let ((ini (hop-initial-weblet)))
		      (cond
			 ((and (string? ini)
			       (substring-at? abspath (hop-service-base) 0)
			       (let ((l1 (string-length abspath))
				     (l2 (string-length (hop-service-base))))
				  (or (=fx l1 l2)
				      (and (=fx l1 (+fx l2 1))
					   (char=? (string-ref abspath l2)
					      #\/)))))
			  (set! abspath
			     (string-append (hop-service-base) "/" ini))
			  ;; resume the hop loop in order to autoload
			  ;; the initial weblet
			  'hop-resume)
			 ((autoload-filter req)
			  =>
			  (lambda (o)
			     (if (eq? o #t)
				 (let ((s (synchronize *service-mutex*
					     (hashtable-get *service-table*
						abspath))))
				    (if (not s)
					(http-service-not-found abspath req)
					(loop s)))
				 (http-error o req))))
			 (else
			  (http-service-not-found abspath req)))))))))))

;*---------------------------------------------------------------------*/
;*    force-reload-service ...                                         */
;*---------------------------------------------------------------------*/
(define (force-reload-service svc)
   (with-access::hop-service svc (resource source path)
      (if (and (string? resource) (string? source))
	  (let ((file (make-file-path resource source)))
	     (if (file-exists? file)
		 (if (eq? (hop-load-modified file) #unspecified)
		     svc
		     (synchronize *service-mutex*
			(hashtable-get *service-table* path)))
		 svc))
	  svc)))

;*---------------------------------------------------------------------*/
;*    register-service! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-service! svc)
   (with-access::hop-service svc (path id)
      (let ((sz (hashtable-size *service-table*)))
	 (hop-verb 4 (hop-color 1 "" "REG. SERVICE ")
	    "(" (/fx sz 2) "): "
	    svc " " path "\n")
	 (synchronize *service-mutex*
	    (when (hashtable-get *service-table* path)
	       (cond
		  ((not (hop-allow-redefine-service))
		   (mutex-unlock! *service-mutex*)
		   (error id
		      "Service re-definition not permitted"
		      "use `--devel' or `-s0' options to enable re-definitions"))
		  ((and (>fx (bigloo-debug) 0) (not (hop-force-reload-service)))
		   (warning 'register-service! "Service re-defined -- " id))))
	    (hashtable-put! *service-table* path svc)
	    (unless (char=? #\/ (string-ref path (-fx (string-length path) 1)))
	       (hashtable-put! *service-table* (string-append path "/") svc))
	    (let ((l (string-length path)))
	       (let loop ((i (+fx (string-length (hop-service-base)) 1)))
		  (cond
		     ((>=fx i l)
		      (hop-weblets-set! (cons svc (hop-weblets))))
		     ((char=? (string-ref path i) #\/)
		      #unspecified)
		     (else
		      (loop (+fx i 1))))))
	    (when (=fx (remainder sz *service-table-count*) 0)
	       (flush-expired-services!))
	    svc))))

;*---------------------------------------------------------------------*/
;*    service-expired? ...                                             */
;*---------------------------------------------------------------------*/
(define (service-expired? svc)
   (with-access::hop-service svc (creation timeout path)
      (and (>fx timeout 0)
	   (>elong (current-seconds) (+elong creation timeout)))))

;*---------------------------------------------------------------------*/
;*    flush-expired-services! ...                                      */
;*---------------------------------------------------------------------*/
(define (flush-expired-services!)
   (hashtable-filter! *service-table*
      (lambda (key svc)
	 (if (service-expired? svc)
	     (with-access::hop-service svc (path)
		(mark-service-path-expired! path)
		#f)
	     #t))))
			 
;*---------------------------------------------------------------------*/
;*    unregister-service! ...                                          */
;*---------------------------------------------------------------------*/
(define (unregister-service! svc)
   (with-access::hop-service svc (path)
      (synchronize *service-mutex*
	 (hashtable-remove! *service-table* path)
	 (hashtable-remove! *service-table* (string-append path "/")))))

;*---------------------------------------------------------------------*/
;*    *expiration-table*                                               */
;*---------------------------------------------------------------------*/
(define *expiration-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    expired-service-path? ...                                        */
;*---------------------------------------------------------------------*/
(define (expired-service-path? path)
   (synchronize *expiration-mutex*
      (hashtable-get *expiration-table* path)))

;*---------------------------------------------------------------------*/
;*    mark-service-path-expired! ...                                   */
;*---------------------------------------------------------------------*/
(define (mark-service-path-expired! path)
   (synchronize *expiration-mutex*
      (hashtable-put! *expiration-table* path #t)
      #f))

;*---------------------------------------------------------------------*/
;*    *etc-table*                                                      */
;*---------------------------------------------------------------------*/
(define *etc-table*
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    service-etc-path-table-fill! ...                                 */
;*---------------------------------------------------------------------*/
(define (service-etc-path-table-fill! file)
   (synchronize *service-etc-table-mutex*
      (let ((etc (make-file-name (dirname file) "etc"))
	    (svc (string->symbol (prefix (basename file)))))
	 (when (directory? etc)
	    (let loop ((dir (file-name-unix-canonicalize etc)))
	       (for-each (lambda (f)
			    (let ((path (make-file-name dir f)))
			       (hashtable-put! *etc-table* path svc)
			       (when (directory? path) (loop path))))
		  (directory->list dir)))))))

;*---------------------------------------------------------------------*/
;*    etc-path->service ...                                            */
;*---------------------------------------------------------------------*/
(define (etc-path->service path)
   (hashtable-get *etc-table* path))

