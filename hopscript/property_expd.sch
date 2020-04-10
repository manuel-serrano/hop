;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/property_expd.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Fri Apr 10 07:39:58 2020 (serrano)                */
;*    Copyright   :  2016-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and property.sch                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-define-jseval-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-define-jseval-expander x e)
   (match-case x
      ((define-jseval ?var ?val ?evname ?evenv ?source ?loc . ?rest)
       (let ((tmp (gensym '%tmp)))
	  (e `(define ,var
		 (let ((,tmp ,val))
		    (js-define %this ,evenv ,evname
		       (lambda (%) ,var)
		       (lambda (% %v) (set! ,var %v))
		       ,source ,loc ,@rest)
		    ,tmp))
	     e)))
      (else
       (error "Js-define-eval" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-profile-log-cache-expander ...                                */
;*---------------------------------------------------------------------*/
(define (js-profile-log-cache-expander x e)
   (cond-expand
      (profile
       (match-case x
	  ((js-profile-log-cache ?cache . ?cspecs)
	   (e `(with-access::JsPropertyCache ,cache (cntimap cntemap
						       cntcmap cntpmap 
						       cntamap cntnmap
						       cntvtable cntmiss src)
		  ,@(filter-map (lambda (c)
				   (when (keyword? c)
				      (let ((id (symbol-append 'cnt
						   (string->symbol
						      (keyword->string c)))))
					 `(set! ,id (+u32 #u32:1 ,id)))))
		       cspecs))
	      e))
	  (else
	   (map (lambda (x) (e x e)) x))))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    js-profile-log-index-expander ...                                */
;*---------------------------------------------------------------------*/
(define (js-profile-log-index-expander x e)
   (cond-expand
      (profile
       (map (lambda (x) (e x e)) x))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    %define-pcache-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (%define-pcache-expander x e)
   (match-case x
      ((?- (and (? integer?) ?num))
       (e `(cond-expand
	     (bigloo-c
	      (static-pragma ,(format "static struct BgL_jspropertycachez00_bgl __bgl_pcache[ ~a ];" num))))
	  e))
      (else
       (error "%define-pache" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-make-pcache-table-expander ...                                */
;*---------------------------------------------------------------------*/
(define (js-make-pcache-table-expander x e)
   (match-case x
      ((?- (and (? integer?) ?num) ?src . ?profile-table-info)
       (e `(cond-expand
	      ((and bigloo-c (not hopjs-worker-slave))
	       ,(if (pair? profile-table-info)
		    `((@ js-pcache-table-profile-init __hopscript_property)
		      ($js-make-pcache-table (pragma::obj "(obj_t)(__bgl_pcache)")
			 ,num ,src (current-thread)
			 (instantiate::JsPropertyCache
			    (pctable (pragma::obj "(obj_t)(__bgl_pcache)"))
			    (src ,src)))
		      ,num
		      ,(car profile-table-info))
		    `($js-make-pcache-table (pragma::obj "(obj_t)(__bgl_pcache)")
			,num ,src (current-thread)
			(instantiate::JsPropertyCache
			   (pctable (pragma::obj "(obj_t)(__bgl_pcache)"))
			   (src ,src)))))
	     (else
	      ((@ js-make-pcache-table __hopscript_property) ,num ,src ,@profile-table-info)))
	  e))
      (else
       (error "js-make-pcache" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-pcache-ref-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-pcache-ref-expander x e)
   (match-case x
      ((js-pcache-ref %pcache ?-)
       (e `(cond-expand
	      ((and bigloo-c (not hopjs-worker-slave))
	       (free-pragma::JsPropertyCache "(BgL_jspropertycachez00_bglt)BOBJECT(&(__bgl_pcache[ $1 ]))" ,(caddr x)))
	      (else
	       ((@ js-pcache-ref __hopscript_property) ,(cadr x) ,(caddr x))))
	  e))
      (else
       (e `((@ js-pcache-ref __hopscript_property) ,(cadr x) ,(caddr x)) e))))

;*---------------------------------------------------------------------*/
;*    js-pcache-pctable-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-pcache-pctable-expander x e)
   (e (match-case x
	 ((js-pcache-imap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_pctablez00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (pctable) pctable))))
	 ((js-pcache-pcache ?c)
	  `(with-access::JsPropertyCache ,c (pctable) pctable))
	 (else
	  (error "js-pcache-pcache" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-imap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-imap-expander x e)
   (e (match-case x
	 ((js-pcache-imap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_imapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (imap) imap))))
	 ((js-pcache-imap ?c)
	  `(with-access::JsPropertyCache ,c (imap) imap))
	 (else
	  (error "js-pcache-imap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-cmap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-cmap-expander x e)
   (e (match-case x
	 ((js-pcache-cmap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_cmapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (cmap) cmap))))
	 ((js-pcache-cmap ?c)
	  `(with-access::JsPropertyCache ,c (cmap) cmap))
	 (else
	  (error "js-pcache-cmap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-pmap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-pmap-expander x e)
   (e (match-case x
	 ((js-pcache-pmap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_pmapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (pmap) pmap))))
	 ((js-pcache-pmap ?c)
	  `(with-access::JsPropertyCache ,c (pmap) pmap))
	 (else
	  (error "js-pcache-pmap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-nmap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-nmap-expander x e)
   (e (match-case x
	 ((js-pcache-nmap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_nmapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (nmap) nmap))))
	 ((js-pcache-nmap ?c)
	  `(with-access::JsPropertyCache ,c (nmap) nmap))
	 (else
	  (error "js-pcache-nmap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-emap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-emap-expander x e)
   (e (match-case x
	 ((js-pcache-emap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_emapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (emap) emap))))
	 ((js-pcache-emap ?c)
	  `(with-access::JsPropertyCache ,c (emap) emap))
	 (else
	  (error "js-pcache-emap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-amap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-amap-expander x e)
   (e (match-case x
	 ((js-pcache-amap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_amapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (amap) amap))))
	 ((js-pcache-amap ?c)
	  `(with-access::JsPropertyCache ,c (amap) amap))
	 (else
	  (error "js-pcache-amap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-index-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (js-pcache-index-expander x e)
   (e (match-case x
	 ((js-pcache-index (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_indexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (index) index))))
	 ((js-pcache-index ?c)
	  `(with-access::JsPropertyCache ,c (index) index))
	 (else
	  (error "js-pcache-index" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-vindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-vindex-expander x e)
   (e (match-case x
	 ((js-pcache-vindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_vindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (vindex) vindex))))
	 ((js-pcache-vindex ?c)
	  `(with-access::JsPropertyCache ,c (vindex) vindex))
	 (else
	  (error "js-pcache-vindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-owner-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (js-pcache-owner-expander x e)
   (e (match-case x
	 ((js-pcache-owner (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_ownerz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (owner) owner))))
	 ((js-pcache-owner ?c)
	  `(with-access::JsPropertyCache ,c (owner) owner))
	 (else
	  (error "js-pcache-owner" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-method-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-method-expander x e)
   (e (match-case x
	 ((js-pcache-method (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_methodz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (method) method))))
	 ((js-pcache-method ?c)
	  `(with-access::JsPropertyCache ,c (method) method))
	 (else
	  (error "js-pcache-method" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-function-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (js-pcache-function-expander x e)
   (e (match-case x
	 ((js-pcache-function (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      `(free-pragma::obj "(__bgl_pcache[ $1 ].BgL_functionz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (function) function))))
	 ((js-pcache-function ?c)
	  `(with-access::JsPropertyCache ,c (function) function))
	 (else
	  (error "js-pcache-function" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-get-jsobject-name/cache-expander ...                          */
;*---------------------------------------------------------------------*/
(define (js-get-jsobject-name/cache-expander x e)

   (define (let-cmap cs obj body)
      (if (pair? cs)
	  `(with-access::JsObject ,obj (cmap)
	      (let ((%cmap cmap)) ,body))
	  body))
	     
   (define (expand-cache-specs cspecs obj prop throw %this cache loc)
      (let-cmap cspecs obj
	 (let loop ((cs cspecs))
	    (cond
	       ((null? cs)
		;; cache miss
		`((@ js-get-proxy-name/cache-miss __hopscript_proxy)
		  ,obj ,prop ,throw ,%this ,cache))
	       ((eq? cs 'imap)
		`(let ((idx (js-pcache-index ,cache)))
		    (js-profile-log-cache ,cache :imap #t)
		    (js-profile-log-index idx)
		    (js-object-inline-ref ,obj idx)))
	       ((eq? cs 'cmap)
		`(let ((idx (js-pcache-index ,cache)))
		    (js-profile-log-cache ,cache :cmap #t)
		    (js-profile-log-index idx)
		    (with-access::JsObject ,obj (elements)
		       (vector-ref elements idx))))
	       ((eq? cs 'pmap)
		`(let ((idx (js-pcache-index ,cache)))
		    (with-access::JsObject (js-pcache-owner ,cache) (elements)
		       (js-profile-log-cache ,cache :pmap #t)
		       (js-profile-log-index idx)
		       (vector-ref elements idx))))
	       ((eq? cs 'amap)
		`(let* ((idx (js-pcache-index ,cache))
			(propowner (js-pcache-owner ,cache)))
		    (with-access::JsObject propowner (elements)
		       (let ((desc (vector-ref elements idx)))
			  (js-profile-log-cache ,cache :amap #t)
			  (js-profile-log-index idx)
			  (js-property-value ,obj
			     propowner ,prop desc ,%this)))))
	       ((not (pair? cs))
		(error "js-get-jsobject-name/cache" "bad form" x))
	       (else
		(case (car cs)
		   ((imap-incache)
		    (loop 'imap))
		   ((cmap-incache)
		    (loop 'cmap))
		   ((imap imap+)
		    ;; direct inlined property get
		    `(if (eq? %cmap (js-pcache-imap ,cache))
			 ,(loop 'imap)
			 ,(if (eq? (car cs) 'imap)
			      (loop (cdr cs))
			      `((@ js-get-jsobject-name/cache-imap+
				   __hopscript_property)
				,obj ,prop ,throw ,%this ,cache ,loc ',cspecs))))
		   ((emap)
		    (loop (cdr cs)))
		   ((cmap cmap+)
		    ;; direct property get
		    `(if (eq? %cmap (js-pcache-cmap ,cache))
			 ,(loop 'cmap)
			 ,(if (eq? (car cs) 'cmap)
			      (loop (cdr cs))
			      `((@ js-get-jsobject-name/cache-cmap+
				   __hopscript_property)
				,obj ,prop ,throw ,%this ,cache ,loc ',cspecs))))
		   ((pmap pmap+)
		    ;; prototype property get
		    `(if (eq? %cmap (js-pcache-pmap ,cache))
			 ,(loop 'pmap)
			 ,(loop (cdr cs))))
		   ((amap amap+)
		    ;; accessor property get
		    `(if (eq? %cmap (js-pcache-amap ,cache))
			 ,(loop 'amap)
			 ,(loop (cdr cs))))
		   ((pmap-dummy-profile)
		    (loop (cdr cs)))
		   ((vtable-dummy-profile)
		    `(begin
			;; this fake entry is used when profiling
			;; method calls
			(js-profile-log-cache ,cache :vtable #t)
			,(loop (cdr cs))))
		   ((vtable)
		    ;; vtable property get
		    (cond-expand
		       ((or no-vtable-cache no-vtable-cache-get)
			(loop (cdr cs)))
		       (else
			`(with-access::JsConstructMap %cmap (vlen vtable %id)
			    (let ((vidx (js-pcache-vindex ,cache)))
			       (if (and (<fx vidx vlen)
					(fixnum? (vector-ref vtable vidx)))
				   (let ((idx (vector-ref vtable vidx)))
				      (js-profile-log-cache ,cache
					 :vtable #t)
				      (js-profile-log-index idx)
				      (with-access::JsObject ,obj (elements)
					 (vector-ref elements idx)))
				   ,(loop (cdr cs))))))))
		   ((mvtable)
		    ;; vtable property get
		    (cond-expand
		       ((or no-vtable-cache no-vtable-cache-get)
			(loop (cdr cs)))
		       (else
			`(with-access::JsConstructMap %cmap (vlen vtable %id)
			    (let ((vidx (js-pcache-vindex ,cache)))
			       (if (and (<fx vidx vlen)
					(js-function? (vector-ref vtable vidx)))
				   (let ((fun (vector-ref vtable vidx)))
				      (js-profile-log-cache ,cache
					 :vtable #t)
				      fun)
				   ,(loop (cdr cs))))))))
		   ((global)
		    `((@ js-global-object-get-name/cache __hopscript_property)
		      ,obj ,prop ,throw ,%this
		      ,cache ,loc ',cspecs))
		   ((omiss)
		    ;; forced cache miss check
		    ;; (only used in js-jsobject-get/name-cache)
		    `((@ js-get-jsobject-name/cache-miss __hopscript_property)
		      ,obj ,prop ,throw ,%this ,cache))
		   ((mmiss)
		    ;; method lookup miss check
		    ;; used by call profiling and inline method dispatch
		    `((@ js-method-jsobject-get-name/cache-miss __hopscript_property)
		      ,obj ,prop ,throw ,%this ,cache))
		   (else
		    (error "js-get-jsobject-name/cache" "bad cache spec"
		       cs))))))))
      
   (cond-expand
      ((or no-macro-cache no-macro-cache-get)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-get-jsobject-name/cache (and (? (lambda (o) (not (symbol? o)))) ?obj) . ?rest)
	   (let ((o (gensym '%o)))
	      (e `(let ((,o ,obj)) (js-get-jsobject-name/cache ,o ,@rest)) e)))
	  ((js-get-jsobject-name/cache (and (? symbol?) ?obj)
	      ?prop ?throw ?%this ?cache
	      ?loc ((kwote quote) ?cspecs))
	   (e (expand-cache-specs cspecs obj prop throw %this cache loc) e))
	  ((js-get-jsobject-name/cache ?obj ?prop ?throw ?%this ?cache ?loc (? symbol?))
	   (set-car! (last-pair x) ''(cmap+))
	   (e x e))
	  ((js-get-jsobject-name/cache ?obj ?prop ?throw ?%this ?cache ?loc)
	   (e (append x '('(cmap+))) e))
	  ((js-get-jsobject-name/cache ?obj ?prop ?throw ?%this ?cache)
	   (e (append x '(-1 '(cmap+))) e))
	  (else
	   (error "js-get-jsobject-name/cache" "bad form" x))))))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) ?prop ?throw ?%this . ?rest)
       (e `(if (js-object? ,obj)
	       (js-get-jsobject-name/cache ,obj ,prop ,throw ,%this ,@rest)
	       (js-get ,obj ,prop ,%this))
	  e))
      ((?- ?obj . ?rest)
       (let ((tmp (gensym 'obj)))
	  (e `(let ((,tmp ,obj))
		 (js-get-name/cache ,tmp ,@rest))
	     e)))
      (else
       (error "js-get-name/cache" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name/cache-expander ...                     */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name/cache-expander x e)
   (match-case x
      ((js-global-object-get-name/cache ?obj ?prop ?throw ?%this ?cache)
       (e `(js-get-jsobject-name/cache ,obj ,prop ,throw ,%this
	      ,cache -1 '(imap+ global))
	  e))
      ((js-global-object-get-name/cache ?obj ?prop ?throw ?%this ?cache ?loc)
       (e `(js-get-jsobject-name/cache ,obj ,prop ,throw ,%this
	      ,cache ,loc '(imap+ global))
	  e))
      ((js-global-object-get-name/cache ?obj ?prop ?throw ?%this ?cache ?loc ?cs)
       (e `(js-get-jsobject-name/cache ,obj ,prop ,throw ,%this
	      ,cache ,loc ,cs)
	  e))
      (else
       (error "js-global-object-get-name/cache" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-get-length ...                                                */
;*---------------------------------------------------------------------*/
(define (js-get-length-expander x e)
   (match-case x
      ((js-get-length (and (? symbol?) ?o) ?%this . ?-)
       (e `(if (isa? ,o JsArray)
	       (js-uint32-tointeger (js-array-length ,o))
	       ((@ js-get-length __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-length ?o ?%this . ?-)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o))
		 (if (isa? ,tmp JsArray)
		     (js-uint32-tointeger (js-array-length ,tmp))
		     ((@ js-get-length __hopscript_property) ,tmp ,@(cddr x))))
	     e)))
      (else
       (error "js-get-length" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-put-jsobject-name/cache-expander ...                          */
;*---------------------------------------------------------------------*/
(define (js-put-jsobject-name/cache-expander x e)

   (define (expand-tmp e val proc)
      (if (or (symbol? val) (number? val) (string? val) (cnst? val))
	  (e (proc val) e)
	  (let ((tmp (gensym 'v)))
	     `(let ((,tmp ,(e val e)))
		 ,(e (proc tmp) e)))))

   (define (expand-cache-specs cspecs obj prop tmp throw %this cache loc cachefun)
      `(with-access::JsObject ,obj (cmap elements)
	  (let ((%cmap cmap))
	     ,(let loop ((cs cspecs))
		 (cond
		    ((null? cs)
		     `((@ js-put-proxy-name/cache-miss! __hopscript_proxy)
		       ,obj ,prop ,tmp ,throw ,%this ,cache ,loc ,cachefun))
		    ((eq? cs 'imap)
		     `(let ((idx (js-pcache-index ,cache)))
			 (js-profile-log-cache ,cache :imap #t)
			 (js-profile-log-index idx)
			 (js-object-inline-set! ,obj idx ,tmp)
			 ,tmp))
		    ((eq? cs 'emap)
		     `(let ((idx (js-pcache-index ,cache)))
			 (js-profile-log-cache ,cache :emap #t)
			 (js-profile-log-index idx)
			 (js-object-inline-set! ,obj idx ,tmp)
			 (set! cmap (js-pcache-cmap ,cache))
			 ,tmp))
		    ((eq? cs 'cmap)
		     `(let ((idx (js-pcache-index ,cache)))
			 (js-profile-log-cache ,cache :cmap #t)
			 (js-profile-log-index idx)
			 (vector-set! elements idx ,tmp)
			 ,tmp))
		    ((eq? cs 'nmap)
		     `(let ((idx (js-pcache-index ,cache)))
			 (js-profile-log-cache ,cache :nmap #t)
			 (js-profile-log-index idx)
			 (js-object-ctor-push! ,obj idx ,tmp)
			 (set! cmap (js-pcache-cmap ,cache))
			 ,tmp))
		    ((eq? cs 'amap)
		     `(let* ((idx (js-pcache-index ,cache))
			     (propowner (js-pcache-owner ,cache)))
			 (with-access::JsObject propowner (elements)
			    (let ((desc (vector-ref elements idx)))
			       (js-profile-log-cache ,cache :amap #t)
			       (js-profile-log-index idx)
			       (js-property-value-set! ,obj
				  propowner ,prop desc ,tmp %this))
			    ,tmp)))
		    ((not (pair? cs))
		     (error "js-put-jsobject-name/cache" "bad form" x))
		    (else
		     (case (car cs)
			((imap-incache)
			 (loop 'imap))
			((cmap-incache)
			 (loop 'cmap))
			((imap imap+)
			 ;; direct property set
			 `(if (eq? %cmap (js-pcache-imap ,cache))
			      ,(loop 'imap)
			      ,(if (eq? (car cs) 'imap)
				   (loop (cdr cs))
				   `((@ js-put-jsobject-name/cache-imap+!
					__hopscript_property)
				     ,obj ,prop ,tmp ,throw ,%this
				     ,cache ,loc ',cspecs))))
			((emap)
			 ;; direct property set
			 `(if (eq? %cmap (js-pcache-emap ,cache))
			      ,(loop 'emap)
			      ,(loop (cdr cs))))
			((cmap cmap+)
			 ;; direct property set
			 `(if (eq? %cmap (js-pcache-cmap ,cache))
			      ,(loop 'cmap)
			      ,(if (eq? (car cs) 'cmap)
				   (loop (cdr cs))
				   `((@ js-put-jsobject-name/cache-cmap+!
					__hopscript_property)
				     ,obj ,prop ,tmp ,throw ,%this
				     ,cache ,loc ',cspecs))))
			((nmap nmap+)
			 ;; prototype property set
			 `(if (eq? %cmap (js-pcache-nmap ,cache))
			      ,(loop 'nmap)
			      ,(if (eq? (car cs) 'nmap)
				   (loop (cdr cs))
				   `((@ js-put-jsobject-name/cache-nmap+!
					__hopscript_property)
				     ,obj ,prop ,tmp ,throw ,%this
				     ,cache ,loc ',cspecs))))
			((pmap)
			 (loop (cons 'nmap (cdr cs))))
			((pmap)
			 (loop (cons 'nmap+ (cdr cs))))
			((amap)
			 ;; accessor property set
			 `(if (eq? %cmap (js-pcache-amap ,cache))
			      ,(loop 'amap)
			      ,(loop (cdr cs))))
			((vtable)
			 ;; vtable property set
			 (cond-expand
			    ((or no-vtable-cache no-vtable-cache-put)
			     (loop (cdr cs)))
			    (else
			     `(with-access::JsConstructMap %cmap (vlen vtable)
				 (let ((vidx (js-pcache-vindex ,cache)))
				    (if (and (<fx vidx vlen)
					     (pair? (vector-ref vtable vidx)))
					(let ((idx (car (vector-ref vtable vidx)))
					      (ncmap (cdr (vector-ref vtable vidx))))
					   (js-profile-log-cache ,cache :vtable #t)
					   (js-profile-log-index idx)
					   (js-object-ctor-push! ,obj idx ,tmp)
					   (set! cmap ncmap)
					   ,tmp)
					,(loop (cdr cs))))))))
			(else
			 (error "js-put-jsobject-name/cache" "bad cache spec"
			    cs)))))))))
				  
   (cond-expand
      ((or no-macro-cache no-macro-cache-put)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-put-jsobject-name/cache! (and (? (lambda (o) (not (symbol? o)))) ?obj) . ?rest)
	   (let ((o (gensym '%o)))
	      (e `(let ((,o ,obj)) (js-put-jsobject-name/cache! ,o ,@rest)) e)))
	  ((js-put-jsobject-name/cache! (and (? symbol?) ?obj)
	      ?prop ?val ?throw ?%this
	      ?cache ?loc ((kwote quote) ?cspecs))
	   (expand-tmp e val
	      (lambda (tmp)
		 (expand-cache-specs cspecs obj prop tmp throw %this cache loc #f))))
	  ((js-put-jsobject-name/cache! (and (? symbol?) ?obj)
	      ?prop ?val ?throw ?%this
	      ?cache ?loc ((kwote quote) ?cspecs) ?cachefun)
	   (expand-tmp e val
	      (lambda (tmp)
		 (expand-cache-specs cspecs obj prop tmp throw %this cache loc cachefun))))
	  ((js-put-jsobject-name/cache! ?obj ?prop ?val ?throw ?%this ?cache ?loc .
	      (and ?rest ((? symbol?) ?cachefun)))
	   (set-car! rest ''(imap emap cmap nmap amap vtable))
	   (e x e))
	  ((js-put-jsobject-name/cache! ?obj ?prop ?val ?throw ?%this ?cache ?loc (? symbol?))
	   (set-car! (last-pair x) ''(imap emap cmap nmap amap vtable))
	   (set-cdr! (last-pair x) '(#f))
	   (e x e))
	  ((js-put-jsobject-name/cache! ?obj ?prop ?val ?throw ?%this ?cache)
	   (e (append x '(-1 '(imap emap cmap pmap amap vtable) #f)) e))
	  (else
	   (error/source "js-put-jsobject-name/cache!" "bad form" x x))))))
	      
;*---------------------------------------------------------------------*/
;*    js-put-name/cache-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache-expander x e)

   (define (expand-tmp e val proc)
      (if (or (symbol? val) (number? val) (string? val) (cnst? val))
	  (e (proc val) e)
	  (let ((tmp (gensym 'v)))
	     `(let ((,tmp ,(e val e)))
		 ,(e (proc tmp) e)))))
   
   (match-case x
      ((js-put-name/cache! (and (? symbol?) ?o)
	  ?prop ?val ?throw ?%this . ?rest)
       (expand-tmp e val
	  (lambda (tmp)
	     `(if (js-object? ,o)
		  (js-put-jsobject-name/cache! ,o ,prop ,tmp ,throw ,%this ,@rest)
		  (js-put! ,o ,prop ,tmp ,throw ,%this)))))
      ((js-put-name/cache! ?obj
	  ?prop ?val ?throw ?%this . ?rest)
       (let ((o (gensym 'o)))
	  (expand-tmp e val
	     (lambda (tmp)
		`(let ((,o ,obj))
		    (if (js-object? ,o)
			(js-put-jsobject-name/cache! ,o ,prop ,tmp ,throw ,%this
			   ,@rest)
			(js-put! ,o ,prop ,tmp ,throw ,%this)))))))
      (else
       (error "js-put-name/cache!" "bad form" x))))
   
;*---------------------------------------------------------------------*/
;*    js-method-jsobject-call-name/cache-expander ...                  */
;*---------------------------------------------------------------------*/
(define (js-method-jsobject-call-name/cache-expander x e)

   (define (calln %this m obj args)
      (let* ((len (length args))
	     (call (string->symbol
		      (format "js-call~a" (if (>=fx len 11) "n" len)))))
	 `(,call ,%this ,m ,obj ,@args)))
   
   (define (calln-uncachable %this ocspecs obj prop args ccache ocache loc)
      `(let ((f (js-get-jsobject-name/cache ,obj ,prop #f ,%this ,ocache ,loc ',ocspecs)))
	  ,(calln %this 'f obj args)))

   (define (calln-miss %this obj prop args ccache ocache loc cspecs ospecs)
      `(begin
	  (js-method-jsobject-call/cache-miss %this ,obj ,prop
	     ,(if (pair? args) `(list ,@args) ''())
	     ,ccache ,ocache ,loc ',cspecs ',ospecs)))
   
   (define (expand-cache-specs/args ccspecs ocspecs %this obj prop args ccache ocache loc)
      `(with-access::JsObject ,obj (cmap)
	  (let ((%cmap cmap))
	     ,(let loop ((cs ccspecs))
		 (if (null? cs)
		     (if (or (memq 'pmap ccspecs) (memq 'pmap-inline ccspecs))
			 `(if (eq? (js-pcache-cmap ,ccache) #t)
			      ,(if (memq 'pmap-inline ccspecs)
				   `(begin
				       (with-access::JsPropertyCache ,ccache (function)
					  (set! function #f))
				       ,(calln-uncachable %this ocspecs obj prop args ccache ocache loc))
				   (calln-uncachable %this ocspecs obj prop args ccache ocache loc))
			      ,(calln-miss %this obj prop args ccache ocache loc ccspecs ocspecs))
			 (calln-uncachable %this ocspecs obj prop args ccache ocache loc))
		     (case (car cs)
			((amap imap emap)
			 (loop (cdr cs)))
			((pmap)
			 `(if (eq? %cmap (js-pcache-pmap ,ccache))
			      (begin
				 (js-profile-log-cache ,ccache :pmap #t)
				 ((js-pcache-method ,ccache) ,obj ,@args))
			      ,(loop (cdr cs))))
			((pmap-inline)
			 `(if (eq? %cmap (js-pcache-pmap ,ccache))
			      (begin
				 (js-profile-log-cache ,ccache :pmap #t)
				 (with-access::JsPropertyCache ,ccache (function)
				    (set! function (procedure-attr (js-pcache-method ,ccache))))
				 ((js-pcache-method ,ccache) ,obj ,@args))
			      ,(loop (cdr cs))))
			((cmap)
			 (let ((idx (gensym 'idx)))
			    `(if (eq? %cmap (js-pcache-cmap ,ccache))
				 (let ((,idx (js-pcache-index ,ccache)))
				    (js-profile-log-cache ,ccache :cmap #t)
				    (js-profile-log-index ,idx)
				    (with-access::JsObject ,obj (elements)
				       ,(calln %this `(vector-ref elements ,idx) obj args)))
				 ,(loop (cdr cs)))))
			((vtable)
			 ;; vtable method call
			 (cond-expand
			    ((or no-vtable-cache no-vtable-cache-call)
			     (loop (cdr cs)))
			    (else
			     `(with-access::JsConstructMap %cmap (vlen vtable)
				 (let ((vidx (js-pcache-vindex ,ccache)))
				    (if (and (<fx vidx vlen)
					     (procedure? (vector-ref vtable vidx)))
					(begin
					   (js-profile-log-cache ,ccache
					      :vtable #t)
					   ((vector-ref vtable vidx) ,obj ,@args))
					,(loop (cdr cs))))))))
			((vtable-inline)
			 ;; vtable method call
			 (cond-expand
			    ((or no-vtable-cache no-vtable-cache-call)
			     (loop (cdr cs)))
			    (else
			     `(with-access::JsConstructMap %cmap (vlen vtable)
				 (let ((vidx (js-pcache-vindex ,ccache)))
				    (if (and (<fx vidx vlen)
					     (procedure? (vector-ref vtable vidx)))
					(let ((proc (vector-ref vtable vidx)))
					   (js-profile-log-cache ,ccache
					      :vtable #t)
					   (with-access::JsPropertyCache ,ccache (function)
					      (set! function (procedure-attr proc)))
					   (proc ,obj ,@args))
					,(loop (cdr cs))))))))
			(else
			 (error "js-method-jsobject-call-name/cache"
			    "bad cache spec" cs))))))))
   
   (define (expand-cache-specs ccspecs ocspecs %this obj prop args ccache ocache loc)
      (let* ((tmps (map (lambda (a)
			   (match-case a
			      ((uint32->fixnum (? symbol?)) #f)
			      ((int32->fixnum (? symbol?)) #f)
			      ((?- . ?-) (gensym '%a))
			      (else #f)))
		      args))
	     (bindings (filter-map (lambda (t o) (when t (list t o))) tmps args))
	     (nargs (map (lambda (t a) (or t a)) tmps args)))
	 (if (pair? bindings)
	     `(let ,bindings
		 ,(expand-cache-specs/args ccspecs ocspecs %this obj prop nargs ccache ocache loc))
	     (expand-cache-specs/args ccspecs ocspecs %this obj prop nargs ccache ocache loc))))
		 
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-method-jsobject-call-name/cache ?%this (and (? symbol?) ?obj)
	      ?prop ?ccache ?ocache
	      ?loc ((kwote quote) ?ccspecs) ((kwote quote) ?ocspecs)
	      . ?args)
	   (e (expand-cache-specs ccspecs ocspecs %this obj prop args ccache ocache loc)
	      e))
	  ((js-method-jsobject-call-name/cache ?%this ?obj . ?rest)
	   (let ((o (gensym '%o)))
	      (e `(let ((,o ,obj))
		     (js-method-jsobject-call-name/cache %this ,o ,@rest))
		 e)))
	  (else
	   (error "js-method-jsobject-call-name/cache" "bad form" x))))))

;*---------------------------------------------------------------------*/
;*    js-method-non-jsobject-call-name-expander ...                    */
;*---------------------------------------------------------------------*/
(define (js-method-non-jsobject-call-name-expander x e)
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (e `(js-call-methodn ,@(cdr x)) e))
      (else
       (match-case x
	  ((?- ?%this ?obj (& "toString"))
	   (e `(js-tojsstring-safe ,obj ,%this) e))
	  ((?- ?%this ?obj ?prop . ?args)
	   (e `(js-call-methodn ,%this ,obj ,prop ,@args) e))
	  (else
	   (error "js-method-non-jsobject-call-name" "Illegal form" x))))))

;*---------------------------------------------------------------------*/
;*    js-method-call-name/cache-expander ...                           */
;*---------------------------------------------------------------------*/
(define (js-method-call-name/cache-expander x e)

   (define (expand-call %this iso obj name ccache ocache loc cs os args)
      `(if ,iso
	   (js-method-jsobject-call-name/cache ,%this ,obj ,name ,ccache ,ocache ,loc ,cs ,os ,@args)
	   (js-method-non-jsobject-call-name %this ,obj ,name ,@args)))
   
   (define (expand-call/tmp %this obj name ccache ocache loc cs os args)
      (let* ((tmps (map (lambda (a)
			   (match-case a
			      ((uint32->fixnum (? symbol?)) #f)
			      ((int32->fixnum (? symbol?)) #f)
			      ((?- . ?-) (gensym '%a))
			      (else #f)))
		      args))
	     (bindings (filter-map (lambda (t o) (when t (list t o))) tmps args))
	     (nargs (map (lambda (t a) (or t a)) tmps args))
	     (iso (gensym 'isobj)))
	 `(let ((,iso (js-object? ,obj)))
	     ,(if (pair? bindings)
		  `(if (or ,iso (not (or (eq? ,obj (js-undefined)) (null? ,obj))))
		       (let ,bindings
			  ,(expand-call %this iso obj name ccache ocache loc cs os nargs))
		       (js-raise-type-error %this "toObject: cannot convert ~s"
			  ,obj))
		  (expand-call %this iso obj name ccache ocache loc cs os args)))))

   (match-case x
      ((js-method-call-name/cache ?%this (and (? symbol?) ?obj)
	  ?name ?ccache ?ocache ?loc ?cs ?os . ?args)
       (e (expand-call/tmp %this obj name ccache ocache loc cs os args) e))
      ((?- ?%this ?obj . ?rest)
       (let ((o (gensym '%o)))
	  (e `(let ((,o ,obj))
		 (js-method-call-name/cache ,%this ,o ,@rest))
	     e)))
      (else
       (error "js-object-call-name/cache" "wrong form" x))))

;*---------------------------------------------------------------------*/
;*    js-call/cache-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-call/cache-expander x e)
   
   (define (call/tmp %this ccache fun this args)
      (let ((len (length args)))
         `(if (eq? (js-pcache-owner ,ccache) ,fun)
	      (let ((idx (js-pcache-index ,ccache)))
		  ;; this fake entry is used when profiling
		  ;; method calls
		  (js-profile-log-cache ,ccache :pmap #t)
		  (js-profile-log-index idx)
		  ((js-pcache-method ,ccache) ,this ,@args))
              ,(case len
                  ((0 1 2 3 4 5 6 7 8)
                   (let ((caller (symbol-append 'js-call/cache-miss
                                    (string->symbol (integer->string len)))))
                      `(,caller ,%this ,ccache ,fun ,this ,@args)))
                  (else
                   `(if (and (js-function? ,fun)
                             (with-access::JsFunction ,fun (procedure arity)
                                (and (>=fx arity 0)
                                     (correct-arity? procedure ,(+fx len 1)))))
                        (with-access::JsPropertyCache ,ccache (method owner)
                           (with-access::JsFunction ,fun (procedure)
                              (set! owner ,fun)
                              (set! method procedure)
                              (procedure ,this ,@args)))
                        ,(if (>=fx len 11)
                             `(js-calln ,%this ,fun ,this ,@args)
                             `(,(string->symbol (format "js-call~a" len))
			       ,%this ,fun ,this ,@args))))))))

   (define (call %this ccache fun this args)
      (let* ((tmps (map (lambda (a)
			   (match-case a
			      ((uint32->fixnum (? symbol?)) #f)
			      ((int32->fixnum (? symbol?)) #f)
			      ((& ?-) #f)
			      ((?- . ?-) (gensym '%a))
			      (else #f)))
		      args))
	     (bdgs (filter-map (lambda (t o) (when t (list t o))) tmps args)))
	 (if (pair? bdgs)
	     `(let ,bdgs
		 ,(call/tmp %this ccache fun this
		     (map (lambda (t a) (or t a)) tmps args)))
	     (call/tmp %this ccache fun this args))))
   
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-call/cache ?%this ?ccache (and (? symbol?) ?fun) ?this . ?args)
	   (e (call %this ccache fun this args) e))
	  ((js-call/cache ?%this ?ccache ?fun ?this . ?args)
	   (let ((f (gensym '%f)))
	      (e `(let ((,f ,fun))
		     (js-call/cache ,%this ,ccache ,f ,this ,@args))
		 e)))
	  (else
	   (error "js-call/cache" "wrong form" x))))))

;* {*---------------------------------------------------------------------*} */
;* {*    js-call-expander ...                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define (js-call-expander x e)                                      */
;*                                                                     */
;*    (define (call/tmp %this fun this args)                           */
;*       (let ((len (length args)))                                    */
;* 	 (case len                                                     */
;* 	    ((0)                                                       */
;* 	     `(if (and (js-function? ,fun) (=fx (js-function-arity ,fun) 1)) */
;* 		  (with-access::JsFunction ,fun (procedure)            */
;* 		     (procedure ,this ,@args))                         */
;* 		  ((@ js-call0 __hopscript_public)                     */
;* 		   ,%this ,fun ,this ,@args)))                         */
;* 	    ((1)                                                       */
;* 	     `(if (and (js-function? ,fun) (=fx (js-function-arity ,fun) 2)) */
;* 		  (with-access::JsFunction ,fun (procedure)            */
;* 		     (procedure ,this ,@args))                         */
;* 		  ((@ js-call1 __hopscript_public)                     */
;* 		   ,%this ,fun ,this ,@args)))                         */
;* 	    ((2)                                                       */
;* 	     `(if (and (js-function? ,fun) (=fx (js-function-arity ,fun) 3)) */
;* 		  (with-access::JsFunction ,fun (procedure)            */
;* 		     (procedure ,this ,@args))                         */
;* 		  ((@ js-call2 __hopscript_public)                     */
;* 		   ,%this ,fun ,this ,@args)))                         */
;* 	    ((3)                                                       */
;* 	     `(if (and (js-function? ,fun) (=fx (js-function-arity ,fun) 4)) */
;* 		  (with-access::JsFunction ,fun (procedure)            */
;* 		     (procedure ,this ,@args))                         */
;* 		  ((@ js-call3 __hopscript_public)                     */
;* 		   ,%this ,fun ,this ,@args)))                         */
;* 	    ((4)                                                       */
;* 	     `(if (and (js-function? ,fun) (=fx (js-function-arity ,fun) 5)) */
;* 		  (with-access::JsFunction ,fun (procedure)            */
;* 		     (procedure ,this ,@args))                         */
;* 		  ((@ js-call4 __hopscript_public)                     */
;* 		   ,%this ,fun ,this ,@args)))                         */
;* 	    ((5)                                                       */
;* 	     `((@ js-call5 __hopscript_public) ,%this ,fun ,this ,@args)) */
;* 	    ((6)                                                       */
;* 	     `((@ js-call6 __hopscript_public) ,%this ,fun ,this ,@args)) */
;* 	    ((7)                                                       */
;* 	     `((@ js-call7 __hopscript_public) ,%this ,fun ,this ,@args)) */
;* 	    ((8)                                                       */
;* 	     `((@ js-call8 __hopscript_public) ,%this ,fun ,this ,@args)) */
;* 	    ((9)                                                       */
;* 	     `((@ js-call9 __hopscript_public) ,%this ,fun ,this ,@args)) */
;* 	    ((10)                                                      */
;* 	     `((@ js-call10 __hopscript_public) ,%this ,fun ,this ,@args)) */
;* 	    (else                                                      */
;* 	     `((@ js-calln __hopscript_public) ,%this ,fun ,this ,@args))))) */
;*                                                                     */
;*    (define (call %this fun this args)                               */
;*       (let* ((tmps (map (lambda (a)                                 */
;* 			   (match-case a                               */
;* 			      ((uint32->fixnum (? symbol?)) #f)        */
;* 			      ((int32->fixnum (? symbol?)) #f)         */
;* 			      ((& ?-) #f)                              */
;* 			      ((?- . ?-) (gensym '%a))                 */
;* 			      (else #f)))                              */
;* 		      args))                                           */
;* 	     (bdgs (filter-map (lambda (t o) (when t (list t o))) tmps args))) */
;* 	 (if (pair? bdgs)                                              */
;* 	     `(let ,bdgs                                               */
;* 		 ,(call/tmp %this fun this                             */
;* 		     (map (lambda (t a) (or t a)) tmps args)))         */
;* 	     (call/tmp %this fun this args))))                         */
;*                                                                     */
;*    (cond-expand                                                     */
;*       ((or no-macro-cache no-macro-cache-call)                      */
;*        (map (lambda (x) (e x e)) x))                                */
;*       (else                                                         */
;*        (match-case x                                                */
;* 	  ((?- ?%this (and (? symbol?) ?fun) ?this . ?args)            */
;* 	   (e (call %this fun this args) e))                           */
;* 	  ((?- ?%this ?fun ?this . ?args)                              */
;* 	   (let ((f (gensym '%f)))                                     */
;* 	      (e `(let ((,f ,fun)) (js-call ,%this ,f ,this ,@args)) e))) */
;* 	  (else                                                        */
;* 	   (error "js-call" "wrong form" x))))))                       */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    js-pcache-prefetch-index-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-pcache-prefetch-index-expander x olde)
   (match-case x
      ((?- (and ?cache (js-pcache-ref %pcache ?idx)) ?body)
       (let* ((id (gensym '%idx))
	      (ne (lambda (x e)
		     (match-case x
			((js-pcache-index (js-pcache-ref %pcache (? (lambda (i) (eq? i idx)))))
			 id)
			(else
			 (olde x e))))))
	  `(let ((,id ,(olde `(js-pcache-index ,cache) olde)))
	      ,(ne body ne))))
      (else
       (error "js-pcache-prefetch-index" "wrong form" x))))
