;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/property_expd.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Tue Sep 14 08:13:00 2021 (serrano)                */
;*    Copyright   :  2016-21 Manuel Serrano                            */
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
						       cntxmap
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
	      ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
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
	      ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	       (free-pragma::JsPropertyCache "(BgL_jspropertycachez00_bglt)BOBJECT(&(__bgl_pcache[ $1 ]))" ,(caddr x)))
	      (else
	       ((@ js-pcache-ref __hopscript_property) ,(cadr x) ,(caddr x))))
	  e))
      (else
       (e `((@ js-pcache-ref __hopscript_property) ,(cadr x) ,(caddr x)) e))))

;*---------------------------------------------------------------------*/
;*    js-pcache-imap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-imap-expander x e)
   (e (match-case x
	 ((js-pcache-imap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_imapz00)" ,idx))
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
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_cmapz00)" ,idx))
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
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_pmapz00)" ,idx))
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
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_nmapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (nmap) nmap))))
	 ((js-pcache-nmap ?c)
	  `(with-access::JsPropertyCache ,c (nmap) nmap))
	 (else
	  (error "js-pcache-nmap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-nextnmap-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (js-pcache-nextnmap-expander x e)
   (e (match-case x
	 ((js-pcache-nextnmap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_nextnmapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (nextnmap) nextnmap))))
	 ((js-pcache-nextnmap ?c)
	  `(with-access::JsPropertyCache ,c (nextnmap) nextnmap))
	 (else
	  (error "js-pcache-nextnmap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-emap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-emap-expander x e)
   (e (match-case x
	 ((js-pcache-emap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_emapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (emap) emap))))
	 ((js-pcache-emap ?c)
	  `(with-access::JsPropertyCache ,c (emap) emap))
	 (else
	  (error "js-pcache-emap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-nextemap-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (js-pcache-nextemap-expander x e)
   (e (match-case x
	 ((js-pcache-nextemap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_nextemapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (nextemap) nextemap))))
	 ((js-pcache-nextemap ?c)
	  `(with-access::JsPropertyCache ,c (nextemap) nextemap))
	 (else
	  (error "js-pcache-nextemap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-amap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-amap-expander x e)
   (e (match-case x
	 ((js-pcache-amap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_amapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (amap) amap))))
	 ((js-pcache-amap ?c)
	  `(with-access::JsPropertyCache ,c (amap) amap))
	 (else
	  (error "js-pcache-amap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-xmap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-xmap-expander x e)
   (e (match-case x
	 ((js-pcache-xmap (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::obj "(obj_t)(__bgl_pcache[ $1 ].BgL_xmapz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (xmap) xmap))))
	 ((js-pcache-xmap ?c)
	  `(with-access::JsPropertyCache ,c (xmap) xmap))
	 (else
	  (error "js-pcache-xmap" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-iindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-iindex-expander x e)
   (e (match-case x
	 ((js-pcache-iindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_iindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (iindex) iindex))))
	 ((js-pcache-iindex ?c)
	  `(with-access::JsPropertyCache ,c (iindex) iindex))
	 (else
	  (error "js-pcache-iindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-eindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-eindex-expander x e)
   (e (match-case x
	 ((js-pcache-eindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_eindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (eindex) eindex))))
	 ((js-pcache-eindex ?c)
	  `(with-access::JsPropertyCache ,c (eindex) eindex))
	 (else
	  (error "js-pcache-eindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-cindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-cindex-expander x e)
   (e (match-case x
	 ((js-pcache-cindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_cindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (cindex) cindex))))
	 ((js-pcache-cindex ?c)
	  `(with-access::JsPropertyCache ,c (cindex) cindex))
	 (else
	  (error "js-pcache-cindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-pindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-pindex-expander x e)
   (e (match-case x
	 ((js-pcache-pindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_pindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (pindex) pindex))))
	 ((js-pcache-pindex ?c)
	  `(with-access::JsPropertyCache ,c (pindex) pindex))
	 (else
	  (error "js-pcache-pindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-nindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-nindex-expander x e)
   (e (match-case x
	 ((js-pcache-nindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_nindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (nindex) nindex))))
	 ((js-pcache-nindex ?c)
	  `(with-access::JsPropertyCache ,c (nindex) nindex))
	 (else
	  (error "js-pcache-nindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-aindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-aindex-expander x e)
   (e (match-case x
	 ((js-pcache-aindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
	      `(free-pragma::long "(__bgl_pcache[ $1 ].BgL_aindexz00)" ,idx))
	     (else
	      `(with-access::JsPropertyCache ,c (aindex) aindex))))
	 ((js-pcache-aindex ?c)
	  `(with-access::JsPropertyCache ,c (aindex) aindex))
	 (else
	  (error "js-pcache-aindex" "bad syntax" x)))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-vindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-vindex-expander x e)
   (e (match-case x
	 ((js-pcache-vindex (and ?c (js-pcache-ref %pcache ?idx)))
	  (cond-expand
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
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
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
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
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
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
	     ((and bigloo-c (not hop-eval) (not hopjs-worker-slave))
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
		`(let ((idx (js-pcache-iindex ,cache)))
		    (js-profile-log-cache ,cache :imap #t)
		    (js-profile-log-index idx)
		    (js-object-inline-ref ,obj idx)))
	       ((eq? cs 'cmap)
		`(let ((idx (js-pcache-cindex ,cache)))
		    (js-profile-log-cache ,cache :cmap #t)
		    (js-profile-log-index idx)
		    (js-object-noinline-ref ,obj idx)))
	       ((eq? cs 'pmap)
		`(let ((idx (js-pcache-pindex ,cache))
		       (own (js-pcache-owner ,cache)))
		    (js-profile-log-cache ,cache :pmap #t)
		    (js-profile-log-index idx)
		    (js-object-ref own idx)))
	       ((eq? cs 'amap)
		`(let* ((idx (js-pcache-aindex ,cache))
			(propowner ,obj))
		    (let ((desc (js-object-ref propowner idx)))
		       (js-profile-log-cache ,cache :amap #t)
		       (js-profile-log-index idx)
		       (js-property-value ,obj
			  propowner ,prop desc ,%this))))
	       ((eq? cs 'xmap)
		;; cached cache miss
		`(js-undefined))
	       ((not (pair? cs))
		(error "js-get-jsobject-name/cache" "bad formx" x))
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
		   ((xmap)
		    `(if (eq? %cmap (js-pcache-xmap ,cache))
			 ,(loop 'xmap)
			 ,(loop (cdr cs))))
		    ;; cached cache miss
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
				      (js-object-ref ,obj idx))
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
	  ((js-get-jsobject-name/cache ?obj ?prop ?throw ?%this ?cache ?loc ((kwote quote) *))
	   (set-car! (last-pair x) ''(imap emap cmap pmap amap xmap vtable))
	   (e x e))
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
;*    js-getprototypeof-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-getprototypeof-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj))
       (e `(if (js-object? ,obj)
	       (js-object-proto ,obj)
	       ((@ js-getprototypeof __hopscript_property) ,obj))
	  e))
      ((?- ?obj)
       (let ((tmp (gensym 'obj)))
	  (e `(let ((,tmp ,obj)) (js-getprototypeof ,tmp)) e)))
      (else
       (map (lambda (x) (e x e)) x))))
       
;*---------------------------------------------------------------------*/
;*    js-has-own-property-expander ...                                 */
;*---------------------------------------------------------------------*/
(define (js-has-own-property-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) (and (? symbol?) ?prop) (and (? symbol?) ?%this))
       (e `(if (js-jsobject? ,obj)
	       ((@ js-has-own-property-jsobject __hopscript_property) ,obj ,prop ,%this)
	       ((@ js-has-own-property __hopscript_property) ,obj ,prop ,%this))
	  e))
      ((?- ?obj ?prop ?%this)
       (let ((tmpo (gensym 'obj))
	     (tmpp (gensym 'prop))
	     (tmpt (gensym '%this)))
	  (e `(let ((,tmpo ,obj)
		    (,tmpp ,prop)
		    (,tmpt ,%this))
		 (js-has-own-property ,tmpo ,tmpp ,tmpt)) e)))
      (else
       (map (lambda (x) (e x e)) x))))
       
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
       (e `(if (js-array? ,o)
	       (js-uint32-tointeger (js-array-length ,o))
	       ((@ js-get-length __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-length ?o ?%this . ?rest)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o)) (js-get-length ,tmp ,%this ,@rest)) e)))
      (else
       (error "js-get-length" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32 ...                                             */
;*---------------------------------------------------------------------*/
(define (js-get-lengthu32-expander x e)
   (match-case x
      ((js-get-lengthu32 (and (? symbol?) ?o) ?%this . ?-)
       (e `(if (js-array? ,o)
	       (js-array-length ,o)
	       ((@ js-get-lengthu32 __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-lengthu32 ?o ?%this . ?rest)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o)) (js-get-length ,tmp ,%this ,@rest)) e)))
      (else
       (error "js-get-lengthu32" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-get-length-maybe-string ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-length-maybe-string-expander x e)
   (match-case x
      ((js-get-length-maybe-string (and (? symbol?) ?o) ?%this . ?-)
       (e `(if (isa? ,o JsStringLiteralASCII)
	       (js-uint32-tointeger (js-jsstring-length ,o))
	       ((@ js-get-length __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-length-maybe-string ?o ?%this . ?rest)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o))
		 (js-get-length-maybe-string ,tmp ,%this ,@rest))
	     e)))
      (else
       (error "js-get-length-maybe-string" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32-maybe-string ...                                */
;*---------------------------------------------------------------------*/
(define (js-get-lengthu32-maybe-string-expander x e)
   (match-case x
      ((js-get-lengthu32-maybe-string (and (? symbol?) ?o) ?%this . ?-)
       (e `(if (isa? ,o JsStringLiteralASCII)
	       (js-jsstring-length ,o)
	       ((@ js-get-lengthu32 __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-lengthu32-maybe-string ?o ?%this . ?rest)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o))
		 (js-get-length-maybe-string ,o ,%this ,@rest))
	     e)))
      (else
       (error "js-get-lengthu32-maybe-string" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-get-length-maybe-arguments ...                                */
;*---------------------------------------------------------------------*/
(define (js-get-length-maybe-arguments-expander x e)
   (match-case x
      ((js-get-length-maybe-arguments (and (? symbol?) ?o) ?%this . ?-)
       (e `(if (isa? ,o JsArgumentsLiteralASCII)
	       (js-uint32-tointeger (js-jsarguments-length ,o))
	       ((@ js-get-length __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-length-maybe-arguments ?o ?%this . ?rest)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o))
		 (js-get-length-maybe-arguments ,tmp ,%this ,@rest))
	     e)))
      (else
       (error "js-get-length-maybe-arguments" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-get-lengthu32-maybe-arguments ...                             */
;*---------------------------------------------------------------------*/
(define (js-get-lengthu32-maybe-arguments-expander x e)
   (match-case x
      ((js-get-lengthu32-maybe-arguments (and (? symbol?) ?o) ?%this . ?-)
       (e `(if (isa? ,o JsArgumentsLiteralASCII)
	       (js-jsarguments-length ,o)
	       ((@ js-get-lengthu32 __hopscript_property) ,@(cdr x)))
	  e))
      ((js-get-lengthu32-maybe-arguments ?o ?%this . ?rest)
       (let ((tmp (gensym)))
	  (e `(let ((,tmp ,o))
		 (js-get-length-maybe-arguments ,o ,%this ,@rest))
	     e)))
      (else
       (error "js-get-lengthu32-maybe-arguments" "bad form" x))))

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
      `(with-access::JsObject ,obj (cmap)
	  (let ((%cmap cmap))
	     ,(let loop ((cs cspecs))
		 (cond
		    ((null? cs)
		     `((@ js-put-proxy-name/cache-miss! __hopscript_proxy)
		       ,obj ,prop ,tmp ,throw ,%this ,cache ,loc ,cachefun))
		    ((eq? cs 'imap)
		     `(let ((idx (js-pcache-iindex ,cache)))
			 (js-profile-log-cache ,cache :imap #t)
			 (js-profile-log-index idx)
			 (js-object-inline-set! ,obj idx ,tmp)
			 ,tmp))
		    ((eq? cs 'emap)
		     `(let ((idx (js-pcache-eindex ,cache)))
			 (js-profile-log-cache ,cache :emap #t)
			 (js-profile-log-index idx)
			  (js-object-inline-set! ,obj idx ,tmp)
			 (set! cmap (js-pcache-nextemap ,cache))
			 ,tmp))
		    ((eq? cs 'cmap)
		     `(let ((idx (js-pcache-cindex ,cache)))
			 (js-profile-log-cache ,cache :cmap #t)
			 (js-profile-log-index idx)
			 (js-object-noinline-set! ,obj idx ,tmp)
			 ,tmp))
		    ((eq? cs 'nmap)
		     `(let ((idx (js-pcache-nindex ,cache)))
			 (js-profile-log-cache ,cache :nmap #t)
			 (js-profile-log-index idx)
			 (js-object-cmap-push! ,obj idx ,tmp
			    (js-pcache-nextnmap ,cache))
			 ,tmp))
		    ((eq? cs 'amap)
		     `(let* ((idx (js-pcache-aindex ,cache))
			     (propowner ,obj))
			 (let ((desc (js-object-ref propowner idx)))
			    (js-profile-log-cache ,cache :amap #t)
			    (js-profile-log-index idx)
			    (js-property-value-set! ,obj
			       propowner ,prop desc ,tmp %this))
			 ,tmp))
		    ((not (pair? cs))
		     (error "js-put-jsobject-name/cache" "bad form" x))
		    (else
		     (case (car cs)
			((imap-incache)
			 (loop 'imap))
			((cmap-incache)
			 (loop 'cmap))
			((imap imap+)
			 ;; direct property put
			 `(if (eq? %cmap (js-pcache-imap ,cache))
			      ,(loop 'imap)
			      ,(if (eq? (car cs) 'imap)
				   (loop (cdr cs))
				   `((@ js-put-jsobject-name/cache-imap+!
					__hopscript_property)
				     ,obj ,prop ,tmp ,throw ,%this
				     ,cache ,loc ',cspecs))))
			((emap)
			 ;; direct property put
			 `(if (eq? %cmap (js-pcache-emap ,cache))
			      ,(loop 'emap)
			      ,(loop (cdr cs))))
			((cmap cmap+)
			 ;; direct property put
			 `(if (eq? %cmap (js-pcache-cmap ,cache))
			      ,(loop 'cmap)
			      ,(if (eq? (car cs) 'cmap)
				   (loop (cdr cs))
				   `((@ js-put-jsobject-name/cache-cmap+!
					__hopscript_property)
				     ,obj ,prop ,tmp ,throw ,%this
				     ,cache ,loc ',cspecs))))
			((nmap nmap+)
			 ;; prototype property put
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
			 ;; accessor property put
			 `(if (eq? %cmap (js-pcache-amap ,cache))
			      ,(loop 'amap)
			      ,(loop (cdr cs))))
			((vtable)
			 ;; vtable property put
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
					   (js-object-vtable-push! ,obj idx ,tmp ncmap)
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
	 `(,call ,%this ,m ,obj ,@(if (>=fx len 11) `((list ,@args)) args))))
   
   (define (calln-uncachable %this ocspecs obj prop args ccache ocache loc)
      (let ((f (gensym 'f)))
	 `(let ((,f (js-get-jsobject-name/cache ,obj ,prop #f ,%this ,ocache ,loc ',ocspecs)))
	     ,(calln %this f obj args))))

   (define (calln-miss %this obj prop args ccache ocache loc cspecs ospecs)
      (if (pair? args)
	  `(js-call-with-stack-list (list ,@args)
	      (lambda (l)
		 (js-method-jsobject-call/cache-miss %this ,obj ,prop
		    l ,ccache ,ocache ,loc ',cspecs ',ospecs)))
	  `(js-method-jsobject-call/cache-miss %this ,obj ,prop
	      '() ,ccache ,ocache ,loc ',cspecs ',ospecs)))
   
   (define (expand-cache-specs/args ccspecs ocspecs %this obj prop args ccache ocache loc)
      `(with-access::JsObject ,obj (cmap)
	  (let ((%cmap cmap))
	     ,(let loop ((cs ccspecs))
		 (if (null? cs)
		     (if (or (memq 'pmap ccspecs) (memq 'pmap-inline ccspecs))
			 `(if (eq? (js-pcache-cmap ,ccache) (js-uncachable-pmap))
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
				 (let ((,idx (js-pcache-cindex ,ccache)))
				    (js-profile-log-cache ,ccache :cmap #t)
				    (js-profile-log-index ,idx)
				    ,(calln %this `(js-object-ref ,obj ,idx) obj args))
				 ,(loop (cdr cs)))))
			((poly)
			 `(with-access::JsPropertyCache ,ccache (cntmiss)
			     (if (>u32 cntmiss #u32:1024)
				 ,(calln-uncachable %this ocspecs obj prop args ccache ocache loc)
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
		     (js-method-jsobject-call-name/cache ,%this ,o ,@rest))
		 e)))
	  (else
	   (error "js-method-jsobject-call-name/cache" "bad form" x))))))

;*---------------------------------------------------------------------*/
;*    js-method-jsrecord-call-index-expander ...                       */
;*    -------------------------------------------------------------    */
;*    Use to call record methods                                       */
;*---------------------------------------------------------------------*/
(define (js-method-jsrecord-call-index-expander x e)
   (match-case x
      ((js-method-jsrecord-call-index
	  (and (? symbol?) ?obj) (and (? fixnum?) ?index) . ?rest)
       (e `(with-access::JsObject ,obj (cmap)
	      (with-access::JsConstructMap cmap (mptable)
		 ((vector-ref mptable ,index) ,obj ,@rest)))
	  e))
      ((js-method-jsrecord-call-index
	  (and (? symbol?) ?obj) (and (? fixnum?) ?index) . ?rest)
       (let ((o (gensym '%o)))
	  (e `(let ((,o ,obj))
		 (js-method-jsrecord-call-index ,o ,index ,@rest))
	     e)))
      (else
       (error "js-method-jsrecord-call-index/expander" "bad form" x))))
   
;*---------------------------------------------------------------------*/
;*    js-method-non-jsobject-call-name-expander ...                    */
;*---------------------------------------------------------------------*/
(define (js-method-non-jsobject-call-name-expander x e)
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (e `(js-call-methodn ,@(cdr x)) e))
      (else
       (match-case x
	  ((?- ?%this ?obj (& "toString" . ?-))
	   (e `(js-tojsstring-safe ,obj ,%this) e))
	  ((?- ?%this ?obj ?prop)
	   (e `(js-call-method0 ,%this ,obj ,prop) e))
	  ((?- ?%this ?obj (& "toString" . ?-) ?a0)
	   (e `(js-tojsstring1-safe ,obj ,a0 ,%this) e))
	  ((?- ?%this ?obj ?prop ?a0)
	   (e `(js-call-method1 ,%this ,obj ,prop ,a0) e))
	  ((?- ?%this ?obj ?prop ?a0 ?a1)
	   (e `(js-call-method2 ,%this ,obj ,prop ,a0 ,a1) e))
	  ((?- ?%this ?obj ?prop ?a0 ?a1 ?a2)
	   (e `(js-call-method3 ,%this ,obj ,prop ,a0 ,a1 ,a2) e))
	  ((?- ?%this ?obj ?prop ?a0 ?a1 ?a2 ?a3)
	   (e `(js-call-method4 ,%this ,obj ,prop ,a0 ,a1 ,a2 ,a3) e))
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
		       (js-raise-type-error/loc %this ,loc
			  ,(format "toObject: cannot convert ~~s (~a)" name)
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
	      (let ((idx (js-pcache-cindex ,ccache)))
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
                             `(js-calln ,%this ,fun ,this (list ,@args))
                             `(,(string->symbol (format "js-call~a" len))
			       ,%this ,fun ,this ,@args))))))))

   (define (call %this ccache fun this args)
      (let* ((tmps (map (lambda (a)
			   (match-case a
			      ((uint32->fixnum (? symbol?)) #f)
			      ((int32->fixnum (? symbol?)) #f)
			      ((& ?- . ?-) #f)
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

;*---------------------------------------------------------------------*/
;*    js-call-with-stack-list ...                                      */
;*    -------------------------------------------------------------    */
;*    List stack allocation (when supported by the back-end).          */
;*---------------------------------------------------------------------*/
(define-macro (js-call-with-stack-list lst proc)
   (match-case lst
      ((list . ?args)
       (match-case proc
	  ((lambda (?l) . ?body)
	   (cond-expand
	      ((and bigloo-c (config have-c99-stack-alloc #t) (not devel) (not debug))
	       (let ((stk (gensym 'stk))
		     (p (gensym 'p))
		     (aux (gensym 'aux))
		     (len (length args)))
		  `(let ()
		      (pragma ,(format "char ~a[ PAIR_SIZE * ~a ]; obj_t ~a = BNIL, ~a;"
				  stk len p aux))
		      ,@(map (lambda (v i)
				`(begin
				    (pragma ,(format "~a = BPAIR(&(~a[~a * PAIR_SIZE ])); SET_CDR( ~a, ~a ); ~a = ~a" aux stk i aux p p aux))
				    (set-car! (pragma::pair ,(symbol->string p)) ,v)))
			   (reverse args)
			   (iota len))
		      (let ((,l (pragma::pair-nil ,(symbol->string p))))
			 ,@body))))
	      (else
	       `(,proc ,lst))))
	  (else
	   (error "js-call-with-stack-list" "bad form"
	      `(js-call-with-stack-list ,lst ,proc)))))
      (else
       `((@ js-call-with-stack-list __hopscript_property) ,lst ,proc))))

