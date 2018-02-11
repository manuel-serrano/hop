;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/property_expd.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Sun Feb 11 21:57:58 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and property.sch                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-profile-log-cache-expander ...                                */
;*---------------------------------------------------------------------*/
(define (js-profile-log-cache-expander x e)
   (cond-expand
      (profile
       (match-case x
	  ((js-profile-log-cache ?cache . ?cspecs)
	   (e `(with-access::JsPropertyCache ,cache (cntcmap cntpmap
						       cntamap cntvtable)
		  ,@(filter-map (lambda (c)
				   (when (keyword? c)
				      (let ((id (symbol-append 'cnt
						   (string->symbol
						      (keyword->string c)))))
					 `(set! ,id (+fx 1 ,id)))))
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
;*    js-make-pcache-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-pcache-expander x e)
   (match-case x
      ((?- (and (? integer?) ?num))
       (e `(cond-expand
	     ((and bigloo-c (not hopjs-worker-slave))
	      ($js-make-pcache (pragma::obj "(obj_t)(__bgl_pcache)")
		 ,num (instantiate::JsPropertyCache)))
	     (else
	      ((@ js-make-pcache __hopscript_property) ,num)))
	  e))
      (else
       (error "js-make-pcache" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-pcache-ref-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-pcache-ref-expander x e)
   (e `(cond-expand
	  ((and bigloo-c (not hopjs-worker-slave))
	   (free-pragma::JsPropertyCache "(BgL_jspropertycachez00_bglt)BOBJECT(&(__bgl_pcache[ $1 ]))" ,(caddr x)))
	  (else
	   ((@ js-pcache-ref __hopscript_property) ,(cadr x) ,(caddr x))))
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
;*    js-object-get-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-expander x e)

   (define (cache-miss-fun prop)
      (match-case prop
	 (((kwote quote) length)
	  '(@ js-get-name/cache-miss __hopscript_property))
	 (else
	  '(@ js-get-lookup __hopscript_property))))
   
   (define (expand-cache-specs cspecs obj prop throw %this cache loc)
      `(with-access::JsObject ,obj (cmap elements)
	  (let ((%cmap cmap))
	     ,(let loop ((cs cspecs)
			 (inpmap #f))
		 (if (null? cs)
		     ;; cache miss
		     `(,(cache-miss-fun prop)
		       ,obj ,prop ,throw ,%this ,cache ,loc ',cspecs)
		     (case (car cs)
			((cmap)
			 ;; direct property get
			 `(if (eq? %cmap (js-pcache-cmap ,cache))
			      (let ((idx (js-pcache-index ,cache)))
				 (js-profile-log-cache ,cache 'cmap #t)
				 (js-profile-log-index idx)
				 (vector-ref elements idx))
			      ,(loop (cdr cs) #f)))
			((pmap pmap+)
			 ;; prototype property get
			 (if inpmap
			     `(with-access::JsObject (js-pcache-owner ,cache) (elements)
				 (js-profile-log-cache ,cache :pmap #t)
				 (js-profile-log-index idx)
				 (vector-ref elements idx))
			     `(if (eq? %cmap (js-pcache-pmap ,cache))
				  (let ((idx (js-pcache-index ,cache)))
				     (if (>=fx idx 0)
					 ,(loop cs #t)
					 ,(loop (cdr cs) #f))))))
			((amap amap+)
			 ;; accessor property get
			 (if inpmap
			     `(with-access::JsObject (js-pcache-owner ,cache) (elements)
				 (let* ((idx (-fx (negfx idx) 1))
					(desc (vector-ref elements idx)))
				    (js-profile-log-cache ,cache :amap #t)
				    (js-profile-log-index idx)
				    (js-property-value ,obj desc ,%this)))
			     `(if (eq? %cmap (js-pcache-pmap ,cache))
				  (let ((idx (js-pcache-index ,cache)))
				     (if (<fx idx 0)
					 ,(loop cs #t)
					 ,(loop (cdr cs) #f))))))
			((vtable)
			 ;; vtable property get
			 (cond-expand
			    (no-vtable-cache
			     (loop (cdr cs) inpmap))
			    (else
			     `(with-access::JsConstructMap %cmap (vlen vtable %id)
				 (let ((vidx (js-pcache-vindex ,cache)))
				    (if (and (<fx vidx vlen) (fixnum? vidx))
					(let ((idx (vector-ref vtable vidx)))
					   (js-profile-log-cache ,cache
					      :vtable #t)
					   (js-profile-log-index idx)
					   (vector-ref elements idx))
					,(loop (cdr cs) #f)))))))
			((global)
			 `((@ js-global-object-get-name/cache __hopscript_property)
			   ,obj ,prop ,throw ,%this
			   ,cache ,loc ',cspecs))
			(else
			 (error "js-object-get-name/cache" "bad cache spec"
			    cs))))))))
      
   (cond-expand
      ((or no-macro-cache no-macro-cache-get)
       (map (lambda (e x e)) x))
      (else
       (match-case x
	  ((js-object-get-name/cache (and (? (lambda (o) (not (symbol? o)))) ?obj) . ?rest)
	   (let ((o (gensym '%o)))
	      (e `(let ((,o ,obj)) (js-object-get-name/cache ,o ,@rest)) e)))
	  ((js-object-get-name/cache (and (? symbol?) ?obj)
	      ?prop ?throw ?%this ?cache
	      ?loc ((kwote quote) ?cspecs))
	   (e (expand-cache-specs cspecs obj prop throw %this cache loc) e))
	  ((js-object-get-name/cache ?obj ?prop ?throw ?%this ?cache ?loc (? symbol?))
	   (set-car! (last-pair x) ''(cmap pmap amap vtable))
	   (e x e))
	  ((js-object-get-name/cache ?obj ?prop ?throw ?%this ?cache ?loc)
	   (e (append x '('(cmap pmap amap vtable))) e))
	  ((js-object-get-name/cache ?obj ?prop ?throw ?%this ?cache)
	   (e (append x '(-1 '(cmap pmap amap vtable))) e))
	  (else
	   (error "js-object-get-name/cache" "bad form" x))))))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) ?prop ?throw ?%this . ?rest)
       (e `(if (js-object? ,obj)
	       (js-object-get-name/cache ,obj ,prop ,throw ,%this ,@rest)
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
;*    js-global-object-get-name-expander ...                           */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?o) ?name ?throw ?%this)
       (e `(let ((pval (js-get-property-value ,o ,o ,name ,%this)))
	      (if (eq? pval (js-absent))
		  (js-get-notfound ,name ,throw ,%this)
		  pval))
	  e))
      (else
       (error "js-global-object-get-name" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name/cache-expander ...                     */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name/cache-expander x e)
   (match-case x
      ((js-global-object-get-name/cache ?obj ?prop ?throw ?%this ?cache)
       (e `(js-object-get-name/cache ,obj ,prop ,throw ,%this
	      ,cache -1 '(cmap global))
	  e))
      ((js-global-object-get-name/cache ?obj ?prop ?throw ?%this ?cache ?loc)
       (e `(js-object-get-name/cache ,obj ,prop ,throw ,%this
	      ,cache ,loc '(cmap global))
	  e))
      ((js-global-object-get-name/cache ?obj ?prop ?throw ?%this ?cache ?loc ?cs)
       (e `(js-object-get-name/cache ,obj ,prop ,throw ,%this
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
;*    js-object-put-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-expander x e)

   (define (expand-tmp e val proc)
      (if (or (symbol? val) (number? val) (string? val) (cnst? val))
	  (e (proc val) e)
	  (let ((tmp (gensym 'v)))
	     `(let ((,tmp ,(e val e)))
		 ,(e (proc tmp) e)))))
      
   (define (expand-cache-specs cspecs obj prop tmp throw %this cache loc)
      `(with-access::JsObject ,obj (cmap elements)
	  (let ((%cmap cmap))
	     ,(let loop ((cs cspecs)
			 (inpmap #f))
		 (if (null? cs)
		     `((@ js-object-put-name/cache-miss! __hopscript_property)
		       ,obj ,prop ,tmp ,throw ,%this
		       ,cache ,loc ',cspecs)
		     (case (car cs)
			((cmap)
			 ;; directory property set
			 `(if (eq? %cmap (js-pcache-cmap ,cache))
			      (let ((idx (js-pcache-index ,cache)))
				 (js-profile-log-cache ,cache :cmap #t)
				 (js-profile-log-index idx)
				 (vector-set! elements idx ,tmp)
				 ,tmp)
			      ,(loop (cdr cs) #f)))
			((pmap)
			 ;; prototype property set
			 (if inpmap
			     `(let ((%vec elements))
				 (js-profile-log-index idx)
				 (if (<fx idx (vector-length %vec))
				     (begin
					(js-profile-log-cache ,cache :pmap #t)
					(vector-set! %vec idx ,tmp))
				     (js-object-add! ,obj idx ,tmp))
				 (set! cmap
				    (if (eq? (js-pcache-cmap ,cache) #t)
					(js-pcache-pmap ,cache)
					(js-pcache-pmap ,cache)))
				 ,tmp)
			     `(if (eq? %cmap (js-pcache-pmap ,cache))
				  (let ((idx (js-pcache-index ,cache)))
				     (if (>=fx idx 0)
					 ,(loop cs #t)
					 ,(loop (cdr cs) #f))))))
			((amap)
			 ;; accessor property set
			 (if inpmap
			     `(with-access::JsObject (js-pcache-owner ,cache) (elements)
				 (let* ((idx (-fx (negfx idx) 1))
					(desc (vector-ref elements idx)))
				    (js-profile-log-cache ,cache :amap #t)
				    (js-profile-log-index idx)
				    (js-property-value-set! ,obj desc ,tmp %this)
				    ,tmp))
			     `(if (eq? %cmap (js-pcache-pmap ,cache))
				  (let ((idx (js-pcache-index ,cache)))
				     (if (<fx idx 0)
					 ,(loop cs #t)
					 ,(loop (cdr cs) #f))))))
			((vtable)
			 ;; vtable property set
			 (cond-expand
			    (no-vtable-cache
			     (loop (cdr cs) inpmap))
			    (else
			     `(with-access::JsConstructMap %cmap (vlen vtable)
				 (let ((vidx (js-pcache-vindex ,cache)))
				    (if (and (<fx vidx vlen)
					     (pair? (vector-ref vtable vidx)))
					(let ((indx (car (vector-ref vtable vidx)))
					      (cmap (cdr (vector-ref vtable vidx))))
					   (js-profile-log-cache ,cache :vtable #t)
					   (js-object-push! ,obj idx ,tmp)
					   (set! %cmap cmap)
					   ,tmp)
					,(loop (cdr cs) #f)))))))
			(else
			 (error "js-object-put-name/cache" "bad cache spec"
			    cs))))))))
				  
   (cond-expand
      ((or no-macro-cache no-macro-cache-put)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-object-put-name/cache! (and (? (lambda (o) (not (symbol? o)))) ?obj) . ?rest)
	   (let ((o (gensym '%o)))
	      (e `(let ((,o ,obj)) (js-object-put-name/cache! ,o ,@rest)) e)))
	  ((js-object-put-name/cache! (and (? symbol?) ?obj)
	      ?prop ?val ?throw ?%this
	      ?cache ?loc ((kwote quote) ?cspecs))
	   (expand-tmp e val
	      (lambda (tmp)
		 (expand-cache-specs cspecs obj prop tmp throw %this cache loc))))
	  ((js-object-put-name/cache! ?obj ?prop ?val ?throw ?%this ?cache ?loc (? symbol?))
	   (set-car! (last-pair x) ''(cmap pmap amap vtable))
	   (e x e))
	  ((js-object-put-name/cache! ?obj ?prop ?val ?throw ?%this ?cache)
	   (e (append x '(-1 '(cmap pmap amap vtable))) e))
	  (else
	   (error "js-object-put-name/cache!" "bad form" x))))))
	      
;*---------------------------------------------------------------------*/
;*    js-put-name/cache-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache-expander x e)
   (match-case x
      ((js-put-name/cache! (and (? symbol?) ?o)
	  ?prop ?val ?throw ?%this . ?rest)
       (e `(if (js-object? ,o)
	       (js-object-put-name/cache! ,o ,prop ,val ,throw ,%this ,@rest)
	       (js-put! ,o ,prop ,val ,throw ,%this))
	  e))
      ((js-put-name/cache! ?obj
	  ?prop ?val ?throw ?%this . ?rest)
       (let ((tmp (gensym 'o)))
	  (e `(let ((,tmp ,obj))
		 (if (js-object? ,tmp)
		     (js-object-put-name/cache! ,tmp ,prop ,val ,throw ,%this
			,@rest)
		     (js-put! ,tmp ,prop ,val ,throw ,%this)))
	     e)))
      (else
       (error "js-put-name/cache!" "bad form" x))))
   
;*---------------------------------------------------------------------*/
;*    js-object-method-call-name/cache-expander ...                    */
;*---------------------------------------------------------------------*/
(define (js-object-method-call-name/cache-expander x e)

   (define (calln %this m obj args)
      (let* ((len (length args))
	     (call (string->symbol
		      (format "js-call~a" (if (>=fx len 11) "n" len)))))
	 `(,call ,%this ,m ,obj ,@args)))
   
   (define (calln-miss %this obj prop args ccache ocache loc cspecs)
      `(js-object-method-call/cache-miss %this ,obj prop (list ,@args)
	  ,ccache ,ocache ,loc ,cspecs))

   (define (calln-uncachable %this obj prop args ccache ocache loc)
      `(let ((f (js-object-get-name/cache ,obj ,prop #f ,%this ,ocache ,loc '())))
	  (js-profile-log-cache ,ccache :cmap #t)
	  ,(calln %this 'f obj args)))
   
   (define (expand-cache-specs cspecs %this obj prop args ccache ocache loc)
      `(with-access::JsObject ,obj (cmap)
	  (let ((%cmap cmap))
	     ,(let loop ((cs cspecs))
		 (if (null? cs)
		     `(if (eq? %cmap #t)
			  ,(calln-uncachable %this obj prop args ccache ocache loc)
			  ,(calln-miss %this obj prop args ccache ocache loc cspecs)))
		 (case (car cs)
		    ((pmap)
		     `(if (eq? %cmap (js-pcache-pmap ,ccache))
			  (begin
			     (js-profile-log-cache ,ccache :pmap #t)
			     ((js-pcache-method ,ccache) ,obj ,@args))
			  ,(loop (cdr cs))))
		    ((vtable)
		     (cond-expand
			(no-vtable-cache 
			 (loop (cdr cs)))
			(else
			 `(with-access::JsConstructMap %omap (vlen vtable)
			     (let ((vidx (js-pcache-vindex ,cache)))
				(if (and (<fx vidx vlen)
					 (procedure? (vector-ref vtable vidx)))
				    (begin
				       (js-profile-log ,cache :vtable #t)
				       ((vector-ref vtable vix) ,obj ,@args))
				    ,(loop (cdr cs))))))))
		    (else
		     (error "js-object-method-call-name/cache"
			"bad cache spec" cs)))))))
		 
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-object-method-call-name/cache ?%this (and (? symbol?) ?obj)
	      ?prop
	      (and ?ccache (js-pcache-ref %pcache ?-))
	      (and ?ocache (js-pcache-ref %pcache ?-))
	      ?loc ((kwote quote) ?cspecs)
	      . ?args)
	   (e (expand-cache-specs cspecs %this obj prop args ccache ocache loc)
	      e))
	  ((js-object-method-call-name/cache ?%this ?obj . ?rest)
	   (let ((o (gensym '%o)))
	      (e `(let ((,o ,obj))
		     (js-object-method-call-name/cache ?%this ,o ,@rest))
		 e)))
	  (else
	   (error "js-object-method-call-name/cache" "bad form" x))))))

;*---------------------------------------------------------------------*/
;*    js-non-object-method-call-name-expander ...                      */
;*---------------------------------------------------------------------*/
(define (js-non-object-method-call-name-expander x e)
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (e `(js-call-methodn ,@(cdr x)) e))
      (else
       (match-case x
	  ((?- ?%this ?obj ((kwote quote) toString))
	   (e `(js-tostring ,obj ,%this) e))
	  ((?- ?%this ?obj ?prop . ?args)
	   (e `(js-call-methodn ,%this ,obj ,prop ,@args) e))
	  (else
	   (error "js-non-object-method-call-name" "Illegal form" x))))))

;*---------------------------------------------------------------------*/
;*    js-method-call-name/cache-expander ...                           */
;*---------------------------------------------------------------------*/
(define (js-method-call-name/cache-expander x e)
   (match-case x
      ((js-method-call-name/cache ?%this (and (? symbol?) ?obj) . ?rest)
       (e `(if (js-object? ,obj)
	       (js-object-method-call-name/cache ,%this ,obj ,@rest)
	       (js-non-object-method-call-name %this ,obj ,@rest))
	  e))
      ((?- ?%this ?obj . ?rest)
       (let ((o (gensym '%o)))
	  (e `(let ((,o ,obj))
		 (js-method-call-name/cache-expander ,%this ,o ,@rest))
	     e)))
      (else
       (error "js-object-call-name/cache" "wrong form" x))))

;*---------------------------------------------------------------------*/
;*    js-call/cache-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-call/cache-expander x e)
   
   (define (call %this fun this ccache args)
      (let ((len (length args)))
	 `(cond
	     ((eq? (js-pcache-owner ,ccache) ,fun)
	      ((js-pcache-method ,ccache) ,this ,@args))
	     ((and (isa? ,fun JsFunction)
		   (with-access::JsFunction ,fun (len)
		      (=fx len ,len)))
	      (with-access::JsPropertyCache ,ccache (method cmap)
		 (with-access::JsFunction ,fun (procedure)
		    (set! cmap ,fun)
		    (set! method procedure)
		    (procedure ,this ,@args))))
	     (else
	      ,(if (>=fx len 11)
		   `(js-calln ,%this ,fun ,this ,@args)
		   `(begin
		       (,(string->symbol (format "js-call~a" len))
			,%this
			,fun ,this ,@args)))))))
   
   (cond-expand
      ((or no-macro-cache no-macro-cache-call)
       (map (lambda (x) (e x e)) x))
      (else
       (match-case x
	  ((js-call/cache ?%this (and (? symbol?) ?fun) ?this
	      (and ?ccache (js-pcache-ref %pcache ?-))
	      . ?args)
	   (e (call %this fun this ccache args) e))
	  ((js-call/cache ?%this ?fun ?this
	      (and ?ccache (js-pcache-ref %pcache ?-))
	      . ?args)
	   (let ((f (gensym '%f)))
	      (e `(let ((,f ,fun))
		     (js-call/cache ,%this ,f ,this ,ccache ,@args))
		 e)))
	  (else
	   (error "js-call/cache" "wrong form" x))))))



