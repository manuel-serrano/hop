;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/property_expd.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Mon Mar 27 10:11:42 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HopScript property expanders                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    %define-pcache-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (%define-pcache-expander x e)
   (e `(cond-expand
	  (bigloo-c
	   (static-pragma ,(format "static struct BgL_jspropertycachez00_bgl __bgl_pcache[ ~a ];" (cadr x)))))
      e))

;*---------------------------------------------------------------------*/
;*    js-make-pcache-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-make-pcache-expander x e)
   (e `(cond-expand
	  (bigloo-c
	   ($js-make-pcache (pragma::obj "(obj_t)(__bgl_pcache)")
	       ,(cadr x) (instantiate::JsPropertyCache)))
	  (else
	   ((@ js-make-pache __hopscript_property) ,(cadr x))))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-ref-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-pcache-ref-expander x e)
   (e `(cond-expand
	  (bigloo-c
	   (free-pragma::JsPropertyCache "(&(__bgl_pcache[ $1 ]))" ,(caddr x)))
	  (else
	   ((@ js-pcache-ref __hopscript_property) ,(cadr x) ,(caddr x))))
      e))

;*---------------------------------------------------------------------*/
;*    js-pcache-cmap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-cmap-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::obj "(__bgl_pcache[ $1 ].BgL_cmapz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c) ,(caddr c)) (cmap)
		 cmap)))
	 e)))

;*---------------------------------------------------------------------*/
;*    js-pcache-pmap-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-pcache-pmap-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::obj "(__bgl_pcache[ $1 ].BgL_pmapz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c) ,(caddr c)) (pmap)
		 pmap)))
	 e)))

;*---------------------------------------------------------------------*/
;*    js-pcache-index-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (js-pcache-index-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::long "(__bgl_pcache[ $1 ].BgL_indexz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c) ,(caddr c)) (index)
		 index)))
	 e)))

;*---------------------------------------------------------------------*/
;*    js-pcache-vindex-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-vindex-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::long "(__bgl_pcache[ $1 ].BgL_vindexz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c) ,(caddr c)) (vindex)
		 vindex)))
	 e)))

;*---------------------------------------------------------------------*/
;*    js-pcache-owner-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (js-pcache-owner-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::obj "(__bgl_pcache[ $1 ].BgL_ownerz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c) ,(caddr c)) (owner)
		 owner)))
	 e)))

;*---------------------------------------------------------------------*/
;*    js-pcache-method-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (js-pcache-method-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::obj "(__bgl_pcache[ $1 ].BgL_methodz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c)) (method)
		 method)))
	 e)))

;*---------------------------------------------------------------------*/
;*    js-get-name/cache-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-get-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name) ?cache (and (? symbol?) ?%this))
       (e `(if (isa? ,obj JsObject)
	       (js-object-get-name/cache ,obj ',name ,cache ,%this)
	       (js-get ,obj ',name ,%this))
	  e))
      ((?- ?obj ((kwote quote) ?name) ?cache (and (? symbol?) ?%this))
       (let ((tmp (gensym 'obj)))
	  (e `(let ((,tmp ,obj))
		 (if (isa? ,tmp JsObject)
		     (js-object-get-name/cache ,tmp ',name ,cache ,%this)
		     (js-get ,tmp ',name ,%this)))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-get-name-index/cache-expander ...                             */
;*---------------------------------------------------------------------*/
(define (js-get-name-index/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name) ?idx ?cache (and (? symbol?) ?%this))
       (e `(if (isa? ,obj JsObject)
	       (js-object-get-name-index/cache ,obj ',name ,idx ,cache ,%this)
	       (js-get ,obj ',name ,%this))
	  e))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-match-expander ...                      */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-match-expander x e ref)
   (match-case x
      ((?- (and (? symbol?) ?obj) ?prop (and ?cache (js-pcache-ref %pcache ?ci))
	  ?%this)
       (e `(with-access::JsObject ,obj ((omap cmap) elements)
	      ,(ref obj prop cache %this
		  `(js-pcache-index ,cache)
		  `(js-pcache-cmap ,cache)
		  `(js-pcache-pmap ,cache)
		  `(js-pcache-owner ,cache)))
	  e))
      ((?- (and (? symbol?) ?obj) ?prop ?cache ?%this)
       (e `(with-access::JsObject ,obj ((omap cmap) elements)
	      (with-access::JsPropertyCache ,cache (cmap pmap index owner)
		 ,(ref obj prop cache %this
		     'index 'cmap 'pmap 'owner)))
	  e))
      ((?- ?obj ?prop ?cache ?%this)
       (let ((tmp (gensym 'tmp)))
	  (e `(let ((,tmp ,obj))
		 (with-access::JsObject ,tmp ((omap cmap) elements)
		    (with-access::JsPropertyCache ,cache (cmap pmap index owner)
		       ,(ref tmp prop cache %this
			   'index 'cmap 'pmap 'owner))))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-expander x e)
   
   (define (ref o prop cache %this cindex ccmap cpmap cowner)
      `(let ((%omap omap))
	  (if (eq? ,ccmap %omap)
	      (with-access::JsObject ,o (elements)
		 (vector-ref elements ,cindex))
	      (js-object-get-name/cache-level2 ,@(cdr x)))))
   
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (let ((e1 (cond-expand
		    (cache-level2
		     e)
		    (else
		     (lambda (x e2)
			(match-case x
			   ((js-object-get-name/cache-level2 . ?-)
			    (map (lambda (x) (e x e)) x))
			   (else
			    (e x e2))))))))
	  (js-object-get-name/cache-match-expander x e1 ref))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name/cache-level2-expander ...                     */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-level2-expander x e)
   
   (define (cache-miss-fun prop)
      (match-case prop
	 (((kwote quote) length) 'js-get-name/cache-miss)
	 (else 'js-get-lookup)))
   
   (define (ref o prop cache %this cindex ccmap cpmap cowner)
      `(cond
	  ((eq? ,cpmap %omap)
	   (with-access::JsObject ,cowner (elements)
	      (if (>=fx ,cindex 0)
		  (vector-ref elements ,cindex)
		  (let ((desc (vector-ref elements (-fx (negfx ,cindex) 1))))
		     (js-property-value ,o desc ,%this)))))
	  (else
	   (,(cache-miss-fun prop) ,o ,prop ,cache #f ,%this))))

   (js-object-get-name/cache-match-expander x e ref))

;*---------------------------------------------------------------------*/
;*    js-object-get-name-index/cache-expander ...                      */
;*---------------------------------------------------------------------*/
(define (js-object-get-name-index/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name) ?cache ?%this)
       (e `(with-access::JsObject ,obj ((omap cmap) elements)
	      (with-access::JsPropertyCache ,cache (cmap index)
		 (if (eq? cmap omap)
		     (vector-ref elements index)
		     (js-get-name/cache-miss ,obj ',name ,cache #f ,%this))))
	  e))
      (else
       (map (lambda (x) (e x e)) x))))

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
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-global-object-get-name/cache-expander ...                     */
;*---------------------------------------------------------------------*/
(define (js-global-object-get-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?o) (and (? symbol?) ?name) ?cache ?throw ?%this)
       (e `(with-access::JsObject ,o ((omap cmap) elements)
	      (with-access::JsPropertyCache ,cache (cmap index)
		 (if (eq? cmap omap)
		     (vector-ref elements index)
		     (js-get-lookup ,o ,name ,cache ,throw ,%this))))
	  e))
      (else
       (map (lambda (x) (e x e)) x))))
       
;*---------------------------------------------------------------------*/
;*    js-put-name/cache-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (js-put-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-))
	  (and (or (? symbol?) (? number?)) ?v)
	  ?throw ?cache (and (? symbol?) ?%this))
       (e `(if (isa? ,o JsObject)
	       (js-object-put-name/cache! ,o ,prop ,v ,throw ,cache ,%this)
	       (js-put! ,o ,prop ,v ,throw ,%this))
	  e))
      ((?- ?o (and ?prop ((kwote quote) ?-)) ?v ?throw ?cache (and (? symbol?) ?%this))
       (let ((tmp (gensym 'tmp)))
	  (e `(let ((,tmp ,v))
		 (if (isa? ,o JsObject)
		     (js-object-put-name/cache! ,o ,prop ,tmp ,throw ,cache ,%this)
		     (js-put! ,o ,prop ,tmp ,throw ,%this)))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-match-expander ...                      */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-match-expander x e set err)

   (define (expand/tmp v set)
      (if (or (symbol? v) (number? v) (string? v) (cnst? v))
	  (set v)
	  (let ((tmp (gensym 'tmp)))
	     `(let ((,tmp ,v))
		 ,(set tmp)))))
   
   (match-case x
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	  (and ?cache (js-pcache-ref %pcache ?ci))
	  (and (? symbol?) ?%this))
       (e (expand/tmp v
	     (lambda (tmp)
		`(with-access::JsObject ,o ((omap cmap) elements)
		    ,(set o prop tmp throw cache %this
			`(js-pcache-index ,cache)
			`(js-pcache-cmap ,cache)
			`(js-pcache-pmap ,cache)
			`(js-pcache-owner ,cache)
			`(js-pcache-vindex ,cache)))))
	  e))
      ((?- ?o ?prop ?v ?throw ?cache ?%this)
       (e (expand/tmp v
	     (lambda (tmp)
		`(with-access::JsObject ,o ((omap cmap) elements)
		    (with-access::JsPropertyCache ,cache (cmap pmap index owner vindex)
		       ,(set o prop tmp throw cache %this
			   'index 'cmap 'pmap 'owner 'vindex)))))
	  e))
      (else
       (err x))))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-expander x e)
   
   (define (set o prop tmp throw cache %this cindx ccmap cpmap cowner vindx)
      `(let ((%omap omap))
	  (if (eq? ,ccmap %omap)
	      (begin
		 (vector-set! elements ,cindx ,tmp)
		 ,tmp)
	      (js-object-put-name/cache-level2! ,@(cdr x)))))
   
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (let ((e1 (cond-expand
		    (cache-level2
		     e)
		    (else
		     (lambda (x e2)
			(match-case x
			   ((js-object-put-name/cache-level2! . ?-)
			    (map (lambda (x) (e x e2)) x))
			   (else
			    (e x e2))))))))
	  (js-object-put-name/cache-match-expander x e1 set
	     (lambda (x) (map (lambda (x) (e x e) x)))))))

;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-level2-expander ...                     */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-level2-expander x e)
   
   (define (set o prop tmp throw cache %this cindx ccmap cpmap cowner vindx)
      `(cond
	  ((eq? ,cpmap %omap)
	   (if (>=fx ,cindx 0)
	       (begin
		  ;; there can be no overflow here, as if there one an
		  ;; overfow, it occurs during the previous cache miss
		  (vector-set! elements ,cindx ,tmp)
		  (set! omap ,ccmap)
		  ,tmp)
	       (with-access::JsObject ,cowner (elements)
		  (let ((desc (vector-ref elements (-fx (negfx ,cindx) 1))))
		     (js-property-value-set! ,o desc ,tmp %this)))))
	  ((not %omap)
	   (js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this))
	  (else
	   (with-access::JsConstructMap %omap (vlen vtable)
	      (if (and (<fx ,vindx vlen)
		       (pair? (vector-ref vtable ,vindx)))
		  (let ((indx (car (vector-ref vtable ,vindx)))
			(cmap (cdr (vector-ref vtable ,vindx))))
		     (vector-set! elements indx ,tmp)
		     (set! omap cmap)
		     ,tmp)
		  (js-object-put-name/cache-miss! ,o ,prop ,tmp ,throw
		     ,cache ,%this))))))

   (js-object-put-name/cache-match-expander x e set
      (lambda (x)
	 (error "js-object-put-name/cache-level2" "wrong syntax" x))))
   
;*---------------------------------------------------------------------*/
;*    js-call/cache-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-call/cache-expander x e)
   
   (define (call %this fun ccache this args)
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
	      ,(if (>=fx len 9)
		   `(js-calln ,%this ,fun ,this ,@args)
		   `(begin
		       (,(string->symbol (format "js-call~a" len))
			,%this
			,fun ,this ,@args)))))))

   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (match-case x
	  ((?- ?%this (and (? symbol?) ?fun)
	      (and ?ccache (js-pcache-ref %pcache ?-))
	      ?this . ?args)
	   (e (call %this fun ccache this args) e))
	  ((?- ?%this ?fun
	      (and ?ccache (js-pcache-ref %pcache ?-))
	      ?this . ?args)
	   (let ((f (gensym '%fun)))
	      `(let ((,f ,(e fun e)))
		  ,(e (call %this f ccache this args) e))))
	  (else
	   (error "js-call/cache" "wrong form" x)))))

;*---------------------------------------------------------------------*/
;*    js-method-call-name/cache-expander ...                           */
;*---------------------------------------------------------------------*/
(define (js-method-call-name/cache-expander x e)
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (match-case x
	  ((?- ?%this (and (? symbol?) ?obj) ?prop ?ccache ?ocache . ?args)
	   (e `(if (isa? ,obj JsObject)
		   (js-object-method-call-name/cache ,%this
		      ,obj ,prop ,ccache ,ocache ,@args)
		   (js-raise-type-error %this "call: not a function ~s" ,obj))
	      e))
	  ((?- ?%this ?obj ?prop ?ccache ?ocache . ?args)
	   (let ((o (gensym '%obj)))
	      (e `(let ((,o ,obj))
		     (if (isa? ,o JsObject)
			 (js-object-method-call-name/cache ,%this
			    ,o ,prop ,ccache ,ocache ,@args)
			 (js-raise-type-error %this "call: not a function ~s" ,o)))
		 e)))
	  (else
	   (error "js-object-call-name/cache" "wrong form" x)))))

;*---------------------------------------------------------------------*/
;*    js-method-call-name/cache-match-expander ...                     */
;*---------------------------------------------------------------------*/
(define (js-method-call-name/cache-match-expander x e call)
   (match-case x
      ((?- ?%this
	  (and (? symbol?) ?obj) ((kwote quote) ?name)
	  (and ?ccache (js-pcache-ref %pcache ?-))
	  (and ?ocache (js-pcache-ref %pcache ?-))
	  . ?args)
       (e (call obj name ccache ocache args
	     `(js-pcache-pmap ,ccache)
	     `(js-pcache-method ,ccache)
	     `(js-pcache-vindex ,ccache))
	  e))
      ((?- ?%this (and (? symbol?) ?obj) ?name ?ccache ?ocache . ?args)
       (e `(with-access::JsPropertyCache ,ccache (pmap method vindex)
	      ,(call obj name ccache ocache args 'pmap 'method 'vindex))
	  e))
      ((?- ?%this ?obj ?name ?ccache ?ocache . ?args)
       (let ((this (gensym 'this)))
	  (e `(let ((,this ,obj))
		 (with-access::JsPropertyCache ,ccache (pmap method vindex)
		    ,(call obj name ccache ocache args 'pmap 'method 'vindex)))
	     e)))
      (else
       (error "js-object-call-name/cache" "wrong form" x))))

;*---------------------------------------------------------------------*/
;*    js-object-method-call-name/cache-level2-expander ...             */
;*---------------------------------------------------------------------*/
(define (js-object-method-call-name/cache-level2-expander x e)

   (define (calln m obj args)
      (let* ((len (length args))
	     (call (string->symbol
		      (format "js-call~a" (if (>=fx len 9) "n" len)))))
	 `(,call %this ,m ,obj ,@args)))

   (define (calln/miss obj name ccache ocache args)
      `(js-object-method-call/cache-miss %this ,obj ',name ,ccache ,ocache
	  (list ,@args)))

   (define (call obj name ccache ocache args pmap method vindex)
      `(with-access::JsConstructMap %omap (vtable)
	  (cond
	     ((not %omap)
	      ;; 2. uncachable call
	      (let ((f (js-get ,obj ',name %this)))
		 ,(calln 'f obj args)))
	     (else
	      (with-access::JsConstructMap %omap (vtable vlen)
		 (if (and (<fx ,vindex vlen)
			  (procedure? (vector-ref vtable ,vindex)))
		     ;; polymorphic call
		     ((vector-ref vtable ,vindex)
		      ,obj ,@args)
		     ;; pure cache miss
		     ,(calln/miss obj name ccache ocache args)))))))

   (js-method-call-name/cache-match-expander x e call))
   
;*---------------------------------------------------------------------*/
;*    js-object-method-call-name/cache-expander ...                    */
;*---------------------------------------------------------------------*/
(define (js-object-method-call-name/cache-expander x e)
   
   (define (call obj name ccache ocache args pmap method vindex)
      `(with-access::JsObject ,obj ((omap cmap))
	  (let ((%omap omap))
	     (with-access::JsConstructMap %omap (vtable)
		(if (eq? ,pmap %omap)
		    ;; prototype method invocation
		    (,method ,obj ,@args)
		    (js-object-method-call-name/cache-level2 ,@(cdr x)))))))
   
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (let ((e1 (cond-expand
		    (cache-level2
		     e)
		    (else
		     (lambda (x e2)
			(match-case x
			   ((js-object-call-name/cache-level2! . ?-)
			    (let* ((len (length args))
				   (call (format "js-object-method-call~a/cache-miss"
					    (if (>=fx len 9) "n" len))))
			       `(,call ,@(map (lambda (x) (e x e2)) x))))
			   (else
			    (e x e2))))))))
	  (js-method-call-name/cache-match-expander x e1 call))))

