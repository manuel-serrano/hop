;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/property_expd.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Tue Feb 28 07:30:31 2017 (serrano)                */
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
	   ($js-make-pache (pragma::obj "(obj_t)(__bgl_pcache)")
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
;*    js-pcache-proto-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (js-pcache-proto-expander x e)
   (let ((c (cadr x)))
      (e `(cond-expand
	     (bigloo-c
	      (free-pragma::obj "(__bgl_pcache[ $1 ].BgL_protoz00)" ,(caddr c)))
	     (else
	      (with-access::JsPropertyCache ((@ js-pcache-ref __hopscript_property) ,(cadr c) ,(caddr c)) (proto)
		 proto)))
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
;*    js-object-get-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-expander x e)
   
   (define (cache-miss-fun propname)
      (if (eq? propname 'length)
	  'js-get-name/cache-miss
	  'js-get-symbol-name/cache-miss))
   
   (match-case x
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name)
	  (and ?cache (js-pcache-ref %pcache ?ci))
	  ?%this)
       (e `(with-access::JsObject ,obj ((omap cmap) elements __proto__)
	      (let ((%omap omap))
		 (cond
		    ((eq? (js-pcache-cmap ,cache) %omap)
		     (vector-ref-ur elements (js-pcache-index ,cache)))
		    ((and (not (null? __proto__))
			  (with-access::JsObject __proto__ ((%pmap cmap))
			     (eq? (js-pcache-cmap ,cache) %pmap)))
		     (with-access::JsObject __proto__ (elements)
			(vector-ref-ur elements (js-pcache-index ,cache))))
		    (else
		     (,(cache-miss-fun name) ,obj ',name ,cache #f ,%this)))))
	  e))
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name) ?cache ?%this)
       (e `(with-access::JsObject ,obj ((omap cmap) elements __proto__)
	      (with-access::JsPropertyCache ,cache (cmap pmap index)
		 (let ((%omap omap)
		       (%cmap cmap))
		    (cond
		       ((eq? %cmap %omap)
			(vector-ref-ur elements index))
		       ((and (not (null? __proto__))
			     (with-access::JsObject __proto__ ((%pmap cmap))
				(eq? %cmap %pmap)))
		       (with-access::JsObject __proto__ (elements)
			  (vector-ref-ur elements index)))
		       (else
			(,(cache-miss-fun name) ,obj ',name ,cache #f ,%this))))))
	  e))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-this-get-name/cache-expander ...                              */
;*---------------------------------------------------------------------*/
(define (js-this-get-name/cache-expander x e)

   (define (cache-miss-fun propname)
      (if (eq? propname 'length)
	  'js-get-name/cache-miss
	  'js-get-symbol-name/cache-miss))
   
   (match-case x
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name)
	  (and ?cache (js-pcache-ref %pcache ?ci))
	  ?%this)
       (e `(cond
	      ((eq? (js-pcache-cmap ,cache) %thismap)
	       (vector-ref-ur %thiselements (js-pcache-index ,cache)))
	      ((eq? (js-pcache-pmap ,cache) %thismap)
	       (with-access::JsObject ,obj (__proto__)
		  (with-access::JsObject __proto__ (elements)
		     (vector-ref-ur elements (js-pcache-index ,cache)))))
	      (else
	       (,(cache-miss-fun name) ,obj ',name ,cache #f ,%this)))
	  e))
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name) ?cache ?%this)
       (e `(with-access::JsPropertyCache ,cache (cmap pmap index)
	      (cond
		 ((eq? cmap %thismap)
		  (vector-ref-ur %thiselements index))
		 ((eq? pmap %thismap)
		  (with-access::JsObject ,obj (__proto__)
		     (with-access::JsObject __proto__ (elements)
			(vector-ref-ur elements index))))
		 (else
		  (,(cache-miss-fun name) ,obj ',name ,cache #f ,%this))))
	  e))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-object-get-name-index/cache-expander ...                      */
;*---------------------------------------------------------------------*/
(define (js-object-get-name-index/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?obj) ((kwote quote) ?name) ?cache ?%this)
       (e `(with-access::JsObject ,obj ((omap cmap))
	      (with-access::JsPropertyCache ,cache (cmap index)
		 (if (eq? cmap omap)
		     (js-object-element-ref ,obj index)
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
       (e `(with-access::JsObject ,o ((omap cmap))
	      (with-access::JsPropertyCache ,cache (cmap index)
		 (if (eq? cmap omap)
		     (js-object-element-ref ,o index)
		     (js-get-symbol-name/cache-miss ,o ,name ,cache ,throw ,%this))))
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
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw ?cache (and (? symbol?) ?%this))
       (let ((tmp (gensym 'tmp)))
	  (e `(let ((,tmp ,v))
		 (if (isa? ,o JsObject)
		     (js-object-put-name/cache! ,o ,prop ,tmp ,throw ,cache ,%this)
		     (js-put! ,o ,prop ,tmp ,throw ,%this)))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))
	  
;*---------------------------------------------------------------------*/
;*    js-object-put-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-put-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	  (and ?cache (js-pcache-ref %pcache ?ci))
	  (and (? symbol?) ?%this))
       (let ((tmp (gensym 'tmp)))
	  (e `(with-access::JsObject ,o ((omap cmap) elements)
		 (let ((,tmp ,v))
		    (cond
		       ((eq? (js-pcache-cmap ,cache) omap)
			(vector-set-ur! elements
			   (js-pcache-index ,cache) ,tmp)
			,tmp)
;* 		       ((eq? (js-pcache-pmap ,cache) omap)             */
;* 			(with-access::JsObject ,o (cmap)               */
;* 			   (set! cmap (js-pcache-cmap ,cache)))        */
;* 			(vector-set-ur! elements                       */
;* 			   (js-pcache-index ,cache) ,tmp)              */
;* 			,tmp)                                          */
		       (else
			(js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this)))))
	     e)))
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw ?cache (and (? symbol?) ?%this))
       (let ((tmp (gensym 'tmp)))
	  (e `(with-access::JsObject ,o ((omap cmap) elements)
		 (let ((,tmp ,v))
		    (with-access::JsPropertyCache ,cache (cmap pmap index)
		       (cond
			  ((eq? cmap omap)
			   (vector-set-ur! elements index ,tmp)
			   ,tmp)
;* 			  ((eq? pmap omap)                             */
;* 			   (with-access::JsObject ,o ((%cmap cmap))    */
;* 			      (set! %cmap cmap))                       */
;* 			   (vector-set-ur! elements index ,tmp)        */
;* 			   ,tmp)                                       */
			  (else
			   (js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this))))))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-this-put-name/cache-expander ...                              */
;*---------------------------------------------------------------------*/
(define (js-this-put-name/cache-expander x e)
   (match-case x
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	  (and ?cache (js-pcache-ref %pcache ?ci))
	  (and (? symbol?) ?%this))
       (let ((tmp (gensym 'tmp)))
	  (e `(let ((,tmp ,v))
		 (cond
		    ((eq? (js-pcache-pmap ,cache) %thismap)
		     (with-access::JsObject ,o (cmap)
			(set! %thismap (js-pcache-cmap ,cache))
			(set! cmap %thismap))
		     (vector-set-ur! %thiselements (js-pcache-index ,cache) ,tmp)
		     ,tmp)
		    ((eq? (js-pcache-cmap ,cache) %thismap)
		     (vector-set-ur! %thiselements (js-pcache-index ,cache) ,tmp)
		     ,tmp)
		    (else
		     (let ((r (js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this)))
			(with-access::JsObject ,o (cmap elements)
			   (set! %thismap cmap)
			   (set! %thiselements elements)
			   r)))))
	     e)))
      ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	  ?cache (and (? symbol?) ?%this))
       (let ((tmp (gensym 'tmp)))
	  (e `(with-access::JsPropertyCache ,cache (cmap index)
		 (let ((,tmp ,v))
		    (cond
		       ((eq? pmap %thismap)
			(with-access::JsObject ,o (cmap elements)
			   (set! %thismap (js-pcache-cmap ,cache))
			   (set! cmap %thismap))
			(vector-set-ur! %thiselements index ,tmp)
			,tmp)
		       ((eq? cmap %thismap)
			(vector-set-ur! %thiselements index ,tmp)
			,tmp)
		       (else
			(let ((r (js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this)))
			   (with-access::JsObject ,o (cmap elements)
			      (set! %thismap cmap)
			      (set! %thiselements elements)
			      r))))))
	     e)))
      (else
       (map (lambda (x) (e x e)) x))))

;*---------------------------------------------------------------------*/
;*    js-call-name/cache-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (js-call-name/cache-expander x e)
   (match-case x
      ((?- ?%this (and (? symbol?) ?obj) ?prop ?ccache ?ocache . ?args)
       (e `(if (isa? ,obj JsObject)
	       (js-object-call-name/cache ,%this ,obj ,prop ,ccache ,ocache ,@args)
	       (js-raise-type-error %this "call: not a function ~s" ,obj))
	  e))
      ((?- ?%this ?obj ?prop ?ccache ?ocache . ?args)
       (let ((o (gensym '%obj)))
 	  (e `(let ((,o ,obj))
		 (if (isa? ,o JsObject)
		     (js-object-call-name/cache ,%this ,o ,prop ,ccache ,ocache ,@args)
		     (js-raise-type-error %this "call: not a function ~s" ,o)))
	     e)))
      (else
       (error "js-object-call-name/cache" "wrong form" x))))

;*---------------------------------------------------------------------*/
;*    js-object-call-name/cache-expander ...                           */
;*---------------------------------------------------------------------*/
(define (js-object-call-name/cache-expander x e)
   
   (define (call obj name ccache ocache args)
      `(with-access::JsObject ,obj ((omap cmap) __proto__)
	  (cond
	     ((and (not (null? __proto__))
		   (with-access::JsObject __proto__ ((%pmap cmap))
		      (eq? (js-pcache-cmap ,ccache) %pmap)))
	      ;; 1. check the prototype first
	      ((js-pcache-method ,ccache) ,obj ,@args))
	     ((eq? omap (js-pcache-cmap ,ccache))
	      ;; 2. check the object
	      ((js-pcache-method ,ccache) ,obj ,@args))
	     ((eq? (js-pcache-cmap ,ccache) #t)
	      ;; 3. an uncached call (polymorph call)
	      ,(let* ((len (length args))
		      (call (if (>=fx len 9)
				'js-calln
				(string->symbol (format "js-call~a" len))))
		      (m (gensym '%method)))
		  `(let ((,m (js-object-get-name/cache ,obj ',name ,ocache %this)))
		      (,call %this ,m ,obj ,@args))))
	     ((eq? (js-pcache-cmap ,ccache) #unspecified)
	      ;; 4. a cache miss
	      (js-object-call/cache-miss %this ,obj ',name ,ccache ,ocache
		 (list ,@args)))
	     (else
	      ;; 5. switch from monomorph to polymorph call
	      ,(let* ((len (length args))
		      (call (if (>=fx len 9)
				'js-calln
				(string->symbol (format "js-call~a" len))))
		      (m (gensym '%method)))
		  `(let ((,m (js-object-get-name/cache ,obj ',name ,ocache %this)))
		      (with-access::JsPropertyCache ,ccache (cmap)
			 (set! cmap #t))
		      (,call %this ,m ,obj ,@args)))))))
   
   (match-case x
      ((?- ?%this
	  (and (? symbol?) ?obj) ((kwote quote) ?name)
	  (and ?ccache (js-pcache-ref %pcache ?-))
	  (and ?ocache (js-pcache-ref %pcache ?-))
	  . ?args)
       (e (call obj name ccache ocache args) e))
      ((?- ?%this
	  ?obj ((kwote quote) ?name)
	  (and ?ccache (js-pcache-ref %pcache ?-))
	  (and ?ocache (js-pcache-ref %pcache ?-))
	  . ?args)
       (e `(let ((this (gensym 'this)))
	      ,(call this name ccache ocache args))
	  e))
      (else
       (error "js-object-call-name/cache" "wrong form" x))))

;*---------------------------------------------------------------------*/
;*    js-call/cache-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-call/cache-expander x e)
   
   (define (call %this fun ccache this args)
      (let ((len (length args)))
	 `(cond
	     ((eq? (js-pcache-proto ,ccache) ,fun)
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
   
   (match-case x
      ((?- ?%this (and (? symbol?) ?fun)
	  (and ?ccache (js-pcache-ref %pcache ?-))
	  ?this . ?args)
       (e (call %this fun ccache this args) e))
      ((?- ?%this ?fun
	  (and ?ccache (js-pcache-ref %pcache ?-))
	  ?this . ?args)
       (let ((f (gensym '%fun)))
	  `(let ((,f ,fun))
	      ,(e (call %this f ccache this args) e))))
      (else
       (error "js-call/cache" "wrong form" x))))
