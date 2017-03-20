;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/property_expd.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 17 09:28:50 2016                          */
;*    Last change :  Fri Mar 17 17:37:44 2017 (serrano)                */
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
;*    js-object-get-name/cache-expander ...                            */
;*---------------------------------------------------------------------*/
(define (js-object-get-name/cache-expander x e)
   
   (define (cache-miss-fun prop)
      (match-case prop
	 (((kwote quote) length)
	  'js-get-name/cache-miss)
	 (else
	  'js-get-lookup)))
   
   (define (ref o prop cache %this cindex ccmap cpmap cowner)
      `(let ((%omap omap))
	  (cond
	     ((eq? ,ccmap %omap)
	      (with-access::JsObject ,o (elements)
		 (vector-ref elements ,cindex)))
	     ((eq? ,cpmap %omap)
	      (with-access::JsObject ,cowner (elements)
		 (if (>=fx ,cindex 0)
		     (vector-ref elements ,cindex)
		     (let ((desc (vector-ref elements (-fx (negfx ,cindex) 1))))
			(js-property-value ,o desc ,%this)))))
	     (else
	      (,(cache-miss-fun prop) ,o ,prop ,cache #f ,%this)))))
   
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
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
	  (else
	   (map (lambda (x) (e x e)) x)))))

;*---------------------------------------------------------------------*/
;*    js-this-get-name/cache-expander ...                              */
;*---------------------------------------------------------------------*/
(define (js-this-get-name/cache-expander x e)

   (define (cache-miss-fun propname)
      (if (eq? propname 'length)
	  'js-get-name/cache-miss
	  ''js-get-lookup))
   
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
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
	   (map (lambda (x) (e x e)) x)))))

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
   
   (define (set o prop tmp throw cache %this cindex ccmap cpmap cowner vindex)
      `(let ((%omap omap))
	  (cond
	     ((eq? ,ccmap %omap)
	      (vector-set! elements ,cindex ,tmp)
	      ,tmp)
	     ((eq? ,cpmap %omap)
	      (if (>=fx ,cindex 0)
		  (begin
		     ;; there can be no overflow here, as if there one an
		     ;; overfow, it occurs during the previous cache miss
		     (vector-set! elements ,cindex ,tmp)
		     ;;(set! elements (js-elements-push! elements ,cindex ,tmp))
		     (set! omap ,ccmap)
		     ,tmp)
		  (with-access::JsObject ,cowner (elements)
		     (let ((desc (vector-ref elements (-fx (negfx ,cindex) 1))))
			(js-property-value-set! ,o desc ,tmp %this)))))
	     ((not %omap)
	      (js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this))
	     (else
	      (with-access::JsConstructMap %omap (vlen vtable)
		 (if (and (<fx ,vindex vlen)
			  (pair? (vector-ref vtable ,vindex)))
		     (let ((index (car (vector-ref vtable ,vindex)))
			   (cmap (cdr (vector-ref vtable ,vindex))))
			(vector-set! elements index ,tmp)
			(set! omap cmap)
			,tmp)
		     (js-object-put-name/cache-miss! ,o ,prop ,tmp ,throw
			,cache ,%this)))))))
;* 		     (let ((%tmp (js-put-jsobject! ,o ,prop ,tmp ,throw #t */
;* 				    ,cache ,%this)))                   */
;* 			(when (>=fx ,cindex 0)                         */
;* 			   (unless (<fx ,vindex (js-vindex-max))       */
;* 			      (with-access::JsPropertyCache ,cache (vindex) */
;* 				 (set! vindex ,cindex)))               */
;* 			   (js-vtable-add! %omap ,vindex (cons ,index ,ccmap))) */
;* 			%tmp)))))))                                    */
		 

   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (match-case x
	  ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	      (and ?cache (js-pcache-ref %pcache ?ci))
	      (and (? symbol?) ?%this))
	   (let ((tmp (gensym 'tmp)))
	      (e `(with-access::JsObject ,o ((omap cmap) elements)
		     (let ((,tmp ,v))
			,(set o prop tmp throw cache %this
			    `(js-pcache-index ,cache)
			    `(js-pcache-cmap ,cache)
			    `(js-pcache-pmap ,cache)
			    `(js-pcache-owner ,cache)
			    `(js-pcache-vindex ,cache))))
		 e)))
	  ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	      ?cache (and (? symbol?) ?%this))
	   (let ((tmp (gensym 'tmp)))
	      (e `(with-access::JsObject ,o ((omap cmap) elements)
		     (let ((,tmp ,v))
			(with-access::JsPropertyCache ,cache (cmap pmap index owner vindex)
			   ,(set o prop tmp throw cache %this
			       'index 'cmap 'pmap 'owner 'vindex))))
		 e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

;*---------------------------------------------------------------------*/
;*    js-this-put-name/cache-expander ...                              */
;*---------------------------------------------------------------------*/
(define (js-this-put-name/cache-expander x e)
   
   (define (set o prop tmp throw cache %this cindex ccmap cpmap cowner)
      `(cond
	  ((eq? ,cpmap %thismap)
	   (if (>=fx ,cindex 0)
	       (with-access::JsObject ,o (elements)
		  (set! %thiselements (js-elements-push! elements ,cindex ,tmp))
		  (set! %thismap ,ccmap)
		  (set! elements %thiselements)
		  (set! omap %thismap)
		  ,tmp)
	       (with-access::JsObject ,cowner (elements)
		  (let ((desc (vector-ref elements (-fx (negfx ,cindex) 1))))
		     (js-property-value-set! ,o desc ,tmp %this)))))
	  ((eq? ,ccmap %thismap)
	   (vector-set-ur! %thiselements ,cindex ,tmp)
	   ,tmp)
	  (else
	   (let ((r (js-put-jsobject! ,o ,prop ,tmp ,throw #t ,cache ,%this)))
	      (set! %thismap cmap)
	      (set! %thiselements elements)
	      r))))
	      
   (if (>= (bigloo-compiler-debug) 1)
       (map (lambda (x) (e x e)) x)
       (match-case x
	  ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	      (and ?cache (js-pcache-ref %pcache ?ci))
	      (and (? symbol?) ?%this))
	   (let ((tmp (gensym 'tmp)))
	      (e `(with-access::JsObject ,o ((omap cmap) elements)
		     (let ((,tmp ,v))
			,(set o prop tmp throw cache %this
			    `(js-pcache-index ,cache)
			    `(js-pcache-cmap ,cache)
			    `(js-pcache-pmap ,cache)
			    `(js-pcache-owner ,cache))))
		 e)))
	  ((?- (and (? symbol?) ?o) (and ?prop ((kwote quote) ?-)) ?v ?throw
	      ?cache (and (? symbol?) ?%this))
	   (let ((tmp (gensym 'tmp)))
	      (e `(with-access::JsObject ,o ((omap cmap) elements)
		     (let ((,tmp ,v))
			(with-access::JsPropertyCache ,cache (cmap pmap index owner)
			   ,(set o prop tmp throw cache %this
			       'index 'cmap 'pmap 'owner))))
		 e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

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

   (define (calln m obj args)
      (let* ((len (length args))
	     (call (if (>=fx len 9)
		       'js-calln
		       (string->symbol (format "js-call~a" len)))))
	 `(,call %this ,m ,obj ,@args)))

   (define (call obj name ccache ocache args)
      `(with-access::JsObject ,obj ((omap cmap) __proto__)
	  (with-access::JsConstructMap omap (vtable)
	     (cond
		((eq? (js-pcache-pmap ,ccache) omap)
		 ;; 1. check the prototype first
		 ((js-pcache-method ,ccache) ,obj ,@args))
		((not omap)
		 ;; 2. uncachable call
		 (let ((f (js-get ,obj ',name %this)))
		    (js-apply %this f ,obj (list ,@args))))
		(else
		 (with-access::JsConstructMap omap (vtable vlen)
		    (if (and (<fx (js-pcache-vindex ,ccache) vlen)
			     (procedure? (vector-ref vtable (js-pcache-vindex ,ccache))))
			((vector-ref vtable (js-pcache-vindex ,ccache))
			 ,obj ,@args)
			(js-object-call/cache-miss %this
			   ,obj ',name ,ccache ,ocache
			   (list ,@args)))))))))
;* 		 (with-access::JsPropertyCache ,ccache (cmap vtable vindex) */
;* 		    (when (=fx vindex (bit-lsh 1 29))                  */
;* 		       (set! vindex (js-get-vindex %this)))            */
;* 		    (js-vtable-add! (js-pcache-pmap ,ccache) vindex    */
;* 		       (js-pcache-method ,ccache))                     */
;* 		    (js-object-call/cache-miss %this ,obj ',name ,ccache ,ocache */
;* 		       (list ,@args)))                                 */
;* 		(else                                                  */
;* 		 ;; 4. uncachabl                                       */
;* 		((eq? (js-pcache-cmap ,ccache) #t)                     */
;* 		 ;; 2. an uncached call because an arity mismatch or   */
;* 		 ;;    a polymorphic call                              */
;* 		 (with-access::JsConstructMap omap (vtable)            */
;* 		    (tprint "CACHE disable name=" ',name               */
;* 		       " vindex=" (js-pcache-vindex ,ccache)           */
;* 		       " vlen=" (vector-length vtable))                */
;* 		    (if (and (>=fx (js-pcache-vindex ,ccache) 0)       */
;* 			     (>fx (vector-length vtable)               */
;* 				(js-pcache-vindex ,ccache)))           */
;* 			((vector-ref vtable (js-pcache-vindex ,ccache)) ,@args) */
;* 			,(let ((m (gensym '%method)))                  */
;* 			    `(let ((,m (js-object-get-name/cache ,obj ',name ,ocache %this))) */
;* 				,(calln m obj args))))))               */
;* 		((eq? (js-pcache-cmap ,ccache) #unspecified)           */
;* 		 ;; 3. a cache miss                                    */
;* 		 (tprint "CAchE MISS name=" ',name)                    */
;* 		 (js-object-call/cache-miss %this ,obj ',name ,ccache ,ocache */
;* 		    (list ,@args)))                                    */
;* 		(else                                                  */
;* 		 ;; 4. switch from monomorphic to polymorphic call     */
;* 		 (tprint "CAchE POLY name=" ',name)                    */
;* 		 (with-access::JsPropertyCache ,ccache (cmap vtable vindex) */
;* 		    (when (=fx vindex -1)                              */
;* 		       (set! vindex (js-get-vindex %this)))            */
;* 		    (js-vtable-add! (js-pcache-pmap ,ccache) vindex    */
;* 		       (js-pcache-method ,ccache))                     */
;* 		    (js-object-call/cache-miss %this ,obj ',name ,ccache ,ocache */
;* 		       (list ,@args))))))))                            */
;* 	      (let ((m (gensym '%method)))                             */
;* 		  `(let ((,m (js-object-get-name/cache ,obj ',name ,ocache %this))) */
;* 		      (with-access::JsPropertyCache ,ccache (cmap vtable vindex) */
;* 			 (when (=fx vindex -1)                         */
;* 			    (set! vindex (js-get-vindex %this)))       */
;* 			 (js-vtable-add! (js-pcache-pmap ,ccache)      */
;* 			    (js-pcache-method ,ccache))                */
;* 			 (set! cmap #t))                               */
;* 		      ,(calln m obj args)))                            */
;* 	      ))))                                                     */
   
   (if (>= (bigloo-compiler-debug) 2)
       (map (lambda (x) (e x e)) x)
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
	   (error "js-object-call-name/cache" "wrong form" x)))))

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
