;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/spawn.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  7 09:04:09 2016                          */
;*    Last change :  Sat Feb 10 10:59:37 2018 (serrano)                */
;*    Copyright   :  2016-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Spawn implementation as defined in                               */
;*                                                                     */
;*    https://tc39.github.io/ecmascript-asyncawait                     */
;*    (See section "Informative desugaring")                           */
;*                                                                     */
;*    async function <name>?<argumentlist><body>                       */
;*       =>                                                            */
;*    function <name>?<args>{return spawn(function*() <body>, this);}  */
;*                                                                     */
;*    function spawn( genF, self ) {                                   */
;*       return new Promise( function( resolve, reject ) {             */
;*          var gen = genF.call( self );                               */
;*          function step( nextF ) {                                   */
;*             var next;                                               */
;*             try {                                                   */
;*                next = nextF();                                      */
;*             } catch( e ) {                                          */
;*                // finished with failure, reject the promise         */
;*                reject( e );                                         */
;*                return;                                              */
;*             }                                                       */
;*             if( next.done ) {                                       */
;*                // finished with success, resolve the promise        */
;*                resolve( next.value );                               */
;*                return;                                              */
;*             }                                                       */
;*             // not finished, chain off the yielded promise          */
;*             // and `step` again                                     */
;*             Promise.resolve( next.value ).then( function( v ) {     */
;*                step( function() { return gen.next( v ); } );        */
;*             }, function( e ) {                                      */
;*                step( function() { return gen.throw( e ); } );       */
;*             });                                                     */
;*          }                                                          */
;*          step( function() { return gen.next( undefined ); } );      */
;*       });                                                           */
;*    }                                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_spawn

   (library hop)
   (import __hopscript_types
	   __hopscript_property
	   __hopscript_worker
	   __hopscript_public
	   __hopscript_function
	   __hopscript_lib
	   __hopscript_promise)

   (export (js-spawn ::JsFunction ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    macro-init ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (macro-init)
   (eval '(define idx 0))
   #unspecified)

(macro-init)

;*---------------------------------------------------------------------*/
;*    ref ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (ref obj prop)
   (eval '(set! idx (+fx 1 idx)))
   (eval '(when (>=fx idx 10) (error "ref" "index out-of-bound" idx)))
   `(js-get-name/cache ,obj ,prop #f %this (js-pcache-ref %pcache ,(eval idx))))

;*---------------------------------------------------------------------*/
;*    call ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (call fun . args)
   (let ((call (string-append "js-call" (number->string (length args)))))
      `(,(string->symbol call) %this ,fun (js-undefined) ,@args)))

;*---------------------------------------------------------------------*/
;*    invoke ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (invoke self met . args)
   (let ((call (string-append "js-call" (number->string (length args)))))
      `(let ((self ,self))
	  (,(string->symbol call) %this (ref self ,met) self ,@args))))

;*---------------------------------------------------------------------*/
;*    fun ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (fun args body)
   `(js-make-function %this (lambda ,args ,body) ,(length args) 'fun))

;*---------------------------------------------------------------------*/
;*    %pcache ...                                                      */
;*---------------------------------------------------------------------*/
(%define-pcache 10)
(define %pcache (js-make-pcache 10))

;*---------------------------------------------------------------------*/
;*    js-spawn ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-spawn genF self %this)
   (with-access::JsGlobalObject %this (js-promise)
      (js-new1 %this js-promise
	 (js-make-function %this 
	    (lambda (this resolve reject)
	       
	       (define gen (invoke genF 'call self))
	       
	       (define (step nextF::procedure)
		  (let ((next (with-handler
				 (lambda (e)
				    (exception-notify e)
				    (call reject e)
				    #f)
				 (nextF))))
		     (cond
			((not next)
			 (js-undefined))
			((js-totest (ref next 'done))
			 (call resolve (ref next 'value))
			 (js-undefined))
			(else
			 (let ((promise (invoke js-promise 'resolve
					   (ref next 'value))))
			    (invoke promise 'then
			       (fun (this v)
				  (begin
				     (step (lambda () (invoke gen 'next v)))))
			       (fun (this e)
				  (step (lambda () (invoke gen 'throw e))))))))))
	       
	       (step (lambda () (invoke gen 'next (js-undefined)))))
	    
	    2 'AsyncPromise))))
