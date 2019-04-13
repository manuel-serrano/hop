;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/spawn.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  7 09:04:09 2016                          */
;*    Last change :  Sat Apr 13 07:48:47 2019 (serrano)                */
;*    Copyright   :  2016-19 Manuel Serrano                            */
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

   (include "names.sch")
   
   (import __hopscript_types
	   __hopscript_property
	   __hopscript_worker
	   __hopscript_public
	   __hopscript_function
	   __hopscript_lib
	   __hopscript_promise
	   __hopscript_stringliteral)

   (export (js-spawn ::JsFunction ::obj ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

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
(define-macro (ref idx obj prop)
   `(js-get-name/cache ,obj ,prop #f %this
       (js-pcache-ref js-spawn-pcache ,idx)))

;*---------------------------------------------------------------------*/
;*    call ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (call fun this . args)
   (let ((call (string-append "js-call" (number->string (length args)))))
      `(,(string->symbol call) %this ,fun ,this ,@args)))

;*---------------------------------------------------------------------*/
;*    invoke ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (invoke idx self met . args)
   (let ((call (string-append "js-call" (number->string (length args)))))
      `(let ((self ,self))
	  (,(string->symbol call) %this (ref ,idx self ,met) self ,@args))))

;*---------------------------------------------------------------------*/
;*    fun ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (fun args body)
   `(js-make-function %this (lambda ,args ,body) ,(length args) "fun"))

;*---------------------------------------------------------------------*/
;*    js-spawn ...                                                     */
;*---------------------------------------------------------------------*/
(define (js-spawn genF self %this)
   (with-access::JsGlobalObject %this (js-promise js-spawn-pcache)
      (when (=fx (vector-length js-spawn-pcache) 0)
	 (set! __js_strings (&init!))
	 (set! js-spawn-pcache
	    ((@ js-make-pcache-table __hopscript_property) 8 "spawn")))
      (js-new1 %this js-promise
	 (js-make-function %this 
	    (lambda (this resolve reject)
	       
	       (define gen (call genF self))
	       
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
			((js-totest (ref 0 next (& "done")))
			 (call resolve (js-undefined) (ref 1 next (& "value")))
			 (js-undefined))
			(else
			 (let ((promise (invoke 3 js-promise (& "resolve")
					   (ref 2 next (& "value")))))
			    (invoke 4 promise (& "then")
			       (fun (this v)
				  (step
				     (lambda () (invoke 5 gen (& "next") v))))
			       (fun (this e)
				  (step
				     (lambda () (invoke 6 gen (& "throw") e))))))))))
	       
	       (step (lambda () (invoke 7 gen (& "next") (js-undefined)))))
	    
	    2 "AsyncPromise"))))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
