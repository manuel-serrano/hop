;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_hop.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 18 06:41:05 2014                          */
;*    Last change :  Fri Apr 18 10:07:33 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop binding                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__hop

   (library hopscript hop)

   (export (nodejs-with-url url success opt %this::JsGlobalObject)
	   (nodejs-with-hop url success opt %this::JsGlobalObject)
	   (nodejs-charset-convert this text from to %this::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    nodejs-with-url ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-with-url url success opt %this)
   
   (define (parse-json in)
      (js-json-parser in (js-undefined) %this))
   
   (let ((url (js-tostring url %this))
	 (fail #f)
	 (timeout 0)
	 (method "GET"))
      (unless (eq? opt (js-undefined))
	 (let ((f (js-get opt 'fail %this))
	       (t (js-get opt 'timeout %this))
	       (m (js-get opt 'method %this)))
	    (when (isa? f JsFunction)
	       (set! fail (lambda (x) (js-call1 %this f %this x))))
	    (unless (eq? t (js-undefined))
	       (set! timeout (js-tointeger t %this)))
	    (unless (eq? m (js-undefined))
	       (set! method (js-tostring m %this)))))
      (with-url url
	 (if (isa? success JsFunction)
	     (lambda (x) (js-call1 %this success %this x))
	     (lambda (x) x))
	 :parse-json parse-json
	 :fail fail 
	 :timeout timeout
	 :method method)))

;*---------------------------------------------------------------------*/
;*    nodejs-with-hop ...                                              */
;*---------------------------------------------------------------------*/
(define (nodejs-with-hop svc success opt %this)
   
   (define (parse-json in)
      (js-json-parser in (js-undefined) %this))
   
   (let ((host "localhost")
	 (port 8080)
	 (user #f)
	 (password #f)
	 (authorization #f)
	 (fail #f))
      (unless (eq? opt (js-undefined))
	 (let ((h (js-get opt 'host %this))
	       (p (js-get opt 'port %this))
	       (u (js-get opt 'user %this))
	       (w (js-get opt 'password %this))
	       (a (js-get opt 'authorization %this))
	       (f (js-get opt 'fail %this)))
	    (unless (eq? h (js-undefined))
	       (set! host (js-tostring h %this)))
	    (unless (eq? p (js-undefined))
	       (set! port (js-tointeger p %this)))
	    (unless (eq? u (js-undefined))
	       (set! user u))
	    (unless (eq? w (js-undefined))
	       (set! password (js-tostring w %this)))
	    (unless (eq? a (js-undefined))
	       (set! authorization (js-tostring a %this)))
	    (when (isa? f JsFunction)
	       (set! fail (lambda (x) (js-call1 %this f %this x))))))
      (with-hop-remote svc
	 (if (isa? success JsFunction)
	     (lambda (x) (js-call1 %this success %this x))
	     (lambda (x) x))
	 fail
	 :host host :port port 
	 :user user :password password :authorization authorization)))
			  
;*---------------------------------------------------------------------*/
;*    nodejs-charset-convert ...                                       */
;*---------------------------------------------------------------------*/
(define (nodejs-charset-convert this text from to %this)
   (let ((from (js-tostring from %this))
	 (to (js-tostring to %this)))
      (charset-convert text
	 (if (string? from) (string->symbol from) (hop-locale))
	 (if (string? to) (string->symbol to) (hop-charset)))))
