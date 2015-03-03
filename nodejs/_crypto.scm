;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_crypto.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 23 08:47:08 2014                          */
;*    Last change :  Sat Feb 28 18:13:53 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Crypto native bindings                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__crypto

   (cond-expand
      (enable-ssl (library ssl)))
   
   (include "nodejs_debug.sch")
   
   (library hopscript)

   (static  (class JsSecureContext::JsObject
	       (ctx (default #unspecified)))

	    (class JsSSLConnection::JsObject
	       (ssl (default #unspecified))))

   (import  __nodejs_process
	    __nodejs__buffer)
   
   (export  (process-crypto ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    debug-crypto ...                                                 */
;*---------------------------------------------------------------------*/
(define debug-crypto
   (let ((env (getenv "NODE_DEBUG")))
      (cond
	 ((not (string? env)) 0)
	 ((string-contains env "_crypto") 2)
	 (else 0))))

(cond-expand
   (enable-ssl
    
;*---------------------------------------------------------------------*/
;*    process-crypto ...                                               */
;*---------------------------------------------------------------------*/
(define (process-crypto %worker %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "crypto" "crypto binding not implemented" name))
	 0 name))

   (define (secure-context-init this . args)
      (with-access::JsSecureContext this (ctx)
	 (let ((met (if (pair? args)
			(js-tostring (car args) %this)
			"SSLv23_method")))
	    (set! ctx (instantiate::secure-context (method met))))))

   (define (add-root-certs this)
      (with-access::JsSecureContext this (ctx)
	 (secure-context-add-root-certs! ctx)))
   
   (define (add-ca-cert this cert)
      (with-access::JsSecureContext this (ctx)
	 (if (isa? cert JsStringLiteral)
	     (let ((cert (js-jsstring->string cert)))
		(secure-context-add-ca-cert! ctx cert 0 (string-length cert)))
	     (with-access::JsTypedArray cert (%data byteoffset length)
		(secure-context-add-ca-cert! ctx %data
		   (uint32->fixnum byteoffset)
		   (uint32->fixnum length))))))

   (define (set-key this cert passphrase)
      (with-access::JsSecureContext this (ctx)
	 (let ((pass (when (string? passphrase) passphrase)))
	    (if (isa? cert JsStringLiteral)
		(let ((cert (js-jsstring->string cert)))
		   (secure-context-set-key! ctx cert 0 (string-length cert) pass))
		(with-access::JsTypedArray cert (%data byteoffset length)
		   (secure-context-set-key! ctx %data
		      (uint32->fixnum byteoffset)
		      (uint32->fixnum length)
		      pass))))))
   
   (define (set-cert this cert)
      (with-access::JsSecureContext this (ctx)
	 (if (isa? cert JsStringLiteral)
	     (let ((cert (js-jsstring->string cert)))
		(secure-context-set-cert! ctx cert 0 (string-length cert)))
	     (with-access::JsTypedArray cert (%data byteoffset length)
		(secure-context-set-cert! ctx %data
		   (uint32->fixnum byteoffset)
		   (uint32->fixnum length))))))

   (define (set-ciphers this ciphers)
      (with-access::JsSecureContext this (ctx)
	 (cond
	    ((isa? ciphers JsStringLiteral)
	     (secure-context-set-ciphers! ctx (js-jsstring->string ciphers)))
	    ((isa? ciphers JsTypedArray)
	     (with-access::JsTypedArray ciphers (%data byteoffset length)
		(secure-context-set-ciphers! ctx
		   (substring %data
		      (uint32->fixnum byteoffset)
		      (uint32->fixnum length)))))
	    (else
	     (js-raise-type-error %this
		(format "Bad parameter (~a) ~~a" (typeof ciphers))
		ciphers)))))
   
   (define (set-options this options)
      (with-access::JsSecureContext this (ctx)
	 (if (integer? options)
	     (secure-context-set-options! ctx options)
	     (js-raise-type-error %this "Bad parameter ~a" options))))
   
   (define secure-context-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'init
	    (js-make-function %this secure-context-init
	       1 "init")
	    #f %this)
	 (js-put! proto 'addRootCerts
	    (js-make-function %this add-root-certs
	       0 "addRootCerts")
	    #f %this)
	 (js-put! proto 'addCACert
	    (js-make-function %this add-ca-cert
	       1 "addCACert")
	    #f %this)
	 (js-put! proto 'setKey
	    (js-make-function %this set-key
	       2 "setKey")
	    #f %this)
	 (js-put! proto 'setCert
	    (js-make-function %this set-cert
	       2 "setCert")
	    #f %this)
	 (js-put! proto 'setCiphers
	    (js-make-function %this set-ciphers
	       2 "setCiphers")
	    #f %this)
	 (js-put! proto 'setOptions
	    (js-make-function %this set-options
	       2 "setOptions")
	    #f %this)
	 
	 (for-each (lambda (name)
		      (js-put! proto name (not-implemented name) #f %this))
	    '(addCRL setSessionIdContext close loadPKCS12))
	 proto))

   (define c -1)
   (define (count)
      (set! c (+fx c 1))
      c)
   
   (define (connection-start this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-start ssl)))

   (define (connection-encout this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (when (>fx debug-crypto 0)
	    (tprint "EncOut(" (count) ") buffer=" length
	       " offset=" offset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-read ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-encin this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (when (>fx debug-crypto 0)
	    (tprint "EncIn(" (count) ") buffer=" length
	       " offset=" offset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-write ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-clearin this buffer offset len)
      (with-access::JsTypedArray buffer (length byteoffset)
	 (when (>fx debug-crypto 0)
	    (tprint "ClearIn(" (count) ") buffer=" length
	       " offset=" offset " byteoffset=" byteoffset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (when (>fx debug-crypto 0)
	       (tprint "ssl-connection-clear-in "
		  (+fx (uint32->fixnum byteoffset) offset)
		  " len=" len))
	    (ssl-connection-clear-in ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-clearout this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (when (>fx debug-crypto 0)
	    (tprint "ClearOut(" (count) ") buffer=" length
	       " offset=" offset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-clear-out ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-is-init-finished this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-init-finished? ssl)))
   
   (define (connection-enc-pending this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-enc-pending ssl)))
   
   (define (connection-clear-pending this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-clear-pending ssl)))
   
   (define (connection-set-session this buffer)
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-set-session ssl %data))))
   
   (define (connection-verify-error this)
      (with-access::JsSSLConnection this (ssl)
	 (js-string->jsstring (ssl-connection-verify-error ssl))))
   
   (define (connection-get-peer-certificate this)
      (with-access::JsSSLConnection this (ssl)
	 (js-alist->jsobject
	    (ssl-connection-get-peer-certificate ssl)
	    %this)))
   
   (define connection-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'start
	    (js-make-function %this connection-start
	       0 "start")
	    #f %this)
	 (js-put! proto 'encOut
	    (js-make-function %this connection-encout
	       3 "encOut")
	    #f %this)
	 (js-put! proto 'encIn
	    (js-make-function %this connection-encin
	       3 "encIn")
	    #f %this)
	 (js-put! proto 'clearIn
	    (js-make-function %this connection-clearin
	       3 "clearIn")
	    #f %this)
	 (js-put! proto 'clearOut
	    (js-make-function %this connection-clearout
	       3 "clearOut")
	    #f %this)
	 (js-put! proto 'isInitFinished
	    (js-make-function %this connection-is-init-finished
	       0 "isInitFinished")
	    #f %this)
	 (js-put! proto 'encPending
	    (js-make-function %this connection-enc-pending
	       0 "encPending")
	    #f %this)
	 (js-put! proto 'clearPending
	    (js-make-function %this connection-clear-pending
	       0 "clearPending")
	    #f %this)
	 (js-put! proto 'setSession
	    (js-make-function %this connection-set-session
	       1 "setSession")
	    #f %this)
	 (js-put! proto 'verifyError
	    (js-make-function %this connection-verify-error
	       1 "verifyError")
	    #f %this)
	 (js-put! proto 'getPeerCertificate
	    (js-make-function %this connection-get-peer-certificate
	       1 "getPeerCertificate")
	    #f %this)
	 (for-each (lambda (name)
		      (js-put! proto name (not-implemented name) #f %this))
	    '(getSession setSettion loadSession
	      isSessionReused getCurrentCipher
	      shutdown close))
	 proto))
   
   (define (secure-context this . args)
      (instantiate::JsSecureContext
	 (__proto__ secure-context-proto)))

   (define (info-callback this state)
      (if (=fx state 0)
	  ;; start
	  (let ((onhandshakestart (js-get this 'onhandshakestart %this)))
	     (js-worker-push-thunk! %worker "connection"
		(lambda ()
		   (tprint "onhandshakestart ")
		   (js-call0 %this onhandshakestart this))))
	  ;; done
	  (let ((onhandshakedone (js-get this 'onhandshakedone %this)))
	     (js-worker-push-thunk! %worker "connection"
		(lambda ()
		   (tprint "onhandshakedone ")
		   (js-call0 %this onhandshakedone this))))))
   
   (define (connection this jsctx serverp request-cert-or-server-name reject)
      (with-access::JsSecureContext jsctx (ctx)
	 (letrec ((conn (instantiate::JsSSLConnection
			   (__proto__ connection-proto)
			   (ssl (instantiate::ssl-connection
				   (ctx ctx)
				   (info-callback (lambda (start-or-done)
						     (info-callback
							conn start-or-done)))
				   (isserver (js-toboolean serverp))
				   (request-cert (when serverp
						    request-cert-or-server-name))
				   (server-name (unless serverp
						   request-cert-or-server-name))
				   (reject-unauthorized reject))))))
	    conn)))

   (define (check-entropy)
      (let loop ()
	 (unless (ssl-rand-status)
	    (unless (ssl-rand-poll)))))
   
   (define (pseudoRandomBytes this size cb)
      (check-entropy)
      (cond
	 ((not (number? size))
	  (js-raise-type-error %this "Bad argument" size))
	 ((or (< size 0) (> size 1073741823.))
	  (js-raise-type-error %this "Bad size" size))
	 (else
	  (let ((buf (js-string->jsslowbuffer
			(ssl-rand-pseudo-bytes size)
			%this)))
	     (if (isa? cb JsFunction)
		 (js-call2 %this cb this (js-undefined) buf)
		 buf)))))
   
   (define (randomBytes this size cb)
      (check-entropy)
      (cond
	 ((not (number? size))
	  (js-raise-type-error %this "Bad argument" size))
	 ((or (< size 0) (> size 1073741823.))
	  (js-raise-type-error %this "Bad size" size))
	 (else
	  (let ((buf (js-string->jsslowbuffer
			(ssl-rand-bytes (js-tointeger size %this))
			%this)))
	     (if (isa? cb JsFunction)
		 (js-call2 %this cb this (js-undefined) buf)
		 buf)))))
   
   (let ((sc (js-make-function %this secure-context 1 "SecureContext"
		:construct secure-context
		:prototype secure-context-proto))
	 (conn (js-make-function %this connection 4 "Connection"
		  :construct connection
		  :prototype connection-proto)))
      (with-access::JsGlobalObject %this (js-object)
	 (js-alist->jsobject
	    `((PBKDF2 . ,(not-implemented "PBKDF2"))
	      (randomBytes . ,(js-make-function %this randomBytes
				 2 "randomBytes"))
	      (pseudoRandomBytes . ,(js-make-function %this pseudoRandomBytes
				       2 "pseudoRandomBytes"))
	      (getSSLCiphers . ,(not-implemented "getSLLCiphers"))
	      (getCiphers . ,(js-new %this js-object))
	      (getHashes . ,(js-new %this js-object))
	      (init . ,(not-implemented "init"))
	      (DiffieHellman . ,(not-implemented "DiffieHellman"))
	      (SecureContext . ,sc)
	      (Connection . ,conn))
	    %this))))


;*---------------------------------------------------------------------*/
;*    no ssl support                                                   */
;*---------------------------------------------------------------------*/
)
(else
 (define (process-crypto %worker %this)
    #unspecified)))

