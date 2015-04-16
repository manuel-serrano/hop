;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_crypto.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 23 08:47:08 2014                          */
;*    Last change :  Mon Apr 13 20:17:15 2015 (serrano)                */
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
	       (ssl (default #unspecified)))

	    (class JsDH::JsObject
	       (dh (default #unspecified))
	       (initp::bool (default #f))))

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

   (define (secure-context-close this)
      (with-access::JsSecureContext this (ctx)
	 (secure-context-close ctx)))

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
	 (js-put! proto 'close
	    (js-make-function %this secure-context-close
	       0 "close")
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
	    '(addCRL setSessionIdContext loadPKCS12))
	 proto))

   (define c -1)
   (define (count)
      (set! c (+fx c 1))
      c)
   
   (define (connection-start this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-start ssl)))

   (define (connection-close this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-close ssl)))

   (define (connection-shutdown this)
      (with-access::JsSSLConnection this (ssl)
	 (let ((flags (ssl-connection-shutdown ssl)))
	    (when (memq 'sent flags)
	       (js-put! this 'sentShutdown #t #f %this))
	    (when (memq 'received flags)
	       (js-put! this 'receivedShutdown #t #f %this)))))

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
	 (js-put! proto 'close
	    (js-make-function %this connection-close
	       0 "close")
	    #f %this)
	 (js-put! proto 'shutdown
	    (js-make-function %this connection-shutdown
	       0 "shutdown")
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
	      isSessionReused getCurrentCipher))
	 proto))
   
   (define (secure-context this . args)
      (instantiate::JsSecureContext
	 (__proto__ secure-context-proto)))

   (define (info-callback this state)
      (if (=fx state 0)
	  ;; start
	  (let ((onhandshakestart (js-get this 'onhandshakestart %this)))
	     (tprint "onhandshakestart")
	     (js-worker-push-thunk! %worker "connection"
		(lambda ()
		   (js-call0 %this onhandshakestart this))))
	  ;; done
	  (let ((onhandshakedone (js-get this 'onhandshakedone %this)))
	     (tprint "onhandshakedone")
	     (js-worker-push-thunk! %worker "connection"
		(lambda ()
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

   (define (buffer->string buf)
      (with-access::JsFastBuffer buf (%data byteoffset length)
	 (let ((start (uint32->fixnum byteoffset))
	       (len (uint32->fixnum length)))
	    (substring %data start (+fx start len)))))

   (define (dh-set-private-key this buffer)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (unless (isa? buffer JsFastBuffer)
	    (js-raise-type-error %this "Argument not a buffer" buffer))
	 (with-access::dh dh (private-key)
	    (set! private-key (bn-bin2bn (buffer->string buffer))))))

   (define (dh-set-public-key this buffer)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (unless (isa? buffer JsFastBuffer)
	    (js-raise-type-error %this "Argument not a buffer" buffer))
	 (with-access::dh dh (public-key)
	    (set! public-key (bn-bin2bn (buffer->string buffer))))))

   (define (dh-generate-keys this)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (unless (dh-generate-key dh)
	    (js-raise-error %this "Key generation failed" this))
	 (with-access::dh dh (public-key)
	    (js-string->jsslowbuffer (bn-bn2bin public-key) %this))))

   (define (dh-compute-secret this buffer)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (unless (isa? buffer JsFastBuffer)
	    (js-raise-type-error %this "Argument not a buffer" buffer))
	 (let* ((str (buffer->string buffer))
		(key (bn-bin2bn str))
		(data (dh-compute-key dh key)))
	    (unwind-protect
	       (if (string? data)
		   (js-string->jsslowbuffer data %this)
		   (case (dh-check-pub-key dh key)
		      ((DH-CHECK-PUBKEY-TOO-SMALL)
		       (js-raise-error %this "Supplied key is too small" key))
		      ((DH-CHECK-PUBKEY-TOO-LARGE)
		       (js-raise-error %this "Supplied key is too large" key))
		      (else
		       (js-raise-error %this "Invalid key" this))))
	       (bn-free key)))))

   (define diffie-hellman-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'setPrivateKey
	    (js-make-function %this dh-set-private-key
	       1 "setPrivateKey")
	    #f %this)
	 (js-put! proto 'setPublicKey
	    (js-make-function %this dh-set-public-key
	       1 "setPublicKey")
	    #f %this)
	 (js-put! proto 'generateKeys
	    (js-make-function %this dh-generate-keys
	       0 "generateKeys")
	    #f %this)
	 (js-put! proto 'computeSecret
	    (js-make-function %this dh-compute-secret
	       1 "computeSecret")
	    #f %this)
	 proto))

   (define (diffie-hellman this . args)
      (let* ((dh (instantiate::dh))
	     (obj (instantiate::JsDH
		     (__proto__ diffie-hellman-proto)
		     (dh dh))))
	 (cond
	    ((integer? (car args))
	     (dh-generate-parameters-ex dh (car args) 'DH-GENERATOR-2)
	     (unless (dh-check dh)
		(with-access::JsDH obj (initp) (set! initp #t))))
	    ((isa? (car args) JsFastBuffer)
	     (with-access::dh dh (p g)
		(set! p (bn-bin2bn (buffer->string (car args))))
		(set! g (bn-new))
		(when (bn-set-word g 2)
		   (unless (dh-check dh)
		      (with-access::JsDH obj (initp)
			 (set! initp #t))))))
	    (else
	     (tprint "diffie-hellman unknown init arg: " (typeof (car args))
		" " args)))
	 obj))

   (let ((sc (js-make-function %this secure-context 1 "SecureContext"
		:construct secure-context
		:prototype secure-context-proto))
	 (conn (js-make-function %this connection 4 "Connection"
		  :construct connection
		  :prototype connection-proto))
	 (dh (js-make-function %this diffie-hellman 4 "diffieHellman"
		  :construct diffie-hellman
		  :prototype diffie-hellman-proto)))
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
	      (DiffieHellman . ,dh)
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

