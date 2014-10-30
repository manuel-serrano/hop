;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_crypto.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 23 08:47:08 2014                          */
;*    Last change :  Thu Oct 30 07:30:47 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
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

   (import  __nodejs_process)
   
   (export  (process-crypto ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    process-crypto ...                                               */
;*---------------------------------------------------------------------*/
(define (process-crypto %this)
   
   (define (not-implemented name)
      (js-make-function %this
	 (lambda (this . l)
	    (error "crypto" "crypto binding not implemented" name))
	 0 name))

   (define (secure-context-init this . args)
      (cond-expand
	 (enable-ssl
	  (with-access::JsSecureContext this (ctx)
	     (let ((met (if (pair? args)
			    (js-tostring (car args) %this)
			    "SSLv23_method")))
		(set! ctx (instantiate::secure-context (method met))))))
	 (else
	  #unspecified)))

   (define (add-root-certs this)
      (cond-expand
	 (enable-ssl
	  (with-access::JsSecureContext this (ctx)
	     (secure-context-add-root-certs! ctx)))
	 (else
	  #unspecified)))
   
   (define (add-ca-cert this cert)
      (cond-expand
	 (enable-ssl
	  (with-access::JsSecureContext this (ctx)
	     (if (string? cert)
		 (secure-context-add-ca-cert! ctx cert 0 (string-length cert))
		 (with-access::JsTypedArray cert (%data byteoffset length)
		    (secure-context-add-ca-cert! ctx %data
		       (uint32->fixnum byteoffset)
		       (uint32->fixnum length))))))
	 (else
	  #unspecified)))
   
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
	 
	 (for-each (lambda (name)
		      (js-put! proto name (not-implemented name) #f %this))
	    '(setKey setCert addCRL setCiphers
	      setOptions setSessionIdContext close loadPKCS12))
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
	 (tprint "EncOut(" (count) ") buffer=" length
	    " offset=" offset
	    " len=" len))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-read ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-encin this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (tprint "EncIn(" (count) ") buffer=" length
	    " offset=" offset
	    " len=" len))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-write ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-clearin this buffer offset len)
      (with-access::JsTypedArray buffer (length byteoffset)
	 (tprint "ClearIn(" (count) ") buffer=" length
	    " offset=" offset " byteoffset=" byteoffset
	    " len=" len))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-clear-in ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-clearout this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (tprint "ClearOut(" (count) ") buffer=" length
	    " offset=" offset
	    " len=" len))
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
	 (ssl-connection-verify-error ssl)))
   
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
   
   (define (connection this jsctx serverp request-cert-or-server-name reject)
      (cond-expand
	 (enable-ssl
	  (with-access::JsSecureContext jsctx (ctx)
	     (instantiate::JsSSLConnection
		(__proto__ connection-proto)
		(ssl (instantiate::ssl-connection
			(ctx ctx)
			(isserver (js-toboolean serverp))
			(request-cert (when serverp
					 request-cert-or-server-name))
			(server-name (unless serverp
					request-cert-or-server-name))
			(reject-unauthorized reject))))))))
   
   (let ((sc (js-make-function %this secure-context 1 "SecureContext"
		:construct secure-context
		:prototype secure-context-proto))
	 (conn (js-make-function %this connection 4 "Connection"
		  :construct connection
		  :prototype connection-proto)))
      (with-access::JsGlobalObject %this (js-object)
	 (js-alist->jsobject
	    `((PBKDF2 . ,(not-implemented "PBKDF2"))
	      (randomBytes . ,(js-new %this js-object))
	      (pseudoRandomBytes . ,(js-new %this js-object))
	      (getSSLCiphers . ,(not-implemented "getSLLCiphers"))
	      (getCiphers . ,(js-new %this js-object))
	      (getHashes . ,(js-new %this js-object))
	      (init . ,(not-implemented "init"))
	      (SecureContext . ,sc)
	      (Connection . ,conn))
	    %this))))



