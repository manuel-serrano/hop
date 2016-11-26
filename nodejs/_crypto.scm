;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/nodejs/_crypto.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 23 08:47:08 2014                          */
;*    Last change :  Sat Nov 26 18:10:26 2016 (serrano)                */
;*    Copyright   :  2014-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Crypto native bindings                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__crypto

   (cond-expand
      (enable-ssl (library ssl)))

   (include "nodejs_debug.sch"
	    "_crypto.sch")
   
   (library hopscript)

   (cond-expand
      (enable-ssl
       (static  (class JsSecureContext::JsObject
		   (ctx (default #unspecified)))
	  
	  (class JsSSLConnection::JsObject
	     (hparser::HelloParser read-only)
	     (ssl (default #unspecified))
	     (next-session::pair-nil (default '())))
	  
	  (class JsDH::JsObject
	     (dh (default #unspecified))
	     (initp::bool (default #f)))
	  
	  (class JsHash::JsObject
	     (hash::ssl-hash read-only))
	  
	  (class JsHmac::JsObject
	     (hmac::obj (default #unspecified)))
	  
	  (class JsSign::JsObject
	     (sign::ssl-sign read-only))
	  
	  (class JsVerify::JsObject
	     (verify::ssl-verify read-only))
	  
	  (class JsCipher::JsObject
	     (cipher::ssl-cipher read-only))
	  
	  (class JsDecipher::JsCipher)
	  
	  (class HelloParser
	     (%worker::WorkerHopThread read-only)
	     (%this::JsGlobalObject read-only)
	     (state::symbol (default 'kWaiting))
	     (data::bstring (default (make-string 18432)))
	     (offset::long (default 0))
	     (body-offset::long (default 0))
	     (frame-len::long (default 0))
	     (conn::JsSSLConnection read-only)))))
	       
   (import  __nodejs_process
	    __nodejs__buffer
	    __nodejs_uv)
   
   (export  (crypto-constants::pair-nil)
	    (process-crypto ::WorkerHopThread ::JsGlobalObject)))

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
;*    crypto-constants ...                                             */
;*---------------------------------------------------------------------*/
(define (crypto-constants)
   `((SSL_OP_CIPHER_SERVER_PREFERENCE . ,(ssl-op-cipher-server-preference))))

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
			"default")))
	    (set! ctx (instantiate::secure-context (method met))))))

   (define (secure-context-close this)
      (with-access::JsSecureContext this (ctx)
	 (secure-context-close ctx)))

   (define (add-root-certs this)
      (with-access::JsSecureContext this (ctx)
	 (secure-context-add-root-certs! ctx)))
   
   (define (add-ca-cert this cert)
      (with-access::JsSecureContext this (ctx)
	 (if (js-jsstring? cert)
	     (let ((cert (js-jsstring->string cert)))
		(secure-context-add-ca-cert! ctx cert 0 (string-length cert)))
	     (with-access::JsTypedArray cert (%data byteoffset length)
		(secure-context-add-ca-cert! ctx %data
		   (uint32->fixnum byteoffset)
		   (js-get cert 'length %this))))))

   (define (add-crl this cert)
      (with-access::JsSecureContext this (ctx)
	 (if (js-jsstring? cert)
	     (let ((cert (js-jsstring->string cert)))
		(secure-context-add-crl! ctx cert 0 (string-length cert)))
	     (with-access::JsTypedArray cert (%data byteoffset length)
		(secure-context-add-crl! ctx %data
		   (uint32->fixnum byteoffset)
		   (js-get cert 'length %this))))))

   (define (set-key this cert passphrase)
      (with-access::JsSecureContext this (ctx)
	 (let ((pass (when (js-jsstring? passphrase)
			(js-jsstring->string passphrase))))
	    (if (js-jsstring? cert)
		(let ((cert (js-jsstring->string cert)))
		   (secure-context-set-key! ctx cert 0 (string-length cert) pass))
		(with-access::JsTypedArray cert (%data byteoffset length)
		   (secure-context-set-key! ctx %data
		      (uint32->fixnum byteoffset)
		      (js-get cert 'length %this)
		      pass))))))
   
   (define (set-cert this cert)
      (with-access::JsSecureContext this (ctx)
	 (if (js-jsstring? cert)
	     (let ((cert (js-jsstring->string cert)))
		(secure-context-set-cert! ctx cert 0 (string-length cert)))
	     (with-access::JsTypedArray cert (%data byteoffset length)
		(secure-context-set-cert! ctx %data
		   (uint32->fixnum byteoffset)
		   (uint32->fixnum length))))))

   (define (set-session-id-context this sic)
      (with-access::JsSecureContext this (ctx)
	 (if (js-jsstring? sic)
	     (let ((sic (js-jsstring->string sic)))
		(secure-context-set-session-id-context! ctx sic 0 (string-length sic)))
	     (with-access::JsTypedArray sic (%data byteoffset length)
		(secure-context-set-session-id-context! ctx %data
		   (uint32->fixnum byteoffset)
		   (uint32->fixnum length))))))

   (define (load-pkcs12 this pfx pass)
      (let ((pass (cond
		     ((js-jsstring? pass)
		      (js-jsstring->string pass))
		     ((isa? pass JsTypedArray)
		      (with-access::JsTypedArray pass (%data byteoffset length)
			 (substring %data
			    (uint32->fixnum byteoffset)
			    (+fx (uint32->fixnum byteoffset)
			       (uint32->fixnum length)))))
		     (else
		      #f))))
	 (with-access::JsSecureContext this (ctx)
	    (cond
	       ((js-jsstring? pfx)
		(secure-context-load-pkcs12 ctx
		   (js-jsstring->string pfx) pass))
	       ((isa? pfx JsTypedArray)
		(with-access::JsTypedArray pfx (%data byteoffset length)
		   (secure-context-load-pkcs12 ctx
		      (substring %data
			 (uint32->fixnum byteoffset)
			 (+fx (uint32->fixnum byteoffset)
			    (uint32->fixnum length)))
		      pass)))
	       (else
		(js-raise-type-error %this
		   (format "Bad parameter (~a, ~a) ~~a" (typeof pfx) (typeof pass))
		   pfx))))))

   (define (set-ciphers this ciphers)
      (with-access::JsSecureContext this (ctx)
	 (cond
	    ((js-jsstring? ciphers)
	     (secure-context-set-ciphers! ctx (js-jsstring->string ciphers)))
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
	 (js-put! proto 'addCRL
	    (js-make-function %this add-crl
	       1 "addCRL")
	    #f %this)
	 (js-put! proto 'setKey
	    (js-make-function %this set-key
	       2 "setKey")
	    #f %this)
	 (js-put! proto 'setCert
	    (js-make-function %this set-cert
	       1 "setCert")
	    #f %this)
	 (js-put! proto 'setSessionIdContext
	    (js-make-function %this set-session-id-context
	       2 "setSessionIdContext")
	    #f %this)
	 (js-put! proto 'loadPKCS12
	    (js-make-function %this load-pkcs12
	       2 "loadPKCS12")
	    #f %this)
	 (js-put! proto 'setCiphers
	    (js-make-function %this set-ciphers
	       2 "setCiphers")
	    #f %this)
	 (js-put! proto 'setOptions
	    (js-make-function %this set-options
	       2 "setOptions")
	    #f %this)
	 
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
	 (ssl-connection-shutdown ssl)))

   (define (connection-encout this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (when (>fx debug-crypto 0)
	    (tprint ">>> EncOut(" (count) ") buffer=" length
	       " offset=" offset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (when (>fx debug-crypto 0)
	       (tprint "~~~ EncOut(" (count) ") offset="
		  (+fx (uint32->fixnum byteoffset) offset)
		  " len=" len))
	    (let ((r (ssl-connection-read ssl %data
			(+fx (uint32->fixnum byteoffset) offset) len)))
	       (when (>fx debug-crypto 0)
		  (tprint "<<< EncOut(" (count) ") => " r))
	       r))))

   (define (connection-write this::JsSSLConnection buffer len)
      (error "connection-write" "Not implemented" (typeof buffer)))
   
   (define (connection-encin this::JsSSLConnection buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (when (>fx debug-crypto 0)
	    (tprint "EncIn(" (count) ") buffer=" length
	       " offset=" offset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl hparser)
	 (with-access::ssl-connection ssl (isserver)
	    (with-access::JsTypedArray buffer (%data byteoffset length)
;* 	       (when (>fx debug-crypto 0)                              */
;* 		  (tprint "EncInc byteoffset=" byteoffset))            */
	       (if (and isserver (not (hello-parser-ended hparser)))
		   (let ((r (hello-parser-write hparser %data
			       (+fx (uint32->fixnum byteoffset) offset) len)))
		      (when (>fx debug-crypto 0)
			 (tprint "HelloParser bytes_written=" r))
		      r)
		   (begin
		      (when (>fx debug-crypto 0)
			 (tprint "BIO_write.2 len=" len))
		      (let ((r (ssl-connection-write ssl %data
				  (+fx (uint32->fixnum byteoffset) offset) len)))
			 (when (>fx debug-crypto 0)
			    (let ((len (if (<fx len 160) len 160))
				  (p (open-output-string))
				  (off (+fx (uint32->fixnum byteoffset) offset)))
			       (let loop ((i 0))
				  (if (=fx i len)
				      (tprint "EncIn data=" (close-output-port p))
				      (begin
					 (display (format "~2,0x "
						     (char->integer
							(string-ref %data (+fx off i))))
					    p)
					 (loop (+fx i 1))))))
			    (tprint "ClientWritten bytes_written=" r))
			 r)))))))
   
   (define (connection-clearin this::JsSSLConnection buffer offset len)
      (with-access::JsTypedArray buffer (length byteoffset)
	 (when (>fx debug-crypto 0)
	    (tprint "ClearIn(" (count) ") buffer=" length
	       " offset=" offset " byteoffset=" byteoffset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (ssl-connection-clear-in ssl %data
	       (+fx (uint32->fixnum byteoffset) offset) len))))
   
   (define (connection-clearout this buffer offset len)
      (with-access::JsTypedArray buffer (length)
	 (when (>fx debug-crypto 0)
	    (tprint ">>> ClearOut(" (count) ") buffer=" length
	       " offset=" offset
	       " len=" len)))
      (with-access::JsSSLConnection this (ssl)
	 (with-access::JsTypedArray buffer (%data byteoffset length)
	    (let ((r (ssl-connection-clear-out ssl %data
			(+fx (uint32->fixnum byteoffset) offset) len)))
	       (when (>fx debug-crypto 0)
		  (tprint "<<< ClearOut(" (count) ") res=" r))
	       r))))

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
   
   (define (connection-get-session this)
      (with-access::JsSSLConnection this (ssl)
	 (js-string->jsfastbuffer (ssl-connection-get-session ssl) %this)))
   
   (define (connection-get-current-cipher this)
      (with-access::JsSSLConnection this (ssl)
	 (let ((c (ssl-connection-get-current-cipher ssl)))
	    (if (pair? c)
		(with-access::JsGlobalObject %this (js-object)
		   (let ((o (js-new %this js-object)))
		      (js-put! o 'name (js-string->jsstring (car c)) #f %this)
		      (js-put! o 'version (js-string->jsstring (cdr c)) #f %this)
		      o))
		(js-undefined)))))
   
   (define (connection-load-session this::JsSSLConnection buffer)
      (when (>fx debug-crypto 0)
	 (tprint "LoadSession"))
      (with-access::JsSSLConnection this (ssl hparser)
	 (when (isa? buffer JsTypedArray)
	    (when (>fx debug-crypto 0)
	       (tprint "LoadSession, with buffer"))
	    (with-access::JsTypedArray buffer (%data byteoffset length)
	       (ssl-connection-load-session ssl %data)))
	 (hello-parser-finish hparser)
	 #t))
   
   (define (connection-verify-error this)
      (with-access::JsSSLConnection this (ssl)
	 (let ((err (ssl-connection-verify-error ssl)))
	    (if (string? err)
		(with-access::JsGlobalObject %this (js-error)
		   (js-new %this js-error (js-string->jsstring err)))
		(js-null)))))
   
   (define (connection-get-peer-certificate this)
      (with-access::JsSSLConnection this (ssl)
	 (let ((cert (ssl-connection-get-peer-certificate ssl)))
	    (if (pair? cert)
		(let ((eku (assq 'ext-key-usage cert)))
		   (when (pair? eku)
		      (vector-map! js-string->jsstring (cdr eku))
		      (set-car! eku 'ext_key_usage)
		      (set-cdr! eku (js-vector->jsarray (cdr eku) %this)))
		   (js-alist->jsobject cert %this))
		(js-undefined)))))
   
   (define (connection-session-reused? this)
      (with-access::JsSSLConnection this (ssl)
	 (ssl-connection-reused? ssl)))

   (define (connection-get-negotiated-protocol this)
      (with-access::JsSSLConnection this (ssl)
	 (let ((s (ssl-connection-get-negotiated-protocol ssl)))
	    (if (string? s)
		(js-string->jsstring s)
		s))))
   
   (define (connection-set-npn-protocols this protos)
      (with-access::JsSSLConnection this (ssl)
	 (with-access::ssl-connection ssl (npn-protos)
	    (with-access::JsTypedArray protos (%data byteoffset length)
	       (set! npn-protos
		  (substring %data
		     (uint32->fixnum byteoffset)
		     (uint32->fixnum (+u32 byteoffset length))))))))
   
   (define (connection-get-servername this)
      (with-access::JsSSLConnection this (ssl)
	 (with-access::ssl-connection ssl (server-name)
	    (if (string? server-name)
		(js-string->jsstring server-name)
		server-name))))
   
   (define (connection-set-sni-callback this cb)
      (with-access::JsSSLConnection this (ssl)
	 (with-access::ssl-connection ssl (sni-context-callback)
	    (set! sni-context-callback
	       (lambda (ssl srvname)
		  (let ((r (!js-callback1 "set-sni" %worker %this cb
			      this (js-string->jsstring srvname))))
		     (when (isa? r JsSecureContext)
			(with-access::JsSecureContext r (ctx)
			   ctx))))))))
   
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
	 (js-put! proto 'getSession
	    (js-make-function %this connection-get-session
	       0 "getSession")
	    #f %this)
	 (js-put! proto 'getCurrentCipher
	    (js-make-function %this connection-get-current-cipher
	       0 "getCurrentCipher")
	    #f %this)
	 (js-put! proto 'loadSession
	    (js-make-function %this connection-load-session
	       1 "loadSession")
	    #f %this)
	 (js-put! proto 'verifyError
	    (js-make-function %this connection-verify-error
	       1 "verifyError")
	    #f %this)
	 (js-put! proto 'getPeerCertificate
	    (js-make-function %this connection-get-peer-certificate
	       1 "getPeerCertificate")
	    #f %this)
	 (js-put! proto 'isSessionReused
	    (js-make-function %this connection-session-reused?
	       1 "isSessionReused")
	    #f %this)
	 (js-put! proto 'getNegotiatedProtocol
	    (js-make-function %this connection-get-negotiated-protocol
	       0 "getNegotiatedProtocol")
	    #f %this)
	 (js-put! proto 'setNPNProtocols
	    (js-make-function %this connection-set-npn-protocols
	       1 "setNPNProtocols")
	    #f %this)
	 (js-put! proto 'getServername
	    (js-make-function %this connection-get-servername
	       0 "getServername")
	    #f %this)
	 (js-put! proto 'setSNICallback
	    (js-make-function %this connection-set-sni-callback
	       1 "setSNICallback")
	    #f %this)
	 proto))
   
   (define (secure-context this . args)
      (instantiate::JsSecureContext
	 (__proto__ secure-context-proto)))

   (define (info-callback this state)
      (when (>fx debug-crypto 0)
	 (tprint ">>> info-callback state=" state))
      (if (=fx state 0)
	  ;; start
	  (let ((onhandshakestart (js-get this 'onhandshakestart %this)))
	     (when (>fx debug-crypto 0)
		(tprint "onhandshakestart"))
	     (!js-callback0 "onhandshakestart" %worker %this
		onhandshakestart this))
	  ;; done
	  (let ((onhandshakedone (js-get this 'onhandshakedone %this)))
	     (when (>fx debug-crypto 0)
		(tprint "onhandshakedone"))
	     (!js-callback0 "onhandshakedone" %worker %this
		onhandshakedone this)))
      (when (>fx debug-crypto 0)
	 (tprint "<<< info-callback")))

   (define (newsession-callback this session-id::bstring serialized::bstring)
      (let ((onnewsession (js-get this 'onnewsession %this)))
	 (!js-callback2 "onnewsession" %worker %this
	    onnewsession this
	    (js-string->jsfastbuffer session-id %this)
	    (js-string->jsfastbuffer serialized %this))))
   
   (define (connection this jsctx serverp request-cert-or-server-name reject)
      (with-access::JsSecureContext jsctx (ctx)
	 (co-instantiate ((hparser (instantiate::HelloParser
				      (%worker %worker)
				      (%this %this)
				      (conn conn)))
			  (conn (instantiate::JsSSLConnection
				   (__proto__ connection-proto)
				   (hparser hparser)
				   (ssl (instantiate::ssl-connection
					   (ctx ctx)
					   (info-callback (lambda (start-or-done)
							     (info-callback
								conn start-or-done)))
					   (newsession-callback (lambda (session-id serialized)
								   (newsession-callback conn
								      session-id serialized)))
					   (isserver (js-toboolean serverp))
					   (request-cert (when serverp
							    request-cert-or-server-name))
					   (server-name (unless serverp
							   (when (js-jsstring? request-cert-or-server-name)
							      (js-jsstring->string request-cert-or-server-name))))
					   (reject-unauthorized reject))))))
	    (js-bind! %this conn 'receivedShutdown
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsSSLConnection this (ssl)
			     (with-access::ssl-connection ssl (received-shutdown)
				(or received-shutdown (js-undefined)))))
		       0 "receivedShutdown"))
	    (js-bind! %this conn 'sentShutdown
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsSSLConnection this (ssl)
			     (with-access::ssl-connection ssl (sent-shutdown)
				(or sent-shutdown (js-undefined)))))
		       0 "sentShutdown"))
	    (js-bind! %this conn 'error
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsSSLConnection this (ssl)
			     (with-access::ssl-connection ssl (err)
				(if (string? err)
				    (with-access::JsGlobalObject %this (js-error)
				       (js-new %this js-error
					  (js-string->jsstring err)))
				    err))))
		       0 "error")
	       :set (js-make-function %this
		       (lambda (this v)
			  (with-access::JsSSLConnection this (ssl)
			     (with-access::ssl-connection ssl (err)
				(if (js-jsstring? v)
				    (set! err (js-jsstring->string v))
				    (set! err #f)))))
		       1 "error"))
	    conn)))

   (define (check-entropy)
      (let loop ()
	 (unless (ssl-rand-status)
	    (unless (ssl-rand-poll)))))
   
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
		 (!js-callback2 "randomBytes" %worker %this
		    cb this (js-undefined) buf)
		 buf)))))

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
		 (!js-callback2 "pseudoRandomBytes" %worker %this
		    cb this (js-undefined) buf)
		 buf)))))

   (define (get-ssl-ciphers this)
      (let ((v (ssl-get-ciphers)))
	 (js-vector->jsarray
	    (vector-map! js-string->jsstring v) %this)))

   (define (get-ciphers this)
      (let ((v (evp-get-ciphers)))
	 (js-vector->jsarray
	    (vector-map! js-string->jsstring (list->vector v)) %this)))

   (define (get-hashes this)
      (let ((v (evp-get-hashes)))
	 (js-vector->jsarray
	    (vector-map! js-string->jsstring (list->vector v)) %this)))

   ;; diffie-hellman
   (define (dh-set-private-key this buffer)
      (with-access::JsDH this (initp dh)
	 (if (not initp)
	     (js-raise-error %this "Not initialize" this)
	     (with-access::dh dh (private-key)
		(set! private-key
		   (bn-bin2bn
		      (buf->string buffer "dh-set-private-key" %this)))))))

   (define (dh-set-public-key this buffer)
      (with-access::JsDH this (initp dh)
	 (if (not initp)
	     (js-raise-error %this "Not initialize" this))
	 (with-access::dh dh (public-key)
	    (set! public-key
	       (bn-bin2bn
		  (buf->string buffer "dh-set-public-key" %this))))))

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
	 (let* ((str (buf->string buffer "dh-compute-secret" %this))
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
	       (begin
		  (ssl-clear-error)
		  (bn-free key))))))

   (define (dh-get-prime this)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (with-access::dh dh (p)
	    (js-string->jsslowbuffer (bn-bn2bin p) %this))))

   (define (dh-get-public-key this)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (with-access::dh dh (public-key)
	    (js-string->jsslowbuffer (bn-bn2bin public-key) %this))))

   (define (dh-get-private-key this)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (with-access::dh dh (private-key)
	    (js-string->jsslowbuffer (bn-bn2bin private-key) %this))))

   (define (dh-get-generator this)
      (with-access::JsDH this (initp dh)
	 (unless initp
	    (js-raise-error %this "Not initialize" this))
	 (with-access::dh dh (g)
	    (js-string->jsslowbuffer (bn-bn2bin g) %this))))

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
	 (js-put! proto 'getPrime
	    (js-make-function %this dh-get-prime
	       0 "getPrime")
	    #f %this)
	 (js-put! proto 'getPublicKey
	    (js-make-function %this dh-get-public-key
	       0 "getPublicKey")
	    #f %this)
	 (js-put! proto 'getPrivateKey
	    (js-make-function %this dh-get-private-key
	       0 "getPrivateKey")
	    #f %this)
	 (js-put! proto 'getGenerator
	    (js-make-function %this dh-get-generator
	       1 "getGenerator")
	    #f %this)
	 proto))

   (define (diffie-hellman-string dh obj str)
      (with-access::dh dh (p g)
	 (set! p (bn-bin2bn str))
	 (set! g (bn-new))
	 (when (bn-set-word g 2)
	    (cond
	       ((dh-check dh)
		=>
		(lambda (m)
		   (js-raise-error %this
		      (format "Initialization failed (~a)" m) dh)))
	       (else
		(with-access::JsDH obj (initp)
		   (set! initp #t)))))
	 obj))

   (define (diffie-hellman-string2 dh obj str str2)
      (with-access::dh dh (p g)
	 (set! p (bn-bin2bn str))
	 (set! g (bn-bin2bn str2))
	 (with-access::JsDH obj (initp)
	    (set! initp #t))
	 obj))
   
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
	    ((isa? (car args) JsSlowBuffer)
	     (diffie-hellman-string dh obj
		(js-jsslowbuffer->string (car args))))
	    ((isa? (car args) JsFastBuffer)
	     (diffie-hellman-string dh obj
		(js-jsfastbuffer->string (car args))))
	    ((pair? (car args))
	     (diffie-hellman-string2 dh obj (caar args) (cdar args)))
	    (else
	     (js-raise-error %this
		(format "Wrong initialization value (~a)" (typeof (car args)))
		(car args))))
	 obj))

   (define (diffie-hellman-group this group-name)
      (unless (js-jsstring? group-name)
	 (js-raise-type-error %this
	    (format "Bad parameter ~a" (typeof group-name))
	    group-name))
      (let* ((name (js-jsstring->string group-name))
	     (buf (assoc name modp_groups)))
	 (if buf
	     (diffie-hellman this (cadr buf))
	     (error "diffie-hellman-group" "todo" group-name))))

   ;; hmac
   (define (hmac-init this type key)
      (if (not (js-jsstring? type))
	  (js-raise-type-error %this
	     "Must be given hashtype string as argument" type)
	  (let ((key (buf->string key "hmac-init" %this)))
	     (with-access::JsHmac this (hmac)
		(ssl-hmac-init hmac (js-jsstring->string type) key)
		this))))

   (define (hmac-update this data)
      (with-access::JsHmac this (hmac)
	 (multiple-value-bind (s offset len)
	    (data->string data "hmac-update" %this)
	    (ssl-hmac-update! hmac s offset len)
	    this)))

   (define (hmac-digest this enc)
      (with-access::JsHmac this (hmac)
	 (string-encode %this (ssl-hmac-digest hmac) enc)))

   (define hmac-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'init
	    (js-make-function %this hmac-init 1 "init")
	    #f %this)
	 (js-put! proto 'update
	    (js-make-function %this hmac-update 1 "update")
	    #f %this)
	 (js-put! proto 'digest
	    (js-make-function %this hmac-digest 1 "digest")
	    #f %this)
	 proto))

   (define (hmac this type data)
      (instantiate::JsHmac
	 (__proto__ hmac-proto)
	 (hmac (instantiate::ssl-hmac))))

   ;; hash
   (define (hash-update this data enc)
      (with-access::JsHash this (hash)
	 (if (eq? enc (js-undefined))
	     (multiple-value-bind (s offset len)
		(data->string data "hash-update" %this)
		(ssl-hash-update! hash s offset len))
	     (let* ((s (buf->string data "hash-update" %this))
		    (str (string-decode s enc %this)))
		(ssl-hash-update! hash str 0 (string-length str))))))

   (define (hash-digest this enc)
      (with-access::JsHash this (hash)
	 (string-encode %this (ssl-hash-digest hash) enc)))
   
   (define hash-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'update
	    (js-make-function %this hash-update 2 "update")
	    #f %this)
	 (js-put! proto 'digest
	    (js-make-function %this hash-digest 0 "digest")
	    #f %this)
	 proto))

   (define (hash this type)
      (instantiate::JsHash
	 (__proto__ hash-proto)
	 (hash (instantiate::ssl-hash
		  (type (js-jsstring->string type))))))

   ;; sign
   (define (sign-init this type)
      (if (not (js-jsstring? type))
	  (js-raise-type-error %this
	     "Must be given signtype string as argument" type)
	  (with-access::JsSign this (sign)
	     (ssl-sign-init sign (js-jsstring->string type))
	     this)))

   (define (sign-update this data enc)
      (with-access::JsSign this (sign)
	 (if (eq? enc (js-undefined))
	     (multiple-value-bind (s offset len)
		(data->string data "sign-update" %this)
		(ssl-sign-update! sign s offset len))
	     (let* ((s (buf->string data "sign-update" %this))
		    (str (string-decode s enc %this)))
		(ssl-sign-update! sign str 0 (string-length str))))))

   (define (sign-sign this data enc)
      (with-access::JsSign this (sign)
	 (multiple-value-bind (s offset len)
	    (data->string data "sign-sign" %this)
	    (string-encode %this (ssl-sign-sign sign s offset len) enc))))
   
   (define sign-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'init
	    (js-make-function %this sign-init 1 "init")
	    #f %this)
	 (js-put! proto 'update
	    (js-make-function %this sign-update 2 "update")
	    #f %this)
	 (js-put! proto 'sign
	    (js-make-function %this sign-sign 2 "sign")
	    #f %this)
	 proto))

   (define (sign this)
      (instantiate::JsSign
	 (__proto__ sign-proto)
	 (sign (instantiate::ssl-sign))))

   ;; verify
   (define (verify-init this type)
      (if (not (js-jsstring? type))
	  (js-raise-type-error %this
	     "Must be given verifytype string as argument" type)
	  (with-access::JsVerify this (verify)
	     (ssl-verify-init verify (js-jsstring->string type))
	     this)))

   (define (verify-update this data enc)
      (with-access::JsVerify this (verify)
	 (if (eq? enc (js-undefined))
	     (multiple-value-bind (s offset len)
		(data->string data "verify-update" %this)
		(ssl-verify-update! verify s offset len))
	     (let* ((s (buf->string data "verify-update" %this))
		    (str (string-decode s enc %this)))
		(ssl-verify-update! verify str 0 (string-length str))))))

   (define (verify-final this data sig enc)
      (with-access::JsVerify this (verify)
	 (multiple-value-bind (ds doffset dlen)
	    (data->string data "verify-verify" %this)
	    (multiple-value-bind (ss soffset slen)
	       (data->string sig "verify-verify" %this)
	       (ssl-verify-final verify
		  ds doffset dlen
		  ss soffset slen)))))
   
   (define verify-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'init
	    (js-make-function %this verify-init 1 "init")
	    #f %this)
	 (js-put! proto 'update
	    (js-make-function %this verify-update 2 "update")
	    #f %this)
	 (js-put! proto 'verify
	    (js-make-function %this verify-final 3 "verify")
	    #f %this)
	 proto))

   (define (verify this)
      (instantiate::JsVerify
	 (__proto__ verify-proto)
	 (verify (instantiate::ssl-verify))))

   ;; cipher
   (define (cipher-init this type key)
      (with-access::JsCipher this (cipher)
	 (if (not (js-jsstring? type))
	     (js-raise-type-error %this
		"Must be given cipher type string as argument" type)
	     (multiple-value-bind (s offset len)
		(data->string key "cipher-init" %this)
		(ssl-cipher-init cipher (js-jsstring->string type)
		   s offset len
		   (not (isa? this JsDecipher)))))))

   (define (cipher-initiv this type key iv)
      (with-access::JsCipher this (cipher)
	 (if (not (js-jsstring? type))
	     (js-raise-type-error %this
		"Cipher must be given cipher type string as argument" type)
	     (multiple-value-bind (ks koffset klen)
		(data->string key "cipher-initiv" %this)
		(multiple-value-bind (is ioffset ilen)
		   (data->string iv "cipher-init" %this)
		   (ssl-cipher-initiv cipher (js-jsstring->string type)
		      ks koffset klen
		      is ioffset ilen
		      (not (isa? this JsDecipher))))))))

   (define (cipher-update this data ienc oenc)
      (with-access::JsCipher this (cipher)
	 ;; this functio should be rewritten to avoid allocating
	 ;; so many auxiliary strings 
	 (let* ((s (buf->string data "cipher-update" %this))
		(str (string-decode s ienc %this))
		(so (ssl-cipher-update! cipher str 0 (string-length str))))
	    (string-encode %this so oenc))))

   (define (cipher-final this enc)
      (with-access::JsCipher this (cipher)
	 (tprint "CIPHER=" cipher)
	 (with-handler
	    (lambda (e)
	       (exception-notify e)
	       (with-access::&error e (msg)
		  (js-raise-type-error %this msg e)))
	    (string-encode %this (ssl-cipher-final cipher) enc))))

   (define (cipher-set-auto-padding this ap)
      (with-access::JsCipher this (cipher)
	 (ssl-cipher-set-auto-padding cipher ap)))
      
   (define cipher-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (js-put! proto 'init
	    (js-make-function %this cipher-init 1 "init")
	    #f %this)
	 (js-put! proto 'initiv
	    (js-make-function %this cipher-initiv 2 "initiv")
	    #f %this)
	 (js-put! proto 'update
	    (js-make-function %this cipher-update 3 "update")
	    #f %this)
	 (js-put! proto 'final
	    (js-make-function %this cipher-final 1 "final")
	    #f %this)
	 (js-put! proto 'setAutoPadding
	    (js-make-function %this cipher-set-auto-padding 1 "setAutoPadding")
	    #f %this)
	 proto))

   (define decipher-proto
      (let ((proto (with-access::JsGlobalObject %this (js-object)
		      (js-new %this js-object))))
	 (with-access::JsObject proto (__proto__)
	    (set! __proto__ cipher-proto))
	 (js-put! proto 'finaltol
	    (js-make-function %this cipher-final 1 "finaltol")
	    #f %this)
	 proto))
	    
   (define (cipher this)
      (instantiate::JsCipher
	 (__proto__ cipher-proto)
	 (cipher (instantiate::ssl-cipher))))

   (define (decipher this)
      (instantiate::JsDecipher
	 (__proto__ decipher-proto)
	 (cipher (instantiate::ssl-cipher))))

   ;; pbkdf2
   (define (pbkdf2 this password salt iterations keylen callback)
      (with-access::JsGlobalObject %this (js-object)
	 (with-handler
	    (lambda (err)
	       (if (isa? callback JsFunction)
		   (let ((obj (js-new %this js-object)))
		      (js-put! obj 'ondone callback #f %this)
		      (js-call2 %this callback obj err (js-undefined)))
		   (raise err)))
	    (let ((r (string-encode %this
			(pkcs5-pbkdf2-hmac-sha1
			   (buf->string password "pbkdf2" %this)
			   (buf->string salt "pbkdf2" %this)
			   iterations
			   keylen)
			(js-undefined))))
	       (if (isa? callback JsFunction)
		   (let ((obj (js-new %this js-object)))
		      (js-put! obj 'ondone callback #f %this)
		      (js-call2 %this callback obj (js-undefined) r))
		   r)))))

   (let ((sc (js-make-function %this secure-context 1 "SecureContext"
		:construct secure-context
		:prototype secure-context-proto))
	 (conn (js-make-function %this connection 4 "Connection"
		  :construct connection
		  :prototype connection-proto))
	 (dh (js-make-function %this diffie-hellman 4 "DiffieHellman"
		  :construct diffie-hellman
		  :prototype diffie-hellman-proto))
	 (dhg (js-make-function %this diffie-hellman-group 1 "DiffieHellmanGroup"
		  :construct diffie-hellman-group
		  :prototype diffie-hellman-proto))
	 (hm (js-make-function %this hmac 1 "Hmac"
		:construct hmac
		:prototype hmac-proto))
	 (hs (js-make-function %this hash 1 "Hash"
		:construct hash
		:prototype hash-proto))
	 (sn (js-make-function %this sign 1 "Sign"
		:construct sign
		:prototype sign-proto))
	 (vf (js-make-function %this sign 1 "Verify"
		:construct verify
		:prototype verify-proto))
	 (ci (js-make-function %this sign 1 "cipher"
		:construct cipher
		:prototype cipher-proto))
	 (dc (js-make-function %this sign 1 "decipher"
		:construct decipher
		:prototype cipher-proto)))
      
      (with-access::JsGlobalObject %this (js-object)
	 (js-alist->jsobject
	    `((SecureContext . ,sc)
	      (Connection . ,conn)
	      (Cipher . ,ci)
	      (Decipher . ,dc)
	      (DiffieHellman . ,dh)
	      (DiffieHellmanGroup . ,dhg)
	      (Hmac . ,hm)
	      (Hash . ,hs)
	      (Sign . ,sn)
	      (Verify . ,vf)
	      
	      (PBKDF2 . ,(js-make-function %this pbkdf2
			    5 "pbkdf2"))
	      (randomBytes . ,(js-make-function %this randomBytes
				 2 "randomBytes"))
	      (pseudoRandomBytes . ,(js-make-function %this pseudoRandomBytes
				       2 "pseudoRandomBytes"))
	      (getSSLCiphers . ,(js-make-function %this get-ssl-ciphers
				   0 "getSSLCiphers"))
	      (getCiphers . ,(js-make-function %this get-ciphers
				0 "getCiphers"))
	      (getHashes . ,(js-make-function %this get-hashes
			       0 "getHashes")))
	    %this))))

;*---------------------------------------------------------------------*/
;*    string-encode ...                                                */
;*---------------------------------------------------------------------*/
(define (string-encode %this data encoding)
   (cond
      ((eq? encoding (js-undefined))
       (js-string->jsfastbuffer data %this))
      ((js-jsstring? encoding)
       (case (string->symbol (js-jsstring->string encoding))
	  ((buffer)
	   (js-string->jsfastbuffer data %this))
	  ((hex)
	   (js-string->jsstring
	      (string-hex-extern data 0 (string-length data))))
	  ((ucs2)
	   (js-string->jsstring
	      (string->ucs2-string data 0 (string-length data))))
	  ((base64)
	   (let ((ip (open-input-string! data 0 (string-length data)))
		 (op (open-output-string)))
	      (base64-encode-port ip op 0)
	      (js-string->jsstring
		 (close-output-port op))))
	  ((ascii)
	   (let* ((len (string-length data))
		  (string (make-string len)))
	      (when (>fx len 0)
		 (blit-string-ascii-clamp! data 0 string 0 len))
	      (js-string->jsstring string)))
	  ((utf8 utf-8)
	   (js-string->jsstring
	      (string-utf8-normalize-utf16 data 0 (string-length data))))
	  ((binary)
	   (js-string->jsstring
	      (8bits-encode-utf8 data 0 (string-length data))))
	  (else
	   (error "crypto" "bad encoding" encoding))))
      (else
       (error "crypto" "bad encoding" encoding))))

;*---------------------------------------------------------------------*/
;*    string-decode ...                                                */
;*---------------------------------------------------------------------*/
(define (string-decode data encoding %this)
   (cond
      ((eq? encoding (js-undefined))
       data)
      ((js-jsstring? encoding)
       (case (string->symbol (js-jsstring->string encoding))
	  ((buffer)
	   data)
	  ((hex)
	   (if (oddfx? (string-length data))
	       (js-raise-type-error %this "Bad input string" data)
	       (string-hex-intern data)))
	  ((ucs2)
	   (string->ucs2-string data 0 (string-length data)))
	  ((base64)
	   (base64-decode data #f))
	  ((ascii)
	   data)
	  ((utf8 utf-8)
	   data)
	  ((binary)
	   data)
;* 	   (utf8->iso-latin data))                                     */
	  (else
	   (error "crypto" "bad encoding" encoding))))
      (else
       (error "crypto" "bad encoding" encoding))))

;*---------------------------------------------------------------------*/
;*    Hello-Parser Constants                                           */
;*---------------------------------------------------------------------*/
(define (kClientHello) #a001)

(define (kChangeCipherSpec) #a020)
(define (kAlert) #a021)
(define (kHandshake) #a022)
(define (kApplicationData) #a023)
(define (kOther) #a255)

;*---------------------------------------------------------------------*/
;*    kBufferSize ...                                                  */
;*---------------------------------------------------------------------*/
(define (kBufferSize data)
   (string-length data))

;*---------------------------------------------------------------------*/
;*    iref ...                                                         */
;*---------------------------------------------------------------------*/
(define (iref str idx)
   (char->integer (string-ref-ur str idx)))

;*---------------------------------------------------------------------*/
;*    hello-parser-ended ...                                           */
;*---------------------------------------------------------------------*/
(define (hello-parser-ended hparser::HelloParser)
   (with-access::HelloParser hparser (state)
      (eq? state 'kEnded)))

;*---------------------------------------------------------------------*/
;*    hello-parser-finish ...                                          */
;*    -------------------------------------------------------------    */
;*    See node_crypto.cc ClientHelloParser::Finish                     */
;*---------------------------------------------------------------------*/
(define (hello-parser-finish hparser::HelloParser)
   (with-access::HelloParser hparser (state data conn offset)
      (with-access::JsSSLConnection conn (ssl)
	 (when (>fx debug-crypto 0)
	    (tprint "BIO_write.1 offset=" offset))
	 (ssl-connection-write ssl data 0 offset))
      (set! state 'kEnded)
      (set! data "")))
   
;*---------------------------------------------------------------------*/
;*    hello-parser-write ...                                           */
;*    -------------------------------------------------------------    */
;*    See src/node_crypto.cc ClientHelloParser::Write                  */
;*---------------------------------------------------------------------*/
(define (hello-parser-write hparser::HelloParser
	   buffer::bstring offset::long len::long)
   (with-access::HelloParser hparser (state (data_ data) (offset_ offset) conn
					frame-len body-offset)
      (when (>fx debug-crypto 0)
	 (tprint "HelloParser state=" state))
      (if (eq? state 'kPaused)
	  0
	  (let* ((available (-fx (kBufferSize data_) offset_))
		 (copied (if (<fx len available) len available))
		 (is-clienthello #f)
		 (session-size -1)
		 (session-id-offset 0))
	     (blit-string! buffer offset data_ offset_ copied)
	     (when (>fx debug-crypto 0)
		(tprint "HelloParser avail=" available " copied=" copied " offset=" offset_ )
		(tprint "data_="
		   (iref data_ offset_) " " 
		   (iref data_ (+fx 1 offset_)) " " 
		   (iref data_ (+fx 2 offset_)) " " 
		   (iref data_ (+fx 3 offset_)) " " 
		   (iref data_ (+fx 4 offset_)) " " 
		   (iref data_ (+fx 5 offset_)))
		(tprint "buffer="
		   (iref buffer 0) " " 
		   (iref buffer (+fx 1 0)) " " 
		   (iref buffer (+fx 2 0)) " " 
		   (iref buffer (+fx 3 0)) " " 
		   (iref buffer (+fx 4 0)) " " 
		   (iref buffer (+fx 5 0))))
	     (set! offset_ (+fx offset_ copied))
	     (let loop ()
		(case state
		   ((kWaiting)
		    (when (>fx debug-crypto 0)
		       (tprint "HelloParser state.2=" state))
		    (if (<fx offset_ 5)
			;; >= 5 bytes for header parsing
			copied
			(begin
			   (if (or (char=? (string-ref data_ 0) (kChangeCipherSpec))
				   (char=? (string-ref data_ 0) (kHandshake))
				   (char=? (string-ref data_ 0) (kApplicationData)))
			       (begin
				  (set! frame-len
				     (+fx (bit-lsh (iref data_ 3) 8) (iref data_ 4)))
				  (set! state 'kTLSHeader)
				  (set! body-offset 5))
			       (begin
				  (set! frame-len
				     (+fx (bit-lsh (iref data_ 0) 8) (iref data_ 1)))
				  (set! state 'kSSLHeader)
				  (if (=fx (bit-and (iref data_ 0) #x40) 0)
				      ;; no padding
				      (set! body-offset 2)
				      ;; padding
				      (set! body-offset 3))))

			   (when (>fx debug-crypto 0)
			      (tprint "HelloParser frame_len=" frame-len
				 " body_offset=" body-offset))
			   ;; Sanity check (too big frame, or too small)
			   (if (>= frame-len (kBufferSize data_))
			       (begin
				  ;; Let OpenSSL handle it
				  (hello-parser-finish hparser)
				  copied)
			       (loop)))))
		   ((kTLSHeader kSSLHeader)
		    (when (>fx debug-crypto 0)
		       (tprint "HelloParser state.3=" state
			  " offset=" offset_ " body+frame=" (+fx body-offset frame-len)))
		    ;; >= 5 + frame size bytes for frame parsing
		    (if (<fx offset_ (+fx body-offset frame-len))
			copied
			(begin
			   ;; Skip unsupported frames and gather some data from frame
			   ;; TODO: Check protocol version
			   (when (char=? (string-ref data_ body-offset) (kClientHello))
			      (set! is-clienthello #t)
			      (case state
				 ((kTLSHeader)
				  ;; Skip frame header, hello header, protocol version and random data
				  (let ((session-offset (+fx body-offset (+fx 4 (+fx 2 32)))))
				     (if (<fx (+fx session-offset 1) offset_)
					 (begin
					    (set! session-size (iref data_ session-offset))
					    (set! session-id-offset (+fx session-offset 1))))
				     (when (>fx debug-crypto 0)
					(tprint "HelloParser == kClientHello, state=" state
					   " session-offset=" session-offset
					   " session-size=" session-size
					   " session-id-offset=" session-id-offset))))
				 ((kSSLHeader)
				  ;; Skip header, version
				  (let ((session-offset (+fx body-offset 3)))
				     (if (<fx (+fx session-offset 4) offset_)
					 (let ((ciphers-size
						  (+fx (bit-lsh (iref data_ session-offset) 8)
						     (iref data_ (+fx session-offset 1)))))
					    (when (<fx (+fx session-offset (+fx 4 ciphers-size)) offset_)
					       (set! session-size
						  (+fx (bit-lsh (iref data_ (+fx session-offset 2)) 8)
						     (iref data_ (+fx session-offset 3))))
					       (set! session-id-offset
						  (+fx session-offset (+fx 4 ciphers-size))))))))
				 (else
				  ;; Whoa? How did we get here?
				  (error "crypto" "bad state" state)))
			      
			      ;; Check if we overflowed (do not reply with any private data)
			      (if (or (=fx session-id-offset 0)
				      (>fx session-size 32)
				      (>fx (+fx session-id-offset session-size) offset_))
				  (begin
				     (hello-parser-finish hparser)
				     copied))
			      ;; TODO: Parse other things?
			      )

			   ;; Not client hello - let OpenSSL handle it
			   (if (not is-clienthello)
			       (begin
				  (hello-parser-finish hparser)
				  copied)
			       (with-access::HelloParser hparser (%this %worker)
				  (with-access::JsGlobalObject %this (js-object)
				     (let ((hello (js-new %this js-object))
					   (buffer (js-string->jsfastbuffer data_ %this))
					   (onclienthello (js-get conn 'onclienthello %this)))
					;; Parse frame, call javascript handler and
					;; move parser into the paused state
					(with-access::JsFastBuffer buffer (byteoffset length)
					   (set! byteoffset (fixnum->uint32 session-id-offset))
					   (set! length (fixnum->uint32 session-size))
					   (js-put! buffer 'length session-size #f %this)
					   (js-put! hello 'sessionId buffer #f %this)
					   (set! state 'kPaused)
					   (when (>fx debug-crypto 0)
					      (tprint "HelloParser session_size=" session-size
						 " callback "
						 (iref data_ (+fx (uint32->fixnum byteoffset) 0)) " " 
						 (iref data_ (+fx (uint32->fixnum byteoffset) 1)) " " 
						 (iref data_ (+fx (uint32->fixnum byteoffset) 2)) " " 
						 (iref data_ (+fx (uint32->fixnum byteoffset) 3))
						 " => copied=" copied)))
					(!js-callback1 "HelloParser" %worker %this
					   onclienthello conn hello)
					copied)))))))
		   (else
		    copied)))))))
   
;*---------------------------------------------------------------------*/
;*    no ssl support                                                   */
;*---------------------------------------------------------------------*/
)
(else
 (define (crypto-constants) '())
 (define (process-crypto %worker %this)
    #unspecified)))

;*---------------------------------------------------------------------*/
;*    data->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (data->string buf proc %this::JsGlobalObject)
   (cond
      ((js-jsstring? buf)
       (let ((s (js-jsstring->string buf)))
	  (values s 0 (string-length s))))
      ((isa? buf JsFastBuffer)
       (with-access::JsFastBuffer buf (%data byteoffset length)
	  (let ((start (uint32->fixnum byteoffset))
		(len (uint32->fixnum length)))
	     (values %data start len))))
      ((isa? buf JsSlowBuffer)
       (with-access::JsSlowBuffer buf (data)
	  (values data 0 (string-length data))))
      (else
       (js-raise-type-error %this
	  (string-append proc ": Not a string or buffer (" (typeof buf) ")") buf))))

;*---------------------------------------------------------------------*/
;*    buf->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (buf->string buf proc %this::JsGlobalObject)
   (cond
      ((js-jsstring? buf)
       (js-jsstring->string buf))
      ((isa? buf JsFastBuffer)
       (js-jsfastbuffer->string buf))
      ((isa? buf JsSlowBuffer)
       (js-jsslowbuffer->string buf))
      (else
       (js-raise-type-error %this
	  (string-append proc ": Not a string or buffer") buf))))
