;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_crypto.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 23 08:47:08 2014                          */
;*    Last change :  Fri May  8 07:57:00 2015 (serrano)                */
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
	       (hparser::HelloParser read-only)
	       (ssl (default #unspecified))
	       (next-session::pair-nil (default '())))

	    (class JsDH::JsObject
	       (dh (default #unspecified))
	       (initp::bool (default #f)))

	    (class HelloParser
	       (%worker::WorkerHopThread read-only)
	       (%this::JsGlobalObject read-only)
	       (state::symbol (default 'kWaiting))
	       (data::bstring (default (make-string 18432)))
	       (offset::long (default 0))
	       (body-offset::long (default 0))
	       (frame-len::long (default 0))
	       (conn::JsSSLConnection read-only)))
	       
   (import  __nodejs_process
	    __nodejs__buffer
	    __nodejs_uv)
   
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
		   (js-get cert 'length %this))))))

   (define (add-crl this cert)
      (with-access::JsSecureContext this (ctx)
	 (if (isa? cert JsStringLiteral)
	     (let ((cert (js-jsstring->string cert)))
		(secure-context-add-crl! ctx cert 0 (string-length cert)))
	     (with-access::JsTypedArray cert (%data byteoffset length)
		(secure-context-add-crl! ctx %data
		   (uint32->fixnum byteoffset)
		   (js-get cert 'length %this))))))

   (define (set-key this cert passphrase)
      (with-access::JsSecureContext this (ctx)
	 (let ((pass (when (string? passphrase) passphrase)))
	    (if (isa? cert JsStringLiteral)
		(let ((cert (js-jsstring->string cert)))
		   (secure-context-set-key! ctx cert 0 (string-length cert) pass))
		(with-access::JsTypedArray cert (%data byteoffset length)
		   (secure-context-set-key! ctx %data
		      (uint32->fixnum byteoffset)
		      (js-get cert 'length %this)
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

   (define (set-session-id-context this sic)
      (with-access::JsSecureContext this (ctx)
	 (if (isa? sic JsStringLiteral)
	     (let ((sic (js-jsstring->string sic)))
		(secure-context-set-session-id-context! ctx sic 0 (string-length sic)))
	     (with-access::JsTypedArray sic (%data byteoffset length)
		(secure-context-set-session-id-context! ctx %data
		   (uint32->fixnum byteoffset)
		   (uint32->fixnum length))))))

   (define (load-pkcs12 this pfx pass)
      (with-access::JsSecureContext this (ctx)
	 (if (and (isa? pfx JsStringLiteral) (isa? pass JsStringLiteral)
		  (secure-context-load-pkcs12 ctx
		     (js-jsstring->string pfx)
		     (js-jsstring->string pass)))
	     (js-raise-type-error %this
		(format "Bad parameter (~a, ~a) ~~a" (typeof pfx) (typeof pass))
		pfx))))

   (define (set-ciphers this ciphers)
      (with-access::JsSecureContext this (ctx)
	 (cond
	    ((isa? ciphers JsStringLiteral)
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
	       (lambda (this srvname)
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
							   request-cert-or-server-name))
					   (reject-unauthorized reject))))))
	    (js-bind! %this conn 'receivedShutdown
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsSSLConnection this (ssl)
			     (with-access::ssl-connection ssl (received-shutdown)
				received-shutdown)))
		       0 "receivedShutdown"))
	    (js-bind! %this conn 'sentShutdown
	       :get (js-make-function %this
		       (lambda (this)
			  (with-access::JsSSLConnection this (ssl)
			     (with-access::ssl-connection ssl (sent-shutdown)
				sent-shutdown)))
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
				(if (isa? v JsStringLiteral)
				    (set! err (js-jsstring->string v))
				    (set! err #f)))))
		       1 "error"))
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
		 (!js-callback2 "pseudoRandomBytes" %worker %this
		    cb this (js-undefined) buf)
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
		 (!js-callback2 "randomBytes" %worker %this
		    cb this (js-undefined) buf)
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
	      (Hash . ,(not-implemented "Hash"))
	      (DiffieHellman . ,dh)
	      (SecureContext . ,sc)
	      (Connection . ,conn))
	    %this))))

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
 (define (process-crypto %worker %this)
    #unspecified)))

