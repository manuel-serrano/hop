;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/arch/android/hopdroid/tts.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 29 16:36:58 2010                          */
;*    Last change :  Sat Oct 13 07:49:00 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Text-to-speech                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-tts

   (library multimedia phone hop)

   (import __hopdroid-phone)

   (export (class androidtts
	      (androidtts-init)
	      (%mutex::mutex (default (make-mutex)))
	      (%open::bool (default #t))
	      (%pitch::double (default 1.0))
	      (%rate::double (default 1.0))
	      (phone::androidphone read-only))

	   (androidtts-init ::androidtts)

	   (register-tts-listener! p::androidphone)
	   (remove-tts-listener! p::androidphone)
	   
	   (tts-close ::androidtts)
	   (tts-closed?::bool ::androidtts)
	   (tts-locale ::androidtts)
	   (tts-locale-set! ::androidtts ::obj)
	   (tts-locale-check::symbol ::androidtts ::obj)
	   (tts-pitch::double ::androidtts)
	   (tts-pitch-set! ::androidtts ::obj)
	   (tts-rate::double ::androidtts)
	   (tts-rate-set! ::androidtts ::obj)
	   (tts-synthesize ::androidtts ::bstring ::bstring)
	   (tts-speak ::androidtts ::bstring #!key stream mode)
	   (tts-silence ::androidtts ::int #!key stream mode)
	   (tts-is-speaking?::bool ::androidtts)
	   (tts-stop ::androidtts)))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define tts-plugin #f)

;*---------------------------------------------------------------------*/
;*    androidtts-init ::androidtts ...                                 */
;*---------------------------------------------------------------------*/
(define (androidtts-init t)
   (with-access::androidtts t (%open phone)
      (unless tts-plugin
	 (set! tts-plugin (android-load-plugin phone "tts")))
      (let ((msg (android-send-command/result phone tts-plugin #\i)))
	 (if (string=? msg "success")
	     (set! %open #t)
	     (error "androidtts" msg t)))))

;*---------------------------------------------------------------------*/
;*    register-tts-listener! ...                                       */
;*---------------------------------------------------------------------*/
(define (register-tts-listener! p::androidphone)
   (unless tts-plugin
      (error "add-event-listener!" "No tts plugin registred" tts-plugin))
   (android-send-command p tts-plugin #\b))

;*---------------------------------------------------------------------*/
;*    remove-tts-listener! ...                                         */
;*---------------------------------------------------------------------*/
(define (remove-tts-listener! p::androidphone)
   (android-send-command p tts-plugin #\e))

;*---------------------------------------------------------------------*/
;*    tts-close ::androidtts ...                                       */
;*---------------------------------------------------------------------*/
(define (tts-close t::androidtts)
   (with-access::androidtts t (%open %mutex phone)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (set! %open #f)
	       (android-send-command phone tts-plugin #\c))))))

;*---------------------------------------------------------------------*/
;*    tts-closed? ::androidtts ...                                     */
;*---------------------------------------------------------------------*/
(define (tts-closed? t::androidtts)
   (with-access::androidtts t (%open %mutex)
      (with-lock %mutex
	 (lambda ()
	    %open))))

;*---------------------------------------------------------------------*/
;*    tts-locale ...                                                   */
;*---------------------------------------------------------------------*/
(define (tts-locale t::androidtts)
   (with-access::androidtts t (phone %mutex %open)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (android-send-command/result phone tts-plugin #\l))))))

;*---------------------------------------------------------------------*/
;*    tts-locale-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (tts-locale-set! t::androidtts locale)
   (with-access::androidtts t (phone %mutex %open)
      (if (and (list? locale) (every string? locale))
	  (with-lock %mutex
	     (lambda ()
		(when %open
		   (android-send-command phone tts-plugin #\L locale))))
	  (error "tts-locale-set!" "Illegal locale" locale))))

;*---------------------------------------------------------------------*/
;*    tts-locale-check ...                                             */
;*---------------------------------------------------------------------*/
(define (tts-locale-check t::androidtts locale)
   (with-access::androidtts t (phone %mutex %open)
      (if (and (list? locale) (every string? locale))
	  (with-lock %mutex
	     (lambda ()
		(when %open
		   (android-send-command/result phone tts-plugin #\a locale))))
	  (error "tts-locale-check" "Illegal locale" locale))))

;*---------------------------------------------------------------------*/
;*    tts-rate ...                                                     */
;*---------------------------------------------------------------------*/
(define (tts-rate t::androidtts)
   (with-access::androidtts t (%rate)
      %rate))

;*---------------------------------------------------------------------*/
;*    tts-rate-set! ...                                                */
;*---------------------------------------------------------------------*/
(define (tts-rate-set! t::androidtts value)
   (with-access::androidtts t (phone %mutex %open %rate)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (let ((d::double (cond
				   ((fixnum? value) (fixnum->flonum value))
				   ((flonum? value) value)
				   (else 1.0))))
		  (set! %rate d)
		  (android-send-command phone tts-plugin #\r d)))))))

;*---------------------------------------------------------------------*/
;*    tts-pitch ...                                                    */
;*---------------------------------------------------------------------*/
(define (tts-pitch t::androidtts)
   (with-access::androidtts t (%pitch)
      %pitch))

;*---------------------------------------------------------------------*/
;*    tts-pitch-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (tts-pitch-set! t::androidtts value)
   (with-access::androidtts t (phone %mutex %open %pitch)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (let ((d::double (cond
				   ((fixnum? value) (fixnum->flonum value))
				   ((flonum? value) value)
				   (else 1.0))))
		  (set! %pitch d)
		  (android-send-command phone tts-plugin #\p d)))))))

;*---------------------------------------------------------------------*/
;*    mode->char ...                                                   */
;*---------------------------------------------------------------------*/
(define (mode->char m)
   (if (eq? m 'queue) #a001 #a002))

;*---------------------------------------------------------------------*/
;*    stream->char ...                                                 */
;*---------------------------------------------------------------------*/
(define (stream->char s)
   (let loop ((l '(alarm dtmf music notification
		   ring system voice-call))
	      (i 2))
      (cond
	 ((null? l) #a001)
	 ((eq? (car l) s) (integer->char i))
	 (else (loop (cdr l) (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    tts-synthesize ...                                               */
;*---------------------------------------------------------------------*/
(define (tts-synthesize t::androidtts text path)
   (with-access::androidtts t (phone %mutex %open)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (android-send-command phone tts-plugin #\z text path))))))

;*---------------------------------------------------------------------*/
;*    tts-speak ...                                                    */
;*---------------------------------------------------------------------*/
(define (tts-speak t::androidtts text #!key stream mode)
   (with-access::androidtts t (phone %mutex %open)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (android-send-command phone tts-plugin #\s
				     text
				     (mode->char mode)
				     (stream->char stream)))))))

;*---------------------------------------------------------------------*/
;*    tts-silence ...                                                  */
;*---------------------------------------------------------------------*/
(define (tts-silence t::androidtts ms #!key stream mode)
   (with-access::androidtts t (phone %mutex %open)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (android-send-command phone tts-plugin #\space
				     ms
				     (mode->char mode)
				     (stream->char stream)))))))

;*---------------------------------------------------------------------*/
;*    tts-is-speaking? ...                                             */
;*---------------------------------------------------------------------*/
(define (tts-is-speaking? t::androidtts)
   (with-access::androidtts t (phone %mutex %open)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (android-send-command/result phone tts-plugin #\?))))))

;*---------------------------------------------------------------------*/
;*    tts-stop ...                                                     */
;*---------------------------------------------------------------------*/
(define (tts-stop t::androidtts)
   (with-access::androidtts t (phone %mutex %open)
      (with-lock %mutex
	 (lambda ()
	    (when %open
	       (android-send-command phone tts-plugin #\h))))))

   
