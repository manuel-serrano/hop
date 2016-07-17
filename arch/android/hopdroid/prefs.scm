;*=====================================================================*/
;*    .../hop-3.1.0-pre1/arch/android/hopdroid/prefs.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 17 09:31:15 2016                          */
;*    Last change :  Sun Jul 17 14:57:32 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Android preferences plugin                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-prefs

   (library phone hop)

   (import __hopdroid-phone)

   (export (preferences-set! ::androidphone ::symbol ::obj)
	   (preferences-get ::androidphone ::symbol)))

;*---------------------------------------------------------------------*/
;*    Standard plugins                                                 */
;*---------------------------------------------------------------------*/
(define prefs-plugin #f)

;*---------------------------------------------------------------------*/
;*    preferences-init ...                                             */
;*---------------------------------------------------------------------*/
(define (preferences-init phone::androidphone)
   (unless prefs-plugin
      (set! prefs-plugin (android-load-plugin phone "prefs"))))

;*---------------------------------------------------------------------*/
;*    preferences-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (preferences-set! phone::androidphone key val)
   (preferences-init phone)
   (android-send-command phone prefs-plugin #\s (symbol->string! key)
      (preference-encode val)))

;*---------------------------------------------------------------------*/
;*    preferences-get ...                                              */
;*---------------------------------------------------------------------*/
(define (preferences-get phone::androidphone key)
   (preferences-init phone)
   (preference-decode
      (android-send-command/result phone prefs-plugin #\g (symbol->string! key))))

;*---------------------------------------------------------------------*/
;*    preference-encode ...                                            */
;*---------------------------------------------------------------------*/
(define (preference-encode val)
   (cond
      ((string? val) (string-append "S:" val))
      ((symbol? val) (string-append "Y:" (symbol->string! val)))
      ((keyword? val) (string-append "K:" (keyword->string! val)))
      ((number? val) (string-append "N:" (number->string val)))
      ((eq? val #unspecified) "U:")
      ((eq? val #f) "F:")
      ((eq? val #t) "T:")
      (else (format "O:~s" val))))

;*---------------------------------------------------------------------*/
;*    preference-decode ...                                            */
;*---------------------------------------------------------------------*/
(define (preference-decode val)
   (when (and (string? val) (not (>=fx (string-length val) 2)))
      (let ((payload (substring val 2)))
	 (case (string-ref val 0)
	    ((#\S) payload)
	    ((#\Y) (string->symbol payload))
	    ((#\K) (string->keyword payload))
	    ((#\N) (string->number payload))
	    ((#\U) #unspecified)
	    ((#\F) #f)
	    ((#\T) #t)
	    (else (call-with-input-string payload read))))))



