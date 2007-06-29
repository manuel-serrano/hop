;*=====================================================================*/
;*    serrano/prgm/project/hop/share/hop-sound.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Mon Oct 16 15:02:51 2006                          */
;*    Last change :                                                    */
;*    Copyright   :  2006 GPL                                          */
;*    -------------------------------------------------------------    */
;*    The wrapper to access sound through Macromedia Flash 7.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    make-sound ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-sound url . args)
   
   (define (key-get lst key default)
      (let loop ((lst lst))
	 (cond
	    ((null? lst) default)
	    ((null? (cdr lst)) default)  ;; to simplify
	    ((eq? (car lst) key) (cadr lst))
	    (else (loop (cddr lst))))))
   
   (let ((stream   (key-get args :stream #f))
	 (onload   (key-get args :onload #f))
	 (onplayed (key-get args :onplayed #f)))
      (let ((snd (hop_make_sound url)))
	 ;; Add handlers
	 (if onload
	     (set! snd.onLoad onload))
	 (if onplayed
	     (set! snd.onSoundComplete onplayed))
	 ;; Load the sound
	 (hop_sound_load snd stream)
	 snd)))

;*---------------------------------------------------------------------*/
;*    sound-play/stop/pause ...                                        */
;*---------------------------------------------------------------------*/
(define (sound-play snd . args)
   (flashProxy.call "start"
		    snd.id
		    (if (null? args) 1 (car args))))

(define (sound-stop snd . args)
   (flashProxy.call "stop" snd.id))

(define (sound-pause snd . args)
   (flashProxy.call "pause" snd.id))

;*---------------------------------------------------------------------*/
;*    sound-volume ...                                                 */
;*---------------------------------------------------------------------*/
(define (sound-volume snd)
   snd.volume)

(define (sound-volume-set! snd value)
   (flashProxy.call "setVolume"  snd.id value)
   (set! snd.volume value))

;*---------------------------------------------------------------------*/
;*    sound-pan ...                                                    */
;*---------------------------------------------------------------------*/
(define (sound-pan snd)
   snd.pan)

(define (sound-pan-set! snd value)
   (flashProxy.call "setPan"  snd.id value)
   (set! snd.pan value))


