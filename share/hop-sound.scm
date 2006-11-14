;*=====================================================================*/
;*    serrano/prgm/project/hop/share/hop-sound.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Mon Oct 16 15:02:51 2006                          */
;*    Last change :  Tue Nov 14 07:47:18 2006 (serrano)                */
;*    Copyright   :  GPL                                               */
;*    -------------------------------------------------------------    */
;*    The wrapper to access sound through Macromedia Flash.            */
;*=====================================================================*/

;; ----------------------------------------------------------------------
;; 	hop-make-sound ...
;; ----------------------------------------------------------------------
(define (hop-make-sound url . args)
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

;; ----------------------------------------------------------------------
;; 	hop-sound-play/stop/pause ...
;; ----------------------------------------------------------------------
(define (hop-sound-play snd . args)
  (flashProxy.call "start"
		   snd.id
		   (if (null? args) 1 (car args))))

(define (hop-sound-stop snd . args)
  (flashProxy.call "stop" snd.id))

(define (hop-sound-pause snd . args)
  (flashProxy.call "pause" snd.id))

;; ----------------------------------------------------------------------
;; 	hop-sound-volume ...
;; ----------------------------------------------------------------------
(define (hop-sound-volume snd)
  snd.volume)

(define (hop-sound-volume-set! snd value)
  (flashProxy.call "setVolume"  snd.id value)
  (set! snd.volume value))

;; ----------------------------------------------------------------------
;; 	hop-sound-pan ...
;; ----------------------------------------------------------------------
(define (hop-sound-pan snd)
  snd.pan)

(define (hop-sound-pan-set! snd value)
  (flashProxy.call "setPan"  snd.id value)
  (set! snd.pan value))

;; ----------------------------------------------------------------------
;; 	Initialize the sound system ...
;; ----------------------------------------------------------------------
(hop_sound_init)

