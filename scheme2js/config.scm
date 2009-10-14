;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module config
   (export (config-init! #!optional config)
	   (extend-config configuration conf value)
	   (extend-config* configuration confs) ;; confs = list of pairs
	   (read-config configuration conf)
	   (config conf)
	   (config-set! conf val)
	   (scheme2js-config conf)
	   (default-config-alist)
	   (default-scheme2js-config)
	   (set-optim-level config level)
	   (configs-backup)
	   (configs-restore! backuped-config)))


(define (configs-backup)
   (thread-parameter '*scheme2js-config*))
(define (configs-restore! backuped-config)
   (thread-parameter-set! '*scheme2js-config* backuped-config))


(define (extend-config c conf val)
   (cons (cons conf val) c))

;; confs must be a list of confs.
(define (extend-config* c confs)
   (append confs c))

(define (read-config c conf)
   (let ((tmp (assq conf c)))
      (and tmp
	   (cdr tmp))))

(define (config-init! #!optional config)
   (thread-parameter-set! '*scheme2js-config* (or config '())))

(define (config conf)
   (read-config (thread-parameter '*scheme2js-config*) conf))
(define scheme2js-config config)

(define (config-set! conf val)
   (let ((configuration (thread-parameter '*scheme2js-config*)))
      (let ((tmp (assq conf configuration)))
	 (unless (and tmp
		      (equal? val (cdr tmp)))
	    ;; thread-parameter-set! is expensive->only update when needed.
	    (thread-parameter-set! '*scheme2js-config*
				   (cons (cons conf val) configuration))))))

(define *O0*
   '((optimize-tail-rec . #f)
     (do-inlining . #f)
     (inline-runtime . #f)
     (runtime-is-constant . #f)
     (propagation . #f)
     (constant-propagation . #f)
     (while . #f)
     (correct-modulo . #t)
     (optimize-calls . #f)
     (optimize-boolify . #f)
     (optimize-set! . #f)
     (max-tail-depth . 40)
     (var-elimination . #f)
     (optimize-consts . #f)
     (bigloo-runtime-eval . #f)))
(define (O0-config) *O0*)

(define *O1*
   '((optimize-tail-rec . #t)
     (do-inlining . #t)
     (rec-inline-nb . 3)
     (max-inline-size . 30)
     (inline-runtime . #t)
     (runtime-is-constant . #t)
     (propagation . #t)
     (constant-propagation . #t)
     (while . #t)
     (correct-modulo . #f)
     (optimize-calls . #t)
     (optimize-boolify . #t)
     (optimize-set! . #t)
     (max-tail-depth . 40)
     (var-elimination . #t)
     (optimize-consts . #t)
     (bigloo-runtime-eval . #f)))
(define (O1-config) *O1*)

(define *O2*
   '((optimize-tail-rec . #t)
     (do-inlining . #t)
     (rec-inline-nb . 1)
     (max-inline-size . 15)
     (inline-runtime . #f)
     (runtime-is-constant . #t)
     (propagation . #t)
     (constant-propagation . #t)
     (while . #t)
     (correct-modulo . #f)
     (optimize-calls . #t)
     (optimize-boolify . #t)
     (optimize-set! . #t)
     (max-tail-depth . 40)
     (var-elimination . #t)
     (optimize-consts . #t)
     (bigloo-runtime-eval . #t)))
(define (O2-config) *O2*)

(define *O3*
   '((optimize-tail-rec . #t)
     (do-inlining . #t)
     (rec-inline-nb . 4)
     (max-inline-size . 45)
     (inline-runtime . #t)
     (runtime-is-constant . #t)
     (propagation . #t)
     (constant-propagation . #t)
     (while . #t)
     (correct-modulo . #f)
     (optimize-calls . #t)
     (optimize-boolify . #t)
     (optimize-set! . #t)
     (max-tail-depth . 40)
     (var-elimination . #t)
     (optimize-consts . #t)))
(define (O3-config) *O3*)

(define *Obench*
   '((optimize-tail-rec . #t)
     (do-inlining . #t)
     (rec-inline-nb . 4)
     (max-inline-size . 45)
     (inline-runtime . #t)
     (runtime-is-constant . #t)
     (propagation . #t)
     (constant-propagation . #t)
     (while . #t)
     (correct-modulo . #f)
     (optimize-calls . #t)
     (optimize-boolify . #t)
     (optimize-set! . #t)
     (max-tail-depth . 40)
     (var-elimination . #t)
     (optimize-consts . #t)
     (bigloo-runtime-eval . #t)))

(define (Obench-config) *Obench*)

(define *default-config*
   (append '((infotron . #f)
	     (direct-js-object-access . #t)
	     (procedures-provide-js-this . #f)
	     (allow-unresolved . module)
	     (export-globals . module)
	     (encapsulate-modules . #f)
	     (trampoline . #f)
	     (print-locations . #f)
	     (return . #f)
	     (suspend/resume . #f)
	     (call/cc . #f)
	     (extern-always-call/cc . #f)
	     (include-paths . ())
	     (indent . 2)
	     (statics-suffix . #f)
	     (bigloo-modules . #t)
	     (pp . #t)
	     (compress . #f)
	     (call-check . #t)
	     (debug . #f))
	   *O1*))
(define (default-config-alist)
   *default-config*)
    
(define (set-optim-level config level)
   (apply-config config
		 (case level
		    ((0) *O0*)
		    ((1) *O1*)
		    ((2) *O2*)
		    ((3) *O3*)
		    ((bench) *Obench*)
		    (else
		     (if (and (number? level)
			      (> 0 level))
			 *O3*
			 (error "config"
				"Optimization-level unknown: "
				level))))))

(define (apply-config config a-list)
   (let loop ((a-list a-list)
	      (res config))
      (cond
	 ((null? a-list)
	  res)
	 ((assq (caar a-list) config)
	  =>
	  (lambda (p)
	     (if (equal? (cdr p) (cdar a-list))
		 ;; no change required
		 (loop (cdr a-list) res)
		 (loop (cdr a-list) (cons (car a-list) res)))))
	 (else
	  (loop (cdr a-list)
		(cons (car a-list) res))))))

(define (default-scheme2js-config)
   (default-config-alist))
