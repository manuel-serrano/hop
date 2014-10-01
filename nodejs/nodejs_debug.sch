;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/nodejs_debug.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 30 07:04:17 2014                          */
;*    Last change :  Sat Sep 27 19:57:44 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Macro used for debugging the nodejs runtime implementation.      */
;*    This macros introduce debugging information when modules         */
;*    are compiled in safe type mode                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-callXXX                                                       */
;*---------------------------------------------------------------------*/
(define-expander js-call0
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call1
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call2
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call3
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call4
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call5
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call6
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call7
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-call8
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

(define-expander js-calln
   (lambda (x e)
      (match-case x
	 ((?js-call . ?rest)
	  (map! (lambda (f) (e f e)) rest)
	  (when (and (not *unsafe-type*) (epair? x))
	     ;; debug
	     (set-cdr! (cdr x) (cons `',(cer x) (cddr x)))
	     (set-car! x (symbol-append js-call '/debug)))
	  x)
	 (else
	  (error "js-call" "wrong form" x)))))

;*---------------------------------------------------------------------*/
;*    js-get ...                                                       */
;*---------------------------------------------------------------------*/
(define-expander js-get
   (lambda (x e)
      (match-case x
	 ((js-get ?obj ?name ?this)
	  (map! (lambda (f) (e f e)) (cdr x))
	  (when (and (not *unsafe-type*) (epair? x))
	     (set-car! x 'js-get/debug)
	     (set-cdr! (last-pair x) (cons `',(cer x) '())))
	  x)
	 (else
	  (error "js-get" "wrong form" x)))))

;*---------------------------------------------------------------------*/
;*    js-put! ...                                                      */
;*---------------------------------------------------------------------*/
(define-expander js-put!
   (lambda (x e)
      (match-case x
	 ((js-put! ?obj ?name ?v ?throw ?this)
	  (map! (lambda (f) (e f e)) (cdr x))
	  (when (and (not *unsafe-type*) (epair? x))
	     (set-car! x 'js-put/debug!)
	     (set-cdr! (last-pair x) (cons `',(cer x) '())))
	  x)
	 (else
	  (error "js-put!" "wrong form" x)))))
