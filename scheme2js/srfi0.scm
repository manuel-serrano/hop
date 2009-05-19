(module srfi0
   (include "version.sch")
   (import error expand)
   (export
    (srfi0-expand expr)
    (srfi0-declare! sym::symbol)
    (srfi0-declared?::bool sym::symbol)))

(define *scheme2js-features*
   `(scheme2js srfi0 ,(string->symbol (format "scheme2js-~a" *version*))))

(define (srfi0-declare! sym)
   (cons sym *scheme2js-features*))

(define (srfi0-declared? sym)
   (memq sym *scheme2js-features*))

(define (srfi0-expand expr)
   (let ((e (lambda (x e) x)))
      (expand-cond-expand expr e *scheme2js-features*)))

(install-expander!
 'cond-expand
 (lambda (x e) (expand-cond-expand x e *scheme2js-features*)))
