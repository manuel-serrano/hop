(module error
   (import export-desc nodes)
   (export (scheme2js-error proc msg obj loc)))

(define (scheme2js-error proc msg obj loc)
   (cond
      ((Node? loc)
       (with-access::Node loc (location)
	  (scheme2js-error proc msg obj location)))
      ((epair? loc)
       (scheme2js-error proc msg obj (cer loc)))
      (else
       (match-case loc
	  ((at ?fname ?loc)
	   (error/location proc msg obj fname loc))
	  (else
	   (error proc msg obj))))))
