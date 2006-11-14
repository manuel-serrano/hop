(define *home-directory* (string-append (dirname (the-loading-file)) "/"))

(define (rel-path path)
   (string-append *home-directory* path))

(define (<CHECKBOX> name checked? . L)
   (if checked?
       (apply <INPUT> (cons* :name name :type "checkbox" :checked "checked" L))
       (apply <INPUT> (cons* :name name :type "checkbox" L))))

(define (<TEXT-FIELD> name . L)
   (apply <INPUT> (cons* :name name :type "text" L)))

(define (<SUBMIT> label . L)
   (apply <INPUT> (cons* :value label :type "submit" L)))

(define (<RESET> label . L)
   (apply <INPUT> (cons* :value label :type "reset" L)))
