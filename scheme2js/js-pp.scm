(module js-pp
   (import js-parser
	   js-nodes
	   js-out)
   (export (js-pp in-p::input-port out-p::output-port
		  next-pragma!::procedure compress?::bool indent-width::bint)))

(define (js-pp in-p out-p next-pragma! compress? indent-width)
   (js-out (parse in-p next-pragma!) out-p
	   :compress? compress?
	   :indent-width indent-width))
