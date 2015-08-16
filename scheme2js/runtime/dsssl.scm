;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/runtime/dsssl.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul  3 11:30:29 1997                          */
;*    Last change :  Fri Aug 23 06:47:25 2013 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Extracted from Bigloo support for Dsssl.                         */
;*    -------------------------------------------------------------    */
;*    WARNING!!! This file is only used to generate dsssl.js. As it    */
;*    is not used in the scheme2js runtime.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dsssl
   (export (dsssl-check-key-args! dsssl-args key-list)
	   (dsssl-get-key-arg dsssl-args keyword initializer)
	   (dsssl-get-key-rest-arg dsssl-args keys)))

;*---------------------------------------------------------------------*/
;*    dsssl-check-key-args! ...                                        */
;*    -------------------------------------------------------------    */
;*    This function checks that dsssl args are, at runtime,            */
;*    correctly formed. That is, the dsssl-args variable must hold     */
;*    a serie of pairs where the first element is a keyword.           */
;*    Furthermore, if key-list is non-nil, we check that for each      */
;*    pair, if the key is present in key-list.                         */
;*---------------------------------------------------------------------*/
(define (dsssl-check-key-args! dsssl-args key-list)
   (if (null? key-list)
       (let loop ((args dsssl-args))
	  (cond
	     ((null? args)
	      dsssl-args)
	     ((or (not (pair? args))
		  (null? (cdr args))
		  (not (keyword? (car args))))
	      (error "dsssl formal parsing"
		     "Unexpected #!keys parameters"
		     args))
	     (else
	      (loop (cddr args)))))
       (let loop ((args dsssl-args)
		  (armed #f)
		  (opts '()))
	  (cond
	     ((null? args)
	      (reverse! opts))
	     ((or (not (pair? args))
		  (null? (cdr args))
		  (not (keyword? (car args)))
		  (not (memq (car args) key-list)))
	      (if (not armed)
		  (loop (cdr args) armed opts)
		  (loop (cdr args)
			#f
			(cons (car args) opts))))
	     (else
	      (loop (cddr args) #t opts))))))
   
;*---------------------------------------------------------------------*/
;*    dsssl-get-key-arg ...                                            */
;*    -------------------------------------------------------------    */
;*    dsssl args have already been tested. We know for sure that       */
;*    it is a serie of pairs where first elements are keywords.        */
;*---------------------------------------------------------------------*/
(define (dsssl-get-key-arg dsssl-args keyword initializer)
   (let loop ((args dsssl-args))
      (cond
	 ((null? args)
	  initializer)
	 ((not (keyword? (car args)))
	  (loop (cdr args)))
	 ((eq? (car args) keyword)
	  (if (not (pair? (cdr args)))
	      (error "dsssl-get-key-arg"
		     "keyword argument misses value"
		     (car args))
	      (cadr args)))
	 (else
	  (if (not (pair? (cdr args)))
	      (error "dsssl-get-key-arg"
		     "keyword argument misses value"
		     (car args))
	      (loop (cddr args)))))))
   
;*---------------------------------------------------------------------*/
;*    dsssl-get-key-rest-arg ...                                       */
;*---------------------------------------------------------------------*/
(define (dsssl-get-key-rest-arg dsssl-args keys)
   (let loop ((args dsssl-args))
      (cond
	 ((null? args)
	  '())
	 ((or (not (keyword? (car args)))
	      (null? (cdr args))
	      (not (memq (car args) keys)))
	  args)
	 (else
	  (loop (cddr args))))))
