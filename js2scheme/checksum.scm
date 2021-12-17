;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/checksum.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 25 17:47:23 2018                          */
;*    Last change :  Fri Dec 17 11:18:06 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute checksum (hash numbers) of anything.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_checksum
   (import __js2scheme_ast)
   (export (checksum ::obj #!optional (seed 0))))

;*---------------------------------------------------------------------*/
;*    checksum ...                                                     */
;*---------------------------------------------------------------------*/
(define (checksum obj #!optional (seed 0))
   (checksum-any obj 0))

;*---------------------------------------------------------------------*/
;*    checksum-any ...                                                 */
;*---------------------------------------------------------------------*/
(define (checksum-any obj checksum::long)
   (cond
      ((pair? obj)
       (list->number obj checksum))
      ((vector? obj)
       (vector->number obj checksum))
      ((object? obj)
       (object->number obj checksum))
      (else
       (atom->number obj checksum))))

;*---------------------------------------------------------------------*/
;*    keyword->number ...                                              */
;*---------------------------------------------------------------------*/
(define (keyword->number keyword)
   (+fx 21 (get-hashnumber (keyword->string keyword))))

;*---------------------------------------------------------------------*/
;*    symbol->number ...                                               */
;*---------------------------------------------------------------------*/
(define (symbol->number symbol)
   (+fx 150 (get-hashnumber (symbol->string symbol))))

;*---------------------------------------------------------------------*/
;*    atom->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (atom->number obj checksum::long)
   (cond
      ((fixnum? obj)
       (bit-xor checksum obj))
      ((flonum? obj)
       (atom->number (real->string obj) checksum))
      ((char? obj)
       (bit-xor checksum (+fx 23 (char->integer obj))))
      ((cnst? obj)
       (bit-xor checksum (+fx 90 (cnst->integer obj))))
      ((string? obj)
       (bit-xor checksum (+fx 4 (get-hashnumber obj))))
      ((symbol? obj)
       (bit-xor checksum (symbol->number obj)))
      ((keyword? obj)
       (bit-xor checksum (+fx 151 (keyword->number obj))))
      ((pair? obj)
       (list->number obj checksum))
      ((elong? obj)
       (bit-xor checksum (elong->fixnum obj)))
      ((llong? obj)
       (bit-xor checksum
		(bit-xorllong (bit-rshllong obj 32)
			      (bit-rshllong (bit-lshllong obj 32) 32))))
      ((int8? obj)
       (bit-xor checksum (int8->fixnum obj)))
      ((uint8? obj)
       (bit-xor checksum (uint8->fixnum obj)))
      ((int16? obj)
       (bit-xor checksum (int16->fixnum obj)))
      ((uint16? obj)
       (bit-xor checksum (uint16->fixnum obj)))
      ((int32? obj)
       (bit-xor checksum (int32->fixnum obj)))
      ((uint32? obj)
       (bit-xor checksum (uint32->fixnum obj)))
      ((int64? obj)
       (bit-xor checksum (int64->fixnum obj)))
      ((uint64? obj)
       (bit-xor checksum (uint64->fixnum obj)))
      ((int32? obj)
       (bit-xor checksum (int32->fixnum obj)))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    list->number ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->number obj checksum::long)
   (match-case obj
      (()
       checksum)
      (((or default assert info) . ?-)
       0)
      (else
       (if (pair? obj)
	   (list->number (cdr obj) (atom->number (car obj) checksum))
	   (atom->number obj checksum)))))
	 
;*---------------------------------------------------------------------*/
;*    vector->number ...                                               */
;*---------------------------------------------------------------------*/
(define (vector->number obj::vector checksum::long)
   (let loop ((cs checksum)
	      (i (-fx (vector-length obj) 1)))
      (if (=fx i -1)
	  cs
	  (loop (+fx 1 (checksum-any cs (vector-ref obj i)))
	     (-fx i 1)))))

;*---------------------------------------------------------------------*/
;*    object->number ::object ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (object->number obj::object checksum)
   (symbol->number (class-name (object-class obj))))

;*---------------------------------------------------------------------*/
;*    object->number ::J2SExport ...                                   */
;*---------------------------------------------------------------------*/
(define-method (object->number obj::J2SExport checksum)
   (with-access::J2SExport obj (id alias index)
      (bit-xor (symbol->number id)
	 (bit-xor (symbol->number alias)
	    (bit-xor index checksum)))))

;*---------------------------------------------------------------------*/
;*    object->number ::J2SProgram ...                                  */
;*---------------------------------------------------------------------*/
(define-method (object->number obj::J2SProgram checksum)
   (with-access::J2SProgram obj (exports imports)
      (checksum-any exports
	 (checksum-any
	    (map (lambda (i)
		    (with-access::J2SImportPath i (path) path))
	       imports)
	    checksum))))

