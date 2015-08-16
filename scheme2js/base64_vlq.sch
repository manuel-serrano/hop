;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/base64_vlq.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 28 15:16:54 2013                          */
;*    Last change :  Mon Jul 29 14:48:34 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    base64-vlq encoding/decoding                                     */
;*    -------------------------------------------------------------    */
;*    This code is largely adapted from a Mozilla library which        */
;*    implement the base64-vlq encoding/decoding in JavaScript.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    vlq-base-shift ...                                               */
;*    -------------------------------------------------------------    */
;*    A single base 64 digit can contain 6 bits of data. For the base  */
;*    64 variable length quantities we use in the source map spec, the */
;*    first bit is the sign, the next four bits are the actual value,  */
;*    and the 6th bit is the continuation bit. The continuation bit    */
;*    tells us whether there are more digits in this value following   */
;*    this digit.                                                      */
;*                                                                     */
;*       Continuation                                                  */
;*       |    Sign                                                     */
;*       |    |                                                        */
;*       V    V                                                        */
;*       101011                                                        */
;*---------------------------------------------------------------------*/
(define (vlq-base-shift) 5)

;*---------------------------------------------------------------------*/
;*    vlq-base ...                                                     */
;*    -------------------------------------------------------------    */
;*    binary: 1000000                                                  */
;*---------------------------------------------------------------------*/
(define (vlq-base) (bit-lsh 1 (vlq-base-shift)))

;*---------------------------------------------------------------------*/
;*    vlq-base-mask ...                                                */
;*    -------------------------------------------------------------    */
;*    binary: 0111111                                                  */
;*---------------------------------------------------------------------*/
(define (vlq-base-mask) (-fx (vlq-base) 1))

;*---------------------------------------------------------------------*/
;*    vlq-continuation-bit ...                                         */
;*---------------------------------------------------------------------*/
(define (vlq-continuation-bit) (vlq-base))

;*---------------------------------------------------------------------*/
;*    base64-decode-char ...                                           */
;*---------------------------------------------------------------------*/
(define (base64-decode-char char)
   (cond
      ((and (char>=? char #\A) (char<=? char #\Z))
       (-fx (char->integer char) (char->integer #\A)))
      ((and (char>=? char #\a) (char<=? char #\z))
       (+fx 26 (-fx (char->integer char) (char->integer #\a))))
      ((and (char>=? char #\0) (char<=? char #\9))
       (+fx 52 (-fx (char->integer char) (char->integer #\0))))
      ((char=? char #\+)
       62)
      ((char=? char #\/)
       63)
      (else
       (error "base64-decode-char" "Illegal char" char))))

;*---------------------------------------------------------------------*/
;*    base64-encode-char ...                                           */
;*---------------------------------------------------------------------*/
(define (base64-encode-char n)
   (cond
      ((and (>=fx n 0) (<fx n 26))
       (integer->char (+fx n (char->integer #\A))))
      ((and (>=fx n 26) (<fx n 52))
       (integer->char (+fx (-fx n 26) (char->integer #\a))))
      ((and (>=fx n 52) (<fx n 62))
       (integer->char (+fx (-fx n 52) (char->integer #\0))))
      ((=fx n 62)
       #\+)
      ((=fx n 63)
       #\/)
      (else
       (error "base64-encode-char" "Illegal integer" n))))

;*---------------------------------------------------------------------*/
;*    base64-vlq-decode ...                                            */
;*---------------------------------------------------------------------*/
(define (base64-vlq-decode str index)

   (define (from-vlq-signed val)
      ;; Converts to a two-complement value from a value where the sign
      ;; bit is is placed in the least significant bit.
      ;; For example, as decimals:
      ;;    2 (10 binary) becomes 1, 3 (11 binary) becomes -1
      ;;    4 (100 binary) becomes 2, 5 (101 binary) becomes -2
      (let ((neg (=fx (bit-and val 1) 1))
	    (shifted (bit-rsh val 1)))
	 (if neg (negfx shifted) shifted)))

   (let ((len (string-length str)))
      (let loop ((i index)
		 (result 0)
		 (shift 0))
	 (if (>=fx i len)
	     (error "base64-vlq-decode"
		"Expected more digits in base 64 VLQ value" i)
	     (let* ((digit (base64-decode-char (string-ref str i)))
		    (cont (=fx (bit-and digit (vlq-continuation-bit)) (vlq-continuation-bit)))
		    (digit (bit-and digit (vlq-base-mask)))
		    (result (+fx result (bit-lsh digit shift))))
		(if cont
		    (loop (+fx i 1)
		       result
		       (+fx shift (vlq-base-shift)))
		    (values (from-vlq-signed result) (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    base64-vlq-encode ...                                            */
;*---------------------------------------------------------------------*/
(define (base64-vlq-encode val::int)

   (define (to-vlq-signed val)
      ;; Converts from a two-complement value to a value where the sign
      ;; bit is is placed in the least significant bit.
      ;; For example, as decimals:
      ;;   1 becomes 2 (10 binary), -1 becomes 3 (11 binary)
      ;;   2 becomes 4 (100 binary), -2 becomes 5 (101 binary)
      (if (<fx val 0)
	  (+fx (bit-lsh (negfx val) 1) 1)
	  (bit-lsh val 1)))

   (let loop ((vlq (to-vlq-signed val))
	      (encoded '()))
      (let* ((digit (bit-and vlq (vlq-base-mask)))
	     (vlq (bit-ursh vlq (vlq-base-shift))))
	 ;;(print "digit=" digit " vlq=" vlq)
	 (if (>fx vlq 0)
	     (let ((digit (bit-or digit (vlq-continuation-bit))))
		(loop vlq (cons (base64-encode-char digit) encoded)))
	     (list->string
		(reverse! (cons (base64-encode-char digit) encoded)))))))
