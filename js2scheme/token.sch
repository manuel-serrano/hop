;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/token.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 23 18:19:18 2015                          */
;*    Last change :  Fri Sep 15 17:23:27 2023 (serrano)                */
;*    Copyright   :  2015-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Token tools                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    the-choord ...                                                   */
;*    -------------------------------------------------------------    */
;*    Builds a Bigloo location object                                  */
;*---------------------------------------------------------------------*/
(define (the-coord input-port offset)
   `(at ,(if (input-string-port? input-port)
	     (string-append "string://" (input-port-buffer input-port))
	     (input-port-name input-port))
       ,(-fx (input-port-position input-port) offset)))

;*---------------------------------------------------------------------*/
;*    make-token ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-token type value loc)
   (econs type value loc))

;*---------------------------------------------------------------------*/
;*    token ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (token type value offset)
   `(make-token ,type ,value (the-coord (the-port) ,offset)))

;*---------------------------------------------------------------------*/
;*    token-type ...                                                   */
;*---------------------------------------------------------------------*/
(define (token-type token)
   (car token))

;*---------------------------------------------------------------------*/
;*    token-type-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (token-type-set! token tag)
   (set-car! token tag))

;*---------------------------------------------------------------------*/
;*    token-value ...                                                  */
;*---------------------------------------------------------------------*/
(define (token-value token)
    (cdr token))

;*---------------------------------------------------------------------*/
;*    token-loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (token-loc token #!optional (shift 0))
   (if (=fx shift 0)
       (cer token)
       (match-case (cer token)
	  ((at ?name ?pos)
	   `(at ,name ,(+fx pos shift)))
	  (else
	   (error "token-loc" "no location" token)))))

