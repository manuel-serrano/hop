;*=====================================================================*/
;*    serrano/prgm/project/hop/3.4.x/js2scheme/usage.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 14 07:04:23 2019                          */
;*    Last change :  Thu May 13 16:52:21 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Ast node usage API                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_usage

   (include "usage-bit.sch")

   (import __js2scheme_ast)
   
   (export (usage::uint32 ::obj)
	   (usage-key->bit::uint32 ::symbol)
	   (usage-bit->key::symbol ::uint32)
	   (usage->keys::pair-nil ::uint32)

	   (usage-has?::bool ::uint32 ::pair-nil)
	   (decl-usage-has?::bool ::J2SDecl ::pair-nil)

	   (usage-strict?::bool ::uint32 ::pair-nil)
	   (decl-usage-strict?::bool ::J2SDecl ::pair-nil)

	   (usage-add ::uint32 ::symbol)
	   (decl-usage-add! ::J2SDecl ::symbol)
	   
	   (usage-rem ::uint32 ::symbol)
	   (decl-usage-rem! ::J2SDecl ::symbol)
	   
	   (decl-ronly?::bool ::J2SDecl)))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*    -------------------------------------------------------------    */
;*    Overridden by usage.sch for performance.                         */
;*---------------------------------------------------------------------*/
(define (usage keys)
   (cond
      ((pair? keys)
       (let ((u::uint32 #u32:0))
	  (for-each (lambda (k) (set! u (bit-oru32 u (usage-key->bit k)))) keys)
	  u))
      ((null? keys)
       #u32:0)
      ((uint32? keys)
       keys)
      (else
       (error "usage" "Illegal keys type" keys))))

;*---------------------------------------------------------------------*/
;*    usage->keys ...                                                  */
;*---------------------------------------------------------------------*/
(define (usage->keys usage)
   (let loop ((i #u32:1))
      (cond
	 ((>u32 i#u32:4096)
	  '())
	 ((=u32 (bit-andu32 i usage) i)
	  (cons (usage-bit->key i) (loop (bit-lshu32 i 1))))
	 (else (loop (bit-lshu32 i 1))))))

;*---------------------------------------------------------------------*/
;*    usage-has? ...                                                   */
;*---------------------------------------------------------------------*/
(define (usage-has? _usage keys)
   (>u32 (bit-andu32 _usage (usage keys)) #u32:0))

(define (usage? keys usage)
   (any (lambda (k)
	   [assert (k) (memq k '(uninit init new ref assig get set call eval delete instanceof rest))]
	   (memq k usage))
      keys))

;*---------------------------------------------------------------------*/
;*    decl-usage-has? ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-usage-has? decl keys)
   (with-access::J2SDecl decl (usage)
      (usage-has? usage keys)))

;*---------------------------------------------------------------------*/
;*    usage-strict? ...                                                */
;*---------------------------------------------------------------------*/
(define (usage-strict? _usage keys)
   (=u32 (bit-andu32 _usage (usage keys)) _usage))

;*---------------------------------------------------------------------*/
;*    decl-usage-strict? ...                                           */
;*---------------------------------------------------------------------*/
(define (decl-usage-strict? decl keys)
   (with-access::J2SDecl decl (usage)
      (usage-strict? usage keys)))

;*---------------------------------------------------------------------*/
;*    usage-add ...                                                    */
;*---------------------------------------------------------------------*/
(define (usage-add usage key)
   (bit-oru32 usage (usage-key->bit key)))

;*---------------------------------------------------------------------*/
;*    decl-usage-add! ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-usage-add! decl key)
   (with-access::J2SDecl decl (usage)
      (set! usage (usage-add usage key))))

;*---------------------------------------------------------------------*/
;*    usage-rem ...                                                    */
;*---------------------------------------------------------------------*/
(define (usage-rem usage key)
   (bit-andu32 usage (bit-notu32 (usage-key->bit key))))

;*---------------------------------------------------------------------*/
;*    decl-usage-rem! ...                                              */
;*---------------------------------------------------------------------*/
(define (decl-usage-rem! decl key)
   (with-access::J2SDecl decl (usage)
      (set! usage (usage-rem usage key))))

;*---------------------------------------------------------------------*/
;*    decl-ronly? ...                                                  */
;*---------------------------------------------------------------------*/
(define (decl-ronly? decl)
   (not (decl-usage-has? decl '(assig eval))))

