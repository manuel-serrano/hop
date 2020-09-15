;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/usage.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 14 06:23:31 2019                          */
;*    Last change :  Sun Dec 15 07:15:59 2019 (serrano)                */
;*    Copyright   :  2019-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Set of macros for deadling with reference usage attributes.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives
   (option (loadq "usage-bit.sch"))
   (import __js2scheme_usage))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*    -------------------------------------------------------------    */
;*    Overrides the definition (@ usage __js2scheme_ast).              */
;*---------------------------------------------------------------------*/
(define-expander usage
   (lambda (x e)
      (match-case x
	 ((usage ((kwote quote) ?keys))
	  (usage-keys->bit keys))
	 (else
	  (e `((@ usage __js2scheme_usage) ,@(cdr x)) e)))))
   
;*---------------------------------------------------------------------*/
;*    usage-key->bit ...                                               */
;*---------------------------------------------------------------------*/
(define-expander usage-key->bit
   (lambda (x e)
      (match-case x
	 ((usage-key->bit ((kwote quote) ?key))
	  (usage-key->bit key))
	 (else
	  (e `((@ usage-key->bit __js2scheme_usage) ,@(cdr x)) e)))))
  
;*---------------------------------------------------------------------*/
;*    usage-has? ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander usage-has?
   (lambda (x e)
      (match-case x
	 ((usage-has? ?usage ((kwote quote) ?keys))
	  (e `(>u32 (bit-andu32 ,usage ,(usage-keys->bit keys)) #u32:0) e))
	 ((usage-has? ?usage ?keys)
	  (e `(>u32 (bit-andu32 usage (usage ,keys)) #u32:0) e))
	 (else
	  (error "usage?" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    decl-usage-has? ...                                              */
;*---------------------------------------------------------------------*/
(define-expander decl-usage-has?
   (lambda (x e)
      (match-case x
	 ((decl-usage-has? ?decl ?keys)
	  (e `(with-access::J2SDecl ,decl (usage)
		 (usage-has? usage ,keys))
	     e))
	 (else
	  (error "decl-usage-has?" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    usage-strict? ...                                                */
;*---------------------------------------------------------------------*/
(define-expander usage-strict?
   (lambda (x e)
      (match-case x
	 ((usage-strict? ?usage ((kwote quote) ?keys))
	  (e `(=u32 (bit-andu32 ,usage ,(usage-keys->bit keys)) ,usage) e))
	 ((usage-strict? ?usage ?keys)
	  (e `(>u32 (bit-oru32 usage (usage ,keys)) #u32:0) e))
	 (else
	  (error "usage?" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    decl-usage-strict? ...                                           */
;*---------------------------------------------------------------------*/
(define-expander decl-usage-strict?
   (lambda (x e)
      (match-case x
	 ((decl-usage-strict? ?decl ?keys)
	  (e `(with-access::J2SDecl ,decl (usage)
		 (usage-strict? usage ,keys))
	     e))
	 (else
	  (error "decl-usage-strict?" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    usage-add ...                                                    */
;*---------------------------------------------------------------------*/
(define-expander usage-add
   (lambda (x e)
      (match-case x
	 ((usage-add ?usage ((kwote quote) ?key))
	  (e `(bit-oru32 ,usage ,(usage-key->bit key)) e))
	 ((usage-add ?usage ?key)
	  (e `(bit-oru32 ,usage (usage-key->bit ,key)) e))
	 (else
	  (error "usage-add!" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    decl-usage-add! ...                                              */
;*---------------------------------------------------------------------*/
(define-expander decl-usage-add!
   (lambda (x e)
      (match-case x
	 ((decl-usage-add! ?decl ?key)
	  (e `(with-access::J2SDecl ,decl (usage)
		 (set! usage (usage-add usage ,key)))
	     e))
	 (else
	  (e `((@ decl-usage-add! __js2scheme_usage) ,@(cdr x)) e)))))
	  
;*---------------------------------------------------------------------*/
;*    usage-rem ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander usage-rem
   (lambda (x e)
      (match-case x
	 ((usage-rem ?usage ((kwote quote) ?key))
	  (e `(bit-andu32 ,usage (bit-notu32 ,(usage-key->bit key))) e))
	 ((usage-rem ?usage ?key)
	  (e `(bit-andu32 ,usage (bit-notu32 (usage-key->bit ,key))) e))
	 (else
	  (error "usage-rem" "bad form" x)))))


;*---------------------------------------------------------------------*/
;*    decl-usage-rem! ...                                              */
;*---------------------------------------------------------------------*/
(define-expander decl-usage-rem!
   (lambda (x e)
      (match-case x
	 ((decl-usage-rem! ?decl ?key)
	  (e `(with-access::J2SDecl ,decl (usage)
		 (set! usage (usage-rem usage ,key)))
	     e))
	 (else
	  (error "decl-usage-rem!" "bad form" x)))))

