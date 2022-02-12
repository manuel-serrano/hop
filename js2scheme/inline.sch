;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 12 09:07:59 2022                          */
;*    Last change :  Sat Feb 12 09:59:20 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Inlining structures                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Structures                                                       */
;*---------------------------------------------------------------------*/
(define-struct protoinfo assig method svar owner)
(define-struct targetinfo decl fun)

(define-struct callloginfo call counter targets)
(define-struct callinfo parent owner)
(define-struct funinfo size freevars decl parent initial-body)

;*---------------------------------------------------------------------*/
;*    J2SMetaInl ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (J2SMetaInl stack optim stmt)
   `(instantiate::J2SMetaInl
       (meta 'inline)
       (loc loc)
       (inlstack ,stack)
       (debug 0)
       (optim ,optim)
       (stmt ,stmt)))


