;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Sun Jul 21 08:45:42 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme2js error handling                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module error
   
   (import export-desc nodes)
   
   (export (scheme2js-error proc msg obj loc)
	   (scheme2js-error-location loc)))

;*---------------------------------------------------------------------*/
;*    scheme2js-error-location ...                                     */
;*---------------------------------------------------------------------*/
(define (scheme2js-error-location loc)
   (cond
      ((isa? loc Node)
       (with-access::Node loc (location)
	  (scheme2js-error-location location)))
      ((epair? loc)
       (cer loc))
      (else
       loc)))

;*---------------------------------------------------------------------*/
;*    scheme2js-error ...                                              */
;*---------------------------------------------------------------------*/
(define (scheme2js-error proc msg obj loc)
   (let ((loc (scheme2js-error-location loc)))
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location proc msg obj fname loc))
	 (else
	  (error proc msg obj)))))
