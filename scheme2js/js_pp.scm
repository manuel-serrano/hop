;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/js_pp.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-11                                           */
;*    Last change :  Thu Jul 18 13:39:01 2013 (serrano)                */
;*    Copyright   :  2013 Florian Loitsch/Manuel Serrano               */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2JS/HOP.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module js-pp
   (import js-parser
	   js-nodes
	   js-out)
   (export (js-pp in-p::input-port out-p::output-port
	      next-pragma!::procedure compress?::bool indent-width::bint)))

;*---------------------------------------------------------------------*/
;*    js-pp ...                                                        */
;*---------------------------------------------------------------------*/
(define (js-pp in-p out-p next-pragma! compress? indent-width)
   (js-out (parse in-p next-pragma!) out-p
      :compress? compress?
      :indent-width indent-width))
