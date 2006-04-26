;*=====================================================================*/
;*    serrano/prgm/project/hop/hopscheme/hop-aliases.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Apr 25 14:33:37 2006                          */
;*    Last change :  Wed Apr 26 09:01:28 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOP aliases.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopscheme_aliases
   (export (hopscheme-aliases)))

;*---------------------------------------------------------------------*/
;*    hopscheme-aliases ...                                            */
;*---------------------------------------------------------------------*/
(define (hopscheme-aliases)
   *hop-aliases*)

;*---------------------------------------------------------------------*/
;*    *hop-aliases* ...                                                */
;*---------------------------------------------------------------------*/
(define *hop-aliases*
   '((base64-encode base64_encode)
     (base64-decode base64_decode)
     (hop-slider-value hop_slider_value_get)
     (hop-slider-value-set! hop_slider_value_set)
     (hop-slider-onchange hop_slider_onchange_get)
     (hop-slider-onchange-set! hop_slider_onchange_set)
     (hop-notepad-select hop_notepad_select)
     (hop-iwindow-open hop_iwindow_open)
     (hop-iwindow-close hop_iwindow_close)))

