;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/init.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 20 08:37:21 2009                          */
;*    Last change :  Sat Jun 20 09:03:59 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    main module for the hopwidget library                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-init
   
   (library hop)
   
   (with    __hopwidget-foldlist
	    __hopwidget-paned
	    __hopwidget-tabslider
	    __hopwidget-paned
	    __hopwidget-slider
	    __hopwidget-tree)
   
   (import  __hopwidget-file
	    __hopwidget-audio)
   
   (export  (init-hop-widgets!)))

;*---------------------------------------------------------------------*/
;*    init-hop-widgets ...                                             */
;*---------------------------------------------------------------------*/
(define (init-hop-widgets!)
   (init-hop-file-services!)
   (init-hop-audio-services!))
   




	 
