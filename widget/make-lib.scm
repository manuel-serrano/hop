;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/make-lib.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Fri Jun 26 07:22:21 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the HOPWIDGET heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-makelib

   (library hop)
   
   (import __hopwidget-init
	   __hopwidget-foldlist
	   __hopwidget-tabslider
	   __hopwidget-slider
	   __hopwidget-notepad
	   __hopwidget-paned
	   __hopwidget-tree
	   __hopwidget-editor
	   __hopwidget-file
	   __hopwidget-audio
	   __hopwidget-video
	   __hopwidget-colorchooser
	   __hopwidget-spinbutton
	   __hopwidget-lframe)

   (eval   (export-all)

	   (class hop-audio-player)))
