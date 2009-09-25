;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/make-lib.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Mon Sep 14 11:32:04 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the HOPWIDGET heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-makelib

   (library hop multimedia)
   
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
	   __hopwidget-lframe
	   __hopwidget-spage)

   (eval   (export-all)

	   (class hop-audio-player)))
