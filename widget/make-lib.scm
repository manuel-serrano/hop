;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/widget/make-lib.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Thu Mar  4 14:24:02 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
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
	   (class audio-server)
	   (class webmusic)))
