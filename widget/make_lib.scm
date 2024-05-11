;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/widget/make_lib.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Tue May  7 18:52:36 2024 (serrano)                */
;*    Copyright   :  2006-24 Manuel Serrano                            */
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
	   __hopwidget-colorchooser
	   __hopwidget-spinbutton
	   __hopwidget-lframe
	   __hopwidget-spage
	   __hopwidget-gauge
	   __hopwidget-sym
	   __hopwidget-foot
	   __hopwidget-sorttable
	   __hopwidget-prefs)

   (eval   (export-all)))
