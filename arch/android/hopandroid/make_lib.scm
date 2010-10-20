;*=====================================================================*/
;*    .../project/hop/2.2.x/arch/android/hopandroid/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Wed Oct 20 10:15:03 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the HOPANDROID heap file.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopandroid-makelib

   (library phone multimedia hop)
   
   (import __hopandroid-phone
	   __hopandroid-music
	   __hopandroid-multimedia)

   (eval   (export-all)
	   (class androidphone)
	   (class androidmusic)))
