;*=====================================================================*/
;*    .../project/hop/2.3.x/arch/android/hopdroid/make_lib.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Thu Dec 22 11:42:31 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the HOPDROID heap file.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopdroid-makelib

   (library phone multimedia hop)
   
   (import __hopdroid-phone
	   __hopdroid-music
	   __hopdroid-multimedia
	   __hopdroid-tts
	   __hopdroid-mpd
	   __hopdroid-zerconf)

   (eval   (export-all)
	   (class androidphone)
	   (class androidmusic)
	   (class androidtts)
	   (class androidmpd-database)))
