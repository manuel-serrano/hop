;*=====================================================================*/
;*    .../project/hop/3.1.x/arch/android/hopdroid/make_lib.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Sun Jul 17 14:32:58 2016 (serrano)                */
;*    Copyright   :  2006-16 Manuel Serrano                            */
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
	   __hopdroid-zeroconf
	   __hopdroid-system
	   __hopdroid-prefs)

   (eval   (export-all)
	   (class androidphone)
	   (class androidmusic)
	   (class androidtts)
	   (class androidmpd-database)
	   (class androidzeroconf)))
