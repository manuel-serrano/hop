;*=====================================================================*/
;*    .../project/hop/2.2.x/arch/android/hopdroid/make_lib.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Wed Dec  1 13:51:04 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
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
	   __hopdroid-tts)

   (eval   (export-all)
	   (class androidphone)
	   (class androidmusic)
	   (class androidtts)))
