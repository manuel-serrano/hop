;*=====================================================================*/
;*    .../project/hop/2.2.x/arch/android/hopdroid/make_lib.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 10:49:38 2006                          */
;*    Last change :  Thu Oct 28 06:12:09 2010 (serrano)                */
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
	   __hopdroid-multimedia)

   (eval   (export-all)
	   (class androidphone)
	   (class androidmusic)))
