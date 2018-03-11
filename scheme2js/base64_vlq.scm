;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/scheme2js/base64_vlq.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 28 15:16:54 2013                          */
;*    Last change :  Sat Mar 10 05:34:38 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    base64-vlq encoding/decoding                                     */
;*    -------------------------------------------------------------    */
;*    This code is largely adapted from a Mozilla library which        */
;*    implements the base64-vlq encoding/decoding in JavaScript.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module base64-vlq

   (include "base64_vlq.sch")
   
   (export (base64-vlq-decode::int ::bstring ::int)
	   (base64-vlq-encode ::int)))

