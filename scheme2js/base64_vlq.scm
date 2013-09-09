;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/base64_vlq.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 28 15:16:54 2013                          */
;*    Last change :  Mon Jul 29 14:48:43 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    base64-vlq encoding/decoding                                     */
;*    -------------------------------------------------------------    */
;*    This code is largely adapted from a Mozilla library which        */
;*    implement the base64-vlq encoding/decoding in JavaScript.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module base64-vlq

   (include "base64_vlq.sch")
   
   (export (base64-vlq-decode::int ::bstring ::int)
	   (base64-vlq-encode ::int)))

