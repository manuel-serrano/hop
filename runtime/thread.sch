;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/thread.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 11 08:20:12 2013                          */
;*    Last change :  Thu Apr 11 08:39:35 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hop thread backend implementation                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (cond-expand
      ((and enable-threads (library pthread))
       (library pthread))))
