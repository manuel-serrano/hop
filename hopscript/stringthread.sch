;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/stringthread.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May  2 12:03:44 2019                          */
;*    Last change :  Fri May 10 13:21:02 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    __js_strings thread location declaration.                        */
;*=====================================================================*/

(directives
   
   (static __js_strings)

   (cond-expand
      (enable-tls
       (pragma (__js_strings thread-local)))))
