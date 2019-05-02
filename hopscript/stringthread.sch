;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/stringthread.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May  2 12:03:44 2019                          */
;*    Last change :  Thu May  2 13:54:17 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    __js_strings thread location declaration.                        */
;*=====================================================================*/

(directives
   
   (static __js_strings)

   (cond-expand
      ((config thread-local-storage #t)
       (pragma (__js_strings thread-local)))))
