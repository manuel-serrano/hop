;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/zeroconf_dummy.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:13:46 2011                          */
;*    Last change :  Thu Dec 15 20:25:16 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Dummy (fake) Zeroconf implementation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop-zeroconf-publish ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-zeroconf-publish svc)
   (warning "hop-zeroconf" "no zeroconf support" #f))
