;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/zeroconf_dummy.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:13:46 2011                          */
;*    Last change :  Mon Jun 25 09:58:03 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dummy (fake) Zeroconf implementation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Register the dummy-backend                                       */
;*---------------------------------------------------------------------*/
(zeroconf-register-backend!
   (instantiate::zeroconf
      (name "dummy")))
      
