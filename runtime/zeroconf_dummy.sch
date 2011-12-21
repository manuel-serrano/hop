;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/zeroconf_dummy.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:13:46 2011                          */
;*    Last change :  Wed Dec 21 15:05:50 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Dummy (fake) Zeroconf implementation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Register the dummy-backend                                       */
;*---------------------------------------------------------------------*/
(hop-zeroconf-register-backend!
   (lambda ()
      (instantiate::zeroconf
	 (name "dummy"))))
      
