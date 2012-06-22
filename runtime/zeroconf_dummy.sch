;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/zeroconf_dummy.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 15 09:13:46 2011                          */
;*    Last change :  Thu Jun 21 09:04:30 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dummy (fake) Zeroconf implementation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Register the dummy-backend                                       */
;*---------------------------------------------------------------------*/
(zeroconf-register-backend!
   (lambda (init)
      (instantiate::zeroconf
	 (name "dummy"))))
      
