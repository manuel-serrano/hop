;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/expanders.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:46:47 2006                          */
;*    Last change :  Fri Apr  3 15:35:27 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop expanders                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_expanders

   (include "service-expd.sch"
	    "prefs-expd.sch"
	    "param-expd.sch"
	    "hss-expd.sch"
	    "xml-expd.sch"
	    "expanders.sch")

   (export  (hop-client-define-markup x e)
	    (hop-client-define-xml-compound x e)
	    
	    (hop-install-expanders!)))


   

   

	   
