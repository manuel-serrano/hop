;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/expanders.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:46:47 2006                          */
;*    Last change :  Sun Nov  7 08:58:36 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop expanders                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_expanders

   (include "service_expd.sch"
	    "prefs_expd.sch"
	    "param_expd.sch"
	    "hss_expd.sch"
	    "xml_expd.sch"
	    "expanders.sch")

   (export  (hop-client-define-markup x e)
	    (hop-client-define-xml-compound x e)
	    
	    (hop-install-expanders!)))


   

   

	   
