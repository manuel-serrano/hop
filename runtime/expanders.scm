;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/runtime/expanders.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:46:47 2006                          */
;*    Last change :  Sun Jun 17 17:17:53 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
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
	    "verbose_expd.sch"
	    "expanders.sch")

   (export  (hop-client-define-tag x e)
	    (hop-client-define-xml-compound x e)
	    
	    (hop-install-expanders!)))


   

   

	   
