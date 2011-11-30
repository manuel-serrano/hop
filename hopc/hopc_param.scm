;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/hopc/hopc_param.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Wed Nov 30 11:48:50 2011 (serrano)                */
;*    Copyright   :  2004-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOPC global parameters                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_param
   
   (library hop)
   
   (export  (hopc-rc-file::bstring)
	    (hopc-rc-file-set! ::bstring)

	    (hopc-bigloo::bstring)
	    (hopc-bigloo-set! ::bstring)
	    (hopc-bigloo-options::pair-nil)
	    (hopc-bigloo-options-set! ::pair-nil)

	    (hopc-pass::symbol)
	    (hopc-pass-set! ::symbol)
	    
	    (hopc-sources::pair-nil)
	    (hopc-sources-set! ::pair-nil)
	    (hopc-destination::obj)
	    (hopc-destination-set! ::obj)

	    (hopc-share-directory::bstring)
	    (hopc-share-directory-set! ::bstring)
   
	    (hopc-access-file::obj)
	    (hopc-access-file-set! ::obj))
   
   (eval    (export-exports)))

;*---------------------------------------------------------------------*/
;*    hopc-rc-file ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hopc-rc-file
   "hopcrc.hop")

;*---------------------------------------------------------------------*/
;*    hopc-bigloo ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bigloo
   "bigloo")

;*---------------------------------------------------------------------*/
;*    hopc-bigloo-options ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bigloo-options
   '())

;*---------------------------------------------------------------------*/
;*    hopc-pass ...                                                    */
;*---------------------------------------------------------------------*/
(define-parameter hopc-pass
   'object)

;*---------------------------------------------------------------------*/
;*    hopc-sources ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter hopc-sources
   '())

;*---------------------------------------------------------------------*/
;*    hopc-destination ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hopc-destination
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-share-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hopc-share-directory
   (hop-share-directory))

;*---------------------------------------------------------------------*/
;*    hopc-access-file ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hopc-access-file
   #f)
