;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/hopc/hopc_param.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Mon Nov  4 19:22:46 2013 (serrano)                */
;*    Copyright   :  2004-13 Manuel Serrano                            */
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
	    (hopc-access-file-set! ::obj)

	    (hopc-jsheap::obj)
	    (hopc-jsheap-set! ::obj)

	    (hopc-clientc-source-map::bool)
	    (hopc-clientc-source-map-set! ::bool)
	    
	    (hopc-clientc-use-strict::bool)
	    (hopc-clientc-use-strict-set! ::bool)
	    
	    (hopc-clientc-arity-check::bool)
	    (hopc-clientc-arity-check-set! ::bool)
	    
	    (hopc-clientc-type-check::bool)
	    (hopc-clientc-type-check-set! ::bool)
	    
	    (hopc-clientc-meta::bool)
	    (hopc-clientc-meta-set! ::bool)
	    
	    (hopc-clientc-debug::bool)
	    (hopc-clientc-debug-set! ::bool)
	    
	    (hopc-clientc-pp::bool)
	    (hopc-clientc-pp-set! ::bool)
	    
	    (hopc-clientc-inlining::bool)
	    (hopc-clientc-inlining-set! ::bool)

	    (hopc-source-language::symbol)
	    (hopc-source-language-set! ::symbol)

	    (hopc-optim-level::int)
	    (hopc-optim-level-set! ::int))
	    
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
   `("-lib-dir" ,(make-file-path (hop-lib-directory) "hop" (hop-version))
       "-fidentifier-syntax" "bigloo"))

;*---------------------------------------------------------------------*/
;*    hopc-pass ...                                                    */
;*---------------------------------------------------------------------*/
(define-parameter hopc-pass
   'link)

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

;*---------------------------------------------------------------------*/
;*    hopc-jsheap ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopc-jsheap
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-source-map ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-source-map
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-use-strict ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-use-strict
   #t)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-arity-check ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-arity-check
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-type-check ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-type-check
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-meta ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-meta
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-debug ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-debug
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-pp ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-pp
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-clientc-inlining ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hopc-clientc-inlining
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-source-language ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hopc-source-language
   'auto)

;*---------------------------------------------------------------------*/
;*    hopc-optim-level ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hopc-optim-level
   1)
