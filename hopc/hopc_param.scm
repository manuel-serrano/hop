;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopc/hopc_param.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:20:19 2004                          */
;*    Last change :  Tue Jun 25 14:57:13 2024 (serrano)                */
;*    Copyright   :  2004-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOPC global parameters                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_param
   
   (library hop)

   (include "../nodejs/nodejs.sch")
   
   (export  (hopc-rc-file::bstring)
	    (hopc-rc-file-set! ::bstring)

	    (hopc-bigloo::bstring)
	    (hopc-bigloo-set! ::bstring)
	    (hopc-bigloo-options::pair-nil)
	    (hopc-bigloo-options-set! ::pair-nil)
	    (hopc-bigloo-O-options::pair-nil)
	    (hopc-bigloo-safe-option::obj)
	    (hopc-bigloo-safe-option-set! ::obj)
	    (hopc-bigloo-profile-options::pair-nil)
	    (hopc-bigloo-profile-options-set! ::pair-nil)

	    (hopc-hop-lib-dir::obj)
	    (hopc-hop-lib-dir-set!::obj ::obj)

	    (hopc-mo-dir::obj)
	    (hopc-mo-dir-set!::obj ::obj)

	    (hopc-pass::symbol)
	    (hopc-pass-set! ::symbol)

	    (hopc-bootstrap-mode::bool)
	    (hopc-bootstrap-mode-set! ::bool)

	    (hopc-js-target::symbol)
	    (hopc-js-target-set! ::symbol)

	    (hopc-sources::pair-nil)
	    (hopc-sources-set! ::pair-nil)
	    (hopc-destination::obj)
	    (hopc-destination-set! ::obj)
	    (hopc-temp::obj)
	    (hopc-temp-set! ::obj)

	    (hopc-pipe-filesize-threshold::elong)

	    (hopc-share-directory::bstring)
	    (hopc-share-directory-set! ::bstring)
   
	    (hopc-access-file::obj)
	    (hopc-access-file-set! ::obj)

	    (hopc-long-size::obj)
	    (hopc-long-size-set! ::obj)
	    (hopc-int-size::obj)
	    (hopc-int-size-set! ::obj)

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

	    (hopc-sofile-dir::obj)
	    (hopc-sofile-dir-set! ::obj)

	    (hopc-sobase::obj)
	    (hopc-sobase-set! ::bstring)

	    (hopc-optim-level::int)
	    (hopc-optim-level-set! ::int)

	    (hopc-node-modules-directory::obj)
	    (hopc-node-modules-directory-set! ::obj)
	    
	    (hopc-js-worker::bool)
	    (hopc-js-worker-set! ::bool)

	    (hopc-js-worker-slave::bool)
	    (hopc-js-worker-slave-set! ::bool)

	    (hopc-js-module-name::obj)
	    (hopc-js-module-name-set! ::obj)
	    
	    (hopc-js-module-path::obj)
	    (hopc-js-module-path-set! ::obj)
	    
	    (hopc-js-module-main::obj)
	    (hopc-js-module-main-set! ::obj)

	    (hopc-source-ast::obj)
	    (hopc-source-ast-set! ::obj)
	    (hopc-source-ast-file::obj)
	    (hopc-source-ast-file-set! ::obj)
	    
	    (hopc-js-header::obj)
	    (hopc-js-header-set! ::obj)

	    (hopc-js-type-annotations::bool)
	    (hopc-js-type-annotations-set! ::bool)

	    (hopc-js-libraries::pair-nil)
	    (hopc-js-libraries-set! ::pair-nil)
	    
	    (hopc-js-driver::obj)
	    (hopc-js-driver-set! ::obj)

	    (hopc-js-return-as-exit::bool)
	    (hopc-js-return-as-exit-set! ::bool)

	    (hopc-j2s-flags::pair-nil)
	    (hopc-j2s-flags-set! ::pair-nil)

	    (hopc-j2s-loaders::pair-nil)
	    (hopc-j2s-loaders-set! ::pair-nil)

	    (hopc-j2s-preprocessor::obj)
	    (hopc-j2s-preprocessor-set! ::obj)

	    (hopc-pp-width))
	    
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
   (hop-bigloo))

;*---------------------------------------------------------------------*/
;*    hopc-bigloo-options ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bigloo-options
   '())

;*---------------------------------------------------------------------*/
;*    hopc-bigloo-O-options ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bigloo-O-options
   '((2 . ("-fisa")) (6 . ("-flocal-exit"))))

;*---------------------------------------------------------------------*/
;*    hopc-bigloo-safe-option ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bigloo-safe-option
   '())

;*---------------------------------------------------------------------*/
;*    hopc-bigloo-profile-options ...                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bigloo-profile-options
   '())

;*---------------------------------------------------------------------*/
;*    hopc-hop-lib-dir ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hopc-hop-lib-dir
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-mo-dir ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopc-mo-dir
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-pass ...                                                    */
;*---------------------------------------------------------------------*/
(define-parameter hopc-pass
   'link)

;*---------------------------------------------------------------------*/
;*    hopc-bootstrap-mode ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-bootstrap-mode
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-target ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-target
   'es6)

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
;*    hopc-temp ...                                                    */
;*---------------------------------------------------------------------*/
(define-parameter hopc-temp
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-pipe-filesize-threshold ...                                 */
;*    -------------------------------------------------------------    */
;*    The file size threshold above which ast transmitted via          */
;*    temporary files (otherwise serialized on Bigloo stdin).          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-pipe-filesize-threshold
   #e65536)

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
;*    hopc-long-size ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hopc-long-size
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-int-size ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hopc-int-size
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
;*    hopc-sofile-dir ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hopc-sofile-dir
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-sobase ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopc-sobase
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-optim-level ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hopc-optim-level
   0)

;*---------------------------------------------------------------------*/
;*    hopc-node-modules-directory ...                                  */
;*---------------------------------------------------------------------*/
(define-parameter hopc-node-modules-directory
   (nodejs-node-modules-directory))

;*---------------------------------------------------------------------*/
;*    hopc-js-worker ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-worker
   #t)

;*---------------------------------------------------------------------*/
;*    hopc-js-worker-slave ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-worker-slave
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-module-name ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-module-name
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-module-path ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-module-path
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-module-main ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-module-main
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hopc-source-ast ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hopc-source-ast
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-source-ast-file ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter hopc-source-ast-file
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-header ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-header
   #t)

;*---------------------------------------------------------------------*/
;*    hopc-js-type-annotations ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-type-annotations
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-libraries ...                                            */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-libraries
   (cond-expand
      (enable-libuv
       '("-library" "http"
	 "-library" "hopscript"
	 "-library" "web"
	 "-library" "nodejs"
	 "-library" "libuv"))
      (else
       '("-library" "http"
	 "-library" "hopscript"
	 "-library" "web"
	 "-library" "nodejs"))))

;*---------------------------------------------------------------------*/
;*    hopc-js-driver ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-driver
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-js-return-as-exit ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter hopc-js-return-as-exit
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-j2s-flags ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter hopc-j2s-flags
   '())

;*---------------------------------------------------------------------*/
;*    hopc-j2s-loaders ...                                             */
;*    -------------------------------------------------------------    */
;*    Enable/disable j2s loaders                                       */
;*---------------------------------------------------------------------*/
(define-parameter hopc-j2s-loaders
   '())

;*---------------------------------------------------------------------*/
;*    hopc-j2s-preprocessor ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter hopc-j2s-preprocessor
   #f)

;*---------------------------------------------------------------------*/
;*    hopc-pp-width ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter hopc-pp-width
   256)
