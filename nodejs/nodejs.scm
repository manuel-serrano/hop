;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/nodejs.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 18 16:19:42 2013                          */
;*    Last change :  Fri Feb 23 17:09:06 2024 (serrano)                */
;*    Copyright   :  2013-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    nodejs boot                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs

   (option (set! *warning-overriden-variables* #f))
   
   (library hopscript)

   (import __nodejs_require
	   __nodejs__process
	   __nodejs__buffer
	   ;; nodejs builtin common modules
	   (__nodejs_console "| echo \"(module __nodejs_console (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_constants "| echo \"(module __nodejs_constants (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_util "| echo \"(module __nodejs_util (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_sys "| echo \"(module __nodejs_sys (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_path "| echo \"(module __nodejs_path (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_process "| echo \"(module __nodejs_process (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__linklist "| echo \"(module __nodejs__linklist (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_events "| echo \"(module __nodejs_events (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_assert "| echo \"(module __nodejs_assert (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__stream_readable "| echo \"(module __nodejs__stream_readable (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__stream_writable "| echo \"(module __nodejs__stream_writable (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__stream_duplex "| echo \"(module __nodejs__stream_duplex (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__stream_transform "| echo \"(module __nodejs__stream_transform (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__stream_passthrough "| echo \"(module __nodejs__stream_passthrough (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_stream "| echo \"(module __nodejs_stream (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_fs "| echo \"(module __nodejs_fs (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_fs_promises "| echo \"(module __nodejs_fs_promises (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_punycode "| echo \"(module __nodejs_punycode (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_buffer "| echo \"(module __nodejs_buffer (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_dgram "| echo \"(module __nodejs_dgram (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_vm "| echo \"(module __nodejs_vm (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_timers "| echo \"(module __nodejs_timers (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_net "| echo \"(module __nodejs_net (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_querystring "| echo \"(module __nodejs_querystring (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_string_decoder "| echo \"(module __nodejs_string_decoder (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_child_process "| echo \"(module __nodejs_child_process (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_cluster "| echo \"(module __nodejs_cluster (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_crypto "| echo \"(module __nodejs_crypto (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_dns "| echo \"(module __nodejs_dns (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_domain "| echo \"(module __nodejs_domain (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_freelist "| echo \"(module __nodejs_freelist (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_url "| echo \"(module __nodejs_url (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_tls "| echo \"(module __nodejs_tls (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_tty "| echo \"(module __nodejs_tty (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_http "| echo \"(module __nodejs_http (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_https "| echo \"(module __nodejs_https (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_zlib "| echo \"(module __nodejs_zlib (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_os "| echo \"(module __nodejs_os (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_hop "| echo \"(module __nodejs_hop (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_hophz "| echo \"(module __nodejs_hophz (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_node_tick "| echo \"(module __nodejs_node_tick (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_node_stdio "| echo \"(module __nodejs_node_stdio (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_node_proc "| echo \"(module __nodejs_node_proc (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_node_timers "| echo \"(module __nodejs_node_timers (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_node_cluster "| echo \"(module __nodejs_node_cluster (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs__module "| echo \"(module __nodejs__module (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   ;; nodejs builtin es6 modules
	   (__nodejs_mod_hop "| echo \"(module __nodejs_mod_hop (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_path "| echo \"(module __nodejs_mod_path (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_fs "| echo \"(module __nodejs_mod_fs (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_fs_promises "| echo \"(module __nodejs_mod_fs_promises (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_http "| echo \"(module __nodejs_mod_http (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_https "| echo \"(module __nodejs_mod_https (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_url "| echo \"(module __nodejs_mod_url (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_process "| echo \"(module __nodejs_mod_process (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
	   (__nodejs_mod_module "| echo \"(module __nodejs_mod_module (library hop hopscript js2scheme) (export (hopscript ::JsGlobalObject ::JsObject ::JsObject ::JsObject)))\"")
)


   (export (core-module-table)
	   (nodejs-deprecation-config! #!key (DEP0134 #t))
	   *nodejs-DEP0134*))

;*---------------------------------------------------------------------*/
;*    make-core-module-table :: ...                                    */
;*---------------------------------------------------------------------*/
(define-macro (make-core-module-table . modules)
   `(list
       ,@(map (lambda (m)
		 `(cons ,(car m) (@ hopscript ,(cadr m))))
	    modules)))

;*---------------------------------------------------------------------*/
;*    module-table ...                                                 */
;*    -------------------------------------------------------------    */
;*    See also js2scheme/module.scm where the table is duplicated      */
;*    and should be maintained in sync.                                */
;*---------------------------------------------------------------------*/
(define module-table
   (cons
      ;; the buffer module is special as the Buffer constructor
      ;; does not allocate a plain JsObject (see _buffer.scm)
      (cons "buffer" (@ hopscript __nodejs__buffer))
      (make-core-module-table
	 ("console" __nodejs_console)
	 ("constants" __nodejs_constants)
	 ("util" __nodejs_util)
	 ("sys" __nodejs_sys)
	 ("path" __nodejs_path)
	 ("path.mod" __nodejs_mod_path)
	 ("_linklist" __nodejs__linklist)
	 ("events" __nodejs_events)
	 ("assert" __nodejs_assert)
	 ("_stream_readable" __nodejs__stream_readable)
	 ("_stream_writable" __nodejs__stream_writable)
	 ("_stream_duplex" __nodejs__stream_duplex)
	 ("_stream_transform" __nodejs__stream_transform)
	 ("_stream_passthrough" __nodejs__stream_passthrough)
	 ("stream" __nodejs_stream)
	 ("fs" __nodejs_fs)
	 ("fs.mod" __nodejs_mod_fs)
	 ("fs/promises" __nodejs_fs_promises)
	 ("fs/promises.mod" __nodejs_mod_fs_promises)
	 ("punycode" __nodejs_punycode)
	 ("process" __nodejs_process)
	 ("process.mod" __nodejs_mod_process)
	 ("dgram" __nodejs_dgram)
	 ("vm" __nodejs_vm)
	 ("timers" __nodejs_timers)
	 ("net" __nodejs_net)
	 ("querystring" __nodejs_querystring)
	 ("string_decoder" __nodejs_string_decoder)
	 ("child_process" __nodejs_child_process)
	 ("cluster" __nodejs_cluster)
	 ("crypto" __nodejs_crypto)
	 ("dns" __nodejs_dns)
	 ("domain" __nodejs_domain)
	 ("freelist" __nodejs_freelist)
	 ("url" __nodejs_url)
	 ("url.mod" __nodejs_mod_url)
	 ("tls" __nodejs_tls)
	 ("tty" __nodejs_tty)
	 ("http" __nodejs_http)
	 ("http.mod" __nodejs_mod_http)
	 ("https" __nodejs_https)
	 ("https.mod" __nodejs_mod_https)
	 ("zlib" __nodejs_zlib)
	 ("os" __nodejs_os)
	 ("hop" __nodejs_hop)
	 ("hop.mod" __nodejs_mod_hop)
	 ("hophz" __nodejs_hophz)
	 ("node_tick" __nodejs_node_tick)
	 ("node_stdio" __nodejs_node_stdio)
	 ("node_proc" __nodejs_node_proc)
	 ("node_timers" __nodejs_node_timers)
	 ("node_cluster" __nodejs_node_cluster)
	 ("module" __nodejs__module)
	 ("module.mod" __nodejs_mod_module))))

;*---------------------------------------------------------------------*/
;*    core-module-table ...                                            */
;*---------------------------------------------------------------------*/
(define (core-module-table)
   module-table)

;*---------------------------------------------------------------------*/
;*    deprecations ...                                                 */
;*---------------------------------------------------------------------*/
;; https://nodejs.org/dist/latest-v18.x/docs/api/deprecations.html#DEP0134   
(define *nodejs-DEP0134* #t)

;*---------------------------------------------------------------------*/
;*    nodejs-deprecation-config! ...                                   */
;*    -------------------------------------------------------------    */
;*    This function is currently never invoked but it should be        */
;*    when Hop or an Hop application starts.                           */
;*---------------------------------------------------------------------*/
(define (nodejs-deprecation-config! #!key (DEP0134 #t))
   (set! *nodejs-DEP0134* DEP0134))
