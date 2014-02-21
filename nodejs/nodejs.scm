;*=====================================================================*/
;*    serrano/prgm/project/hop/2.6.x/nodejs/nodejs.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 18 16:19:42 2013                          */
;*    Last change :  Fri Feb 14 12:06:50 2014 (serrano)                */
;*    Copyright   :  2013-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    nodejs boot                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs

   (library hopscript)

   (with __nodejs_require
         __nodejs_process
         (__nodejs_console "| echo \"(module __nodejs_console)\"")
	 (__nodejs_constants "| echo \"(module __nodejs_constants)\"")
	 (__nodejs_util "| echo \"(module __nodejs_util)\"")
	 (__nodejs_sys "| echo \"(module __nodejs_sys)\"")
	 (__nodejs_path "| echo \"(module __nodejs_path)\"")
	 (__nodejs__linklist "| echo \"(module __nodejs__linklist)\"")
	 (__nodejs_events "| echo \"(module __nodejs_events)\"")
	 (__nodejs_assert "| echo \"(module __nodejs_assert)\"")
	 (__nodejs__stream_readable "| echo \"(module __nodejs__stream_readable)\"")
	 (__nodejs__stream_writable "| echo \"(module __nodejs__stream_writable)\"")
	 (__nodejs__stream_duplex "| echo \"(module __nodejs__stream_duplex)\"")
	 (__nodejs__stream_transform "| echo \"(module __nodejs__stream_transform)\"")
	 (__nodejs__stream_passthrough "| echo \"(module __nodejs__stream_passthrough)\"")
	 (__nodejs_%stream "| echo \"(module __nodejs_%stream)\"")
	 (__nodejs_fs "| echo \"(module __nodejs_fs)\"")
	 (__nodejs_punycode "| echo \"(module __nodejs_punycode)\"")
	 (__nodejs_buffer "| echo \"(module __nodejs_buffer)\"")
	 (__nodejs_dgram "| echo \"(module __nodejs_dgram)\"")
	 (__nodejs_vm "| echo \"(module __nodejs_vm)\"")
	 (__nodejs_timers "| echo \"(module __nodejs_timers)\"")
	 (__nodejs_net "| echo \"(module __nodejs_net)\"")
	 (__nodejs_querystring "| echo \"(module __nodejs_querystring)\"")
	 (__nodejs_string_decoder "| echo \"(module __nodejs_string_decoder)\"")
	 (__nodejs_child_process "| echo \"(module __nodejs_child_process)\"")
	 (__nodejs_cluster "| echo \"(module __nodejs_cluster)\"")
	 (__nodejs_crypto "| echo \"(module __nodejs_crypto)\"")
	 (__nodejs_dns "| echo \"(module __nodejs_dns)\"")
	 (__nodejs_domain "| echo \"(module __nodejs_domain)\"")
	 (__nodejs_freelist "| echo \"(module __nodejs_freelist)\"")
	 (__nodejs_url "| echo \"(module __nodejs_url)\"")
	 (__nodejs_tls "| echo \"(module __nodejs_tls)\"")
	 (__nodejs_http "| echo \"(module __nodejs_http)\"")
	 (__nodejs_https "| echo \"(module __nodejs_https)\"")
	 (__nodejs_zlib "| echo \"(module __nodejs_zlib)\"")
	 ))
