;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/debug.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 11 12:32:21 2014                          */
;*    Last change :  Tue Dec 30 07:20:15 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript debugging instrumentation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_debug
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_scheme
	   __js2scheme_stage)
   
   (export j2s-debug-stage
	   (generic j2s-debug ::obj ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    j2s-debug-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-debug-stage
   (instantiate::J2SStageProc
      (name "debug")
      (comment "JavaScript debug intrumentation")
      (proc (lambda (n conf) (j2s-debug n conf 'server)))))

;*---------------------------------------------------------------------*/
;*    j2s-debug ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (j2s-debug this conf site)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-debug ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-debug this::J2SProgram conf site)
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (n) (debug n conf site)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    debug ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2SNode conf site)
   (default-walk this conf site))

;*---------------------------------------------------------------------*/
;*    debug ::J2SUnresolvedRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2SUnresolvedRef conf site)
   (when (eq? site 'client)
      (with-access::J2SUnresolvedRef this (id)
	 (cond
	    ((eq? id 'setTimeout)
	     (set! id 'BgL_setTimeoutz00))
	    ((eq? id 'setInterval)
	     (set! id 'BgL_setIntervalz00))))
      (call-default-walker)))
	 
;*---------------------------------------------------------------------*/
;*    debug ::J2STilde ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2STilde conf side)
   (with-access::J2STilde this (stmt)
      (debug stmt conf 'client)
      this))
   
;*---------------------------------------------------------------------*/
;*    debug ::J2SDollar ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2SDollar conf site)
   (with-access::J2SDollar this (node)
      (debug node conf 'server)))
