;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/debug.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 11 12:32:21 2014                          */
;*    Last change :  Tue Jul 15 20:22:00 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript debugging intrumentation                              */
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
	   (generic j2s-debug ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-debug-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-debug-stage
   (instantiate::J2SStageProc
      (name "debug")
      (comment "JavaScript debug intrumentation")
      (proc j2s-debug)))

;*---------------------------------------------------------------------*/
;*    j2s-debug ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (j2s-debug this conf)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-debug ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-debug this::J2SProgram conf)
   (with-access::J2SProgram this (nodes)
      (for-each (lambda (n) (debug n conf)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    debug ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2SNode conf)
   (default-walk this conf))

;*---------------------------------------------------------------------*/
;*    debug ::J2SUnresolvedRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2SUnresolvedRef conf)
   (with-access::J2SUnresolvedRef this (id)
      (cond
	 ((eq? id 'setTimeout)
	  (set! id 'BgL_setTimeoutz00)))
      (call-default-walker)))
	 
;*---------------------------------------------------------------------*/
;*    debug ::J2STilde ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (debug this::J2STilde conf)
   (with-access::J2STilde this (stmt)
      (debug stmt conf)
      this))
   
