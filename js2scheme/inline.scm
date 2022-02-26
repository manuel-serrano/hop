;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Sat Feb 26 10:41:23 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function/Method inlining optimization                            */
;*    -------------------------------------------------------------    */
;*    Three strageties are available                                   */
;*    -------------------------------------------------------------    */
;*      1- profile-based                                               */
;*      2- depth-first (inline from root to leaves)                    */
;*      3- breadth-first (inline all functions and iterate)            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_inline

   (library web)
   
   (include "ast.sch"
	    "usage.sch"
	    "inline.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_use
	   __js2scheme_node-size
	   __js2scheme_freevars
	   __js2scheme_classutils
	   __js2scheme_inline-common
	   __js2scheme_inline-profile
	   __js2scheme_inline-depth
	   __js2scheme_inline-breadth)

   (export j2s-inline-stage))

;*---------------------------------------------------------------------*/
;*    j2s-inline-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-inline-stage
   (instantiate::J2SStageProc
      (name "inline")
      (comment "Method inlining optimization")
      (optional :optim-inline)
      (proc j2s-inline!)))

;*---------------------------------------------------------------------*/
;*    j2s-inline! ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-inline! this conf)
   (cond
      ((not (isa? this J2SProgram))
       this)
      ((config-get conf :profile-call #f)
       (j2s-count-calls! this conf)
       this)
      ((config-get conf :profile-log #f)
       =>
       (lambda (logfile)
	  (or (j2s-inline-profile this conf logfile)
	      (j2s-inline-noprofile this conf))))
      (else
       (j2s-inline-noprofile this conf))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-noprofile ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-inline-noprofile this::J2SProgram conf)
   (let ((mi (config-get conf :optim-inline-strategy 'depth)))
      (case mi
	 ((depth) (j2s-inline-depth this conf))
	 ((breadth) (j2s-inline-breadth this conf))
	 (else (error "j2s-inline" "unknown inline method" mi)))))

