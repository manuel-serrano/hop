;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/ronly.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 20 07:55:23 2013                          */
;*    Last change :  Wed Oct 19 09:13:23 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mark read-only variables in the J2S AST.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_ronly
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage)

   (export j2s-ronly-stage
	   (generic j2s-ronly ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-ronly-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-ronly-stage
   (instantiate::J2SStageProc
      (name "read-only")
      (comment "Mark read-only variables")
      (proc j2s-ronly)))

;*---------------------------------------------------------------------*/
;*    j2s-ronly ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (j2s-ronly this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-ronly ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-ronly this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls mode)
      (for-each (lambda (o) (ronly! o mode)) headers)
      (for-each (lambda (o) (ronly! o mode)) decls)
      (for-each (lambda (o) (ronly! o mode)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SNode mode::symbol)
   (default-walk! this mode))

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SAssig mode::symbol)
   (with-access::J2SAssig this (lhs rhs loc)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (when (j2s-const? decl)
	       (with-access::J2SDecl decl (id)
		  (raise
		     (instantiate::&io-error
			(proc "ronly")
			(msg "Const variables cannot be assigned")
			(obj id)
			(fname (cadr loc))
			(location (caddr loc))))))
	    (with-access::J2SDecl decl (ronly id)
	       (set! ronly #f))))
      (ronly! rhs mode))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SInit mode::symbol)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (ronly id)
	       (set! ronly #f))))
      (ronly! rhs mode))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SDecl mode::symbol)
   (with-access::J2SDecl this (ronly id scope writable)
      (if (eq? mode 'hopscript)
	  (set! ronly #t)
	  (set! ronly (or (not (memq scope '(global %scope))) (not writable)))))
   this)
   
;*---------------------------------------------------------------------*/
;*    ronly! ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly! this::J2SDeclInit mode::symbol)
   (call-next-method)
   (with-access::J2SDeclInit this (val ronly)
      (ronly! val mode))
   this)
   
;* {*---------------------------------------------------------------------*} */
;* {*    ronly! ::J2SLetInit ...                                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (ronly! this::J2SLetInit)                       */
;*    (call-next-method)                                               */
;*    (with-access::J2SLetInit this (val)                              */
;*       (ronly! val))                                                 */
;*    this)                                                            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    ronly! ::J2SLetOpt ...                                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (ronly! this::J2SLetOpt)                        */
;*    (call-next-method)                                               */
;*    (with-access::J2SLetOpt this (val id)                            */
;*       (ronly! val))                                                 */
;*    this)                                                            */
