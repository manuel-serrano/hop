;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/constant.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Wed Mar  9 13:57:49 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init the this variable of all function in non-strict mode        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_constant

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-constant-stage
	   (generic j2s-constant ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-constant-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-constant-stage
   (instantiate::J2SStageProc
      (name "constant")
      (comment "Pre-allocated constants")
      (proc j2s-constant)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-constant ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-constant this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-constant ::J2SProgram ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-constant this::J2SProgram args)
   (with-access::J2SProgram this (nodes headers decls loc pcache-size cnsts)
      (let ((env (env 0 '() (create-hashtable)
		    (create-hashtable :eqtest equal?))))
	 (for-each (lambda (n) (constant! n env 0)) headers)
	 (for-each (lambda (n) (constant! n env 0)) decls)
	 (for-each (lambda (n) (constant! n env 0)) nodes)
	 (set! cnsts (reverse! (env-list env)))))
   this)

;*---------------------------------------------------------------------*/
;*    env ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct env cnt list table inits-table)

;*---------------------------------------------------------------------*/
;*    add-env! ...                                                     */
;*---------------------------------------------------------------------*/
(define (add-env! this env::struct sharep)
   (with-access::J2SLiteralValue this (val)
      (if sharep
	  (let* ((t (env-table env))
		 (k val)
		 (old (hashtable-get t k)))
	     (or old
		 (let ((n (env-cnt env)))
		    (hashtable-put! t k n)
		    (env-cnt-set! env (+fx 1 n))
		    (env-list-set! env (cons this (env-list env)))
		    n)))
	  (let ((n (env-cnt env)))
	     (env-cnt-set! env (+fx 1 n))
	     (env-list-set! env (cons this (env-list env)))
	     n))))

;*---------------------------------------------------------------------*/
;*    add-expr! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-expr! this env::struct sharep)
   (let ((index (add-env! this env sharep)))
      (with-access::J2SExpr this (loc)
	 (instantiate::J2SLiteralCnst
	    (type (j2s-type this))
	    (loc loc)
	    (index index)
	    (val this)))))

;*---------------------------------------------------------------------*/
;*    add-cmap! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-cmap! loc keys env::struct)
   (let* ((t (env-inits-table env))
	  (k keys)
	  (old (hashtable-get t k)))
      (or old
	  (let ((n (env-cnt env)))
	     (hashtable-put! t k n)
	     (env-cnt-set! env (+fx 1 n))
	     (let ((cnst (instantiate::J2SCmap
			    (type 'pair)
			    (loc loc)
			    (val keys))))
		(env-list-set! env (cons cnst (env-list env)))
		n)))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SNode env::struct nesting)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SString ...                                        */
;*---------------------------------------------------------------------*/
;* (define-walk-method (constant! this::J2SString env nesting)         */
;*    (if (=fx nesting 0)                                              */
;*        (add-expr! this env #t)                                      */
;*        this))                                                       */

;*---------------------------------------------------------------------*/
;*    constant! ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SRegExp env nesting)
   (with-access::J2SRegExp this (val flags)
      (if (=fx nesting 0)
	  (add-expr! this env #f)
	  this)))

;*---------------------------------------------------------------------*/
;*    constant! ::J2STilde ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2STilde env nesting)
   (with-access::J2STilde this (stmt)
      (set! stmt (constant! stmt env (+fx nesting 1)))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SDollar ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDollar env nesting)
   (with-access::J2SDollar this (node)
      (set! node (constant! node env (-fx nesting 1)))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SObjInit ...                                       */
;*---------------------------------------------------------------------*/
(define-method (constant! this::J2SObjInit env nesting)
   (with-access::J2SObjInit this (inits cmap loc)
      (let ((keys (map (lambda (i)
			  (when (isa? i J2SDataPropertyInit)
			     (with-access::J2SDataPropertyInit i (name)
				(cond
				   ((isa? name J2SString)
				    (with-access::J2SString name (val)
				       (unless (string=? val "__proto__")
					  (string->symbol val))))
				   ((isa? name J2SNumber)
				    (with-access::J2SNumber name (val)
				       (string->symbol
					  (number->string val))))
				   (else
				    #f)))))
		     inits)))
	 (if (every (lambda (x) x) keys)
	     ;; constant cmap
	     (let ((n (add-cmap! loc (list->vector keys) env)))
		(set! cmap
		   (instantiate::J2SLiteralCnst
		      (type 'obj)
		      (loc loc)
		      (index n)
		      (val (car (env-list env)))))
		this)
	     (call-next-method)))))
