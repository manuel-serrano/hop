;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/strbuffer.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  1 16:06:44 2018                          */
;*    Last change :  Thu Jun  4 12:35:39 2020 (serrano)                */
;*    Copyright   :  2018-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    hint typing of numerical values.                                 */
;*    -------------------------------------------------------------    */
;*    This optimization consists replacing plain strings with          */
;*    string-buffers. A string is replaced with a string-buffer iff    */
;*      1- it is initialized as a fresh string (for instance a literal)*/
;*      2- it grows with string concatenation.                         */
;*    Example:                                                         */
;*      let x = ""                                                     */
;*      for( let i = 0; i < k; i++ ) x += String.fromCharCode( i )     */
;*    The variable x will allocated a string-buffer and the +=         */
;*    expression will be replaced with a buffer += concatenation.      */
;*    The benefit comes from the buffer compact memory representation. */
;*                                                                     */
;*    The optimization applies to variables that are:                  */
;*      - typed as "string";                                           */
;*      - initialized with a fresh string;                             */
;*      - used in string concatenation.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_strbuffer

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage)

   (export j2s-strbuffer-stage))

;*---------------------------------------------------------------------*/
;*    j2s-strbuffer-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-strbuffer-stage
   (instantiate::J2SStageProc
      (name "strbuffer")
      (comment "String buffer optimization")
      (optional :optim-strbuffer)
      (proc j2s-strbuffer)))

;*---------------------------------------------------------------------*/
;*    j2s-strbuffer ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-strbuffer this conf)
   (when (isa? this J2SProgram)
      (display "!!! WARNING !!!\n" (current-error-port))
      (display "Currently wrong because escaping" (current-error-port))
      (display "variables are not correctly handled." (current-error-port))
      (newline (current-error-port))
      (strbuffer-mark this #f)
      (let ((verb (>= (config-get conf :verbose 0) 2)))
	 (when verb
	    (fprintf (current-error-port) " "))
	 (strbuffer-type! this #f verb)))
   this)

;*---------------------------------------------------------------------*/
;*    fresh-string? ...                                                */
;*---------------------------------------------------------------------*/
(define (fresh-string? this::J2SExpr)
   (cond
      ((isa? this J2SString) #t)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    is-buffer? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-buffer? this::J2SDecl)
   (with-access::J2SDecl this (%info)
      (and (pair? %info) (eq? (car %info) 'strbuffer) (>fx (cdr %info) 5))))

;*---------------------------------------------------------------------*/
;*    strbuffer-mark ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-mark this::J2SNode inloop::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    strbuffer-mark ::J2SDeclInit ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-mark this::J2SDeclInit inloop)
   (call-default-walker)
   (with-access::J2SDeclInit this (val vtype %info)
      (when (and (eq? vtype 'string) (fresh-string? val))
	 (set! %info (cons 'strbuffer 0)))))

;*---------------------------------------------------------------------*/
;*    strbuffer-mark ::J2SInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-mark this::J2SInit inloop)
   (call-default-walker)
   (with-access::J2SInit this (lhs rhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (%info vtype)
	       (when (and (eq? vtype 'string) (fresh-string? rhs))
		  (set! %info (cons 'strbuffer 0))))))))
   
;*---------------------------------------------------------------------*/
;*    strbuffer-mark ::J2SLoop ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-mark this::J2SLoop inloop)
   (set! inloop #t)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    strbuffer-mark ::J2SAssigOp ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-mark this::J2SAssigOp inloop)
   (call-default-walker)
   (with-access::J2SAssigOp this (op lhs rhs type)
      (when (and (eq? op '+) (eq? type 'string) (isa? lhs J2SRef))
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (%info)
	       (when (and (pair? %info) (eq? (car %info) 'strbuffer))
		  (let ((cnt (cdr %info)))
		     (if inloop
			 (set-cdr! %info (+fx cnt 10))
			 (set-cdr! %info (+fx cnt 1))))))))))

;*---------------------------------------------------------------------*/
;*    strbuffer-mark ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-mark this::J2SAssig inloop)
   (call-default-walker)
   (with-access::J2SAssig this (lhs rhs type)
      (when (and (isa? lhs J2SRef) (isa? rhs J2SBinary) (eq? type 'string))
	 (with-access::J2SBinary rhs (op (blhs lhs))
	    (when (and (eq? op '+) (isa? blhs J2SRef))
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SRef blhs ((bdecl decl))
		     (when (eq? decl bdecl)
			(with-access::J2SDecl decl (%info)
			   (when (and (pair? %info) (eq? (car %info) 'strbuffer))
			      (let ((cnt (cdr %info)))
				 (if inloop
				     (set-cdr! %info (+fx cnt 10))
				     (set-cdr! %info (+fx cnt 1))))))))))))))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SNode inexpr::bool verbose::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SExpr ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SExpr inexpr::bool verbose)
   (set! inexpr #t)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SStmt ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SStmt inexpr::bool verbose)
   (set! inexpr #f)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SDecl ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SDecl inexpr verbose)
   (call-default-walker)
   (with-access::J2SDecl this (loc vtype)
      (when (is-buffer? this)
	 (when verbose
	    (fprintf (current-error-port) "~a." (caddr loc)))
	 (set! vtype 'buffer)))
   this)

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SDeclInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SDeclInit inexpr verbose)
   (with-access::J2SDeclInit this (loc vtype val)
      (if (is-buffer? this)
	  (begin
	     (when verbose
		(fprintf (current-error-port) "~a." (caddr loc)))
	     (with-access::J2SExpr val (type)
		(set! type 'buffer))
	     (set! vtype 'buffer)
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SInit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SInit inexpr verbose)
   (with-access::J2SInit this (lhs rhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (if (is-buffer? decl)
		 (with-access::J2SExpr rhs (type)
		    (set! type 'buffer)
		    this)
		 (call-default-walker)))
	  (call-default-walker))))
		  
;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SRef inexpr verbose)
   (call-default-walker)
   (with-access::J2SRef this (decl loc type)
      (if (is-buffer? decl)
	  (begin
	     (set! type 'buffer)
	     (J2SCast 'string this))
	  this)))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SAssigOp ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SAssigOp inexpr verbose)
   (with-access::J2SAssigOp this (op lhs rhs type loc)
      (set! rhs (strbuffer-type! rhs #t verbose))
      (if (and (eq? op '+) (eq? type 'string) (isa? lhs J2SRef))
	  (with-access::J2SRef lhs (decl (rtype type))
	     (if (is-buffer? decl)
		 (begin
		    (set! type 'buffer)
		    (set! rtype 'buffer)
		    (if inexpr
			(J2SCast 'string this)
			this))
		 (begin
		    (set! lhs (strbuffer-type! lhs #t verbose))
		    this)))
	  (begin
	     (set! lhs (strbuffer-type! lhs #t verbose))
	     this))))

;*---------------------------------------------------------------------*/
;*    strbuffer-type! ::J2SAssig ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (strbuffer-type! this::J2SAssig inexpr verbose)
   (with-access::J2SAssig this (lhs rhs type loc)
      (set! rhs (strbuffer-type! rhs #t verbose))
      (if (and (isa? lhs J2SRef) (isa? rhs J2SBinary) (eq? type 'string))
	  (with-access::J2SBinary rhs (op (blhs lhs))
	     (if (and (eq? op '+) (isa? blhs J2SRef))
		 (with-access::J2SRef lhs (decl (rtype type))
		    (with-access::J2SRef blhs ((bdecl decl))
		       (if (eq? decl bdecl)
			   (if (is-buffer? decl)
			       (begin
				  (set! type 'buffer)
				  (set! rtype 'buffer)
				  (if inexpr
				      (J2SCast 'string this)
				      this))
			       (begin
				  (set! lhs (strbuffer-type! lhs #t verbose))
				  this))
			   (begin
			      (set! lhs (strbuffer-type! lhs #t verbose))
			      this))))
		 (begin
		    (set! lhs (strbuffer-type! lhs #t verbose))
		    this)))
	  (begin
	     (set! lhs (strbuffer-type! lhs #t verbose))
	     this))))
