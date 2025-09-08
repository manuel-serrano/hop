;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/js.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 23 09:28:30 2013                          */
;*    Last change :  Mon Sep  8 13:26:02 2025 (serrano)                */
;*    Copyright   :  2013-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Js->Js (for client side code).                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_js

   (include "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_scheme
	   __js2scheme_stmtassign
	   __js2scheme_stage
	   __js2scheme_scheme-constant)
   
   (export j2s-javascript-stage
	   (generic j2s-js-literal ::obj ctx)
	   (generic j2s-js::pair-nil ::J2SNode tildec dollarc mode evalp conf)))

;*---------------------------------------------------------------------*/
;*    j2s-javascript-stage ...                                         */
;*---------------------------------------------------------------------*/
(define j2s-javascript-stage
   (instantiate::J2SStageProc
      (name "javascript")
      (comment "JavaScript code generation")
      (proc (lambda (ast conf)
	       (add-header! ast conf)
	       (add-hop! ast conf)
	       (call-with-eval-module (eval! `(module ,(gensym)))
		  (lambda ()
		     (eval! `(define %this ,(config-get conf :%this)))
		     (eval! `(define %resource ,(config-get conf :resource)))
		     (eval! `(define %source ,(config-get conf :source)))
		     (eval! `(define %worker ,(config-get conf :worker)))
		     (eval! `(define %scope %this))
		     (eval! `(define this %this))
		     (let ((ctx (compiler-context conf)))
			(j2s-js ast
			   (lambda (this::J2STilde tildec dollarc mode evalp ctx)
			      (with-access::J2STilde this (loc stmt)
				 (cons* this "new hop_tilde( function() {"
				    (append (j2s-js stmt
					       tildec
					       dollarc
					       mode evalp ctx)
				       '("})")))))
			   (lambda (this::J2SDollar tildec dollarc mode evalp ctx)
			      (let ((prog (context-program ctx)))
				 (with-strings prog
				    (lambda ()
				       (with-access::J2SDollar this (node)
					  (let ((expr (j2s-scheme node mode evalp ctx)))
					     (list (j2s-js-literal
						      (eval! `(let ((__js_strings ,(j2s-jsstring-init prog)))
								 ,expr))
						      (context-get ctx :%this)))))))))
			   'normal (lambda (x) x) ctx))))))))

;*---------------------------------------------------------------------*/
;*    with-strings ...                                                 */
;*---------------------------------------------------------------------*/
(define (with-strings prog thunk)
   (with-access::J2SProgram prog (strings)
      (let ((strs strings))
	 (set! strings '())
	 (unwind-protect
	    (thunk)
	    (set! strings strs)))))

;*---------------------------------------------------------------------*/
;*    j2s-js-literal ::obj ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (j2s-js-literal obj::obj ctx)
   (cond
      ((number? obj) obj)
      ((boolean? obj) (if obj "true" "false"))
      ((string? obj) (format "~s" obj))
      ((eq? obj #unspecified) "(js-undefined)")
      (else (error "js" "Illegal $value" obj))))
   
;*---------------------------------------------------------------------*/
;*    add-header! ...                                                  */
;*---------------------------------------------------------------------*/
(define (add-header! ast conf)
   (let ((header (config-get conf :header #f)))
      (when (and header (isa? ast J2SProgram))
	 (with-access::J2SProgram ast (headers loc)
	    (set! headers (cons (instantiate::J2SPragma
				   (loc loc)
				   (lang 'javascript)
				   (expr header))
			     headers))))))

;*---------------------------------------------------------------------*/
;*    add-hop! ...                                                     */
;*---------------------------------------------------------------------*/
(define (add-hop! ast conf)
   (let ((header (config-get conf :hop-require #f)))
      (when (and header (isa? ast J2SProgram))
	 (with-access::J2SProgram ast (headers loc)
	    (set! headers (cons (instantiate::J2SPragma
				   (loc loc)
				   (lang 'javascript)
				   (expr "const hop = require(\"./hop.js\");"))
			     headers))))))

;*---------------------------------------------------------------------*/
;*    j2s-js-id ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-js-id::bstring decl::J2SDecl)
   (with-access::J2SDecl decl (_scmid id key)
      (if (symbol? _scmid)
	  (if (eq? _scmid 'this)
	      "this"
	      (string-append (string-replace (symbol->string! _scmid) #\% #\$)
		 "$$" (fixnum->string key)))
	  (let ((s (symbol->string! id)))
	     (if (char=? (string-ref s 0) #\%)
		 (string-append (string-replace s #\% #\$)
		    "$$" (fixnum->string key))
		 s)))))

;*---------------------------------------------------------------------*/
;*    append-map* ...                                                  */
;*---------------------------------------------------------------------*/
(define (append-map* sep proc l)
   (if (null? l)
       '()
       (let loop ((l l))
	  (if (null? (cdr l))
	      (proc (car l))
	      (append (proc (car l))
		 (cons sep (loop (cdr l))))))))

;*---------------------------------------------------------------------*/
;*    append-map2* ...                                                 */
;*---------------------------------------------------------------------*/
(define (append-map2* sep proc l1 l2)
   (if (null? l1)
       '()
       (let loop ((l1 l1)
		  (l2 l2))
	  (if (null? (cdr l1))
	      (proc (car l1) (car l2))
	      (append (proc (car l1) (car l2))
		 (cons sep (loop (cdr l1) (cdr l2))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js* ...                                                      */
;*---------------------------------------------------------------------*/
(define (j2s-js* this::J2SNode start end sep nodes tildec dollarc mode evalp ctx)
   (if (null? nodes)
       (list this start end)
       (cons* this start
	  (let loop ((nodes nodes))
	     (append (j2s-js (car nodes) tildec dollarc mode evalp ctx)
		(if (null? (cdr nodes))
		    (list end)
		    (cons sep (loop (cdr nodes)))))))))
				
;*---------------------------------------------------------------------*/
;*    j2s-js-ellipsis ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-js-ellipsis this::J2SNode start end sep ellipsis nodes tildec dollarc mode evalp ctx)
   (cond
      ((null? nodes)
       (list this start ellipsis end))
      ((null? (cdr nodes))
       (cons* this start ellipsis
	  (append (j2s-js (car nodes) tildec dollarc mode evalp ctx)
	     (list end)  )))
      (else
       (cons* this start
	  (let loop ((nodes nodes))
	     (append (j2s-js (car nodes) tildec dollarc mode evalp ctx)
		(cond
		   ((null? (cddr nodes))
		    (cons* sep ellipsis
		       (append
			  (j2s-js (cadr nodes) tildec dollarc mode evalp ctx)
			  (list end))))
		   (else
		    (cons sep (loop (cdr nodes)))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNode ...                                             */
;*    -------------------------------------------------------------    */
;*    This function returns a list of tokens and nodes. The nodes      */
;*    are meant to used for generating source-map tables. When         */
;*    generating JS code, they should be filtered out.                 */
;*---------------------------------------------------------------------*/
(define-generic (j2s-js::pair-nil this::J2SNode tildec dollarc mode evalp ctx)
   (list this (format "\"~a\"" (typeof this))))

;*---------------------------------------------------------------------*/
;*    j2s-js::pair-nil ::J2SProgram ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-js::pair-nil this::J2SProgram tildec dollarc mode evalp ctx)

   (define (not-literal node)
      (not (or (isa? node J2SLiteral)
	       (and (isa? node J2SStmtExpr)
		    (with-access::J2SStmtExpr node (expr)
		       (isa? expr J2SLiteral))))))
   
   (with-access::J2SProgram this (headers imports exports decls nodes mode)
      (let* ((nctx (compiler-context-set! (new-compiler-context ctx)
		      :program this))
	     (body (append headers
		      (filter (lambda (x)
				 (not (isa? x J2SExportDefault)))
			 exports)
		      decls (filter not-literal nodes)))
	     (prgm (j2s-js* this "" "" "" body tildec dollarc mode evalp nctx)))
	 (case mode
	    ((normal)
	     prgm)
	    ((strict hopscript)
	     (cons "\"use strict\";\n"
		(append (j2s-import this tildec dollarc mode evalp nctx)
		   prgm)))
	    (else
	     prgm)))))

;*---------------------------------------------------------------------*/
;*    j2s-js::pair-nil ::J2SMeta ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-js::pair-nil this::J2SMeta tildec dollarc mode evalp ctx)
   (with-access::J2SMeta this (stmt)
      (j2s-js stmt tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSeq ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSeq tildec dollarc mode evalp ctx)
   (with-access::J2SSeq this (nodes)
      (j2s-js* this "" "" "" nodes tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBlock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBlock tildec dollarc mode evalp ctx)
   (with-access::J2SBlock this (nodes)
      (match-case nodes
	 (((and ?node (? (lambda (n) (isa? n J2SBlock)))))
	  (j2s-js node tildec dollarc mode evalp ctx))
	 (else
	  (j2s-js* this "{" "}" "" nodes tildec dollarc mode evalp ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SWith ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SWith tildec dollarc mode evalp ctx)
   (with-access::J2SWith this (obj block)
      (cons* this "with (" (append (j2s-js obj tildec dollarc mode evalp ctx)
         (cons ") " (j2s-js block tildec dollarc mode evalp ctx) )))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SWithRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SWithRef tildec dollarc mode evalp ctx)
   (with-access::J2SWithRef this (id)
      (list this (symbol->string! id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SArrayAbsent ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SArrayAbsent tildec dollarc mode evalp ctx)
   (list this))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLabel ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLabel tildec dollarc mode evalp ctx)
   (with-access::J2SLabel this (id body)
      (cons* this (format "{ ~a: " id)
         (if (isa? body J2SBlock)
             (with-access::J2SBlock body (nodes)
		(if (= (length nodes) 1) 
	            (append (j2s-js* body "" "" "" nodes tildec dollarc mode evalp ctx) '("}"))
	            (append (j2s-js* body "{" "}" "" nodes tildec dollarc mode evalp ctx) '("}"))))
             (append (j2s-js body tildec dollarc mode evalp ctx) '("}"))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SVarDecls ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SVarDecls tildec dollarc mode evalp ctx)
   (with-access::J2SVarDecls this (decls)
      (j2s-js* this "" "" "" decls tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLetBlock tildec dollarc mode evalp ctx)
   (with-access::J2SLetBlock this (decls nodes)
      (append
	 (j2s-js* this "{" "" "" decls tildec dollarc mode evalp ctx)
	 (j2s-js* this "" "}" "" nodes tildec dollarc mode evalp ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-binder ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-binder binder writable)
   (case binder
      ((let let-opt let-forin) (if writable "let " "const "))
      ((const const-opt) "const ")
      ((var) "var ")
      ((param) "")
      ((export) "export ")
      (else (error "js" "Illegal binder" binder))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDecl ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDecl tildec dollarc mode evalp ctx)
   (with-access::J2SDecl this (binder writable id loc scope)
      (cond
	 ((eq? binder 'class)
	  '())
	 ((j2s-param? this)
	  (list this (j2s-binder binder #t) (j2s-js-id this)))
	 ((eq? scope 'unbound)
	  '())
	 (else
	  (list this (j2s-binder binder #t) (j2s-js-id this) ";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclImport ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclImport tildec dollarc mode evalp ctx)
   '())

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclInit tildec dollarc mode evalp ctx)
   (with-access::J2SDeclInit this (val binder writable scope id loc)
      (cond
	 ((isa? val J2SImportNamespace)
	  '())
	 ((and (eq? scope 'global)
	       (> (context-get ctx :debug-client 0) 0)
	       (or (j2s-let-opt? this) (not (isa? val J2SUndefined))))
	  (if writable
	      (cons* this "var " (j2s-js-id this) "="
		 (append (j2s-js val tildec dollarc mode evalp ctx)
		    (if (j2s-param? this) '() '(";\n"))))
	      (cons* this "Object.defineProperty( window, \""
		 (j2s-js-id this) "\", { value: "
		 (append (j2s-js val tildec dollarc mode evalp ctx)
		    '(", writable: false} );\n")))))
	 ((or (j2s-let-opt? this) (not (isa? val J2SUndefined)))
	  (cons* this
	     (j2s-binder binder writable) (j2s-js-id this) "="
	     (append (j2s-js val tildec dollarc mode evalp ctx)
		(if (j2s-param? this) '() '(";\n")))))
	 (else
	  (list this (j2s-binder binder #t) (j2s-js-id this) ";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclClass ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclClass tildec dollarc mode evalp ctx)
   (with-access::J2SDeclClass this (val binder writable scope id)
      (cons* this "let " (j2s-js-id this) "="
	 (append (j2s-js val tildec dollarc mode evalp ctx)
	    (if (j2s-param? this) '() '(";\n"))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDProducer ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDProducer tildec dollarc mode evalp ctx)
   (with-access::J2SDProducer this (expr size)
      (j2s-js expr tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SReturn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SReturn tildec dollarc mode evalp ctx)
   (with-access::J2SReturn this (expr)
      (cons* this "return "
	 (append (j2s-js expr tildec dollarc mode evalp ctx) '(";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBindExit ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBindExit tildec dollarc mode evalp ctx)
   (with-access::J2SBindExit this (lbl stmt)
      (cond
	 (lbl
	  (error "BINDEXIT" "LBLB NOT SUPPORTED" (j2s->sexp this)))
	 ((isa? stmt J2SBlock)
	  (cons* this "((() => "
	     (append (j2s-js stmt tildec dollarc mode evalp ctx)
		'(")())"))))
	 (else
	  (cons* this "(() => { "
	     (append (j2s-js stmt tildec dollarc mode evalp ctx)
		'("})())")))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SReturnYield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SReturnYield tildec dollarc mode evalp ctx)
   
   (define (identity-kont? kont)
      (or (not (isa? kont J2SKont))
	  (with-access::J2SKont kont (body param)
	     (when (isa? body J2SStmtExpr)
		(with-access::J2SStmtExpr body (expr)
		   (when (isa? expr J2SRef)
		      (with-access::J2SRef expr (decl)
			 (eq? decl param))))))))
   
   (with-access::J2SReturnYield this (expr kont generator)
      (cond
	 ((identity-kont? kont)
	  (cond
	     (generator
	      (cons* this
		 "return $GEN.YieldS(" 
		 (append (j2s-js expr tildec dollarc mode evalp ctx)
		    '(",$GEN.kid);\n"))))
	     ((isa? kont J2SUndefined)
	      (cons* this
		 "return $GEN.Return("
		 (append (j2s-js expr tildec dollarc mode evalp ctx)
		    '(");\n"))))
	     (else
	      (cons* this
		 "return $GEN.Yield("
		 (append (j2s-js expr tildec dollarc mode evalp ctx)
		    '(",$GEN.kid);\n"))))))
	 (generator
	  (cons* this
	     "return $GEN.YieldS("
	     (append (j2s-js expr tildec dollarc mode evalp ctx)
		'(",")
		(if (isa? kont J2SUndefined) '("true") '("false"))
		'(",")
		(if (isa? kont J2SUndefined)
		    '("$GEN.done")
		    (j2s-js kont tildec dollarc mode evalp ctx))
		'(");\n"))))
	 ((isa? kont J2SUndefined)
	  (cons* this "return $GEN.Return("
	     (append (j2s-js expr tildec dollarc mode evalp ctx)
		'(");\n"))))
	 (else
	  (cons* this
	     "return $GEN.Yield("
	     (append (j2s-js expr tildec dollarc mode evalp ctx)
		'(",")
		(j2s-js kont tildec dollarc mode evalp ctx)
		'(");\n")))))))

;*---------------------------------------------------------------------*/
;*    j2s-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-error proc msg obj #!optional str)
   (with-access::J2SNode obj (loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location proc msg (or str (j2s->sexp obj)) fname loc))
	 (else
	  (error proc msg obj)))))

;*---------------------------------------------------------------------*/
;*    j2s-js-fun ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-js-fun this::J2SFun tildec dollarc mode evalp ctx funid
	   #!optional (funkwd "function "))
   
   (define (svc-import-body? body)
      (when (isa? body J2SBlock)
	 (with-access::J2SBlock body (nodes)
	    (cond
	       ((null? nodes)
		#t)
	       ((pair? (cdr nodes))
		#f)
	       ((isa? (car nodes) J2SNop)
		#t)
	       ((isa? (car nodes) J2SStmtExpr)
		(with-access::J2SStmtExpr (car nodes) (expr)
		   (isa? expr J2SPragma)))))))

   (define (svc id body)
      (cond
	 ((not id)
	  (j2s-error "js" "wrong service import (missing id)" this))
	 ((svc-import-body? body)
	  (format "HopService('~a', undefined)"
	     funid funid))
	 (else
	  (j2s-error "js" "wrong service import (body should be omitted)" this))))

   (define (async-body this::J2SFun)
      ;; The shape of async functions is:
      ;; (J2SBlock (J2SRetrun (J2SCall (J2SHopRef js-span) (J2SFun* ...))))
      (with-access::J2SFun this (body)
	 (if (isa? body J2SBlock)
	     (with-access::J2SBlock body (nodes)
		(if (and (pair? nodes) (null? (cdr nodes)))
		    (if (isa? (car nodes) J2SReturn)
			(with-access::J2SReturn (car nodes) (expr)
			   (if (isa? expr J2SCall)
			       (with-access::J2SCall expr (fun args)
				  (if (and (isa? fun J2SHopRef) (pair? args))
				      (with-access::J2SHopRef fun (id)
					 (if (and (eq? id 'js-spawn)
						  (isa? (car args) J2SFun))
					     (with-access::J2SFun (car args) (body)
						(values body #t))
					     (values body #f)))
				      (values body #f)))
			       (values body #f)))
			(values body #f))
		    (values body #f)))
	     (values body #f))))
   
   (with-access::J2SFun this (params idthis vararg generator)
      (multiple-value-bind (body async)
	 (async-body this)
	 (let ((ellipsis (if (eq? vararg 'rest) "... " "")))
	    (cond
	       ((isa? this J2SSvc)
		;; a service
		(list this (svc funid body)))
	       ((isa? this J2SArrow)
		;; an arrow function
		(cons* this (if async "(async " "(")
		   (append
		      (j2s-js-ellipsis this "(" ") => " "," ellipsis
			 params tildec dollarc mode evalp ctx)
		      (j2s-js body tildec dollarc mode evalp ctx)
		      '(")"))))
	       ((not generator)
		;; a regular function
		(cons* this (if async "async " "") (format "~a~a" funkwd funid)
		   (append
		      (j2s-js-ellipsis this "(" ")" "," ellipsis
			 params tildec dollarc mode evalp ctx)
		      (j2s-js body tildec dollarc mode evalp ctx)
		      '("\n"))))
	       ((not (eq? (context-get ctx :target) 'es5))
		;; a generator, for es6
		(cons* this (format "~a*~a" funkwd funid)
		   (append
		      (j2s-js-ellipsis this "(" ")" "," ellipsis
			 params tildec dollarc mode evalp ctx)
		      (j2s-js body tildec dollarc mode evalp ctx)
		      '("\n"))))
	       (else
		;; a generator, for es5
		(let ((predecls (collect-generator-predecls! body)))
		   (cons* this (format "~a~a" funkwd funid)
		      (append
			 (j2s-js-ellipsis this "(" ")" "," ellipsis
			    params tildec dollarc mode evalp ctx)
			 '("{")
			 (append-map! (lambda (decl)
					 (j2s-js decl tildec dollarc mode evalp ctx))
			    predecls)
			 '("var $GEN=new hop.Generator(function(_$v,_$e)")
			 (j2s-js body tildec dollarc mode evalp ctx)
			 '(");return $GEN;}\n"))))))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFun tildec dollarc mode evalp ctx)

   (define (fun-id this)
      (with-access::J2SFun this (decl)
	 (if (isa? decl J2SDecl)
	     (j2s-js-id decl)
	     "")))

   (j2s-js-fun this tildec dollarc mode evalp ctx (fun-id this)))

;*---------------------------------------------------------------------*/
;*    collect-generator-predecls! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function collects from BODY, the list of declaration        */
;*    that can be executed before the generator.                       */
;*---------------------------------------------------------------------*/
(define (collect-generator-predecls! body)
   
   (define (predecl? this)
      (with-access::J2SDeclInit this (val binder)
	 ;; see j2s-js ::J2SDeclInit
	 (not (or (j2s-let-opt? this) (not (isa? val J2SUndefined))))))

   (define (loop* node predecls)
      (with-access::J2SBlock node (nodes)
	 (let liip ((n nodes)
		    (predecls predecls))
	    (cond
	       ((null? n)
		(values #t predecls))
	       ((and (isa? (car n) J2SDeclInit) (predecl? (car n)))
		(let ((decl (car n))
		      (next (cdr n)))
		   (set! nodes next)
		   (liip nodes (cons decl predecls))))
	       ((isa? (car n) J2SBlock)
		(multiple-value-bind (complete predecls)
		   (loop (car n) predecls)
		   (if complete
		       (liip (cdr nodes) predecls)
		       (values #f predecls))))
	       (else
		(values #f predecls))))))

   (define (loop body predecls)
      (let liip ((body body)
		 (predecls predecls))
	 (cond
	    ((isa? body J2SLetBlock)
	     (with-access::J2SLetBlock body (decls nodes)
		(let liip ((d decls)
			   (predecls predecls))
		   (cond
		      ((null? d)
		       (loop* body predecls))
		      ((not (isa? (car d) J2SDeclInit))
		       (values #f predecls))
		      ((predecl? (car d))
		       (let ((decl (car d))
			     (next (cdr d)))
			  (set! decls next)
			  (liip next (cons decl predecls))))
		      (else
		       (values #f predecls))))))
	    ((isa? body J2SBlock)
	     (loop* body predecls))
	    (else
	     (values #f predecls)))))

   (multiple-value-bind (complete predecls)
      (loop body '())
      predecls))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SObjInit ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SObjInit tildec dollarc mode evalp ctx)
   (with-access::J2SObjInit this (inits)
      (j2s-js* this "{" "}" "," inits tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDataPropertyInit ...                                 */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDataPropertyInit tildec dollarc mode evalp ctx)
   (with-access::J2SDataPropertyInit this (name val)
      (cond
	 ((or (isa? name J2SString) (isa? name J2SNumber))
	  (cons this
	     (append (j2s-js name tildec dollarc mode evalp ctx)
		'(":")
		(j2s-js val j2s-js-attribute-tilde dollarc mode evalp ctx))))
	 ((and (isa? name J2SUndefined) (isa? val J2SSpread))
	  (with-access::J2SSpread val (expr)
	     (cons* this "..." (j2s-js expr tildec dollarc mode evalp ctx))))
	 (else
	  (cons this
	     (cons "["
		(append (j2s-js name tildec dollarc mode evalp ctx)
		   '("]:")
		   (j2s-js val j2s-js-attribute-tilde dollarc mode evalp ctx))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccessorPropertyInit ...                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccessorPropertyInit tildec dollarc mode evalp ctx)
   
   (define (j2s-js-propname name)
      (if (isa? name J2SString)
	  (with-access::J2SString name (val)
	     (list val))
	  (j2s-js name tildec dollarc mode evalp ctx)))
   
   (with-access::J2SAccessorPropertyInit this (name get set)
      (cons this
	 (cond
	    ((and (isa? get J2SFun) (isa? set J2SFun))
	     (append
		(with-access::J2SFun get (id body)
		   (append '("get ")
		      (j2s-js-propname name)
		      '("()")
		      (j2s-js body tildec dollarc mode evalp ctx)))
		'(", ")
		(with-access::J2SFun set (id body params)
		   (append '("set ")
		      (j2s-js-propname name)
		      (j2s-js* this "(" ")" "," params tildec dollarc mode evalp ctx)
		      (j2s-js body tildec dollarc mode evalp ctx)))))
	    ((isa? set J2SFun)
	     (with-access::J2SFun set (id body params)
		(append '("set ")
		   (j2s-js-propname name)
		   (j2s-js* this "(" ")" "," params tildec dollarc mode evalp ctx)
		   (j2s-js body tildec dollarc mode evalp ctx))))
	    (else
	     (with-access::J2SFun get (id body)
		(append '("get ")
		   (j2s-js-propname name)
		   '("()")
		   (j2s-js body tildec dollarc mode evalp ctx))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SWhile tildec dollarc mode evalp ctx)
   (with-access::J2SWhile this (test body)
      (cons* this "while ("
	 (append (j2s-js test tildec dollarc mode evalp ctx)
	    '(") ")
	    (if (isa? body J2SNop)
		'(";\n")
		(j2s-js body tildec dollarc mode evalp ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDo tildec dollarc mode evalp ctx)
   (with-access::J2SWhile this (test body)
      (cons* this "do "
	 (append
	    (if (isa? body J2SNop)
		'("{}")
		(j2s-js body tildec dollarc mode evalp ctx))
	    '("while (") (j2s-js test tildec dollarc mode evalp ctx)
	    '(")")))))

;*---------------------------------------------------------------------*/
;*    join ...                                                         */
;*---------------------------------------------------------------------*/
(define (join sep el)
   (if (null? el)
       '()
       (let loop ((el el))
	  (if (null? (cdr el))
	      (car el)
	      (append (car el) (list sep) (loop (cdr el)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SFor tildec dollarc mode evalp ctx)
   
   (define (var-decl decl::J2SDecl)
      (cons (j2s-js-id decl)
	 (if (isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val)
		(cons "=" (j2s-js val tildec dollarc mode evalp ctx)))
	     '())))
   
   (define (stmt-expr stmt::J2SStmtExpr)
      (with-access::J2SStmtExpr stmt (expr)
	 (j2s-js expr tildec dollarc mode evalp ctx)))
   
   (with-access::J2SFor this (init test incr body)
      (cons* this "for ("
	 (append
	    (cond
	       ((isa? init J2SVarDecls)
		(with-access::J2SVarDecls init (decls)
		   (cons "var " (join "," (map var-decl decls)))))
	       ((isa? init J2SSeq)
		(with-access::J2SSeq init (nodes)
		   (if (every (lambda (n) (isa? n J2SStmtExpr)) nodes)
		       (join "," (map stmt-expr nodes))
		       (j2s-js init tildec dollarc mode evalp ctx))))
	       (else
		(j2s-js init tildec dollarc mode evalp ctx)))
	    '(";\n")
	    (j2s-js test tildec dollarc mode evalp ctx)
	    '(";\n")
	    (j2s-js incr tildec dollarc mode evalp ctx)
	    '(") ")
	    (if (isa? body J2SNop)
		'(";\n")
		(j2s-js body tildec dollarc mode evalp ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SForIn ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SForIn tildec dollarc mode evalp ctx)
   (with-access::J2SForIn this (lhs obj body op)
      (cons* this "for("
         (append (if (isa? lhs J2SVarDecls)
		     (with-access::J2SVarDecls lhs (decls)
			(list "var " (j2s-js-id (car decls))))
		     (j2s-js lhs tildec dollarc mode evalp ctx))
	    (if (eq? op 'in) '(" in ") '(" of "))
	    (j2s-js obj tildec dollarc mode evalp ctx)
	    '(") ")
	    (if (isa? body J2SNop)
		'(";\n")
		(j2s-js body tildec dollarc mode evalp ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDeclFun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDeclFun tildec dollarc mode evalp ctx)
   (with-access::J2SDeclInit this (id val scope)
      (cond
	 ((isa? val J2SSvc)
	  (cons this
	     (append
		(cons* "var " id " = "
		   (cdr (j2s-js-fun val tildec dollarc mode evalp ctx id)))
		'(";\n"))))
	 (else
	  (j2s-js-fun val tildec dollarc mode evalp ctx id)))))
                                             
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SStmtExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SStmtExpr tildec dollarc mode evalp ctx)
   (with-access::J2SStmtExpr this (expr)
      (cons this
	 (append (j2s-js expr tildec dollarc mode evalp ctx) '(";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SIf ...                                               */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SIf tildec dollarc mode evalp ctx)
   (with-access::J2SIf this (test then else)
      (cons* this "if( "
	 (append (j2s-js test tildec dollarc mode evalp ctx)
	    (cons ") "
	       (append
		  (if (isa? then J2SNop)
		      '("{}\n") (j2s-js then tildec dollarc mode evalp ctx))
		  (if (isa? else J2SNop)
		      '("else {}\n")
		      (cons " else "
			 (j2s-js else tildec dollarc mode evalp ctx)))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSequence tildec dollarc mode evalp ctx)
   (with-access::J2SSequence this (exprs)
      (j2s-js* this "" "" "," exprs tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCast tildec dollarc mode evalp ctx)
   (with-access::J2SCast this (expr)
      (j2s-js expr tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRef tildec dollarc mode evalp ctx)
   (with-access::J2SRef this (decl)
      (list this (j2s-js-id decl))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSuper ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSuper tildec dollarc mode evalp ctx)
   (list this "super"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnresolvedRef ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnresolvedRef tildec dollarc mode evalp ctx)
   (with-access::J2SUnresolvedRef this (id)
      (list this (symbol->string! id))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SHopRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SHopRef tildec dollarc mode evalp ctx)
   (with-access::J2SHopRef this (id)
      (case id
	 ((%import-meta)
	  (if (context-get ctx :es6-module-client #f)
	      (list this "import.meta")
	      '()))
	 ((%this)
	  (list this "this"))
	 ((js-raise-utype-error)
	  (list this "raiseUtypeError"))
	 (else
	  (list this (symbol->string! id))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SThis ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SThis tildec dollarc mode evalp ctx)
   (list this "this"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCond ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCond tildec dollarc mode evalp ctx)
   (with-access::J2SCond this (test then else)
      (cons* this "(("
	 (append (j2s-js test tildec dollarc mode evalp ctx)
	    (cons ") ? ("
	       (append (j2s-js then tildec dollarc mode evalp ctx)
		  (cons ") : ("
		     (append (j2s-js else tildec dollarc mode evalp ctx)
			'("))")))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSwitch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSwitch tildec dollarc mode evalp ctx)
   (with-access::J2SSwitch this (key cases)
      (cons* this "switch("
	 (append
	    (j2s-js key tildec dollarc mode evalp ctx)
	    '(") {")
	    (append-map (lambda (n) (j2s-js n tildec dollarc mode evalp ctx))
	       cases)
	    '("}")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCase ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCase tildec dollarc mode evalp ctx)
   (with-access::J2SCase this (expr body)
      (cons* this "case "
	 (append
	    (j2s-js expr tildec dollarc mode evalp ctx)
	    '(": ")
	    (j2s-js body tildec dollarc mode evalp ctx)
	    '("\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDefault ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDefault tildec dollarc mode evalp ctx)
   (with-access::J2SDefault this (expr body)
      (cons* this "default: "
	 (append
	    (j2s-js body tildec dollarc mode evalp ctx)
	    '("\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBreak ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBreak tildec dollarc mode evalp ctx)
   (with-access::J2SBreak this (target id)
      (if id
	  (list this "break " id ";\n")
	  (list this "break;\n"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SContinue ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SContinue tildec dollarc mode evalp ctx)
   (with-access::J2SContinue this (target id)
      (if id
	  (list this "continue " id ";\n")
	  (list this "continue;\n"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLiteralValue ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLiteralValue tildec dollarc mode evalp ctx)

   (define (num2js val)
      (cond
	 ((bignum? val) (string-append (bignum->string val) "n"))
	 ((not (flonum? val)) val)
	 ((nanfl? val) "NaN")
	 ((=fl val +inf.0) "Infinity")
	 ((=fl val -inf.0) "-Infinity")
	 (else val)))
   
   (with-access::J2SLiteralValue this (val)
      (list this (format "~a" (num2js val)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SLiteralCnst ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SLiteralCnst tildec dollarc mode evalp ctx)
   (with-access::J2SLiteralCnst this (val)
      (j2s-js val tildec dollarc mode evalp ctx)))
   
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBool ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBool tildec dollarc mode evalp ctx)
   (with-access::J2SBool this (val)
      (list this (if val "true" "false"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNativeString ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNativeString tildec dollarc mode evalp ctx)
   (with-access::J2SNativeString this (val)
      (list this (string-append "\"" (string-for-read val) "\""))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SString ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SString tildec dollarc mode evalp ctx)
   (with-access::J2SString this (val)
      (if (ascii-string? val)
	  (list this (string-append "\"" (string-for-read val) "\""))
	  (let ((u (apply string-append
		      (map (lambda (uc)
			      (let ((n (ucs2->integer uc)))
				 (cond
				    ((<fx n 32)
				     (string-for-read (string (integer->char n))))
				    ((=fx n 34)
				     "\\\"")
				    ((=fx n 92)
				     "\\\\")
				    ((<=fx n 127)
				     (string (integer->char n)))
				    (else
				     (format "\\u~4,0x" n)))))
			 (ucs2-string->list (utf8-string->ucs2-string val))))))
	     (list this (string-append "\"" u "\""))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STemplate ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STemplate tildec dollarc mode evalp ctx)
   (with-access::J2STemplate this (exprs)
      (let loop ((nodes exprs))
	 (cons "("
	    (append (j2s-js (car nodes) tildec dollarc mode evalp ctx)
	       (cons ")"
		  (if (null? (cdr nodes))
		      '()
		      (cons "+" (loop (cdr nodes))))))))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SArray ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SArray tildec dollarc mode evalp ctx)
   (with-access::J2SArray this (exprs)
      ; hop parser intentionally ignores the last absent element of 
      ;  the array as the JavaScript interpreter does.
      (if (and (> (length exprs) 0) (isa? (car (last-pair exprs)) J2SArrayAbsent))
	  (j2s-js* this "[" ", ]" "," exprs tildec dollarc mode evalp ctx)
	  (j2s-js* this "[" "]" "," exprs tildec dollarc mode evalp ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SSpread ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SSpread tildec dollarc mode evalp ctx)
   (with-access::J2SSpread this (expr)
      (cons* this "..." (j2s-js expr tildec dollarc mode evalp ctx))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRegExp ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRegExp tildec dollarc mode evalp ctx)
   (with-access::J2SRegExp this (val flags)
      (list this (string-append "/" val "/" flags))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNull ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNull tildec dollarc mode evalp ctx)
   (list this "null"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUndefined ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUndefined tildec dollarc mode evalp ctx)
   (list this "undefined"))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCall tildec dollarc mode evalp ctx)
   (with-access::J2SCall this (fun args)
      (let ((f (j2s-js fun tildec dollarc mode evalp ctx)))
	 (cons this
	    (append (if (or (isa? fun J2SRef)
			    (isa? fun J2SAccess)
			    (isa? fun J2SUnresolvedRef))
			f
			(cons "(" (append f '(")"))))
	       (j2s-js* this "(" ")" "," args tildec dollarc mode evalp ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAccess ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAccess tildec dollarc mode evalp ctx)

   (define (id-string? field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (pregexp-match "^[a-zA-Z_$][a-zA-Z0-9_$]+$" val))))
      
   (with-access::J2SAccess this (obj field)
      (if (id-string? field)
	  (cons this
	     (append (j2s-js obj tildec dollarc mode evalp ctx)
		(list "." (with-access::J2SString field (val) val))))
	  (cons this
	     (append (j2s-js obj tildec dollarc mode evalp ctx)
		'("[")
		(j2s-js field tildec dollarc mode evalp ctx)
		'("]"))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SParen ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SParen tildec dollarc mode evalp ctx)
   (with-access::J2SParen this (expr)
      (cons* this "("
	 (append (j2s-js expr tildec dollarc mode evalp ctx) '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SUnary tildec dollarc mode evalp ctx)
   (with-access::J2SUnary this (op expr)
      (cond
	 ((memq op '(void typeof delete))
	  (cons* this (symbol->string op) " "
	     (j2s-js expr tildec dollarc mode evalp ctx)))
	 ((eq? op '?.)
	  (cons this 
	     (append (j2s-js expr tildec dollarc mode evalp ctx)
		'("?"))))
	 (else
	  (cons* this (symbol->string op) 
	     (j2s-js expr tildec dollarc mode evalp ctx))))))

;*---------------------------------------------------------------------*/
;*    j2s-op ...                                                       */
;*---------------------------------------------------------------------*/
(define (j2s-op op)
   (case op
      ((OR) "||")
      ((BIT_OR) "|")
      ((++) "+")
      ((--) "-")
      (else (symbol->string op))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SBinary ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SBinary tildec dollarc mode evalp ctx)
   (with-access::J2SBinary this (lhs rhs op)
      (case op
	 ((OR*)
	  (let ((tmp (gensym)))
	     (cons this
		(append
		   (list (format "((function (~a) { return (~a !== undefined) ? ~a :" tmp tmp tmp))
		   (j2s-js rhs tildec dollarc mode evalp ctx)
		   (list ";})(")
		   (j2s-js lhs tildec dollarc mode evalp ctx)
		   (list "))")))))
	 ((as)
	  ;; typescript as
	  (j2s-js lhs tildec dollarc mode evalp ctx))
	 (else
	  (cons this
	     (append (j2s-js lhs tildec dollarc mode evalp ctx)
		(list " " (j2s-op op) " ")
		(j2s-js rhs tildec dollarc mode evalp ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STilde ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STilde tildec dollarc mode evalp ctx)
   (let ((ndollarc (lambda (this::J2SDollar tildec dollarc mode evalp ctx)
		      (with-access::J2SDollar this (node)
			 (j2s-js node tildec dollarc mode evalp ctx)))))
      (cons this
	 (cond
	    ((procedure? tildec)
	     (tildec this tildec ndollarc mode evalp ctx))
	    ((eq? tildec #t)
	     (j2s-js-script-tilde this tildec dollarc mode evalp ctx))
	    (else
	     (with-access::J2STilde this (loc stmt)
		(cons* this "<script>"
		   (append (j2s-js stmt
			      j2s-js-script-tilde
			      ndollarc
			      mode evalp ctx)
		      '("</script>")))))))))

;*---------------------------------------------------------------------*/
;*    j2s-js-script-tilde ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-js-script-tilde this::J2STilde tildec dollarc mode evalp ctx)
   (with-access::J2STilde this (loc stmt)
      (let ((ndollarc (j2s-js-client-dollar dollarc)))
	 (cons* this "SCRIPT( undefined, '"
	    (string-for-read
	       (call-with-output-string
		  (lambda (port)
		     (for-each (lambda (n)
				  (unless (isa? n J2SNode)
				     (display n port)))
			(j2s-js stmt tildec ndollarc mode evalp ctx)))))
	    '("')")))))

;*---------------------------------------------------------------------*/
;*    j2s-js-attribute-tilde ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-js-attribute-tilde this::J2STilde tildec dollarc mode evalp ctx)
   (with-access::J2STilde this (loc stmt)
      (if (context-get ctx :hopjs-client)
	  (let ((stmt (j2s-js stmt tildec dollarc mode evalp ctx)))
	     (cons* this "TILDE(undefined, `" (append stmt (list "`)"))))
	  (let* ((temp (gensym))
		 (assign (j2s-stmt-assign stmt temp))
		 (id (gensym "$"))
		 (env (cons 0 '()))
		 (ndollarc (j2s-js-client-dollar-env dollarc id env))
		 (body (j2s-js assign tildec ndollarc mode evalp ctx)))
	     (cons* this (format "new hop_xml_tilde( function( event, ~a ) { var " id)
		(symbol->string! temp) "; "
		(append body
		   (list (format "\nreturn ~a}, [" temp))
		   (join "," (reverse! (cdr env)))
		   '("])")))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDollar ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDollar tildec dollarc mode evalp ctx)
   (let ((nctx (new-compiler-context ctx :site 'dollar)))
      (if (procedure? dollarc)
	  (cons this (dollarc this tildec dollarc mode evalp nctx))
	  (j2s-js-default-dollar this tildec dollarc mode evalp nctx))))

;*---------------------------------------------------------------------*/
;*    j2s-js-default-dollar ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-js-default-dollar this::J2SDollar tildec dollarc mode evalp ctx)
   (with-access::J2SDollar this (node)
      (list this
	 `(call-with-output-string
	     (lambda (op)
		(obj->javascript-attr
		   ,(j2s-scheme node mode evalp ctx) op %this))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-js-client-dollar ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-js-client-dollar odollarc)
   (lambda (this::J2SDollar tildec dollarc mode evalp ctx)
      (with-access::J2SDollar this (node)
	 (j2s-js node tildec odollarc mode evalp ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-js-client-dollar-env ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-js-client-dollar-env odollarc id env)
   (lambda (this::J2SDollar tildec dollarc mode evalp ctx)
      (with-access::J2SDollar this (node)
	 (let ((expr (j2s-js node tildec odollarc mode evalp ctx)))
	    (let ((count (car env)))
	       (set-car! env (+fx count 1))
	       (set-cdr! env (cons expr (cdr env)))
	       (list this (format "~a[~a]" id count)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNew ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNew tildec dollarc mode evalp ctx)
   (with-access::J2SNew this (clazz args)
      (cons* this "new "
	 (append (j2s-js clazz tildec dollarc mode evalp ctx)
	    (j2s-js* this "(" ")" "," args tildec dollarc mode evalp ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAssig tildec dollarc mode evalp ctx)
   (with-access::J2SAssig this (lhs rhs)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp ctx)
	    '("=")
	    (j2s-js rhs tildec dollarc mode evalp ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SInit tildec dollarc mode evalp ctx)
   (with-access::J2SInit this (lhs rhs)
      (cond
	 ((and (isa? rhs J2SClass) (isa? lhs J2SRef))
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (binder)
		(if (eq? binder 'class)
		    (j2s-js rhs tildec dollarc mode evalp ctx)
		    (call-next-method)))))
	 ((and (isa? lhs J2SRef)
	       (with-access::J2SRef lhs (decl)
		  (with-access::J2SDecl decl (id)
		     (eq? id 'default))))
	  (cons* this "export default "
	     (j2s-js rhs tildec dollarc mode evalp ctx)))
	 (else
	  (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SAssigOp ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SAssigOp tildec dollarc mode evalp ctx)
   (with-access::J2SAssigOp this (op lhs rhs)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp ctx)
	    (list (j2s-op op) "=")
	    (j2s-js rhs tildec dollarc mode evalp ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPrefix ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPrefix tildec dollarc mode evalp ctx)
   (with-access::J2SPrefix this (lhs op)
      (cons* this (symbol->string op)
	 (j2s-js lhs tildec dollarc mode evalp ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPostfix ...                                          */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPostfix tildec dollarc mode evalp ctx)
   (with-access::J2SPostfix this (lhs op)
      (cons this
	 (append (j2s-js lhs tildec dollarc mode evalp ctx)
	    (list (symbol->string op))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SThrow ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SThrow tildec dollarc mode evalp ctx)
   (with-access::J2SThrow this (expr)
      (cons* this "throw "
	 (append (j2s-js expr tildec dollarc mode evalp ctx) '(";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SNop ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SNop tildec dollarc mode evalp ctx)
   '())

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2STry ...                                              */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2STry tildec dollarc mode evalp ctx)
   (with-access::J2STry this (body catch finally)
      (cons* this "try "
	 (append (j2s-js body tildec dollarc mode evalp ctx)
	    (if (isa? catch J2SNop)
		'()
		(j2s-js catch tildec dollarc mode evalp ctx) )
	    (if (isa? finally J2SNop)
		'()
		(cons* this "finally "
		   (j2s-js finally tildec dollarc mode evalp ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SCatch ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SCatch tildec dollarc mode evalp ctx)
   (with-access::J2SCatch this (param body)
      (cons* this "catch("
	 (append (j2s-js param tildec dollarc mode evalp ctx)
	    '(")")
	    (j2s-js body tildec dollarc mode evalp ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SPragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SPragma tildec dollarc mode evalp ctx)
   (with-access::J2SPragma this (expr lang)
      (if (eq? lang 'javascript)
	  (list this expr)
	  (list this "undefined"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SYield ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SYield tildec dollarc mode evalp ctx)
   (with-access::J2SYield this (expr kont generator await)
      (cons* this (if generator "yield* " (if await "await " "yield "))
	 (j2s-js expr tildec dollarc mode evalp ctx))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SKont ...                                             */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SKont tildec dollarc mode evalp ctx)
   (with-access::J2SKont this (param exn body)
      (cons* this "function( "
	 (append (j2s-js param tildec dollarc mode evalp ctx)
	    '(", ")
	    (j2s-js exn tildec dollarc mode evalp ctx)
	    '(") {")
	    (j2s-js body tildec dollarc mode evalp ctx)
	    '("}")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SClass ...                                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SClass tildec dollarc mode evalp ctx)
   (with-access::J2SClass this (name super elements)
      (cons* this "class "
	 (append (if name (list name " ") '())
	    (if (isa? super J2SUndefined)
		'()
		(cons "extends " (j2s-js super tildec dollarc mode evalp ctx)))
	    (j2s-js* this "{\n" "}\n" ""
	       elements tildec dollarc mode evalp ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SClassElement ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SClassElement tildec dollarc mode evalp ctx)
   
   (define (j2s-js-name name tildec dollarc mode evalp ctx)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val private)
	     (if private
		 (let ((i (string-index val #\@)))
		    (if i
			(list (substring val 0 i))
			(list val)))
		 (list val))))
	 ((isa? name J2SLiteralCnst)
	  (with-access::J2SLiteralCnst name (val)
	     (j2s-js-name val tildec dollarc mode evalp ctx)))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     (list val)))
	 (else
	  (j2s-js name tildec dollarc mode evalp ctx))))
   
   (with-access::J2SClassElement this (static prop)
      (let ((m (cond
		  ((isa? prop J2SMethodPropertyInit)
		   (with-access::J2SMethodPropertyInit prop (name val)
		      (append (j2s-js-name name tildec dollarc mode evalp ctx)
			 (j2s-js-fun val tildec dollarc mode evalp ctx "" ""))))
		  ((isa? prop J2SDataPropertyInit)
		   (with-access::J2SDataPropertyInit prop (name val)
		      (append (j2s-js-name name tildec dollarc mode evalp ctx)
			 (j2s-js val tildec dollarc mode evalp ctx))))
		  ((isa? prop J2SAccessorPropertyInit)
		   (with-access::J2SAccessorPropertyInit prop (name get set)
		      (let ((nm (j2s-js-name name tildec dollarc mode evalp ctx)))
			 (if (isa? set J2SUndefined)
			     (cons "get "
				(append nm
				   (j2s-js-fun get tildec dollarc mode evalp ctx "" "")))
			     (cons "set "
				(append nm
				   (j2s-js-fun set tildec dollarc mode evalp ctx "" "")))))))
		  (else
		   (with-access::J2SPropertyInit prop (name)
		      (let ((nm (j2s-js-name name tildec dollarc mode evalp ctx)))
			 (append nm '(";"))))))))
	 (if static
	     (cons* this "static " m)
	     (cons* this m)))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SImport ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SImport tildec dollarc mode evalp ctx)
   
   (define (import-name->js this::J2SImportName)
      (with-access::J2SImportName this (id alias)
	 (if (eq? id alias)
	     (list (symbol->string! id))
	     (list (symbol->string! id) " as " (symbol->string! alias)))))

   (define (namespace? this::J2SImportName)
      (with-access::J2SImportName this (id)
	 (eq? id '*)))

   (define (import-path path)
      (cond
	 ((not (memq (context-get ctx :site) '(client tilde)))
	  path)
	 ((string? path)
	  path)
	 (else
	  path)))
   
   (define (import-module this path)
      (with-access::J2SImport this (names)
	 (cond
	    ((null? names)
	     (list this "import " (import-path path) ";\n"))
	    ((namespace? (car names))
	     (with-access::J2SImportName (car names) (id alias)
		(list this "import * as "
		   (symbol->string alias) " from " (import-path path) ";\n")))
;* 	    ((isa? (car names) J2SImportRedirect)                      */
;* 	     (cons* this "export {"                                    */
;* 		(append (append-map* ","                               */
;* 			   (lambda (a)                                 */
;* 			      (with-access::J2SImportRedirect a (id alias) */
;* 				 (if (eq? id alias)                    */
;* 				     id                                */
;* 				     (list id " as " alias))))         */
;* 			   names)                                      */
;* 		   `("} from " ,path ";\n"))))                           */
	    (else
	     (cons* this "import {"
		(append (append-map* "," import-name->js names)
		   `("} from " ,(import-path path) ";\n")))))))

   (if (context-get ctx :es6-module-client #f)
       (with-access::J2SImport this (names ipath dollarpath path %info)
	  (if (eq? %info 'generated)
	      '()
	      (let ((p (cond
			  ((not (isa? dollarpath J2SUndefined))
			   (cadr (j2s-js dollarpath tildec dollarc mode evalp ctx)))
			  ((isa? ipath J2SImportPath)
			   (with-access::J2SImportPath ipath (name)
			      (string-append "'" name "'")))
			  ((string? path)
			   (string-append "'" path "'"))
			  (else
			   (error "import"
			      (format "Illegal path (~a)" (typeof path))
			      (j2s->sexp this))))))
		 (set! %info 'generated)
		 (import-module this p))))
       '()))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SExport ...                                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SExport tildec dollarc mode evalp ctx)
   (with-access::J2SExport this (id alias)
      (if (eq? id alias)
	  (list this "export {" id "};\n")
	  (list this "export {" id " as " alias "};\n"))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRedirect ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRedirect tildec dollarc mode evalp ctx)
   (with-access::J2SRedirect this (id alias import)
      (with-access::J2SImport import (path)
	 (if (eq? id alias)
	     (list this "export {" id "} from \"" path "\";\n")
	     (list this "export {" id " as " alias "} from \"" path "\";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SRedirectNamespace ...                                */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SRedirectNamespace tildec dollarc mode evalp ctx)
   (with-access::J2SRedirect this (id alias import)
      (with-access::J2SImport import (path %info)
	 (set! %info 'generated)
	 (if (eq? '* alias)
	     (list this "export * from \"" path "\";\n")
	     (list this "export * as " alias " from \"" path "\";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SExportDefault ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SExportDefault tildec dollarc mode evalp ctx)
   (with-access::J2SExportDefault this (id alias expr)
      (cons* this
	 (if (eq? id alias)
	     "export default "
	     (format "export default as ~a " alias))
	 (append (j2s-js expr tildec dollarc mode evalp ctx) '(";\n")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SImportDynamic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SImportDynamic tildec dollarc mode evalp ctx)
   (with-access::J2SImportDynamic this (path)
      (cons* this "import("
	 (append (j2s-js path tildec dollarc mode evalp ctx)
	    '(")")))))

;*---------------------------------------------------------------------*/
;*    j2s-js ::J2SDConsumer ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::J2SDConsumer tildec dollarc mode evalp ctx)
   (with-access::J2SDConsumer this (expr)
      (j2s-js expr tildec dollarc mode evalp ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-import ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-import this::J2SProgram tildec dollarc mode evalp ctx)
   (with-access::J2SProgram this (imports)
      (append-map (lambda (ip)
		     (with-access::J2SImportPath ip (import)
			(j2s-js import tildec dollarc mode evalp ctx)))
	 imports)))

;*---------------------------------------------------------------------*/
;*    j2s-js ::TsInterface ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-js this::TsInterface tildec dollarc mode evalp ctx)
   (list this))
