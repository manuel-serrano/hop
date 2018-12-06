;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-spread.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  6 16:35:12 2018                          */
;*    Last change :  Thu Dec  6 19:01:27 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Utility functions to deal with spread syntax.                    */
;*=====================================================================*/

(module __js2scheme_scheme-spread
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-cast)

   (export (spread->array-expr::J2SExpr loc exprs::pair)
	   (j2s-spread->expr-list exprs mode return conf)))
   
;*---------------------------------------------------------------------*/
;*    spread->array-expr::J2SExpr ...                                  */
;*---------------------------------------------------------------------*/
(define (spread->array-expr::J2SExpr loc exprs::pair)
   
   (define (exprs->arr-list exprs)
      (if (null? exprs)
	  '()
	  (multiple-value-bind (nospread rest)
	     (collect-no-spread exprs)
	     (if (null? nospread)
		 (with-access::J2SSpread (car rest) (expr)
		    (cons (j2s-cast-array expr) (exprs->arr-list (cdr rest))))
		 (with-access::J2SExpr (car nospread) (loc)
		    (cons (instantiate::J2SArray
			     (loc loc)
			     (len (length nospread))
			     (exprs nospread))
		       (exprs->arr-list rest)))))))
   
   (let* ((arrlst (exprs->arr-list exprs))
	  (arr (car arrlst))
	  (rest (cdr arrlst)))
      (if (null? rest)
	  arr
	  (instantiate::J2SCall
	     (loc loc)
	     (fun (instantiate::J2SAccess
		     (loc loc)
		     (obj arr)
		     (field (instantiate::J2SString
			       (loc loc)
			       (val "concat")))))
	     (thisarg (list arr))
	     (args rest)))))

;*---------------------------------------------------------------------*/
;*    j2s-spread->expr-list ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-spread->expr-list exprs mode return conf)
   (let loop ((exprs exprs))
      (if (null? exprs)
	  ''()
	  (multiple-value-bind (nospread rest)
	     (collect-no-spread exprs)
	     (if (null? nospread)
		 (with-access::J2SSpread (car rest) (expr)
		    (let ((arr (j2s-iterable->list expr mode return conf)))
		       (if (null? (cdr rest))
			   arr
			   `(append ,arr ,(loop (cdr rest))))))
		 `(cons* ,@(map (lambda (n) (j2s-scheme n mode return conf))
			      nospread)
		     ,(loop rest)))))))

;*---------------------------------------------------------------------*/
;*    collect-no-spread ...                                            */
;*---------------------------------------------------------------------*/
(define (collect-no-spread exprs)
   (let loop ((nospread '())
	      (exprs exprs))
      (cond
	 ((null? exprs) (values (reverse! nospread) exprs))
	 ((isa? (car exprs) J2SSpread) (values (reverse! nospread) exprs))
	 (else (loop (cons (car exprs) nospread) (cdr exprs))))))

;*---------------------------------------------------------------------*/
;*    j2s-iterable->list ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-iterable->list expr mode return conf)
   (with-access::J2SExpr expr (type)
      (case type
	 ((array)
	  `(jsarray->list
	      ,(j2s-scheme expr mode return conf)
	      %this))
	 ((string)
	  `(jsstring->list
	      ,(j2s-scheme expr mode return conf)
	      %this))
	 (else
	  `(js-iterable->list
	      ,(j2s-scheme expr mode return conf)
	      %this)))))
	  

;*---------------------------------------------------------------------*/
;*    j2s-cast-array ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-cast-array expr)
   (with-access::J2SExpr expr (type loc)
      (if (eq? type 'array)
	  expr
	  (J2SCast 'iterable expr))))
