;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/constants_expd.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 28 15:09:08 2019                          */
;*    Last change :  Wed Apr 10 13:44:24 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript constant expanders                                     */
;*    -------------------------------------------------------------    */
;*    See constant.sch                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    %define-cnst-table-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (%define-cnst-table-expander x e)
   (match-case x
      ((?- (and (? integer?) ?num))
       (e `(cond-expand
	      (bigloo-c
	       ;; see bigloo_vector.h for details
	      (static-pragma ,(format "static struct { struct bgl_vector vec; obj_t objs[ ~a ]; } __js_cnst_table = \n#if( !defined( TAG_VECTOR ) )\n { { MAKE_HEADER( VECTOR_TYPE, 0 ), ~a } }; \n#else\n { {~a } };\n#endif" num (+fx 1 num) (+fx 1 num)))))
	  e))
      (else
       (error "%define-cnst" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-cnst-table ...                                                */
;*---------------------------------------------------------------------*/
(define (js-cnst-table-expander x e)
   (match-case x
      ((?-)
       (e `(cond-expand
	      (bigloo-c
	       (pragma::obj "BVECTOR( &__js_cnst_table )"))
	      (else
	       #f))
	  e))
      (else
       (error "js-cnst-table" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    js-cnst-table-ref ...                                            */
;*---------------------------------------------------------------------*/
(define (js-cnst-table-ref-expander x e)
   (match-case x
      ((?- (and (? integer?) ?num))
       (e `(cond-expand
	      (bigloo-c
	       (free-pragma::obj ,(format "(__js_cnst_table.objs[ ~a ])" num)))
	      (else
	       (vector-ref-ur %cnst-table ,num)))
	  e))
      (else
       (error "js-cnst-table-ref" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    &with-cnst! ...                                                  */
;*---------------------------------------------------------------------*/
(define (&with-cnst!-expander x e)
   (match-case x
      ((&with-cnst! . ?body)
       (thread-parameter-set! '&cnsts '())
       (thread-parameter-set! '&x-cnsts x)
       (register-srfi! '&with-cnst!)
       (unwind-protect
	  (let* ((nbody (map (lambda (x) (e x e)) body))
		 (cnsts (map cadr (reverse! (thread-parameter '&cnsts)))))
	     `(let ((__js_cnst (&cnst-init #f ,(obj->string (apply vector cnsts)))))
		 ,@nbody))
	  (thread-parameter-set! '&cnsts #f)
	  (unregister-srfi! '&with-cnst!)))
      (else
       (error "&with-cnst!" "Illegal form" x))))
       
;*---------------------------------------------------------------------*/
;*    &begin!-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (&begin!-expander x e)
   (match-case x
      ((&begin!)
       (thread-parameter-set! '&cnsts '())
       (thread-parameter-set! '&x-cnsts x)
       x)
      (else
       (error "&begin!" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    &end!-expander ...                                               */
;*---------------------------------------------------------------------*/
(define (&end!-expander x e)
   (match-case x
      ((&end!)
       (let* ((xbeg (thread-parameter '&x-cnsts))
	      (newx (e `(&define-cnst
			   ,@(map cadr (reverse! (thread-parameter '&cnsts))))
		       e)))
	  (set-car! xbeg (car newx))
	  (set-cdr! xbeg (cdr newx))
	  (thread-parameter-set! '&cnsts #f)
	  #unspecified))
      (else
       (error "&end!" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    &-expander ...                                                   */
;*---------------------------------------------------------------------*/
(define (&-expander x e)
   (match-case x
      ((& (and ?str (? string?)))
       (if (not (thread-parameter '&cnsts))
	   (e `(js-name->jsstring ,str) e)
	   (let* ((&cnsts (thread-parameter '&cnsts))
		  (old (assoc str &cnsts)))
	      (if (pair? old)
		  (e `(&cnst-ref ,(cddr old)) e)
		  (let ((cnst (&name-expander x e)))
		     (if (vector? cnst)
			 (let ((len (length &cnsts)))
			    (thread-parameter-set! '&cnsts
			       (cons (cons str (cons cnst len)) &cnsts))
			    (e `(&cnst-ref ,len) e))
			 cnst))))))
      (else
       (error "&" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    &define-cnst-expander ...                                        */
;*---------------------------------------------------------------------*/
(define (&define-cnst-expander x e)
   (match-case x
      ((?- . ?cnsts)
       (let ((num (length cnsts)))
	  (e `(cond-expand
		((and bigloo-c bigloo-compile)
		 ;; see bigloo_vector.h for details
		 (begin
		     (static-pragma ,(format "static struct { struct bgl_vector vec; obj_t objs[ ~a ]; } __js_cnst = \n#if( !defined( TAG_VECTOR ) )\n { { MAKE_HEADER( VECTOR_TYPE, 0 ), ~a } }; \n#else\n { {~a } };\n#endif" num (+fx 1 num) (+fx 1 num)))
		     (&cnst-init (pragma::obj "BVECTOR( &__js_cnst )")
			,(obj->string (apply vector cnsts)))))
		(else
		 (&cnst-init #f ,(obj->string (apply vector cnsts)))))
	     e)))
      (else
       (error "&define-cnst" "bad syntax" x))))

;*---------------------------------------------------------------------*/
;*    &cnst-ref-expander ...                                           */
;*---------------------------------------------------------------------*/
(define (&cnst-ref-expander x e)
   (match-case x
      ((?- (and (? integer?) ?num))
       (e `(cond-expand
	      ((and bigloo-c bigloo-compile (not &with-cnst!))
	       (free-pragma::obj ,(format "(__js_cnst.objs[ ~a ])" num)))
	      (else
	       (vector-ref-ur __js_cnst ,num)))
	  e))
      (else
       (error "cnst-ref" "bad syntax" x))))

