;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/constants_expd.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 28 15:09:08 2019                          */
;*    Last change :  Mon Apr 15 09:51:45 2019 (serrano)                */
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
;*    &with! ...                                                       */
;*---------------------------------------------------------------------*/
(define (&with!-expander x e)
   (match-case x
      ((&with! . ?body)
       (let ((o (thread-parameter '&cnsts)))
	  (thread-parameter-set! '&cnsts '())
	  (thread-parameter-set! '&x-cnsts x)
	  (unwind-protect
	     (let* ((nbody (map (lambda (x) (e x e)) body))
		    (cnsts (map cadr (reverse! (thread-parameter '&cnsts)))))
		(e `(let ((__js_strings (&cnst-init
					  ,(obj->string (apply vector cnsts)) %this)))
		       ,@nbody)
		   e))
	     (thread-parameter-set! '&cnsts o))))
      (else
       (error "&with!" "Illegal form" x))))
       
;*---------------------------------------------------------------------*/
;*    &begin!-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (&begin!-expander x e)
   (match-case x
      ((&begin!)
       (thread-parameter-set! '&cnsts '())
       ''#())
      (else
       (error "&begin!" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    &init!-expander ...                                              */
;*---------------------------------------------------------------------*/
(define (&init!-expander x e)
   (match-case x
      ((&init!)
       (thread-parameter-set! '&x-cnsts x)
       ;; need to bind %this local to expand possible object access for %this
       ;; (as the (&end!) that will actually expand the (&init) form will
       ;; expand it in a different context that those of the (&init!)
       `(let ((%this ,(e '%this e))) ,x))
      (else
       (error "&init!" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    &end!-expander ...                                               */
;*---------------------------------------------------------------------*/
(define (&end!-expander x e)
   (match-case x
      ((&end!)
       (let* ((xbeg (thread-parameter '&x-cnsts))
	      (cnsts (map cadr (reverse! (thread-parameter '&cnsts))))
	      (newx `(&cnst-init ,(obj->string (apply vector cnsts)) %this)))
	  (if xbeg
	      (begin
		 (set-car! xbeg (car newx))
		 (set-cdr! xbeg (cdr newx))
		 (thread-parameter-set! '&cnsts #f)
		 #unspecified) 
	      (error "&end!" "Cannot expand find &init! format" x))))
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
		  (e `(vector-ref __js_strings ,(cddr old)) e)
		  (let ((cnst (&name-expander x e)))
		     (if (vector? cnst)
			 (let ((len (length &cnsts)))
			    (thread-parameter-set! '&cnsts
			       (cons (cons str (cons cnst len)) &cnsts))
			    (e `(vector-ref __js_strings ,len) e))
			 cnst))))))
      ((& strings)
       (&name-expander x e))
      (else
       (error "&" "bad form" x))))
