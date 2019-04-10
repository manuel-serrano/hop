;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/constant_expd.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 28 15:09:08 2019                          */
;*    Last change :  Wed Apr  3 07:36:29 2019 (serrano)                */
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
	       (free-pragma::obj "(__js_cnst_table.objs[ $1 ])" ,num))
	      (else
	       (vector-ref-ur %cnst-table ,num)))
	  e))
      (else
       (error "js-cnst-table-ref" "bad syntax" x))))


