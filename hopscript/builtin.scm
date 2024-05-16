;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/builtin.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan 26 08:54:32 2019                          */
;*    Last change :  Thu May 16 14:46:35 2024 (serrano)                */
;*    Copyright   :  2019-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop builtin JavaScript objects                                   */
;*                                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_builtin
   
   (library hop js2scheme)
   
   (include "types.sch" "stringliteral.sch" "property.sch")
   
   (import __hopscript_types
	   __hopscript_arithmetic
	   __hopscript_property
	   __hopscript_object
	   __hopscript_error
	   __hopscript_private
	   __hopscript_public
	   __hopscript_lib
	   __hopscript_worker
	   __hopscript_function
	   __hopscript_names)
   
   (export (js-init-hop-builtin! ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    js-bind-tag! ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (js-bind-tag! %this obj __proto__ tag . tagjs)
   `(begin
       (js-bind! ,%this ,obj (& ,(symbol->string! (if (pair? tagjs) (car tagjs) tag)))
          :value (js-make-function ,%this
                    (lambda (this attrs . nodes)
                       (if (js-object? attrs)
			   (if (null? nodes)
			       (apply ,(symbol-append '< tag '>)
				  :%context ,%this
				  (js-jsobject->keyword-plist attrs ,%this))
			       (apply ,(symbol-append '< tag '>)
				  :%context ,%this
				  (append
				     (js-jsobject->keyword-plist attrs ,%this)
				     nodes)))
                           (apply ,(symbol-append '< tag '>)
			      :%context ,%this
                              nodes)))
		    (js-function-arity 1 -1 'scheme)
		    (js-function-info :name ,(symbol->string tag) :len 2)
		    :__proto__ ,__proto__)
          :writable #t :configurable #f :enumerable #f :hidden-class #f)))

;*---------------------------------------------------------------------*/
;*    js-bind-tags! ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (js-bind-tags! %this obj __proto__ . tags)
   `(begin
       ,@(map (lambda (tag)
		 `(js-bind-tag! ,%this ,obj ,__proto__ ,tag))
	    tags)))
   
;*---------------------------------------------------------------------*/
;*    js-bind-svg-tags! ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (js-bind-svg-tags! %this obj __proto__ . tags)
   `(begin
       ,@(map (lambda (tag)
		 (let* ((s (symbol->string tag))
			(i (string-index s #\:)))
		    (if i
			`(begin
			    (js-bind-tag! ,%this ,obj ,__proto__ ,tag)
			    (js-bind-tag! ,%this ,obj ,__proto__ ,tag
			       ,(string->symbol (substring s (+fx i 1)))))
			`(js-bind-tag! ,%this  ,obj ,__proto__ ,tag))))
	    tags)))
   
;*---------------------------------------------------------------------*/
;*    js-init-hop-builtin! ...                                         */
;*---------------------------------------------------------------------*/
(define (js-init-hop-builtin! %this builtin::JsObject)
   (unless (vector? __js_strings) (set! __js_strings (&init!)))
   (with-access::JsGlobalObject %this (js-function-prototype)
      ;; html_base
      (js-bind-tags! %this builtin js-function-prototype
	 A ABBR ACRONYM ADDRESS APPLET AREA ARTICLE AUDIO B BASE
	 BASEFONT BDI BDO BIG BLOCKQUOTE BODY BR BUTTON
	 CANVAS CAPTION CENTER CITE CODE COL COLGROUP
	 DATALIST DD DEL DETAILS DFN DIR DIV DL DT EM EMBED FIELDSET
	 FIGURE FIGCAPTION FONT FOOTER FORM FRAME FRAMESET
	 H1 H2 H3 H4 H5 H6
	 HR HEADER HGROUP I IFRAME INPUT INS ISINDEX KBD LABEL LEGEND
	 LI MAIN MAP MARQUEE MENU MENUITEM META METER NAV NOFRAMES NOSCRIPT
	 OBJECT OL OPTGROUP OPTION P PARAM PRE PROGRESS
	 Q S SAMP SECTION SELECT SMALL SOURCE SPAN STRIKE
	 STRONG SUB SUMMARY SUP TABLE TBODY TD TEXTAREA TFOOT TH
	 THEAD TIME TITLE TR TRACK TT U UL VAR VIDEO REACT)
      
      ;; html_head
      (js-bind-tags! %this builtin js-function-prototype LINK STYLE)
      (js-bind-tags! %this builtin js-function-prototype IMG)
      
      ;; svg
      (js-bind-svg-tags! %this builtin js-function-prototype
	 SVG SVG:DEFS SVG:RECT SVG:CIRCLE SVG:ELLIPSE SVG:FILTER
	 SVG:FEGAUSSIANBLUR SVG:FECOLORMATRIX SVG:FOREIGNOBJECT SVG:G
	 SVG:LINE SVG:PATH SVG:POLYLINE SVG:POLYGON SVG:TEXT
	 SVG:TEXTPATH SVG:TREF SVG:TSPAN
	 SVG:RADIALGRADIENT SVG:LINEARGRADIENT)
      
      (js-bind-tag! %this builtin js-function-prototype SVG:IMG)
      
      ;; mathml
      (js-bind-tags! %this builtin js-function-prototype
	 MATH MATH:MSTYLE MATH:MI MATH:MN MATH:MO
	 MATH:MROW MATH:MUNDER MATH:MOVER MATH:MUNDEROVER
	 MATH:MSUP MATH:MSUB MATH:MSUBSUP MATH:MFRAC
	 MATH:MROOT MATH:MSQRT MATH:MTEXT MATH:MTABLE
	 MATH:MTR MATH:MTD MATH:MPADDED MATH:TEX)
      
      (js-bind! %this builtin (& "!--")
	 :value (js-make-function %this
		   (lambda (this data)
		      (instantiate::xml-comment
			 (data (js-tostring data %this))))
		   (js-function-arity 1 0)
		   (js-function-info :name "<!--" :len 1)
		   :__proto__ js-function-prototype)
	 :enumerable #f :writable #f :configurable #f :hidden-class #f)
      
      builtin))


;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)
