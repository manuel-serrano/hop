;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/xml_types.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 20 09:22:36 2010                          */
;*    Last change :  Thu May 10 07:05:09 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The definition of XML classes                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_xml-types

   (export (class xml-backend
	      (id::symbol read-only)
	      (mime-type::bstring read-only)
	      (doctype::bstring read-only)
	      (header-format::bstring read-only)
	      (html-attributes::pair-nil read-only (default '()))
	      (no-end-tags-elements::pair-nil read-only (default '()))
	      (cdata-start (default #f))
	      (cdata-stop (default #f))
	      (css-start (default #f))
	      (css-stop (default #f))
	      (meta-delimiter::bstring read-only)
	      (abbrev-emptyp::bool (default #f))
	      (security::obj read-only (default #f))
	      (empty-end-tag::bool (default #t)))

	    (class security-manager
	       (name::bstring read-only (default "*"))
	       (xml-sanitize::procedure read-only (default (lambda (xml) xml)))
	       (string-sanitize::procedure read-only (default (lambda (s) s)))
	       (attribute-sanitize::procedure read-only (default (lambda (a i) a)))
	       (inline-sanitize::procedure read-only (default (lambda (n) n)))
	       (script-sanitize::procedure read-only (default (lambda (n) n)))
	       (runtime::pair-nil read-only (default '())))

	    (class xml
	       (%xml-constructor))

	    (class xml-verbatim::xml
	       (body::bstring read-only))
	    
	    (class css::xml)
	    
	    (class xml-if::xml
	       (test::procedure read-only)
	       (then read-only)
	       (otherwise read-only))
	    
	    (class xml-delay::xml
	       (id read-only (default #unspecified))
	       (thunk::procedure read-only)
	       (value::obj (default #f)))

	    (class xml-markup::xml
	       (tag::symbol read-only)
	       (attributes::pair-nil (default '()))
	       body::pair-nil)

	    (class xml-html::xml-markup)

	    (class xml-document::xml-markup
	       (id read-only)
	       (%idtable read-only (default (make-hashtable))))

	    (class xml-element::xml-markup
	       (id read-only (default #unspecified))
	       (parent (default #f)))

	    (class xml-empty-element::xml-element)

	    (class xml-cdata::xml-element)
	    
	    (class xml-style::xml-cdata)
	    
	    (class xml-tilde::xml
	       (body read-only)
	       (parent (default #f))
	       (src read-only (default #f))
	       (loc read-only (default #f))
	       (%js-expression (default #f))
	       (%js-statement (default #f))
	       (%js-return (default #f))
	       (%js-attribute (default #f))
	       (env (default #f))
	       (menv (default #f)))

	    (class xml-meta::xml-markup
	       (content::obj (default #f)))

	    (class xml-svg::xml-element)

	    (class xml-lazy-attribute
	       (proc::procedure read-only))

	    (generic %xml-constructor ::xml)))

;*---------------------------------------------------------------------*/
;*    object-print ::xml-element ...                                   */
;*    -------------------------------------------------------------    */
;*    Because of their parent slot xml-element are cyclic and cannot   */
;*    thus be display as is.                                           */
;*---------------------------------------------------------------------*/
(define-method (object-print o::xml-element p print-slot)
   (with-access::xml-element o (tag attributes body id)
      (display "#|xml-element tag=" p)
      (print-slot tag p)
      (display " id=" p)
      (print-slot id p)
      (display " parent=..." p)
      (display " attributes=" p)
      (print-slot attributes p)
      (display " body=" p)
      (print-slot body p)
      (display "|" p)))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (%xml-constructor o::xml)
   o)



