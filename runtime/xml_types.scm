;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/runtime/xml_types.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 20 09:22:36 2010                          */
;*    Last change :  Mon May 30 14:26:15 2016 (serrano)                */
;*    Copyright   :  2010-16 Manuel Serrano                            */
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
	      (header-format::obj read-only (default #f))
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
	       (xml-sanitize::procedure read-only)
	       (string-sanitize::procedure read-only)
	       (attribute-sanitize::procedure read-only)
	       (inline-sanitize::procedure read-only)
	       (script-sanitize::procedure read-only)
	       (runtime::pair-nil read-only (default '())))

	    (class xml
	       (%xml-constructor))

	    (class xml-verbatim::xml
	       data::bstring
	       (parent (default #f)))

	    (class xml-comment::xml
	       (data::bstring read-only)
	       (parent (default #f)))
	    
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

	    (class xml-document::xml-html
	       id
	       (%idtable read-only (default (make-hashtable))))

	    (class xml-element::xml-markup
	       (id (default #unspecified))
	       (parent (default #f)))

	    (class xml-empty-element::xml-element)

	    (class xml-cdata::xml-element)
	    
	    (class xml-react::xml-element)
	    
	    (class xml-style::xml-cdata)
	    
	    (class xml-tilde::xml
	       (lang read-only (default 'hop))
	       (body read-only)
	       (parent (default #f))
	       (src read-only (default #f))
	       (loc::obj read-only (default #f))
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
      (display "#|" p)
      (display (class-name (object-class o)) p)
      (display " tag=" p)
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

