;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/wiki-syntax.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Tue May 11 15:42:43 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP wiki syntax tools                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_wiki-syntax
   
   (library web)
   
   (import  __hop_xml
	    __hop_param
	    __hop_read
	    __hop_charset
	    __hop_img
	    __hop_hop-mathml)
   
   (export  (class wiki-syntax
	       (section1::procedure (default list))
	       (section2::procedure (default list))
	       (section3::procedure (default list))
	       (section4::procedure (default list))
	       (section5::procedure (default list))
	       (h1::procedure (default <H1>))
	       (h2::procedure (default <H2>))
	       (h3::procedure (default <H3>))
	       (h4::procedure (default <H4>))
	       (h5::procedure (default <H5>))
	       (hr::procedure (default <HR>))
	       (p::procedure (default <P>))
	       (ol::procedure (default <OL>))
	       (ul::procedure (default <UL>))
	       (li::procedure (default <LI>))
	       (b::procedure (default <STRONG>))
	       (em::procedure (default <EM>))
	       (u::procedure (default <U>))
	       (del::procedure (default <DEL>))
	       (sub::procedure (default (lambda x
					   (<SUB>
					      (<SPAN>
						 :style "font-size: smaller"
						 x)))))
	       (sup::procedure (default (lambda x
					   (<SUP>
					      (<SPAN>
						 :style "font-size: smaller"
						 x)))))
	       (q::procedure (default <Q>))
	       (tt::procedure (default <TT>))
	       (code::procedure (default <CODE>))
	       (strike::procedure (default (lambda l
					      (<SPAN> :class "wiki-strike" l))))
	       (math::procedure (default (lambda (s)
					    (<MATH>
					       (<MATH:TEX> s)))))
	       (note::procedure (default (lambda l
					    (<SPAN> :class "wiki-note" l))))
	       (pre::procedure (default <PRE>))
	       (table::procedure (default <TABLE>))
	       (tr::procedure (default <TR>))
	       (th::procedure (default <TH>))
	       (td::procedure (default <TD>))
	       (href::procedure (default (lambda (href node)
					    (<A> :href href node))))
	       (keyword::procedure (default (lambda (x) x)))
	       (type::procedure (default (lambda (x) x)))
	       (plugins::procedure (default (lambda (id) #f)))
	       (verbatims::procedure (default (lambda (id) #f)))
	       (prehook::procedure (default (lambda () #f)))
	       (posthook::procedure (default (lambda (l) l))))))
