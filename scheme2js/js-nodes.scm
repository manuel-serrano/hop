;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module js-nodes
   (export
    (abstract-class Node)
    (final-class Program::Node
       body::Node)
    (final-class Block::Node
       stmts::pair-nil)
    (class Ref::Node
       id::symbol)
    (final-class This::Node)
    (final-class Decl::Ref)
    (final-class Var-Decl-List::Node ;; can be used as stmt and expression
       vars::pair-nil) ;; list of Nodes. might be Vassigs.
    (final-class NOP::Node)
    (final-class If::Node
       test::Node
       then::Node
       else::Node)
    (final-class For::Node
       init ;; might be #f
       test ;; might be #f
       incr ;; might be #f
       body::Node)
    (final-class For-In::Node
       lhs::Node
       obj::Node
       body::Node)
    (final-class While::Node
       test::Node
       body::Node)
    (final-class Do::Node
       body::Node
       test::Node)
    (final-class Continue::Node
       id)
    (final-class Break::Node
       id)
    (final-class Return::Node
       val)
    (final-class With::Node
       obj::Node
       body::Node)
    (final-class Switch::Node
       key::Node
       cases::pair-nil)
    (final-class Case::Node
       expr::Node
       body::Node)
    (final-class Default::Node
       body::Node)
    (final-class Throw::Node
       expr::Node)
    (final-class Try::Node
       body::Node
       catch
       finally)
    (final-class Catch::Node
       exception::Param
       body::Node)
    (final-class Labeled::Node
       id::symbol
       body::Node)
    ;; Fun-Binding is a stmt even though it inherits from Vassig
    (final-class Fun-Binding::Vassig)

    ;; Expressions ---------------------------------------------------
    (class Assig::Node
       lhs::Node
       rhs::Node)
    (class Assig-op::Assig
       op::symbol)
    (class Vassig::Assig)
    (final-class Accsig::Assig)
    (final-class Vassig-op::Assig-op)
    (final-class Accsig-op::Assig-op)
    (final-class Init::Vassig)
    (final-class Param::Ref)
    (final-class Named-Fun::Node
       name::Node
       fun::Fun)
    (final-class Fun::Node
       params::pair-nil ;; of Param
       body::Node)
    (final-class Sequence::Node
       exprs::pair-nil)
    (final-class Cond::Node
       test::Node
       then::Node
       else::Node)
    (final-class Binary::Node ;; could inherit from calls.
       lhs::Node
       op::symbol
       rhs::Node)
    (final-class Unary::Node ;; could inherit from calls
       op::symbol
       expr::Node)
    (final-class Postfix::Node ;; could inherit from calls
       expr::Node
       op::symbol)
    (final-class New::Node
       class::Node
       args::pair-nil)
    (final-class Access::Node
       obj::Node
       field::Node)
    (final-class Dot::Node
       obj::Node
       field::symbol)
    (final-class Call::Node
       fun::Node
       args::pair-nil)
    (final-class Pragma::Node
       str::bstring)
    (final-class Array::Node
       els::pair-nil
       len::bint)
    (final-class Array-Element::Node
       index::bint
       expr::Node)
    (final-class Obj-Init::Node
       inits::pair-nil)
    (final-class Property-Init::Node
       name
       val::Node)
    (abstract-class Literal::Node
       val)
    (final-class String::Literal) ;; val includes the delimiting chars (' or ")
    (final-class Number::Literal) ;; number is in string-form
    (final-class Undefined::Literal)
    (final-class Null::Literal)
    (final-class Bool::Literal)
    (final-class RegExp::Node pattern::bstring)))
