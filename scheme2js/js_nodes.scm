;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
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
    (abstract-class JsNode)
    (final-class JsProgram::JsNode
       body::JsNode)
    (final-class JsBlock::JsNode
       stmts::pair-nil)
    (class JsRef::JsNode
       id::symbol)
    (final-class JsThis::JsNode)
    (final-class JsDecl::JsRef)
    (final-class JsVar-Decl-List::JsNode ;; can be used as stmt and expression
       vars::pair-nil) ;; list of JsNodes. might be Vassigs.
    (final-class JsNOP::JsNode)
    (final-class JsIf::JsNode
       test::JsNode
       then::JsNode
       else::JsNode)
    (final-class JsFor::JsNode
       init ;; might be #f
       test ;; might be #f
       incr ;; might be #f
       body::JsNode)
    (final-class JsFor-In::JsNode
       lhs::JsNode
       obj::JsNode
       body::JsNode)
    (final-class JsWhile::JsNode
       test::JsNode
       body::JsNode)
    (final-class JsDo::JsNode
       body::JsNode
       test::JsNode)
    (final-class JsContinue::JsNode
       id)
    (final-class JsBreak::JsNode
       id)
    (final-class JsReturn::JsNode
       val)
    (final-class JsWith::JsNode
       obj::JsNode
       body::JsNode)
    (final-class JsSwitch::JsNode
       key::JsNode
       cases::pair-nil)
    (final-class JsCase::JsNode
       expr::JsNode
       body::JsNode)
    (final-class JsDefault::JsNode
       body::JsNode)
    (final-class JsThrow::JsNode
       expr::JsNode)
    (final-class JsTry::JsNode
       body::JsNode
       catch
       finally)
    (final-class JsCatch::JsNode
       exception::JsParam
       body::JsNode)
    (final-class JsLabeled::JsNode
       id::symbol
       body::JsNode)
    ;; Fun-Binding is a stmt even though it inherits from Vassig
    (final-class JsFun-Binding::JsVassig)

    ;; Expressions ---------------------------------------------------
    (class JsAssig::JsNode
       lhs::JsNode
       rhs::JsNode)
    (class JsAssig-op::JsAssig
       op::symbol)
    (class JsVassig::JsAssig)
    (final-class JsAccsig::JsAssig)
    (final-class JsVassig-op::JsAssig-op)
    (final-class JsAccsig-op::JsAssig-op)
    (final-class JsInit::JsVassig)
    (final-class JsParam::JsRef)
    (final-class JsNamed-Fun::JsNode
       name::JsNode
       fun::JsFun)
    (final-class JsFun::JsNode
       params::pair-nil ;; of Param
       body::JsNode)
    (final-class JsSequence::JsNode
       exprs::pair-nil)
    (final-class JsCond::JsNode
       test::JsNode
       then::JsNode
       else::JsNode)
    (final-class JsBinary::JsNode ;; could inherit from calls.
       lhs::JsNode
       op::symbol
       rhs::JsNode)
    (final-class JsUnary::JsNode ;; could inherit from calls
       op::symbol
       expr::JsNode)
    (final-class JsPostfix::JsNode ;; could inherit from calls
       expr::JsNode
       op::symbol)
    (final-class JsNew::JsNode
       class::JsNode
       args::pair-nil)
    (final-class JsAccess::JsNode
       obj::JsNode
       field::JsNode)
    (final-class JsCall::JsNode
       fun::JsNode
       args::pair-nil)
    (final-class JsPragma::JsNode
       str::bstring
       args::pair-nil)
    (final-class JsArray::JsNode
       els::pair-nil
       len::bint)
    (final-class JsArray-Element::JsNode
       index::bint
       expr::JsNode)
    (final-class JsObj-Init::JsNode
       inits::pair-nil)
    (final-class JsProperty-Init::JsNode
       name
       val::JsNode)
    (abstract-class JsLiteral::JsNode
       val)
    (final-class JsString::JsLiteral) ;; val includes the delimiting chars (' or ")
    (final-class JsNumber::JsLiteral) ;; number is in string-form
    (final-class JsUndefined::JsLiteral)
    (final-class JsNull::JsLiteral)
    (final-class JsBool::JsLiteral)
    (final-class JsRegExp::JsNode pattern::bstring)))
