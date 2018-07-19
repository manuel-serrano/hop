;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/make_lib.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  9 14:00:32 2013                          */
;*    Last change :  Tue Jul 17 09:38:42 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    THe module used to build the js2scheme heap file.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_makelib

   (import __js2scheme_compile
	   __js2scheme_parser
	   __js2scheme_html
	   __js2scheme_ast
	   __js2scheme_symbol
	   __js2scheme_header
	   __js2scheme_return
	   __js2scheme_ronly
	   __js2scheme_dump
	   __js2scheme_scheme
	   __js2scheme_js
	   __js2scheme_sourcemap
	   __js2scheme_stage)
   
   (eval   (class J2SStage)
           (class J2SStageProc)
           (class J2SStageUrl)
	   (class J2SNode)
	   (class J2SStmt)
	   (class J2SBlock)
           (class J2SProgram)
	   (class J2SStmtExpr)
	   (class J2SIf)
	   (class J2SVarDecls)
	   (class J2SIdStmt)
	   (class J2SSwitch)
	   (class J2SLoop)
	   (class J2SFor)
	   (class J2SForIn)
	   (class J2SWhile)
	   (class J2SDo)
	   (class J2SLabel)
	   (class J2SBreak)
	   (class J2SContinue)
	   (class J2SNop)
	   (class J2SCase)
	   (class J2SDefault)
	   (class J2SReturn)
	   (class J2SWith)
	   (class J2SThrow)
	   (class J2SFun)
	   (class J2SSvc)
	   (class J2SCatch)
	   (class J2STry)
	   (class J2SExpr)
	   (class J2SPragma)
	   (class J2SSequence)
	   (class J2SUnresolvedRef)
	   (class J2SRef)
	   (class J2SWithRef)
	   (class J2SHopRef)
	   (class J2SThis)
	   (class J2SCond)
	   (class J2SDecl)
	   (class J2SDeclInit)
	   (class J2SDeclFun)
	   (class J2SDeclSvc)
	   (class J2SDeclExtern)
	   (class J2SLiteral)
	   (class J2SArrayAbsent)
	   (class J2SNull)
	   (class J2SUndefined)
	   (class J2SLiteralValue)
	   (class J2SString)
	   (class J2SBool)
	   (class J2SNumber)
	   (class J2SOctalNumber)
	   (class J2SRegExp)
	   (class J2SArray)
	   (class J2SParen)
	   (class J2SUnary)
	   (class J2SBinary)
	   (class J2SAssig)
	   (class J2SPrefix)
	   (class J2SPostfix)
	   (class J2SAccess)
	   (class J2SCall)
	   (class J2STilde)
	   (class J2SDollar)
	   (class J2SNew)
	   (class J2SInit)
	   (class J2SAssigOp)
	   (class J2SVAssig)
	   (class J2SCAssig)
	   (class J2SFunBinding)
	   (class J2SObjInit)
	   (class J2SPropertyInit)
	   (class J2SDataPropertyInit)
	   (class J2SAccessorPropertyInit)
	   (class J2SKont)
	   (class J2SOPTInitSeq)
	   (class J2SDProducer)
	   (class J2SDConsumer)

           (export-all)))
