// bateman -- Copyright 2021-2026 -- Justin Patterson
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalawag.bateman.json.generic.encoding

import scala.quoted.*

object MacroImpl:
  def extractDefaultImpl[T: Type](idx: Expr[Int])(using Quotes): Expr[Option[Any]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    idx match
      case Expr(i: Int) =>
        if !sym.isClassDef || !sym.companionModule.exists then
          '{ None }
        else
          val companion = Ref(sym.companionModule)
          val defaultMethodName = s"$$lessinit$$greater$$default$$${i + 1}"
          val defaultMethodSymbols = sym.companionClass.methodMember(defaultMethodName)

          if defaultMethodSymbols.isEmpty then
            '{ None }
          else
            val methodSymbol = defaultMethodSymbols.head
            val defaultValue = companion.select(methodSymbol).asExprOf[Any]
            '{ Some($defaultValue) }
      case _ =>
        report.errorAndAbort("Index must be a constant integer")