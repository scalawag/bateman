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

package org.scalawag.bateman.json.generic.defaults

import scala.quoted.*

trait Defaults[T]:
  def defaults: Product

object Defaults:
  inline def summonDefaults[T]: Defaults[T] = ${ defaultsImpl[T] }

  def defaultsImpl[T: Type](using Quotes): Expr[Defaults[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    if !sym.isClassDef || !sym.companionModule.exists then
      '{ new Defaults[T] { def defaults = EmptyTuple } }
    else
      val companion = Ref(sym.companionModule)

      // Extract type arguments for generic case classes
      val typeArgs = tpe match
        case AppliedType(_, args) => args
        case _ => Nil

      // Check if the type is defined inside a trait/class (cake pattern).
      val isInnerClass = sym.owner.isClassDef && !sym.owner.flags.is(Flags.Module)

      val defaultMethods = (1 to sym.primaryConstructor.paramSymss.flatten.size).map { i =>
        sym.companionClass.methodMember(s"$$lessinit$$greater$$default$$$i")
      }

      val defaultExprs = defaultMethods.map { methodSymbols =>
        if methodSymbols.isEmpty then
          '{ None }
        else
          val methodSymbol = methodSymbols.head
          val selected = companion.select(methodSymbol)
          val applied = if typeArgs.nonEmpty && methodSymbol.paramSymss.headOption.exists(_.exists(_.isTypeParam)) then
            selected.appliedToTypes(typeArgs)
          else
            selected
          try
            val defaultValue = applied.asExpr
            '{ Some($defaultValue) }
          catch
            case _: Exception if isInnerClass =>
              report.errorAndAbort(
                s"Case class ${sym.name} is defined inside a trait or class, which prevents extraction of default " +
                s"parameter values. Move it to a top-level or object scope, or remove its default parameter values."
              )
            case _: Exception => '{ None }
      }

      val tuple = Expr.ofTupleFromSeq(defaultExprs.toSeq)
      '{ new Defaults[T] { def defaults = $tuple } }

  inline given derived[T]: Defaults[T] = summonDefaults[T]
