// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.json.generic

import cats.Eq
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.syntax.traverse._
import cats.syntax.validated._
import shapeless.tag.Tagged

import scala.annotation.tailrec
import scala.reflect.macros.whitebox.Context

/** This contains macros that validate the input classes prior to generating the codecs using shapeless.
  * This puts most of the logic into non-macro code (which was a goal). The macros generate the creation of
  * the factory that is used to create the actual codec. That's because if the macro call is made from library code,
  * the developer doesn't get an error message that reflects where the problem is in _their_ code.
  */

abstract class MacroBase(protected val c: Context) {
  import c.universe._

  protected type MacroError = String
  protected type MacroResult[+A] = ValidatedNec[MacroError, A]

  protected def abortOnNothingOrAny[B](a: Type): MacroResult[Type] =
    if (a =:= typeOf[Nothing] || a =:= typeOf[Any])
      "It looks like the type argument probably wasn't inferred properly.".invalidNec
    else
      a.validNec

  protected val debug = Option(System.getProperty("org.scalawag.bateman.debugMacros")).exists(_.nonEmpty)

  protected def getOrAbort[A: WeakTypeTag](
      fn: Type => MacroResult[Tree]
  ): Tree = {
    val a = weakTypeOf[A]

    def failure(errors: NonEmptyChain[MacroError]): Nothing = {
      val header = s"Type $a failed to meet the requirements for this type class derivation:"
      c.abort(c.enclosingPosition, errors.toNonEmptyList.toList.mkString(s"$header\n - ", "\n - ", "\n"))
    }

    val success: Tree => Tree =
      if (debug) { t => println(t); t }
      else
        identity

    abortOnNothingOrAny(a).andThen(fn).fold(failure, success)
  }

  protected case class Field(nameTerm: TermName, name: String, typ: Type, tag: Option[ClassSymbol]) {
    def hasTag(tag: ClassSymbol): Boolean = this.tag.exists(isAssignableTo(_, tag))
  }

  protected sealed trait TypeParameter
  protected case class SealedTrait(sym: ClassSymbol, subclasses: Set[ClassSymbol]) extends TypeParameter
  protected case class CaseClass(sym: ClassSymbol, fields: List[Field]) extends TypeParameter

  protected def isAssignableTo(candidate: TypeSymbol, target: TypeSymbol): Boolean =
    candidate.asClass.baseClasses.contains(target)

  protected val optionType = typeOf[Option[_]].typeSymbol.asClass

  protected val taggedType = typeOf[Tagged[_]].typeSymbol.asClass
  protected val batemanTag = typeOf[Tag].typeSymbol.asClass
  protected val sourceTag = typeOf[SourceTag].typeSymbol.asClass

  protected val supportedTags: Iterable[ClassSymbol] = Iterable(sourceTag)

  protected def getCaseClassSymbol(a: Type): MacroResult[ClassSymbol] = {
    val symbol = a.typeSymbol
    if (symbol.isClass && symbol.asClass.isCaseClass) {
      val classSymbol = symbol.asClass
      classSymbol.validNec
    } else
      "must be a case class".invalidNec
  }

  protected def asCaseClass(a: Type): MacroResult[CaseClass] =
    getCaseClassSymbol(a).andThen { x =>
      val fields =
        a.decls.toList
          .collect {
            case t: TermSymbol if t.isVal && t.isCaseAccessor => t
          }
          .traverse { t =>
            val fieldNameTerm = t.name.toTermName
            val fieldName = fieldNameTerm.toString.trim
            val fieldType = t.typeSignatureIn(a)
            val fieldTypeClass = fieldType.typeSymbol.asClass

            if (isAssignableTo(fieldTypeClass, taggedType)) {
              val List(bareType, tagType) = fieldType.typeArgs
              val tagClass = tagType.typeSymbol.asClass

              if (isAssignableTo(tagClass, batemanTag)) {
                supportedTags.find(_ == tagClass) match {
                  case Some(supportedTag) =>
                    Field(fieldNameTerm, fieldName, bareType, Some(supportedTag)).validNec
                  case None =>
                    s"field '$fieldName' is tagged with the unsupported tag ${tagClass.fullName}".invalidNec
                }
              } else
                Field(fieldNameTerm, fieldName, fieldType, None).validNec
            } else
              Field(fieldNameTerm, fieldName, fieldType, None).validNec
          }

      fields.map(CaseClass(x, _))
    }

  protected def asSealedTrait(a: Type): MacroResult[SealedTrait] = {
    val symbol = a.typeSymbol
    if (symbol.isClass) {
      val classSymbol = symbol.asClass
      if (classSymbol.isTrait) {
        if (classSymbol.isSealed) {
          val subclasses = classSymbol.knownDirectSubclasses.map(_.asClass)
          if (subclasses.isEmpty)
            s"has no subclasses".invalidNec
          else
            SealedTrait(classSymbol, subclasses).validNec
        } else s"is not sealed".invalidNec
      } else s"is not a sealed trait".invalidNec
    } else s"is not a sealed trait".invalidNec
  }

  protected def validateSourceField[From: WeakTypeTag](fields: List[Field]): MacroResult[Unit] =
    fields.find(_.hasTag(sourceTag)).map { f =>
      val srcType = weakTypeOf[From]
      val error =
        s"field '${f.name}' (with SourceTag) must have a type to which $srcType (or Option[$srcType]) can be assigned".invalidNec
      val fieldType = f.typ.typeSymbol.asClass

      if (isAssignableTo(srcType.typeSymbol.asClass, fieldType)) {
        ().validNec
      } else if (isAssignableTo(fieldType, optionType)) {
        val List(optionTypeArg) = f.typ.typeArgs
        if (isAssignableTo(srcType.typeSymbol.asClass, optionTypeArg.typeSymbol.asClass))
          ().validNec
        else
          error
      } else
        error
    } getOrElse ().validNec

  // I just left this in here, even though it's not being used at the moment, because I kept going back and forth
  // between thinking that it was valuable to capture the values of the pre-check implicits as they were resolved
  // or not. On the one hand, it seems like there should be a compile-time performance improvement (that I'm not
  // really seeing). On the other hand, it causes compile time failures that otherwise wouldn't happen (ambiguous
  // implicits etc.).
  //
  // I decide to commit to removing it, there's a lot of code that can go, regarding the precheck code (generating
  // names, grouping types, etc.).

  protected def instanceName(prefix: String, ff: List[Field]): TermName =
    TermName(s"${prefix}_${ff.map(_.name).mkString("_")}")

  protected def instanceName(prefix: String, cs: ClassSymbol): TermName =
    TermName(s"${prefix}_${cs.fullName.replace('.', '_')}")

  protected def maybeNamedImplicit(name: TermName, value: Tree): Tree =
    if (false)
      q"""implicit val $name = $value"""
    else
      value

  // This stuff is used to group by Type, which uses a special equivalence operator instead of just using equals.
  // This breaks the normal groupBy provided by TraversableLike!

  protected implicit val typeEq: Eq[Type] = _ =:= _
  protected implicit val classSymbolEq: Eq[ClassSymbol] = Eq.fromUniversalEquals

  protected implicit class TraversableOps[A](aa: Traversable[A]) {
    def groupByEq[K](f: A => K)(implicit K: Eq[K]): Map[K, List[A]] = {
      @tailrec
      def collapse(l: List[(K, List[A])], r: List[(K, List[A])]): List[(K, List[A])] =
        r match {
          case Nil => l
          case (rk, raa) :: rtail =>
            val lbefore = l.dropWhile(x => K.neqv(x._1, rk))
            val ltarget = l.drop(lbefore.length).headOption
            val lafter = l.drop(lbefore.length + 1)

            val newTarget = ltarget match {
              case Some((lk, laa)) => (lk, laa ::: raa)
              case None            => (rk, raa)
            }

            collapse(lbefore ::: newTarget :: lafter, rtail)
        }

      val pairs = aa.map(a => f(a) -> List(a)).toList
      collapse(pairs.take(1), pairs.drop(1)).toMap
    }
  }
}
