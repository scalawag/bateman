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

package org.scalawag.bateman.json.literal

import org.scalawag.bateman.json.JNull.JNullImpl
import org.scalawag.bateman.json.{
  JAny,
  JArray,
  JBoolean,
  JField,
  JLocation,
  JNull,
  JNumber,
  JObject,
  JString,
  JAnyEncoder
}

import scala.collection.compat.immutable.LazyList
import scala.util.Random
import scala.quoted.*

object LiteralMacros {

  transparent inline def stringContext(inline ctx: StringContext)(inline args: Any*): JAny =
    ${ stringContextImpl('ctx, 'args) }

  private def stringContextImpl[A <: JAny : Type](ctx: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[JAny] = {
    import quotes.reflect.*

    // Extract string parts from StringContext
    val stringParts: List[String] = ctx match {
      case '{ StringContext(${ Varargs(parts) }: _*) } =>
        parts.map(_.valueOrAbort).toList
      case _ => report.errorAndAbort("Unable to extract string parts from context")
    }

    // Extract argument expressions
    val argExprs: List[Expr[Any]] = args match {
      case Varargs(expressions) => expressions.toList
      case _ => Nil
    }

    def randomStrings =
      LazyList.continually(randomString).distinct.dropWhile(s => stringParts.exists(_.contains(s)))

    // Generate info for each arg (interpolated expression) in the string.
    val exprs = argExprs.zip(randomStrings).zipWithIndex.map {
      case ((arg, standin), n) => InterpolatedExpression(encode(arg), standin)
    }

    // Generate a new JSON text which contains the stand-in strings.
    val text = insertStandIns(stringParts, exprs.map(_.standIn))

    // Parse the JSON text (with stand-ins)
    val jany = org.scalawag.bateman.json
      .parse(text)
      .fold(
        { e =>
          // If there were interpolations, don't include the location (because it's all wrong).
          if (argExprs.isEmpty)
            report.errorAndAbort(e.getMessage)
          else
            report.errorAndAbort(s"syntax error: ${e.reason}")
        },
        { jany =>
          // If there were interpolations, remove the location information (because it's all wrong).
          val value = if (argExprs.isEmpty) jany else jany.value.stripLocation.asRootFocus
          value.value
        }
      )

    // Walk the resulting JAny, replacing the stand-ins with their associated values.


    // Walk the resulting JAny, turning it into direct JAny constructor calls and replacing the stand-ins
    toTree(jany, exprs.map(x => x.standIn -> x.expr).toMap)
  }

  // Turns a JAny into the code that's needed to construct it.
  private def toTree(jany: JAny, mapping: Map[String, Expr[JAny]])(using Quotes): Expr[JAny] = {
    given ToExpr[JLocation] with {
      override def apply(x: JLocation)(using Quotes): Expr[JLocation] =
        '{ JLocation(${ Expr(x.line) }, ${ Expr(x.column) }, ${Expr(x.source)}) }
    }

    val loc = Expr(jany.location)

    jany match {
      case s: JString if mapping.contains(s.value) => mapping(s.value)
      case s: JString => '{ JString(${ Expr(s.value) }, $loc) }
      case n: JNullImpl => '{ JNull.JNullImpl($loc) }
      case _: JNull => '{ JNull } // Should not happen, but technically possible.
      case b: JBoolean => '{ JBoolean(${ Expr(b.value) }, $loc) }
      case n: JNumber => '{ JNumber(${ Expr(n.value) }, $loc) }
      case a: JArray =>
        val items = a.items.map(toTree(_, mapping))
        val itemsExpr = Expr.ofSeq(items)
        '{ JArray($itemsExpr.toList, $loc) }
      case o: JObject =>
        val fields = o.fieldList.map {
          case JField(k, v) =>
            '{ JField(${ toTree(k, mapping).asExprOf[JString] }, ${ toTree(v, mapping) }) }
        }
        val fieldsExpr = Expr.ofSeq(fields)
        '{ JObject($fieldsExpr.toList, $loc) }
    }
  }

  private def encode(expr: Expr[Any])(using Quotes): Expr[JAny] = {
    import quotes.reflect.*
    expr match {
      case '{ $arg: t } =>
        arg.asTerm.tpe.widen.asType match {
          case '[t] =>
            Expr.summon[JAnyEncoder[t]] match {
              case Some(encoder) => '{ $encoder.encode($arg.asInstanceOf[t]) }
              case None => report.errorAndAbort(s"Missing implicit JAnyEncoder for type ${Type.show[t]}", arg)
            }
        }
    }
  }

  private def interleave[A, B](aa: Iterable[A], bb: Iterable[B]): Iterable[Either[A, B]] =
    aa.map(Left(_)).zip(bb.map(Right(_))).foldRight(List(Left(aa.last): Either[A, B])) {
      case ((a, b), acc) =>
        a :: b :: acc
    }

  private def insertStandIns(ss: Iterable[String], aa: Iterable[String]): String = {
    interleave(ss, aa.map(JString(_).render)).map(_.fold(identity, identity)).mkString
  }

  private val random = new Random()

  private def randomString = List.fill(12)(random.nextPrintableChar()).mkString

  private case class InterpolatedExpression(expr: Expr[JAny], standIn: String)
}