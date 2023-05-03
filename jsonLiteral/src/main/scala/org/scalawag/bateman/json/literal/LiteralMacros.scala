// bateman -- Copyright 2021-2023 -- Justin Patterson
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
  JErrors,
  JField,
  JLocation,
  JNull,
  JNumber,
  JObject,
  JString,
  JType
}

import scala.reflect.ClassTag
import scala.reflect.macros.whitebox.Context
import scala.collection.compat.immutable.LazyList
import scala.util.Random

class LiteralMacros(val c: Context) {
  import c.universe._

  // Turns a JAny into the code that's needed to construct it.
  private def toTree(jany: JAny, mapping: Map[String, TermName]): Tree = {
    val loc = jany.location match {
      case Some(JLocation(l, c, Some(s))) =>
        q"_root_.scala.Some(_root_.org.scalawag.bateman.json.JLocation($l, $c, _root_.scala.Some(${Literal(Constant(s))})))"
      case Some(JLocation(l, c, None)) =>
        q"_root_.scala.Some(_root_.org.scalawag.bateman.json.JLocation($l, $c))"
      case None => q"_root_.scala.None"
    }
    jany match {
      case s: JString if mapping.contains(s.value) =>
        q"${mapping(s.value)}"
      case n: JNullImpl =>
        q"_root_.org.scalawag.bateman.json.JNull.JNullImpl($loc)"
      case _: JNull => // Should not happen, but technically possible.
        q"_root_.org.scalawag.bateman.json.JNull"
      case b: JBoolean =>
        q"_root_.org.scalawag.bateman.json.JBoolean(${b.value}, $loc)"
      case n: JNumber =>
        q"""_root_.org.scalawag.bateman.json.JNumber(${Literal(Constant(n.value))}, $loc)"""
      case s: JString =>
        q"""_root_.org.scalawag.bateman.json.JString(${Literal(Constant(s.value))}, $loc)"""
      case a: JArray =>
        val items = a.items.map(toTree(_, mapping))
        q"_root_.org.scalawag.bateman.json.JArray(_root_.scala.List(..$items), $loc)"
      case o: JObject =>
        val fields = o.fieldList.map {
          case JField(k, v) =>
            q"_root_.org.scalawag.bateman.json.JField(${toTree(k, mapping)},${toTree(v, mapping)})"
        }
        q"_root_.org.scalawag.bateman.json.JObject(_root_.scala.List(..$fields), $loc)"
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

  private def random = new Random()

  private def randomString = List.fill(12)(random.nextPrintableChar()).mkString

  private case class InterpolatedExpression(term: TermName, decl: Tree, standIn: String)

  private def commonStringContext[A <: JAny: ClassTag: JType.Summoner](args: c.Expr[Any]*): Tree =
    c.prefix.tree match {
      case Apply(_, Apply(_, parts) :: Nil) =>
        // Get the StringContext parts from lexical context
        val stringParts: List[String] = parts.map {
          case Literal(Constant(part: String)) => part
        }

        def randomStrings =
          LazyList.continually(randomString).distinct.dropWhile(s => stringParts.exists(_.contains(s)))

        // Generate info for each arg (interpolated expression) in the string.
        //  - term name of the expression encoded as a JAny
        //  - declaration for the encoded JAny term
        //  - unique random string to be used as a stand-in during parsing

        val exprs = args.zip(randomStrings).zipWithIndex.map {
          case ((arg, standin), n) =>
            val term = TermName(s"arg$n")
            InterpolatedExpression(
              term,
              q"""val $term = _root_.org.scalawag.bateman.json.JAnyEncoder.encode($arg)""",
              standin
            )
        }

        // Generate a new JSON text which contains the stand-in strings.
        val text = insertStandIns(stringParts, exprs.map(_.standIn))

        // Parse the JSON text (with stand-ins)
        val jany = org.scalawag.bateman.json
          .parse(text)
          .fold(
            { e =>
              // If there were interpolations, don't include the location (because it's all wrong).
              if (args.isEmpty)
                c.abort(c.enclosingPosition, e.getMessage)
              else
                c.abort(c.enclosingPosition, s"syntax error: ${e.reason}")
            },
            { jany =>
              // If there were interpolations, remove the location information (because it's all wrong).
              if (args.isEmpty)
                jany
              else
                jany.value.stripLocation.asRootFocus
            }
          )
          .narrow[A]
          .fold(ee => c.abort(c.enclosingPosition, JErrors.formatErrorReport(ee)), _.value)

        // Walk the resulting JAny, turning it into direct JAny constructor calls and replacing the stand-ins
        // with their associated terms.
        val tree = toTree(jany, exprs.map(x => x.standIn -> x.term).toMap)

        // Output the term declarations and the JAny construction code
        q"""
          ..${exprs.map(_.decl)}
          $tree
        """
    }

  final def jarrayStringContext(args: c.Expr[Any]*): Tree = commonStringContext[JArray](args: _*)
  final def jobjectStringContext(args: c.Expr[Any]*): Tree = commonStringContext[JObject](args: _*)
}
