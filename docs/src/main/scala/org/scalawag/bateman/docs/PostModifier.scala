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

package org.scalawag.bateman.docs

import cats.data.NonEmptyChain
import org.scalawag.bateman.json._
import mdoc._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.parser.SyntaxError
import org.scalawag.sarong._

import scala.annotation.tailrec
import scala.io.Source

class PostModifier extends mdoc.PostModifier {
  val name = "bateman"

  def simplifyStaticType(t: String): String = {
    val delim = "[],".toSet

    def simpleName(name: String): String = name.split('.').last

    @tailrec
    def go(in: String, acc: String): String =
      in.indexWhere(delim) match {
        case -1 => acc + simpleName(in)
        case 0  => go(in.tail, acc + in.head)
        case n  => go(in.drop(n), acc + simpleName(in.take(n)))
      }

    go(t, "")
  }

  private val handleSyntaxError: Handler = {
    case error: SyntaxError =>
      List("syntax-error") -> sarong"""
        @@@ warning { title='Syntax Error' }
        ```
        $error
        ```
        @@@
      """
  }

  private val handleErrors: Handler = {
    // Scala is stupid. If I don't do this, other types sneak through this guard. For some reason, it can't even
    // match the on the outer NonEmptyChain. It says something about two different classloaders. Uncomment it if
    // you don't believe me and watch it break.
    case errors: NonEmptyChain[JError] if errors.getClass.getName.toLowerCase.contains("chain") =>
      List("errors") -> sarong"""
        @@@ warning { title='Errors' }
        ```
        ${JErrors.formatErrorReport(errors)}
        ```
        @@@
      """
  }

  private def handleLeft: Handler = {
    case Left(l) =>
      val (types, block) = handleAny(l)

      ("left" :: types) -> block
  }

  private def handleRight: Handler = {
    case Right(r) =>
      val (types, block) = handleAny(r)

      ("right" :: types) -> sarong"""
        @@@ note { title='Right' }
        > ${Source.fromString(block).getLines().toList.iterate}
        @@@
      """
  }

  private def handleList: Handler = {
    case Nil =>
      List("nil") -> sarong"""
        @@@ note { title='List' }
        (empty)
        @@@
      """

    case list: List[Any] =>
      val (eachTypes, eachBlocks) = list.map(handleAny).unzip
      val List(types) = eachTypes.distinct
      val blocks =
        eachBlocks.zipWithIndex.map {
          case (block, n) =>
            sarong"""
              > ${Source.fromString(block).getLines().toList.iterate}
            """
        }

      ("list" :: types) -> sarong"""
        @@@ note { title='List' }
        ${blocks.iterate}
        @@@
      """
  }

  private def handleOption: Handler = {
    case Some(value) =>
      val (types, block) = handleAny(value)

      ("some" :: types) -> sarong"""
        @@@ note { title=Some }
        > ${Source.fromString(block).getLines().toList.iterate}
        @@@
      """

    case None =>
      List("none") -> sarong"""
        @@@ note { title=None }
        (empty)
        @@@
      """
  }

  private val handleFocus: Handler = {
    case focus: JFocus[JAny] =>
      List("focus") -> sarong"""
        @@@ note { title='Focus' }
          * value: `${focus.value.render}` (`${focus.value.getClass.getTypeName}`)
          * location: `${focus.value.location.toList.iterate}`
          * pointer: `${focus.pointer}`
        @@@
      """
  }

  private val handleJAny: Handler = {
    case jany: JAny =>
      List("jany") -> sarong"""
        @@@ note { title='JAny' }
        ```
        ${jany.spaces2}
        ```
        @@@
      """
  }

  private val handleValue: Handler = {
    case value: Any =>
      List("value") -> sarong"""
        @@@ note { title='${value.getClass.getSimpleName}' }
        ```
        $value
        ```
        @@@
      """
  }

  private def handleAny: Handler =
    List(
      handleLeft,
      handleRight,
      handleList,
      handleOption,
      handleFocus,
      handleJAny,
      handleSyntaxError,
      handleErrors,
      handleValue,
    ).reduceLeft(_ orElse _)

  type Handler = PartialFunction[Any, (List[String], String)]

  def process(ctx: PostModifierContext): String = {
    val outputType = simplifyStaticType(ctx.variables.last.staticType)
    val tokens = ctx.info.split(':').toList

    val (types, block) = handleAny.lift(ctx.lastValue) match {
      case Some(x) => x
      case None =>
        ctx.reporter.error(
          ctx.variables.last.pos,
          sarong"""
            type mismatch:
              unhandled type: $outputType
              obtained: ${ctx.variables.last.runtimeValue}
          """
        )

        Nil -> ""
    }

    if (!types.startsWith(tokens))
      ctx.reporter.error(
        ctx.variables.last.pos,
        sarong"""
          type mismatch:
            expected: mdoc:bateman:${tokens.mkString(":")}
            obtained: mdoc:bateman:${types.mkString(":")}
        """
      )

    scala.io.Source.fromString(sarong"""
      ```scala
      ${ctx.originalCode.text}
      ```
      $block
    """).getLines().map(">" + _).mkString("\n", "\n", "\n")
  }

}
