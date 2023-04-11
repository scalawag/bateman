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

package org.scalawag.bateman.json

import cats.data.State
import cats.instances.list._
import cats.syntax.traverse._
import scala.annotation.tailrec

trait Renderer {
  def render(any: JAny): String
}

trait AppenderRenderer extends Renderer {
  protected def emptyAppender: Appender

  protected def append(x: JObject): AppenderState[Unit]
  protected def append(x: JArray): AppenderState[Unit]
  protected def append(x: JString): AppenderState[Unit]
  protected def append(x: JNumber): AppenderState[Unit]
  protected def append(x: JBoolean): AppenderState[Unit]
  protected def append(x: JNull): AppenderState[Unit]

  def render(any: JAny): String = {
    val prog =
      for {
        _ <- append(any)
        s <- makeString
      } yield s

    prog.runA(emptyAppender).value
  }

  protected def append(any: JAny): AppenderState[Unit] =
    any match {
      case x: JObject  => append(x)
      case x: JArray   => append(x)
      case x: JString  => append(x)
      case x: JNumber  => append(x)
      case x: JBoolean => append(x)
      case x: JNull    => append(x)
    }

  protected case class Appender(indentSpaces: String, output: List[String] = Nil, currentIndent: Int = 0)

  @tailrec
  private def appendAll(ss: Iterable[String], acc: List[String]): List[String] =
    ss.headOption match {
      case Some(s) => appendAll(ss.tail, s :: acc)
      case None    => acc
    }

  @tailrec
  private def indent(spaces: String, n: Int, acc: List[String]): List[String] =
    n match {
      case n if n > 0 => indent(spaces, n - 1, spaces :: acc)
      case _          => acc
    }

  protected type AppenderState[A] = State[Appender, A]

  protected def append(ss: String*): AppenderState[Unit] =
    State.modify(s => s.copy(output = appendAll(ss, s.output)))

  protected def newline: AppenderState[Unit] =
    State.modify(s => s.copy(output = indent(s.indentSpaces, s.currentIndent, "\n" :: s.output)))

  protected def tabIn: AppenderState[Unit] =
    State.modify(s => s.copy(currentIndent = s.currentIndent + 1))

  protected def tabOut: AppenderState[Unit] =
    State.modify(s => s.copy(currentIndent = s.currentIndent - 1))

  protected def makeString: State[Appender, String] =
    State.get.map(s => s.output.reverseIterator.mkString)

  protected def intersperse(
      delimiter: AppenderState[Unit]
  )(states: List[AppenderState[Unit]]): List[AppenderState[Unit]] =
    if (states.size > 1)
      states.foldRight(List.empty[AppenderState[Unit]])(delimiter :: _ :: _).tail
    else
      states
}

trait PrimitiveAppenders extends AppenderRenderer {
  protected val asciiOnly: Boolean = false

  private val zeroes = "0000"
  private def pad(s: String) =
    (4 - s.length) match {
      case n if n > 0 => zeroes.take(n) + s
      case _          => s
    }

  private def appendStringCharacter(c: Char): AppenderState[Unit] =
    c match {
      case '\"'                         => append("\\\"")
      case '\\'                         => append("\\\\")
      case '\b'                         => append("\\b")
      case '\f'                         => append("\\f")
      case '\r'                         => append("\\r")
      case '\n'                         => append("\\n")
      case '\t'                         => append("\\t")
      case c if c < 0x001f              => append("\\u", pad(c.toInt.toHexString))
      case c if asciiOnly && c > 0x00ff => append("\\u", pad(c.toInt.toHexString))
      case c                            => append(c.toString)
    }

  protected override def append(x: JString): AppenderState[Unit] =
    for {
      _ <- append("\"")
      _ <- x.value.toList.traverse(appendStringCharacter)
      _ <- append("\"")
    } yield ()

  protected override def append(x: JBoolean): AppenderState[Unit] = append(x.value.toString)
  protected override def append(x: JNumber): AppenderState[Unit] = append(x.value)
  protected override def append(x: JNull): AppenderState[Unit] = append("null")
}

object NoSpacesRenderer extends AppenderRenderer with PrimitiveAppenders {
  override protected def emptyAppender: Appender = Appender("")

  private val comma = append(",")

  private def append(field: JField): AppenderState[Unit] =
    for {
      _ <- append(field.name)
      _ <- append(":")
      _ <- append(field.value)
    } yield ()

  protected override def append(x: JObject): AppenderState[Unit] =
    for {
      _ <- append("{")
      _ <- intersperse(comma)(x.fieldList.map(append)).sequence
      _ <- append("}")
    } yield ()

  protected override def append(x: JArray): AppenderState[Unit] =
    for {
      _ <- append("[")
      _ <- intersperse(comma)(x.items.map(append)).sequence
      _ <- append("]")
    } yield ()
}

class PrettyRenderer(indent: Int = 2) extends AppenderRenderer with PrimitiveAppenders {
  override protected def emptyAppender: Appender = Appender(" " * indent)

  private val comma =
    for {
      _ <- append(",")
      _ <- newline
    } yield ()

  private def append(field: JField): AppenderState[Unit] =
    for {
      _ <- append(field.name)
      _ <- append(": ")
      _ <- append(field.value)
    } yield ()

  protected override def append(x: JObject): AppenderState[Unit] =
    for {
      _ <- append("{")
      _ <- tabIn
      _ <- newline
      _ <- intersperse(comma)(x.fieldList.map(append)).sequence
      _ <- tabOut
      _ <- newline
      _ <- append("}")
    } yield ()

  protected override def append(x: JArray): AppenderState[Unit] =
    for {
      _ <- append("[")
      _ <- tabIn
      _ <- newline
      _ <- intersperse(comma)(x.items.map(append)).sequence
      _ <- tabOut
      _ <- newline
      _ <- append("]")
    } yield ()
}

class PrettySpaces2 extends PrettyRenderer(2)
object PrettySpaces2 extends PrettySpaces2

class PrettySpaces4 extends PrettyRenderer(4)
object PrettySpaces4 extends PrettySpaces4

trait SortedFields extends AppenderRenderer {
  protected abstract override def append(x: JObject): AppenderState[Unit] = {
    val s = JObject(x.fieldList.sortBy(_.name.value), None)
    super.append(s)
  }
}

trait AsciiOnly extends PrimitiveAppenders {
  override protected val asciiOnly: Boolean = true
}
