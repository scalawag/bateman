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

import scala.language.{higherKinds, implicitConversions}
import cats.syntax.either._
import cats.syntax.parallel._
import cats.instances.option._
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json.focus.{JFoci, JFocus, Single}
import org.scalawag.bateman.json.focus.weak._

import scala.reflect.ClassTag

package object lens {

  /** A lens that transforms a focus into zero or more other foci.
    *
    * @tparam F the cardinality of the output
    * @tparam A the input focus type
    * @tparam B the output focus type
    */

  sealed trait JLens[F[+_], -A <: JAny, +B <: JAny] extends Function1[JFocus[A], JResult[F[JFocus[B]]]]

  //===================================================================================================================

  /** A lens that can be used to create a field as well as read from it. It always has a singular
    * output cardinality type and may only be composed of field lenses.
    *
    * Note: This is declared as a trait (and not just a type alias) because it's used as a magnet in lens
    * composition overrides to maintain the proper output cardinality. It must be distinct from the other
    * cardinalities after type erasure.
    *
    * @tparam A the input focus type
    * @tparam B the output focus type
    */

  trait CreatableJLens[-A <: JAny, B <: JAny] extends JLens[Single, A, B] { self =>
    def toIdJLens: IdJLens[A, B] =
      new IdJLens[A, B] {
        override def apply(in: JFocus[A]): JResult[Single[JFocus[B]]] = self(in)
        override lazy val toString: String = self.toString
      }

    def toOptionJLens: OptionJLens[A, B] =
      new OptionJLens[A, B] {
        override def apply(in: JFocus[A]): JResult[Option[JFocus[B]]] = self(in).map(Some(_))
        override lazy val toString: String = self.toString
      }

    def toListJLens: ListJLens[A, B] =
      new ListJLens[A, B] {
        override def apply(in: JFocus[A]): JResult[JFoci[B]] = self(in).map(x => JFoci(List(x)))
        override lazy val toString: String = self.toString
      }
  }

  implicit class CreatableJLensOps[-A <: JAny, B <: JAny, +C](val c: C)(implicit fn: C => CreatableJLens[A, B]) {
    private val me: CreatableJLens[A, B] = fn(c) // Scala 3 needs this...

    def ~>[D <: JAny](that: CreatableJLens[B, D]): CreatableJLens[A, D] = CompositeCreatableJLens(me, that)
    def ~>[D <: JAny](that: IdJLens[B, D]): IdJLens[A, D] = CompositeIdJLens(me.toIdJLens, that)
    def ~>[D <: JAny](that: OptionJLens[B, D]): OptionJLens[A, D] = CompositeOptionJLens(me.toOptionJLens, that)
    def ~>[D <: JAny](that: ListJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me.toListJLens, that)

    def ? : OptionJLens[A, B] = AllowMissingValueOptionJLens(me)
  }

  //===================================================================================================================

  /** A lens that outputs exactly one focus for each input focus.
    *
    * Note: This is declared as a trait (and not just a type alias) because it's used as a magnet in lens
    * composition overrides to maintain the proper output cardinality. It must be distinct from the other
    * cardinalities after type erasure.
    *
    * @tparam A the input focus type
    * @tparam B the output focus type
    */

  trait IdJLens[-A <: JAny, B <: JAny] extends JLens[Single, A, B] { self =>
    def toOptionJLens: OptionJLens[A, B] =
      new OptionJLens[A, B] {
        override def apply(in: JFocus[A]): JResult[Option[JFocus[B]]] = self(in).map(Some(_))
        override lazy val toString: String = self.toString
      }

    def toListJLens: ListJLens[A, B] =
      new ListJLens[A, B] {
        override def apply(in: JFocus[A]): JResult[JFoci[B]] = self(in).map(x => JFoci(List(x)))
        override lazy val toString: String = self.toString
      }
  }

  implicit class IdJLensOps[-A <: JAny, B <: JAny, +C](val c: C)(implicit fn: C => IdJLens[A, B]) {
    private val me = fn(c) // Scala 3 needs this...

    def ~>[D <: JAny](that: CreatableJLens[B, D]): IdJLens[A, D] = CompositeIdJLens(me, that.toIdJLens)
    def ~>[D <: JAny](that: IdJLens[B, D]): IdJLens[A, D] = CompositeIdJLens(me, that)
    def ~>[D <: JAny](that: OptionJLens[B, D]): OptionJLens[A, D] = CompositeOptionJLens(me.toOptionJLens, that)
    def ~>[D <: JAny](that: ListJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me.toListJLens, that)

    def ? : OptionJLens[A, B] = AllowMissingValueOptionJLens(me)
  }

  //===================================================================================================================

  /** A lens that outputs zero or one focus for each input focus.
    *
    * Note: This is declared as a trait (and not just a type alias) because it's used as a magnet in lens
    * composition overrides to maintain the proper output cardinality. It must be distinct from the other
    * cardinalities after type erasure.
    *
    * @tparam A the input focus type
    * @tparam B the output focus type
    */

  trait OptionJLens[-A <: JAny, B <: JAny] extends JLens[Option, A, B] { self =>
    def toListLens: ListJLens[A, B] =
      new ListJLens[A, B] {
        override def apply(in: JFocus[A]): JResult[JFoci[B]] = self(in).map(x => JFoci(x.toList))
        override lazy val toString: String = self.toString
      }
  }

  object OptionJLens {
    implicit def fromCreatableJLens[A <: JAny, B <: JAny](that: CreatableJLens[A, B]): OptionJLens[A, B] =
      that.toOptionJLens
    implicit def fromIdJLens[A <: JAny, B <: JAny, C](that: C)(implicit fn: C => IdJLens[A, B]): OptionJLens[A, B] =
      fn(that).toOptionJLens
  }

  implicit class OptionJLensOps[-A <: JAny, B <: JAny](val me: OptionJLens[A, B]) {

    def ~>[D <: JAny](that: CreatableJLens[B, D]): OptionJLens[A, D] = CompositeOptionJLens(me, that.toOptionJLens)
    def ~>[D <: JAny](that: IdJLens[B, D]): OptionJLens[A, D] = CompositeOptionJLens(me, that.toOptionJLens)
    def ~>[D <: JAny](that: OptionJLens[B, D]): OptionJLens[A, D] = CompositeOptionJLens(me, that)
    def ~>[D <: JAny](that: ListJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me.toListLens, that)

    def ? : OptionJLens[A, B] = AllowMissingValueOptionJLens(me)
  }

  //===================================================================================================================

  /** A lens that outputs zero or more foci for each input focus.
    *
    * Note: This is declared as a trait (and not just a type alias) because it's used as a magnet in lens
    * composition overrides to maintain the proper output cardinality. It must be distinct from the other
    * cardinalities after type erasure.
    *
    * @tparam A the input focus type
    * @tparam B the output focus type
    */

  trait ListJLens[-A <: JAny, +B <: JAny] extends Function1[JFocus[A], JResult[JFoci[B]]]

  object ListJLens {
    implicit def fromCreatableJLens[A <: JAny, B <: JAny](that: CreatableJLens[A, B]): ListJLens[A, B] =
      that.toListJLens
    implicit def fromIdJLens[A <: JAny, B <: JAny](that: IdJLens[A, B]): ListJLens[A, B] = that.toListJLens
    implicit def fromOptionJLens[A <: JAny, B <: JAny](that: OptionJLens[A, B]): ListJLens[A, B] = that.toListLens
  }

  implicit class ListJLensOps[-A <: JAny, B <: JAny](val me: ListJLens[A, B]) {

    def ~>[D <: JAny](that: CreatableJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me, that)
    def ~>[D <: JAny](that: IdJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me, that)
    def ~>[D <: JAny](that: OptionJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me, that)
    def ~>[D <: JAny](that: ListJLens[B, D]): ListJLens[A, D] = CompositeListJLens(me, that)

    def ? : ListJLens[A, B] = AllowMissingValueListJLens(me)
  }

  //===================================================================================================================

  case class CompositeIdJLens[-A <: JAny, B <: JAny, C <: JAny](l: IdJLens[A, B], r: IdJLens[B, C])
      extends IdJLens[A, C] {
    override def apply(in: JFocus[A]): JResult[JFocus[C]] = l(in).flatMap(r.apply)
    override lazy val toString: String = s"$l ~> $r"
  }

  case class CompositeCreatableJLens[-A <: JAny, B <: JAny, C <: JAny](l: CreatableJLens[A, B], r: CreatableJLens[B, C])
      extends CreatableJLens[A, C] {
    override def apply(in: JFocus[A]): JResult[JFocus[C]] = l(in).flatMap(r.apply)
    override lazy val toString: String = s"$l ~> $r"
  }

  case class CompositeOptionJLens[-A <: JAny, B <: JAny, C <: JAny](l: OptionJLens[A, B], r: OptionJLens[B, C])
      extends OptionJLens[A, C] {
    override def apply(in: JFocus[A]): JResult[Option[JFocus[C]]] = l(in).flatMap(_.parFlatTraverse(r.apply))
    override lazy val toString: String = s"$l ~> $r"
  }

  case class CompositeListJLens[-A <: JAny, B <: JAny, C <: JAny](l: ListJLens[A, B], r: ListJLens[B, C])
      extends ListJLens[A, C] {
    override def apply(in: JFocus[A]): JResult[JFoci[C]] =
      l(in).flatMap(_.foci.parFlatTraverse(r.apply(_).map(_.foci))).map(JFoci(_))
    override lazy val toString: String = s"$l ~> $r"
  }

  //===================================================================================================================

  case class AllowMissingValueOptionJLens[-A <: JAny, B <: JAny](inner: OptionJLens[A, B]) extends OptionJLens[A, B] {
    override def apply(in: JFocus[A]): JResult[Option[JFocus[B]]] =
      inner(in).ignoreMissingValuesWith(None)
    override val toString: String =
      if (inner.toString.exists(_.isWhitespace)) s"($inner).?" else s"$inner.?"
  }

  case class AllowMissingValueListJLens[-A <: JAny, B <: JAny](inner: ListJLens[A, B]) extends ListJLens[A, B] {
    override def apply(in: JFocus[A]): JResult[JFoci[B]] =
      inner(in).map(_.foci).ignoreMissingValuesWith(Nil).map(JFoci(_))
    override val toString: String =
      if (inner.toString.exists(_.isWhitespace)) s"($inner).?" else s"$inner.?"
  }

  //===================================================================================================================

  /** A lens that doesn't actually change the focus. It is often a useful way to begin a chain of len compositions
    * where the compiler may not understand that a JLens is what it should be expecting.
    */

  def focus: CreatableJLens[JAny, JAny] = FocusJLens[JAny]()

  case class FocusJLens[A <: JAny]() extends CreatableJLens[A, A] {
    override def apply(in: JFocus[A]): JResult[JFocus[A]] = in.rightNec
    override lazy val toString: String = "focus"
  }

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that move the focus to its root.
    *
    * Note: Although the root of the focus will ''usually'' be the root of the JSON document, it  ''may not'' be,
    * depending on how the focus was constructed.
    */

  def root: RootJLens.type = RootJLens

  case object RootJLens extends IdJLens[JAny, JAny] {
    override def apply(in: JFocus[JAny]): JResult[JFocus[JAny]] = in.root.rightNec
    override lazy val toString: String = "root"
  }

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that narrows the focus to a more specific JSON type. This has the same behavior as [[as]] ''except''
    * that it maintains the creatability of the lens by being restricted to a JSON type.
    *
    * @tparam A the type to narrow to
    */

  def narrow[A <: JAny](implicit summoner: Summoner[A]): NarrowTargetPlaceholder[A] =
    NarrowTargetPlaceholder[A](summoner())

  case class NarrowTargetPlaceholder[To](jtype: JType)

  object NarrowTargetPlaceholder {
    // The unused parameter is only being used for its type argument. That's kind of all it has, really.
    implicit def narrowTargetToJLens[B <: JAny: ClassTag: Summoner](
        unused: NarrowTargetPlaceholder[B]
    ): CreatableJLens[JAny, B] =
      NarrowJLens[B]()
  }

  case class NarrowJLens[A <: JAny: ClassTag: Summoner]() extends CreatableJLens[JAny, A] {
    override def apply(in: JFocus[JAny]): JResult[JFocus[A]] = in.narrow[A]
    override val toString: String = s"narrow[${JType[A]}]"
  }

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that descends into the ''single'' specified field of the focused JSON object with the specified name.
    * If the value in focus is not a JSON object or if the that object does not have exactly one field with the
    * specified name, this lens will fail.
    *
    * This lens can also be implicitly created from a bare String.
    *
    * @param name the name of the field to descend into
    */

  def field(name: String): FieldJLens = FieldJLens(name)

  case class FieldJLens(name: String) extends CreatableJLens[JAny, JAny] {
    override def apply(in: JFocus[JAny]): JResult[JFocus[JAny]] = in.asObject.flatMap(_.field(name))
    def ** : ListJLens[JAny, JAny] = all
    def all: ListJLens[JAny, JAny] = FieldAllJLens(name)
    override lazy val toString: String = s""""$name""""
  }

  implicit def stringToLens(name: String): FieldJLens = field(name)

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that descends into the ''all'' fields of the focused JSON object with the specified name. If the
    * object does not have any fields with the specified name, this lens will return an empty list of foci.
    * If the value in focus is not a JSON object, this lens will fail.
    *
    * This lens can also be created from a [[field]] lens (or just a bare string) using its [[FieldJLens.**]]
    * operator (e.g., `"key".*`).
    *
    * @param name the name of the fields to descend into
    */

  def fields(name: String): ListJLens[JAny, JAny] = FieldAllJLens(name)

  case class FieldAllJLens(name: String) extends ListJLens[JAny, JAny] {
    override def apply(in: JFocus[JAny]): JResult[JFoci[JAny]] = in.asObject.map(_.fields(name)).map(JFoci(_))
    override lazy val toString: String = s""""$name".**"""
  }

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that descends into focused JSON array's item with the specified index. If the value in focus is not a
    * JSON array, this lens will fail.
    *
    * This lens can also be implicitly created from a bare Int.
    *
    * @param name the index of the item to descend into
    */

  def item(index: Int): IdJLens[JAny, JAny] = ItemJLens(index)

  case class ItemJLens(index: Int) extends IdJLens[JAny, JAny] {
    override def apply(in: JFocus[JAny]): JResult[JFocus[JAny]] = in.asArray.flatMap(_.item(index))
    override lazy val toString: String = index.toString
  }

  implicit def intToLens(index: Int): IdJLens[JAny, JAny] = ItemJLens(index)

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that descends into all items of the focused JSON array. If the value in focus is not a
    * JSON array, this lens will fail.
    *
    * This is an alias for [[org.scalawag.bateman.json.lens.*]], for those who prefer words.
    *
    * @see [[org.scalawag.bateman.json.lens.*]]
    */

  val items: ListJLens[JAny, JAny] = AllItemsJLens

  /** A lens that descends into all items of the focused JSON array. If the value in focus is not a
    * JSON array, this lens will fail.
    *
    * This is an alias for [[org.scalawag.bateman.json.lens.items]], for those who prefer symbols.
    *
    * @see [[org.scalawag.bateman.json.lens.items]]
    */

  val * : ListJLens[JAny, JAny] = items

  case object AllItemsJLens extends ListJLens[JAny, JAny] {
    override def apply(in: JFocus[JAny]): JResult[JFoci[JAny]] = in.asArray.map(_.items).map(JFoci(_))
    override val toString: String = "*"
  }

  //-------------------------------------------------------------------------------------------------------------------

  /** A lens that descends into all fields of the focused JSON object. If the value in focus is not a
    * JSON object, this lens will fail.
    *
    * This is an alias for [[org.scalawag.bateman.json.lens.**]], for those who prefer words.
    *
    * @see [[org.scalawag.bateman.json.lens.**]]
    */

  val fields: ListJLens[JAny, JAny] = AllFieldsJLens

  /** A lens that descends into all fields of the focused JSON object. If the value in focus is not a
    * JSON object, this lens will fail.
    *
    * This is an alias for [[org.scalawag.bateman.json.lens.items]], for those who prefer symbols.
    *
    * @see [[org.scalawag.bateman.json.lens.items]]
    */

  val ** : ListJLens[JAny, JAny] = fields

  case object AllFieldsJLens extends ListJLens[JAny, JAny] {
    override def apply(in: JFocus[JAny]): JResult[JFoci[JAny]] = in.asObject.map(_.fields).map(JFoci(_))
    override val toString: String = "**"
  }
}
