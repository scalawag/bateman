package org.scalawag.bateman.json.literal

import org.scalawag.bateman.json.encoding.{JAny, JAnyEncoder, JArray, JBoolean, JNull, JNumber, JObject, JString}

import scala.reflect.macros.whitebox.Context
import scala.util.Random

class LiteralMacros(val c: Context) {
  import c.universe._

  // Turns a JAny into the code that's needed to construct it.
  private def toTree(jany: JAny, mapping: Map[String, TermName]): Tree =
    jany match {
      case s: JString if mapping.contains(s.value) =>
        q"${mapping(s.value)}"
      case _: JNull =>
        q"_root_.org.scalawag.bateman.json.encoding.JNull"
      case b: JBoolean =>
        q"_root_.org.scalawag.bateman.json.encoding.JBoolean(${b.value})"
      case n: JNumber =>
        q"""_root_.org.scalawag.bateman.json.encoding.JNumber.unsafe(${Literal(Constant(n.value))})"""
      case s: JString =>
        q"""_root_.org.scalawag.bateman.json.encoding.JString(${Literal(Constant(s.value))})"""
      case a: JArray =>
        val items = a.items.map(toTree(_, mapping))
        q"_root_.org.scalawag.bateman.json.encoding.JArray(..$items)"
      case o: JObject =>
        val fields = o.fields.map { case (k, v) => q"(${Literal(Constant(k))},${toTree(v, mapping)})" }
        q"_root_.org.scalawag.bateman.json.encoding.JObject(..$fields)"
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

  final def jsonStringContext(args: c.Expr[Any]*): Tree =
    c.prefix.tree match {
      case Apply(_, Apply(_, parts) :: Nil) =>
        // Get the StringContext parts from lexical context
        val stringParts: List[String] = parts.map {
          case Literal(Constant(part: String)) => part
        }

        def randomStrings = Stream.continually(randomString).distinct.dropWhile(s => stringParts.exists(_.contains(s)))

        // Generate info for each arg (interpolated expression) in the string.
        //  - term name of the expression encoded as a JAny
        //  - declaration for the encoded JAny term
        //  - unique random string to be used as a stand-in during parsing

        val exprs = args.zip(randomStrings).zipWithIndex.map {
          case ((arg, standin), n) =>
            val term = TermName(s"arg$n")
            InterpolatedExpression(
              term,
              q"""val $term = _root_.org.scalawag.bateman.json.encoding.JAnyEncoder.encode($arg)""",
              standin
            )
        }

        // Generate a new JSON text which contains the stand-in strings.
        val text = insertStandIns(stringParts, exprs.map(_.standIn))

        // Parse the JSON text (with stand-ins)
        val jany = org.scalawag.bateman.json
          .parse(text.toStream)
          .map(_.toEncoding)
          .fold(
            e => c.abort(c.enclosingPosition, e.description),
            // Walk the resulting JAny, turning it into direct JAny constructor calls and replacing the stand-ins
            // with their associated terms.
            toTree(_, exprs.map(x => x.standIn -> x.term).toMap)
          )

        // Output the term declarations and the JAny construction code
        q"""
          ..${exprs.map(_.decl)}
          $jany
        """
    }
}
