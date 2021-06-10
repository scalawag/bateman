package org.scalawag.bateman.json.codec

import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.{Decoder, JAnyDecoder}
import org.scalawag.bateman.json.encoding.JAnyEncoder
import org.scalawag.bateman.json.{ProgrammerError, decoding, encoding}

sealed trait JAny {
  def toEncoding: encoding.JAny
  def toDecoding: decoding.JAny
}

object JAny {
  case class Encoding(toEncoding: encoding.JAny) extends JAny {
    override def toDecoding: decoding.JAny =
      throw ProgrammerError("This value was not parsed and can not be turned into a decoding JAny.")
  }

  case class Decoding(toDecoding: decoding.JAny) extends JAny {
    override def toEncoding: encoding.JAny = toDecoding.toEncoding
  }

  implicit val decoder: JAnyDecoder[JAny] = Decoder(Decoding(_).validNec)
  implicit val encoder: JAnyEncoder[JAny] = _.toEncoding
}
