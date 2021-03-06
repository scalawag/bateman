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

package org.scalawag.bateman.json.generic.decoding

import org.scalawag.bateman.json.ProgrammerError
import org.scalawag.bateman.json.decoding.{DecodeResult, JAny, JObject, JPointer, UnexpectedValue, UnspecifiedField}

trait JSourceLike {
  val root: JObject
  val fields: Map[String, JPointer]

  def getFieldSource(name: String): DecodeResult[JAny] = fields(name).navigate(root)
  def getFieldSourceUnsafe(name: String): JAny =
    getFieldSource(name).getOrElse {
      throw ProgrammerError(s"Field $name is not represented in the source JSON text.")
    }

  def unspecifiedField(name: String): UnspecifiedField =
    fields.get(name) match {
      case Some(p: JPointer.Child) => UnspecifiedField(root, p)
      case Some(_)                 => throw ProgrammerError(s"Field $name was not sourced from a child in the JSON text.")
      case None                    => throw ProgrammerError(s"Field $name is not represented in the source JSON text.")
    }

  def unexpectedValue(name: String): UnexpectedValue =
    UnexpectedValue(getFieldSourceUnsafe(name))
}

final case class JSource(root: JObject, fields: Map[String, JPointer] = Map.empty) extends JSourceLike
