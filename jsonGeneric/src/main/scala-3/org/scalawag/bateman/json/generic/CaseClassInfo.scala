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

package org.scalawag.bateman.json.generic

case class CaseClassInfo(
    defaultsAsTuple: Product,
    fieldNames: List[String]
):
  def defaults: List[Option[Any]] = defaultsAsTuple.productIterator.toList.map(_.asInstanceOf[Option[Any]])

  def indexedFields: List[(String, Option[Any])] =
    fieldNames.zip(defaults)

  def fieldByName(name: String): Option[(String, Option[Any])] =
    indexedFields.find(_._1 == name)