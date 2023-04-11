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

package org.scalawag.bateman.json.focus

import org.scalawag.bateman.json._

// TODO: I want to do this, but Scala2 (at least) can't seem to infer the types (InValue) property
class JFocusOps[InFocus <: JStrongFocus[JAny], InValue](me: InFocus)(implicit
    valueFinder: ValueFinder.Aux[InFocus, InValue]
) {

  def modifyF[OutValue <: JAny](fn: InFocus => JResult[OutValue])(implicit
      replacer: ValueReplacer[OutValue, InFocus],
  ): JResult[replacer.NewFocus] = fn(me).map(replacer(_, me))

  def modify[OutValue <: JAny](fn: InFocus => OutValue)(implicit
      replacer: ValueReplacer[OutValue, InFocus],
  ): replacer.NewFocus = replacer(fn(me), me)

  def modifyValueF[OutValue <: JAny](fn: InValue => JResult[OutValue])(implicit
      replacer: ValueReplacer[OutValue, InFocus],
  ): JResult[replacer.NewFocus] = fn(valueFinder(me)).map(replacer(_, me))

  def modifyValue[OutValue <: JAny](fn: InValue => OutValue)(implicit
      replacer: ValueReplacer[OutValue, InFocus],
  ): replacer.NewFocus = replacer(fn(valueFinder(me)), me)

}
