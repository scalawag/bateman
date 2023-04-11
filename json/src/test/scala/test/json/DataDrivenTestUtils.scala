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

package test.json

import org.scalactic.source.Position

trait DataDrivenTestUtils {
  // This allows us to save the source location so that the test appears to come from where the test cases are listed
  // instead of the loop.

  implicit class DataDrivenTestCase[+A](val defn: A)(implicit val pos: Position)

  object DataDrivenTestCase {
    def unapply[A](arg: DataDrivenTestCase[A]): Option[(A, Position)] = Some(arg.defn -> arg.pos)
  }
}
