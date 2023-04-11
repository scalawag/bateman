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

package test.jsonapi.encoding

import cats.data.NonEmptyChain
import cats.syntax.either._
import org.scalawag.bateman.json.JString
import org.scalawag.bateman.jsonapi.encoding.FieldsSpec.Fields
import org.scalawag.bateman.jsonapi.encoding.IncludeSpec.{Never, Opportunistically}
import org.scalawag.bateman.jsonapi.encoding.{FieldsSpec, IncludePathTooDeep, IncludeSpec, IncludeTooLong}
import test.json.BatemanTestBase
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._

class FieldsSpecTest extends BatemanTestBase {
  describe("Fields") {

    describe("All") {
      it("should include everything") {
        forAll { (s: String) =>
          Fields.All(s) shouldBe true
        }
      }
    }

    describe("None") {
      it("should exclude everything") {
        forAll { (s: String) =>
          Fields.None(s) shouldBe false
        }
      }
    }

    describe("Explicit") {
      val fields = listOf(arbitrary[String]).sample.map(_.toSet).get
      val subject = Fields.Explicit(fields)

      it("should include the specified fields") {
        fields.forall(subject(_)) shouldBe true
      }

      it("should not include unspecified fields") {
        forAll(arbitrary[String].filterNot(fields)) { s =>
          subject(s) shouldBe false
        }
      }
    }

    it("should statically type fallibility") {
      val f1: Fields = Fields.All
      val f2: Fields = Fields.None
      val f3: Fields = Fields.Explicit("a")

      val f4: Fields.Infallible = Fields.All
      val f5: Fields.Infallible = Fields.None
      assertTypeError("""val f6: Fields.Infallible = Fields.Explicit("a")""")
    }
  }

  describe("FieldsSpec") {
    describe("All") {
      it("should include everything from everything") {
        forAll { (t: String, f: String) =>
          FieldsSpec.All.forResourceType(t)(f) shouldBe true
        }
      }
    }

    describe("None") {
      it("should exclude everything from everything") {
        forAll { (t: String, f: String) =>
          FieldsSpec.None.forResourceType(t)(f) shouldBe false
        }
      }
    }

    describe("Explicit") {
      val t1 = arbitrary[String].sample.get
      val t2 = arbitrary[String].sample.get
      val ff1 = listOf(arbitrary[String]).sample.map(_.toSet).get
      val ff2 = listOf(arbitrary[String]).sample.map(_.toSet).get
      val subject = FieldsSpec(Map(t1 -> Fields.Explicit(ff1), t2 -> Fields.Explicit(ff2)))

      it("should include the specified fields") {
        ff1.forall(subject.forResourceType(t1)(_)) shouldBe true
        ff2.forall(subject.forResourceType(t2)(_)) shouldBe true
      }

      it("should not include unspecified fields") {
        forAll(arbitrary[String].filterNot(ff1).filterNot(ff2)) { f =>
          subject.forResourceType(t1)(f) shouldBe false
          subject.forResourceType(t2)(f) shouldBe false
        }
      }

      it("should include everything from unknown type") {
        forAll(arbitrary[String].filterNot(t => t == t1 || t == t2)) { t =>
          ff1.forall(subject.forResourceType(t)(_)) shouldBe true
          ff2.forall(subject.forResourceType(t)(_)) shouldBe true
        }
      }

      it("should exclude everything from unknown type") {
        val subject = FieldsSpec(Map.empty, Fields.None)
        forAll { (t: String, f: String) =>
          subject.forResourceType(t)(f) shouldBe false
        }
      }
    }

    it("should statically type fallibility") {
      val f1: FieldsSpec = FieldsSpec.All
      val f2: FieldsSpec = FieldsSpec.None
      val f3: FieldsSpec = FieldsSpec(Map.empty)

      val f4: FieldsSpec.Infallible = FieldsSpec.All
      val f5: FieldsSpec.Infallible = FieldsSpec.None
      assertTypeError("""val f6: FieldsSpec.Infallible = FieldsSpec(Map.empty)""")
    }
  }
}
