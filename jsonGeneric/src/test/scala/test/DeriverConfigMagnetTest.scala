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

package test

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, KebabCase, PascalCase, SnakeCase}
import org.scalawag.bateman.json.generic.semiauto.{Defer, DeriverConfigMagnet, Replace, Transform}
import test.json.BatemanTestBase

class DeriverConfigMagnetTest extends BatemanTestBase {
  val genCase = Gen.oneOf(PascalCase, CamelCase, SnakeCase, KebabCase)
  val genConfig = for {
    ff <- genCase
    ft <- genCase
    cf <- genCase
    ct <- genCase
    d <- arbitrary[Boolean]
    u <- arbitrary[Boolean]
    e <- arbitrary[Boolean]
  } yield Config(ff to ft, cf to ct, d, u, e)

  it("should defer to the specified config") {
    forAll(genConfig) { cfg =>
      Defer(cfg) shouldBe cfg
    }
  }

  it("should replace the specified config entirely") {
    forAll(genConfig, genConfig) { (cfg1, cfg2) =>
      Replace(cfg1)(cfg2) shouldBe cfg1
    }
  }

  it("should transform the specified config") {
    forAll(genConfig) { cfg =>
      Transform(_.copy(fieldNameMapping = CamelCase to SnakeCase))(cfg) shouldBe
        cfg.copy(fieldNameMapping = CamelCase to SnakeCase)
    }
  }

  it("should implicitly create a Replace") {
    val m: Replace = Config()
  }

  it("should implicitly create a Transform") {
    val m: Transform = (_: Config).copy(useDefaultsForMissingFields = false)
  }
}
