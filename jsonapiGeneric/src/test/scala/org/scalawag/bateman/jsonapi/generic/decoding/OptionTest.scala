package org.scalawag.bateman.jsonapi.generic.decoding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{DataDrivenTestUtils, NotNull, Null, Nullable, ParserTestUtils}
import OptionTest._
import cats.data.NonEmptyChain
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import org.scalatest.Inside
import org.scalawag.bateman.json.decoding.{
  ContextualDecoder,
  DecodeResult,
  InvalidValue,
  JAnyDecoder,
  JObject,
  JPointer,
  JStringDecoder,
  UnspecifiedField
}
import org.scalawag.bateman.json.encoding.JStringEncoder
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.jsonapi.encoding.{NullData, Relationship, ResourceIdentifier, ResourceObjectOptionalId}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, MetaTag, RelationshipTag, semiauto}
import shapeless.tag.@@
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.jsonapi.decoding
import org.scalawag.bateman.jsonapi.decoding.{Document, ResourceLike}
import org.scalawag.bateman.jsonapi.query.{data, required}

object OptionTest {
  final case class MyIdClass(id: String @@ IdTag)
  final case class MyIdClassDefault(id: String @@ IdTag = "1")
  final case class MyIdClassOption(id: Option[String] @@ IdTag)
  final case class MyIdClassOptionDefault(id: Option[String] @@ IdTag = None)
  final case class MyIdClassOptionDefaultSome(id: Option[String] @@ IdTag = Some("1"))

  final case class MyAttrClass(
      a: Option[Int] @@ AttributeTag,
      b: Option[Int] @@ AttributeTag = None,
      c: Option[Int] @@ AttributeTag = Some(7),
      d: Int @@ AttributeTag,
      e: Int @@ AttributeTag = 7,
      f: Nullable[Int] @@ AttributeTag,
      g: Nullable[Int] @@ AttributeTag = Null,
      h: Nullable[Int] @@ AttributeTag = NotNull(7),
      i: Option[Nullable[Int]] @@ AttributeTag,
      j: Option[Nullable[Int]] @@ AttributeTag = None,
      k: Option[Nullable[Int]] @@ AttributeTag = Some(Null),
      l: Option[Nullable[Int]] @@ AttributeTag = Some(NotNull(7))
  )

  object MyAttrClass {
    val minimal = MyAttrClass(a = None, d = 7, f = Null, i = None)

    val full = MyAttrClass(
      a = Some(1),
      b = Some(2),
      c = Some(3),
      d = 4,
      e = 5,
      f = NotNull(6),
      g = NotNull(7),
      h = NotNull(8),
      i = Some(NotNull(9)),
      j = Some(NotNull(10)),
      k = Some(NotNull(11)),
      l = Some(NotNull(12)),
    )
  }

  final case class MyMetaClass(
      a: Option[Int] @@ MetaTag,
      b: Option[Int] @@ MetaTag = None,
      c: Option[Int] @@ MetaTag = Some(7),
      d: Int @@ MetaTag,
      e: Int @@ MetaTag = 7,
      f: Nullable[Int] @@ MetaTag,
      g: Nullable[Int] @@ MetaTag = Null,
      h: Nullable[Int] @@ MetaTag = NotNull(7),
      i: Option[Nullable[Int]] @@ MetaTag,
      j: Option[Nullable[Int]] @@ MetaTag = None,
      k: Option[Nullable[Int]] @@ MetaTag = Some(Null),
      l: Option[Nullable[Int]] @@ MetaTag = Some(NotNull(7))
  )

  object MyMetaClass {
    val minimal = MyMetaClass(a = None, d = 7, f = Null, i = None)

    val full = MyMetaClass(
      a = Some(1),
      b = Some(2),
      c = Some(3),
      d = 4,
      e = 5,
      f = NotNull(6),
      g = NotNull(7),
      h = NotNull(8),
      i = Some(NotNull(9)),
      j = Some(NotNull(10)),
      k = Some(NotNull(11)),
      l = Some(NotNull(12)),
    )
  }

  final case class MyRelativeId(id: Int @@ IdTag)

  final case class MyRelClass(
      a: Option[MyRelativeId] @@ RelationshipTag,
      b: Option[MyRelativeId] @@ RelationshipTag = None,
      c: Option[MyRelativeId] @@ RelationshipTag = Some(MyRelativeId(7)),
      d: MyRelativeId @@ RelationshipTag,
      e: MyRelativeId @@ RelationshipTag = MyRelativeId(7),
      f: Nullable[MyRelativeId] @@ RelationshipTag,
      g: Nullable[MyRelativeId] @@ RelationshipTag = Null,
      h: Nullable[MyRelativeId] @@ RelationshipTag = NotNull(MyRelativeId(7)),
      i: Option[Nullable[MyRelativeId]] @@ RelationshipTag,
      j: Option[Nullable[MyRelativeId]] @@ RelationshipTag = None,
      k: Option[Nullable[MyRelativeId]] @@ RelationshipTag = Some(Null),
      l: Option[Nullable[MyRelativeId]] @@ RelationshipTag = Some(NotNull(MyRelativeId(7))),
      m: List[MyRelativeId] @@ RelationshipTag,
      n: List[MyRelativeId] @@ RelationshipTag = Nil,
      o: List[MyRelativeId] @@ RelationshipTag = List(MyRelativeId(7), MyRelativeId(8)),
      p: Option[List[MyRelativeId]] @@ RelationshipTag,
      q: Option[List[MyRelativeId]] @@ RelationshipTag = None,
      r: Option[List[MyRelativeId]] @@ RelationshipTag = Some(Nil)
  )

  object MyRelClass {
    val minimal = MyRelClass(a = None, d = MyRelativeId(7), f = Null, i = None, m = Nil, p = None)

    val full = MyRelClass(
      a = Some(MyRelativeId(1)),
      b = Some(MyRelativeId(2)),
      c = Some(MyRelativeId(3)),
      d = MyRelativeId(4),
      e = MyRelativeId(5),
      f = NotNull(MyRelativeId(6)),
      g = NotNull(MyRelativeId(7)),
      h = NotNull(MyRelativeId(8)),
      i = Some(NotNull(MyRelativeId(9))),
      j = Some(NotNull(MyRelativeId(10))),
      k = Some(NotNull(MyRelativeId(11))),
      l = Some(NotNull(MyRelativeId(12))),
      m = List(MyRelativeId(13), MyRelativeId(14)),
      n = List(MyRelativeId(15), MyRelativeId(16)),
      o = List(MyRelativeId(17), MyRelativeId(18)),
      p = Some(List(MyRelativeId(19), MyRelativeId(20))),
      q = Some(List(MyRelativeId(21), MyRelativeId(22))),
      r = Some(List(MyRelativeId(23), MyRelativeId(24))),
    )
  }

}

class OptionTest extends AnyFunSpec with Matchers with Inside with ParserTestUtils with DataDrivenTestUtils {
  def primaryDataAs[R <: ResourceLike, A](
      json: String
  )(implicit
      rdec: ContextualDecoder[ResourceLike, R, Document],
      adec: ContextualDecoder[R, A, Document]
  ): DecodeResult[A] =
    parseAs[Document](json).dquery(_ ~> data ~> required ~> as[R] ~> as[A])

  describe("id") {

    describe("MyIdClass") {
      it("should handle required ID with no default") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClass]()

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClass]("""
            {
              "data": {
                "type": "MyIdClass",
                "id": "1"
              }
            }
          """).shouldSucceed

        actual shouldBe MyIdClass("1")
      }

      it("should fail with no value required ID with no default") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClass]()

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClass]("""
            {
              "data": {
                "type": "MyIdClass"
              }
            }
          """).shouldFailSingle

        inside(actual) {
          case e: UnspecifiedField =>
            e.obj.pointer shouldBe JPointer.Root / "data"
            e.pointers shouldBe NonEmptyChain(JPointer.Root / "id")
        }
      }

      it("should fail with no ID value specified and no default (even with setting)") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClass](config =
          Config.default.copy(useDefaultsForMissingFields = true)
        )

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClass]("""
            {
              "data": {
                "type": "MyIdClass"
              }
            }
          """).shouldFailSingle

        inside(actual) {
          case e: UnspecifiedField =>
            e.obj.pointer shouldBe JPointer.Root / "data"
            e.pointers shouldBe NonEmptyChain(JPointer.Root / "id")
        }
      }
    }

    describe("MyIdClassDefault") {
      it("should handle required ID with default (even when set to not encode defaults)") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClassDefault]()

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClassDefault]("""
          {
            "data": {
              "type": "MyIdClassDefault",
              "id": "888"
            }
          }
        """).shouldSucceed

        actual shouldBe MyIdClassDefault("888")
      }

      it("should handle required ID with default") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClassDefault]()

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClassDefault]("""
          {
            "data": {
              "type": "MyIdClassDefault"
            }
          }
        """).shouldSucceed

        actual shouldBe MyIdClassDefault()
      }

      it("should fail required ID with default when configured not to use it") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClassDefault](config =
          Config(
            useDefaultsForMissingFields = false
          )
        )

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClassDefault]("""
          {
            "data": {
              "type": "MyIdClassDefault"
            }
          }
        """).shouldFailSingle

        inside(actual) {
          case e: UnspecifiedField =>
            e.obj.pointer shouldBe JPointer.Root / "data"
            e.pointers shouldBe NonEmptyChain(JPointer.Root / "id")
        }
      }
    }

    describe("MyIdClassDefault") {
      it("should handle option ID without default") {
        implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyIdClassOption]()

        val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyIdClassOption]("""
            {
              "data": {
                "type": "MyIdClassOption",
                "id": "888"
              }
            }
          """).shouldSucceed

        actual shouldBe MyIdClassOption(Some("888"))
      }
    }

    it("should handle option ID with default None") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOptionDefault]()

      MyIdClassOptionDefault(None).to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOptionDefault"
      )
    }

    it("should handle option ID with default None and value Some") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOptionDefault]()

      MyIdClassOptionDefault(Some("1")).to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOptionDefault",
        Some("1")
      )
    }

    it("should handle option ID with default Some (even when set to not encode defaults)") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOptionDefaultSome]()

      MyIdClassOptionDefaultSome().to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOptionDefaultSome",
        Some("1")
      )
    }
  }

  describe("attributes") {
    it("should succeed without defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyAttrClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyAttrClass]("""
        {
          "data": {
            "type": "MyAttrClass",
            "attributes": {
              "d": 7,
              "f": null
            }
          }
        }
      """).shouldSucceed

      actual.f shouldBe a[Null]
      actual.copy(f = Null) shouldBe MyAttrClass(a = None, d = 7, f = Null, i = None)
    }

    it("should succeed with defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyAttrClass](config =
        Config.default.copy(useDefaultsForMissingFields = false)
      )

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyAttrClass]("""
        {
          "data": {
            "type": "MyAttrClass",
            "attributes": {
              "d": 7,
              "e": 8,
              "f": null,
              "g": 9,
              "h": null
            }
          }
        }
      """).shouldSucceed

      actual.f shouldBe a[Null]
      actual.h shouldBe a[Null]
      actual.copy(f = Null, h = Null) shouldBe MyAttrClass(
        a = None,
        b = None,
        c = None,
        d = 7,
        e = 8,
        f = Null,
        g = NotNull(9),
        h = Null,
        i = None,
        j = None,
        k = None,
        l = None,
      )
    }

    it("should succeed full instance") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyAttrClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyAttrClass]("""
        {
          "data": {
            "type": "MyAttrClass",
            "attributes": {
              "a": 1,
              "b": 2,
              "c": 3,
              "d": 4,
              "e": 5,
              "f": 6,
              "g": 7,
              "h": 8,
              "i": 9,
              "j": 10,
              "k": 11,
              "l": 12
            }
          }
        }
      """).shouldSucceed

      actual shouldBe MyAttrClass(
        a = Some(1),
        b = Some(2),
        c = Some(3),
        d = 4,
        e = 5,
        f = NotNull(6),
        g = NotNull(7),
        h = NotNull(8),
        i = Some(NotNull(9)),
        j = Some(NotNull(10)),
        k = Some(NotNull(11)),
        l = Some(NotNull(12)),
      )
    }
  }

  describe("meta") {
    it("should succeed without defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyMetaClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyMetaClass]("""
        {
          "data": {
            "type": "MyMetaClass",
            "meta": {
              "d": 7,
              "f": null
            }
          }
        }
      """).shouldSucceed

      actual.f shouldBe a[Null]
      actual.copy(f = Null) shouldBe MyMetaClass(a = None, d = 7, f = Null, i = None)
    }

    it("should succeed with defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyMetaClass](config =
        Config.default.copy(useDefaultsForMissingFields = false)
      )

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyMetaClass]("""
        {
          "data": {
            "type": "MyMetaClass",
            "meta": {
              "d": 7,
              "e": 8,
              "f": null,
              "g": 9,
              "h": null
            }
          }
        }
      """).shouldSucceed

      actual.f shouldBe a[Null]
      actual.h shouldBe a[Null]
      actual.copy(f = Null, h = Null) shouldBe MyMetaClass(
        a = None,
        b = None,
        c = None,
        d = 7,
        e = 8,
        f = Null,
        g = NotNull(9),
        h = Null,
        i = None,
        j = None,
        k = None,
        l = None,
      )
    }

    it("should succeed full instance") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyMetaClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyMetaClass]("""
        {
          "data": {
            "type": "MyMetaClass",
            "meta": {
              "a": 1,
              "b": 2,
              "c": 3,
              "d": 4,
              "e": 5,
              "f": 6,
              "g": 7,
              "h": 8,
              "i": 9,
              "j": 10,
              "k": 11,
              "l": 12
            }
          }
        }
      """).shouldSucceed

      actual shouldBe MyMetaClass(
        a = Some(1),
        b = Some(2),
        c = Some(3),
        d = 4,
        e = 5,
        f = NotNull(6),
        g = NotNull(7),
        h = NotNull(8),
        i = Some(NotNull(9)),
        j = Some(NotNull(10)),
        k = Some(NotNull(11)),
        l = Some(NotNull(12)),
      )
    }
  }

  describe("relationships (identifiers)") {
    implicit val intdec = ContextualDecoder.jstringToJNumber(ContextualDecoder.jnumberToIntDecoder)
    implicit val iddec = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyRelativeId]()

    it("should succeed without defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyRelClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyRelClass]("""
        {
          "data": {
            "type": "MyRelClass",
            "relationships": {
              "d": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "7"
                }
              },
              "f": {
                "data": null
              },
              "m": {
                "data": []
              }
            }
          }
        }
      """).shouldSucceed

      actual.f shouldBe a[Null]
      actual.copy(f = Null) shouldBe MyRelClass(a = None, d = MyRelativeId(7), f = Null, i = None, m = Nil, p = None)
    }

    it("should succeed with defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyRelClass](config =
        Config(
          useDefaultsForMissingFields = false
        )
      )

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyRelClass]("""
        {
          "data": {
            "type": "MyRelClass"
          }
        }
      """).shouldFail

      val nonOptionalFields = Iterable("d", "e", "f", "g", "h", "m", "n", "o")
      actual.length shouldBe nonOptionalFields.size

      actual.iterator.zip(nonOptionalFields.iterator) foreach {
        case (e: UnspecifiedField, fieldName) =>
          e.obj.pointer shouldBe JPointer.Root / "data"
          e.pointers shouldBe NonEmptyChain(JPointer.Root / "relationships" / fieldName)
      }
    }

    it("should succeed full instance") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyRelClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyRelClass]("""
        {
          "data": {
            "type": "MyRelClass",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "1"
                }
              },
              "b": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "2"
                }
              },
              "c": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "3"
                }
              },
              "d": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "4"
                }
              },
              "e": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "5"
                }
              },
              "f": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "6"
                }
              },
              "g": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "7"
                }
              },
              "h": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "8"
                }
              },
              "i": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "9"
                }
              },
              "j": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "10"
                }
              },
              "k": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "11"
                }
              },
              "l": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "12"
                }
              },
              "m": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "13"
                  }
                ]
              },
              "n": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "14"
                  }
                ]
              },
              "o": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "15"
                  },
                  {
                    "type": "MyRelativeId",
                    "id": "16"
                  },
                  {
                    "type": "MyRelativeId",
                    "id": "17"
                  }
                ]
              },
              "p": {
                "data": [
                ]
              },
              "q": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "18"
                  }
                ]
              },
              "r": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "19"
                  }
                ]
              }
            }
          }
        }
      """).shouldSucceed

      actual shouldBe MyRelClass(
        a = Some(MyRelativeId(1)),
        b = Some(MyRelativeId(2)),
        c = Some(MyRelativeId(3)),
        d = MyRelativeId(4),
        e = MyRelativeId(5),
        f = NotNull(MyRelativeId(6)),
        g = NotNull(MyRelativeId(7)),
        h = NotNull(MyRelativeId(8)),
        i = Some(NotNull(MyRelativeId(9))),
        j = Some(NotNull(MyRelativeId(10))),
        k = Some(NotNull(MyRelativeId(11))),
        l = Some(NotNull(MyRelativeId(12))),
        m = List(MyRelativeId(13)),
        n = List(MyRelativeId(14)),
        o = List(MyRelativeId(15), MyRelativeId(16), MyRelativeId(17)),
        p = Some(Nil),
        q = Some(List(MyRelativeId(18))),
        r = Some(List(MyRelativeId(19))),
      )
    }
  }

  describe("relationships (objects)") {
    implicit val intdec = ContextualDecoder.jstringToJNumber(ContextualDecoder.jnumberToIntDecoder)
    implicit val iddec = semiauto.deriveResourceObjectDecoderForCaseClass[MyRelativeId]()

    it("should succeed without defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyRelClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyRelClass]("""
        {
          "data": {
            "type": "MyRelClass",
            "relationships": {
              "d": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "7"
                }
              },
              "f": {
                "data": null
              },
              "m": {
                "data": []
              }
            }
          },
          "included": [
            {
              "type": "MyRelativeId",
              "id": "7"
            }
          ]
        }
      """).shouldSucceed

      actual.f shouldBe a[Null]
      actual.copy(f = Null) shouldBe MyRelClass(a = None, d = MyRelativeId(7), f = Null, i = None, m = Nil, p = None)
    }

    it("should succeed with defaults") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyRelClass](config =
        Config(
          useDefaultsForMissingFields = false
        )
      )

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyRelClass]("""
        {
          "data": {
            "type": "MyRelClass"
          }
        }
      """).shouldFail

      val nonOptionalFields = Iterable("d", "e", "f", "g", "h", "m", "n", "o")
      actual.length shouldBe nonOptionalFields.size

      actual.iterator.zip(nonOptionalFields.iterator) foreach {
        case (e: UnspecifiedField, fieldName) =>
          e.obj.pointer shouldBe JPointer.Root / "data"
          e.pointers shouldBe NonEmptyChain(JPointer.Root / "relationships" / fieldName)
      }
    }

    it("should succeed full instance") {
      implicit val dec = semiauto.deriveResourceObjectOptionalIdDecoderForCaseClass[MyRelClass]()

      val actual = primaryDataAs[decoding.ResourceObjectOptionalId, MyRelClass]("""
        {
          "data": {
            "type": "MyRelClass",
            "relationships": {
              "a": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "1"
                }
              },
              "b": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "2"
                }
              },
              "c": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "3"
                }
              },
              "d": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "4"
                }
              },
              "e": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "5"
                }
              },
              "f": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "6"
                }
              },
              "g": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "7"
                }
              },
              "h": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "8"
                }
              },
              "i": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "9"
                }
              },
              "j": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "10"
                }
              },
              "k": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "11"
                }
              },
              "l": {
                "data": {
                  "type": "MyRelativeId",
                  "id": "12"
                }
              },
              "m": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "13"
                  }
                ]
              },
              "n": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "14"
                  }
                ]
              },
              "o": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "15"
                  },
                  {
                    "type": "MyRelativeId",
                    "id": "16"
                  },
                  {
                    "type": "MyRelativeId",
                    "id": "17"
                  }
                ]
              },
              "p": {
                "data": [
                ]
              },
              "q": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "18"
                  }
                ]
              },
              "r": {
                "data": [
                  {
                    "type": "MyRelativeId",
                    "id": "19"
                  }
                ]
              }
            }
          },
          "included": [
            {
              "type": "MyRelativeId",
              "id": "1"
            },
            {
              "type": "MyRelativeId",
              "id": "2"
            },
            {
              "type": "MyRelativeId",
              "id": "3"
            },
            {
              "type": "MyRelativeId",
              "id": "4"
            },
            {
              "type": "MyRelativeId",
              "id": "5"
            },
            {
              "type": "MyRelativeId",
              "id": "6"
            },
            {
              "type": "MyRelativeId",
              "id": "7"
            },
            {
              "type": "MyRelativeId",
              "id": "8"
            },
            {
              "type": "MyRelativeId",
              "id": "9"
            },
            {
              "type": "MyRelativeId",
              "id": "10"
            },
            {
              "type": "MyRelativeId",
              "id": "11"
            },
            {
              "type": "MyRelativeId",
              "id": "12"
            },
            {
              "type": "MyRelativeId",
              "id": "13"
            },
            {
              "type": "MyRelativeId",
              "id": "14"
            },
            {
              "type": "MyRelativeId",
              "id": "15"
            },
            {
              "type": "MyRelativeId",
              "id": "16"
            },
            {
              "type": "MyRelativeId",
              "id": "17"
            },
            {
              "type": "MyRelativeId",
              "id": "18"
            },
            {
              "type": "MyRelativeId",
              "id": "19"
            }
          ]
        }
      """).shouldSucceed

      actual shouldBe MyRelClass(
        a = Some(MyRelativeId(1)),
        b = Some(MyRelativeId(2)),
        c = Some(MyRelativeId(3)),
        d = MyRelativeId(4),
        e = MyRelativeId(5),
        f = NotNull(MyRelativeId(6)),
        g = NotNull(MyRelativeId(7)),
        h = NotNull(MyRelativeId(8)),
        i = Some(NotNull(MyRelativeId(9))),
        j = Some(NotNull(MyRelativeId(10))),
        k = Some(NotNull(MyRelativeId(11))),
        l = Some(NotNull(MyRelativeId(12))),
        m = List(MyRelativeId(13)),
        n = List(MyRelativeId(14)),
        o = List(MyRelativeId(15), MyRelativeId(16), MyRelativeId(17)),
        p = Some(Nil),
        q = Some(List(MyRelativeId(18))),
        r = Some(List(MyRelativeId(19))),
      )
    }
  }
}
