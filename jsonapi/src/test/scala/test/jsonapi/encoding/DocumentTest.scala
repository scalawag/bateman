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

import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{Null, Nullable}
import org.scalawag.bateman.jsonapi.encoding.{Document, Error, ErrorSource, Jsonapi, Relationship, ResourceIdentifier, ResourceObject, RichLink}
import test.json.BatemanTestBase

class DocumentTest extends BatemanTestBase {
  describe("Document") {
    it("should encode data") {
      val json = json"""
        {
          "jsonapi": {
            "version": "3"
          },
          "data": {
            "type": "thing",
            "id": "23",
            "attributes": {
              "a": 17
            },
            "relationships": {
              "b": {
                "data": {
                  "type": "other",
                  "id": "B"
                },
                "meta": {
                  "created_on": "2023-03-24"
                },
                "links": {
                  "self": {
                    "href": "http://example.com/other/B",
                    "meta": {
                      "key": "K0080"
                    }
                  }
                }
              }
            },
            "meta": {
              "level": "top_secret"
            },
            "links": {
              "next": "http://example.com/next"
            }
          },
          "included": [
            {
              "type": "other",
              "id": "B",
              "attributes": {
                "foo": "bar"
              },
              "meta": {
                "version": 4
              }
            }
          ],
          "meta": {
            "service_version": "1.0.22"
          },
          "links": {
            "about": "http://example.com/about"
          }
        }
      """

      Document
        .withData(
          ResourceObject("thing", "23")
            .withAttribute("a", 17)
            .withRelationship(
              "b",
              Relationship(ResourceIdentifier("other", "B"))
                .withLink("self", RichLink("http://example.com/other/B").withMeta("key", "K0080"))
                .withMeta("created_on", "2023-03-24")
            )
            .withMeta("level", "top_secret")
            .withLink("next", "http://example.com/next")
        )
        .withIncluded(ResourceObject("other", "B").withAttribute("foo", "bar").withMeta("version", 4))
        .withMeta("service_version", "1.0.22")
        .withLink("about", "http://example.com/about")
        .withJsonapi(Jsonapi(version = Some("3"))) shouldEncodeTo json
    }

    it("should encode errors") {
      val json = json"""
        {
          "jsonapi": {
            "version": "3"
          },
          "errors": [
            {
              "id": "E001",
              "status": "403",
              "code": "pseudo_error",
              "title": "Badness 10000",
              "detail": "Sometimes, bad things happen to good code.",
              "source": {
                "pointer": "/data/attributes/a"
              },
              "links": {
                "about": "http://example.com/errors/E001"
              },
              "meta": {
                "more_info": "bar"
              }
            },
            {
              "source": {
                "header": "Content-Type"
              }
            },
            {
              "source": {
                "parameter": "filter[name]"
              }
            }
          ],
          "meta": {
            "service_version": "1.0.22"
          },
          "links": {
            "about": "http://example.com/about"
          }
        }
      """

      Document
        .withErrors(
          Error()
            .withId("E001")
            .withStatus(403)
            .withCode("pseudo_error")
            .withTitle("Badness 10000")
            .withDetail("Sometimes, bad things happen to good code.")
            .withMeta("more_info", "bar")
            .withLink("about", "http://example.com/errors/E001")
            .withSource(ErrorSource.Pointer("/data/attributes/a")),
          Error().withSource(ErrorSource.Header("Content-Type")),
          Error().withSource(ErrorSource.Parameter("filter[name]")),
        )
        .withMeta("service_version", "1.0.22")
        .withLink("about", "http://example.com/about")
        .withJsonapi(Jsonapi(version = Some("3"))) shouldEncodeTo json
    }

    it("should encode metadata-only") {
      val json = json"""
        {
          "jsonapi": {
            "version": "3"
          },
          "meta": {
            "service_version": "1.0.22"
          },
          "links": {
            "about": "http://example.com/about"
          }
        }
      """

      Document
        .withMeta("service_version", "1.0.22")
        .withLink("about", "http://example.com/about")
        .withJsonapi(Jsonapi(version = Some("3"))) shouldEncodeTo json
    }

    it("should create a document with array data") {
      Document.withData(List(ResourceIdentifier("A", "B"))) shouldEncodeTo json"""
        {
          "data": [
            {
              "type": "A",
              "id": "B"
            }
          ]
        }
      """
    }

    it("should create a document with nullable data") {
      Document.withData(Null: Nullable[ResourceIdentifier]) shouldEncodeTo json"""
        {
          "data": null
        }
      """
    }

    it("should abstractly encoder data document") {
      (Document.withData(Null: Nullable[ResourceIdentifier]): Document) shouldEncodeTo json"""
        {
          "data": null
        }
      """
    }

    it("should abstractly encoder error document") {
      (Document.withError(Error(Some("A"))): Document) shouldEncodeTo json"""
        {
          "errors": [{"id": "A"}]
        }
      """
    }

    it("should abstractly encoder meta-only document") {
      (Document.withMeta("a", 4): Document) shouldEncodeTo json"""
        {
          "meta": {
            "a": 4
          }
        }
      """
    }
  }

  describe("MetadataDocument") {
    it("should allow metadata to error document conversion") {
      Document.withMetas("v" -> 1.toJAny).withError(Error()) shouldEncodeTo json"""
        {
          "errors": [{}],
          "meta": {"v": 1}
        }
      """
    }

    it("should allow metadata to object data document conversion") {
      Document.withMeta("v", 1).withData(ResourceIdentifier("A", "B")) shouldEncodeTo json"""
        {
          "data": {"type": "A", "id": "B"},
          "meta": {"v": 1}
        }
      """
    }

    it("should allow metadata to array data document conversion") {
      Document.withMeta("v", 1).withData(List(ResourceIdentifier("A", "B"))) shouldEncodeTo json"""
        {
          "data": [{"type": "A", "id": "B"}],
          "meta": {"v": 1}
        }
      """
    }

    it("should allow metadata to null data document conversion") {
      Document.withMeta("v", 1).withData(Null: Nullable[ResourceIdentifier]) shouldEncodeTo json"""
        {
          "data": null,
          "meta": {"v": 1}
        }
      """
    }

    it("should add meta to meta document") {
      Document.withMeta("v", 1).withMeta("a", 4) shouldEncodeTo json"""
        {
          "meta": {
            "v": 1,
            "a": 4
          }
        }
      """
    }

    it("should add metas to meta document") {
      Document.withMeta("v", 1).withMetas("a" -> "b".toJAny, "c" -> true.toJAny) shouldEncodeTo json"""
        {
          "meta": {
            "v": 1,
            "a": "b",
            "c": true
          }
        }
      """
    }

  }

  describe("ErrorDocument") {
    it("should add error to error document") {
      (Document.withError(Error(Some("A"))).withError(Error(Some("B"))): Document) shouldEncodeTo json"""
        {
          "errors": [{"id": "A"}, {"id": "B"}]
        }
      """
    }

    it("should add errors to error document") {
      (Document
        .withError(Error(Some("A")))
        .withErrors(Error(Some("B")), Error(Some("C"))): Document) shouldEncodeTo json"""
        {
          "errors": [{"id": "A"}, {"id": "B"}, {"id": "C"}]
        }
      """
    }
  }

  describe("Relationship") {
    it("should build object data") {
      Relationship(ResourceIdentifier("A", "B")) shouldEncodeTo json"""
        {
          "data": {
            "type": "A",
            "id": "B"
          }
        }
      """
    }

    it("should build null data") {
      Relationship(Null: Nullable[ResourceIdentifier]) shouldEncodeTo json"""
        {
          "data": null
        }
      """
    }

    it("should build array data") {
      Relationship(List(ResourceIdentifier("A", "B"))) shouldEncodeTo json"""
        {
          "data": [
            {
              "type": "A",
              "id": "B"
            }
          ]
        }
      """
    }
  }

  describe("ResourceObject") {
    it("should build resource object with lid") {
      ResourceObject.withLid("A", "B") shouldEncodeTo json"""
        {
          "type": "A",
          "lid": "B"
        }
      """
    }
  }

  describe("ResourceIdentifier") {
    it("should build resource identifier with lid") {
      ResourceIdentifier.withLid("A", "B") shouldEncodeTo json"""
        {
          "type": "A",
          "lid": "B"
        }
      """
    }

    it("should add metadata") {
      ResourceIdentifier.withLid("A", "B").withMeta("c", "d") shouldEncodeTo json"""
        {
          "type": "A",
          "lid": "B",
          "meta": {
            "c": "d"
          }
        }
      """
    }
  }
}
