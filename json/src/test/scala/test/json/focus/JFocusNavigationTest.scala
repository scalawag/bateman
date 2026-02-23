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

package test.json.focus

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import test.json.BatemanTestBase

class JFocusNavigationTest extends BatemanTestBase {

  val json: JRootFocus[JObject] = parseAs[JObject]("""
    {
      "users": [
        {
          "name": "alice",
          "scores": [95, 87]
        },
        {
          "name": "bob",
          "scores": [70]
        }
      ]
    }
  """)

  // Types spelled out to verify the full chain at each level.
  type Root = JRootFocus[JObject]
  type UsersField = JFieldFocus[JArray, Root]
  type UserItem = JItemFocus[JAny, UsersField]
  type UserItemObj = JItemFocus[JObject, UsersField]
  type NameField = JFieldFocus[JAny, UserItemObj]

  describe("deep field navigation") {
    it("should navigate through nested objects and preserve the type chain") {
      val usersField: JFieldFocus[JAny, Root] = json.field("users").shouldSucceed
      val usersArray: JFieldFocus[JArray, Root] = usersField.asArray.shouldSucceed
      usersArray.value.items should have size 2
      usersArray.parent shouldBe json
    }
  }

  describe("deep item navigation") {
    it("should navigate into arrays and preserve the type chain") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItem = usersArray.item(0).shouldSucceed
      firstUser.parent shouldBe usersArray
      firstUser.index shouldBe 0
    }
  }

  describe("mixed navigation") {
    it("should navigate object > field(array) > item(object) > field and preserve the full chain") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItemObj = usersArray.item(0).shouldSucceed.asObject.shouldSucceed
      val nameField: NameField = firstUser.field("name").shouldSucceed

      nameField.asString.shouldSucceed.value.value shouldBe "alice"
      nameField.parent shouldBe firstUser
      firstUser.parent shouldBe usersArray
      usersArray.parent shouldBe json
    }

    it("should navigate to a deeply nested array item") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItemObj = usersArray.item(0).shouldSucceed.asObject.shouldSucceed
      val scoresField: JFieldFocus[JArray, UserItemObj] = firstUser.field("scores").shouldSucceed.asArray.shouldSucceed
      val firstScore: JItemFocus[JAny, JFieldFocus[JArray, UserItemObj]] = scoresField.item(0).shouldSucceed

      firstScore.asNumber.shouldSucceed.value.toBigDecimal shouldBe BigDecimal(95)
      firstScore.parent shouldBe scoresField
    }
  }

  describe("root from depth") {
    it("should return JRootFocus[JObject] from a deeply nested field focus") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItemObj = usersArray.item(0).shouldSucceed.asObject.shouldSucceed
      val nameField: NameField = firstUser.field("name").shouldSucceed

      val result = nameField.root
      result.value shouldBe json.value
    }

    it("should return JRootFocus[JObject] from a deeply nested item focus") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItemObj = usersArray.item(0).shouldSucceed.asObject.shouldSucceed
      val scoresField: JFieldFocus[JArray, UserItemObj] = firstUser.field("scores").shouldSucceed.asArray.shouldSucceed
      val firstScore: JItemFocus[JAny, JFieldFocus[JArray, UserItemObj]] = scoresField.item(0).shouldSucceed

      val result = firstScore.root
      result.value shouldBe json.value
    }
  }

  describe("modify at depth") {
    it("should modify a deep field via weak ops and update the root document") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItemObj = usersArray.item(0).shouldSucceed.asObject.shouldSucceed
      val nameField: NameField = firstUser.field("name").shouldSucceed

      // Use weak replace at depth (strong modify requires deep implicit chains).
      val modified: JFocus[JString] = (nameField: JFocus[JAny]).replace(JString(nameField.value.stripLocation.asInstanceOf[JString].value.toUpperCase))

      modified.value.value shouldBe "ALICE"
      modified.pointer shouldBe nameField.pointer
      modified.root.value.shouldHaveNoLocations

      // Verify the root document reflects the change.
      val newRoot = modified.root
      newRoot.asObject.shouldSucceed
        .field("users").shouldSucceed.asArray.shouldSucceed
        .item(0).shouldSucceed.asObject.shouldSucceed
        .field("name").shouldSucceed.asString.shouldSucceed
        .value.value shouldBe "ALICE"

      // Verify the other user is untouched.
      newRoot.asObject.shouldSucceed
        .field("users").shouldSucceed.asArray.shouldSucceed
        .item(1).shouldSucceed.asObject.shouldSucceed
        .field("name").shouldSucceed.asString.shouldSucceed
        .value.value shouldBe "bob"
    }

    it("should modify a shallow field with strong types preserved") {
      val usersField: JFieldFocus[JArray, Root] = json.field("users").shouldSucceed.asArray.shouldSucceed
      val modified: JFieldFocus[JObject, Root] = usersField.modify(_ => JObject.Empty)
      modified.value shouldBe JObject.Empty
      modified.name.value shouldBe "users"
    }
  }

  describe("delete at depth") {
    it("should delete a deep field via weak ops") {
      val usersArray: UsersField = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItemObj = usersArray.item(0).shouldSucceed.asObject.shouldSucceed
      val nameField: NameField = firstUser.field("name").shouldSucceed
      val weakNameField: JFocus[JAny] = nameField

      // Use weak delete at depth via JFocus.
      val result: JFocus[JAny] = weakNameField.delete().shouldSucceed
      // Trying to type the result strongly should fail
      assertTypeError("""
        val strong: UserItemObj = weakNameField.delete().shouldSucceed
      """)


      // The parent object should no longer have a "name" field.
      result.asObject.shouldSucceed.value.fieldList.map(_.name.value) should not contain "name"
      result.root.value.shouldHaveNoLocations
    }

    it("should delete a shallow field with strong types preserved") {
      val usersField: JFieldFocus[JArray, Root] = json.field("users").shouldSucceed.asArray.shouldSucceed
      val result: Root = usersField.delete()
      result.value.fieldList.map(_.name.value) should not contain "users"
      result.root.value.shouldHaveNoLocations
    }

    it("should delete a shallow array item with strong types preserved") {
      val usersField: JFieldFocus[JArray, Root] = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser: UserItem = usersField.item(0).shouldSucceed
      val result: UsersField = firstUser.delete()
      result.value.items should have size 1
    }
  }

  describe("parentOption") {
    it("should return None for a root focus") {
      json.parentOption shouldBe None
    }

    it("should return Some for a field focus") {
      val f = json.field("users").shouldSucceed
      f.parentOption shouldBe Some(json)
    }

    it("should return Some for an item focus") {
      val usersArray = json.field("users").shouldSucceed.asArray.shouldSucceed
      val firstUser = usersArray.item(0).shouldSucceed
      firstUser.parentOption shouldBe Some(usersArray)
    }
  }
}
