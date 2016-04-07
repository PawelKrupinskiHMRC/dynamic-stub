/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.stub.dynamic

import java.net.URI

import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.libs.json.Reads._
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.stub.dynamic.repository.DynamicRepository

import scala.concurrent.Future



class EndpointSpec extends UnitSpec with WithFakeApplication with ScalaFutures {

  private val mongoCache = DynamicRepository()

  trait Setup {

    private val expectationCllr = new DynamicStubDataController{
      override def expectationUrl(id: BSONObjectID) = Call("GET", "/aa/bb")
      override lazy val cache = mongoCache
      override def endpoint = TestEndpoint
    }

    def prime: String

    def defaultResponse: Results.Status = Results.NotFound

    val stubbed = new Controller with ServiceStubResponse {
      override lazy val cache = mongoCache

      def endpoint = Action.async{implicit request =>
        stubbedResponse("/foo", TestEndpoint) {
          Future.successful(defaultResponse)
        }
      }
    }


    await(expectationCllr.recordService(FakeRequest().withJsonBody(Json.parse(prime))))

    def response: Result = await(stubbed.endpoint(FakeRequest()))

    lazy val stubbedResponseBody = contentAsString(response)

  }

  "Priming an Endpoint" should {
    "return the templated reply" in new Setup {

      val prime = """{
          |"testId":"123",
          |"data":{
          |  "id":"123",
          |  "husband": {"value": {
          |    "name": {"value": "joe bloggs"},
          |    "age": {"value":34}
          |  }}
          |}}
        """.stripMargin

      stubbedResponseBody should be ("")

    }
  }
}

object TestEndpoint extends EndPoint{

  val name = SingleConfigKey("name")
  val age = SingleConfigKey.asType[Int]("age")
  val dob = SingleConfigKey("date-of-birth", jodaLocalDateReads("yyyy-MM-dd"))

  val husband = ObjectConfigKey("husband", Seq(name, age, dob))
  val wife = ObjectConfigKey("husband", Seq(name, age, dob))
  val children = MultiConfigKey("husband", Seq(name, age, dob))

  override def keys = Seq(husband, wife, children)
  override def bodyTemplate = views.txt.family(_)
  override def urlTemplates = Seq(new UrlTemplate(Set()) {
    override def apply(data: DataSupplier) = new URI("/foo")
  })
  override def defaults = Map(husband -> ObjectValue(Map(name -> SingleValue("Joe"))))
}
