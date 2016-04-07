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

import play.api.libs.json.Reads._
import play.api.libs.json._

trait JsonFormats {
  def endpoint: EndPoint

  implicit lazy val expectationReads: Reads[Expectation] = {

    implicit val dataReads = Config.mapReads(endpoint.keys)

    Reads[Expectation] { jv =>
      val testId = (jv \ "testId").as[String]
      val data = (jv \ "data").as[Map[ConfigKey[_ <: ValueType], ValueType]]
      val delay = (jv \ "delay").asOpt[Long]
      val resultCode = (jv \ "resultCode").asOpt[Int]
      val timeToLive = (jv \ "timeToLive").asOpt[Long]
      JsSuccess(Expectation(testId, endpoint, data, delay, resultCode, timeToLive))
    }
  }
}
