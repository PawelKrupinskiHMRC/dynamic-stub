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

import play.api.libs.json._
import play.twirl.api.TxtFormat


object Config {
  def mapReads(keys: Seq[ConfigKey[_ <: ValueType]]): Reads[Map[ConfigKey[_ <: ValueType], ValueType]] = Reads[Map[ConfigKey[_ <: ValueType], ValueType]] { jv =>

    val result = for {
      key <- keys
      value: ValueType <- key.read(jv)
    } yield key -> value

    JsSuccess(result.toMap)
  }


}

sealed trait ConfigKey[U <: ValueType] {
  protected def name: String
  protected def reads: Reads[U]
  def read(jv: JsValue): Option[U] = (jv \ name).asOpt[U](reads)

  protected def extract(children: Seq[ConfigKey[_ <: ValueType]])(js: JsValue) =
    js.as[Map[ConfigKey[_ <: ValueType], ValueType]](Config.mapReads(children))

}

/**
  * converts json like:  {"value": "leaf-value"}
  * to an instance of StringValue which is like a String
  */
case class SingleConfigKey[T](name: String, valueReads: Reads[T]) extends ConfigKey[SingleValue[T]] {
  def reads = Reads[SingleValue[T]] { jv =>
    val maybeT = (jv \ "value").asOpt[T](valueReads)
    maybeT.fold[JsResult[SingleValue[T]]](JsError("no value"))(value => JsSuccess(SingleValue(value)))
  }
}
case object SingleConfigKey {
  def apply(name: String): SingleConfigKey[String] = SingleConfigKey(name, Reads.StringReads)
  def asType[T](name: String)(implicit valueReads: Reads[T]): SingleConfigKey[T] = SingleConfigKey(name, valueReads)
}

/**
  * converts json like:
  * {"value" : {
  *   "a-key" : {"value": "leaf-value-a"},
  *   "b-key": {"value": "leaf-value-b}
  * }}
  * to an instance of ObjectValue which is like a map
  */
case class ObjectConfigKey(name: String, children: Seq[ConfigKey[_ <: ValueType]]) extends ConfigKey[ObjectValue] {
  def reads =  Reads[ObjectValue] { jv =>
      jv \ "value" match {
        case obj@JsObject(_) => JsSuccess(ObjectValue(extract(children)(obj)))
        case _ => JsError()
      }
    }
}

/**
  * converts json like:
  * {"values" :[
  *   {"a-key" : {"value": "leaf-value-1a"}, "b-key": {"value": "leaf-value-1b}},
  *   {"a-key" : {"value": "leaf-value-2a"}, "b-key": {"value": "leaf-value-2b}},
  * ...
  * ]}
  * to an instance of ListValue which is like a sequence of maps
  */
case class MultiConfigKey(name: String, children: Seq[ConfigKey[_ <: ValueType]]) extends ConfigKey[ListValue] {
  def reads =  Reads[ListValue] { jv =>
      jv \ "values" match {
        case JsArray(elements) => JsSuccess(ListValue(elements.map(extract(children))))
        case _ => JsError()
      }
    }
}


abstract class UrlTemplate(keys: Set[ConfigKey[_ <: ValueType]]) {
  def isConfiguredFor(expectation: Expectation) = {
    keys.intersect(expectation.keys) == keys
  }

  def apply(data: DataSupplier): URI

}


trait EndPoint {
  def keys: Seq[ConfigKey[_ <: ValueType]]
  def defaults: Map[ConfigKey[_ <: ValueType], ValueType]
  def bodyTemplate: DataSupplier => TxtFormat.Appendable
  def urlTemplates: Seq[UrlTemplate]
}

