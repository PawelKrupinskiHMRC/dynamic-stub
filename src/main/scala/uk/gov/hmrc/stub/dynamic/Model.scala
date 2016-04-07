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

class DataSupplier(data: Map[ConfigKey[_ <: ValueType], ValueType]) {
  def apply[T](key: SingleConfigKey[T]): T = data(key).asInstanceOf[SingleValue[T]].value
  def apply(key: ObjectConfigKey): DataSupplier = new DataSupplier(data(key).asInstanceOf[ObjectValue].data)
  def apply(key: MultiConfigKey): Seq[DataSupplier] = data(key).asInstanceOf[ListValue].data.map(new DataSupplier(_))
  def isDefinedAt(key: ConfigKey[_ <: ValueType]): Boolean = data.isDefinedAt(key)
}


sealed trait ValueType
case class SingleValue[T](value: T) extends ValueType
case class ObjectValue(data: Map[ConfigKey[_ <: ValueType], ValueType]) extends ValueType
case class ListValue(data: Seq[Map[ConfigKey[_ <: ValueType], ValueType]]) extends ValueType

case class Expectation(testId: String,
                       endpoint: EndPoint,
                       data: Map[ConfigKey[_ <: ValueType], ValueType],
                       delay: Option[Long],
                       resultCode: Option[Int],
                       timeToLive: Option[Long]) {
  def keys = data.keys.toSet
}
