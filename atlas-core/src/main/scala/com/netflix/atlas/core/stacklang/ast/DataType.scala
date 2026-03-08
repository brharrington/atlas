/*
 * Copyright 2014-2026 Netflix, Inc.
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
package com.netflix.atlas.core.stacklang.ast

import com.netflix.atlas.core.stacklang.Extractors

/**
  * Represents a type for a stack language parameter. Provides a name for display
  * and an extractor for type checking and coercion. Custom types can be created by
  * extending this trait.
  */
trait DataType {

  /** Display name for this type (e.g. "Int", "Double", "TimeSeriesExpr"). */
  def name: String

  /**
    * Attempt to extract/coerce a stack value to this type. Returns `Some` with the
    * extracted value if the value is compatible, `None` otherwise.
    */
  def extract(value: Any): Option[Any]
}

object DataType {

  /** Matches any value on the stack. */
  case object AnyType extends DataType {
    def name: String = "Any"
    def extract(value: Any): Option[Any] = Some(value)
  }

  /** Matches values that can be coerced to Int (strings, constants, etc.). */
  case object IntType extends DataType {
    def name: String = "Int"
    def extract(value: Any): Option[Any] = Extractors.IntType.unapply(value)
  }

  /** Matches values that can be coerced to Double (strings, constants, etc.). */
  case object DoubleType extends DataType {
    def name: String = "Double"
    def extract(value: Any): Option[Any] = Extractors.DoubleType.unapply(value)
  }

  /** Matches String values. */
  case object StringType extends DataType {
    def name: String = "String"
    def extract(value: Any): Option[Any] = value match {
      case s: String => Some(s)
      case _         => None
    }
  }
}