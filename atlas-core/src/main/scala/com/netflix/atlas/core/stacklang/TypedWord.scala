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
package com.netflix.atlas.core.stacklang

import com.netflix.atlas.core.stacklang.ast.DataType
import com.netflix.atlas.core.stacklang.ast.Parameter

/**
  * A word with structured parameter declarations. The `matches` and `signature` methods
  * are automatically derived from the declared parameters and output type. Subclasses
  * must implement `execute` manually.
  *
  * Parameters are declared in user-facing order (deepest stack item first). For example,
  * a word with signature `"a:Int b:Double -- String"` would declare:
  * {{{
  *   def parameters = List(
  *     Parameter("a", "first param", DataType.IntType),
  *     Parameter("b", "second param", DataType.DoubleType)
  *   )
  * }}}
  */
trait TypedWord extends Word {

  override def parameters: List[Parameter]

  override def outputType: Option[DataType]

  override def signature: String = {
    val inputs = parameters.map { p =>
      if (p.name.isEmpty) p.dataType.name else s"${p.name}:${p.dataType.name}"
    }
    val output = outputType.map(_.name).getOrElse("*")
    s"${inputs.mkString(" ")} -- $output"
  }

  override def matches(stack: List[Any]): Boolean = {
    val params = parameters
    if (stack.length < params.length) return false
    // Parameters are in user-facing order (deepest first).
    // Stack list has top-of-stack first. Reverse parameters to align.
    var i = 0
    val reversed = params.reverse
    while (i < reversed.length) {
      if (reversed(i).dataType.extract(stack(i)).isEmpty) return false
      i += 1
    }
    true
  }
}