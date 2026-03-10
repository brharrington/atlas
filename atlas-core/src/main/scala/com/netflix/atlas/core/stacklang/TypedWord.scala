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

import scala.collection.immutable.ArraySeq

import com.netflix.atlas.core.stacklang.ast.DataType
import com.netflix.atlas.core.stacklang.ast.Parameter

/**
  * A word with structured parameter declarations. The `matches`, `signature`, and
  * `execute` methods are automatically derived from the declared parameters and outputs.
  *
  * Parameters are declared in user-facing order (deepest stack item first). For example,
  * a word with signature `"a:Int b:Double -- String"` would declare:
  * {{{
  *   def parameters = ArraySeq(
  *     Parameter("a", "first param", DataType.IntType),
  *     Parameter("b", "second param", DataType.DoubleType)
  *   )
  *   def outputs = ArraySeq(DataType.StringType)
  * }}}
  *
  * Subclasses implement `execute(context, params)` where `params` is an indexed sequence
  * of already-extracted values in user-facing order and the consumed items have been
  * removed from the context stack.
  */
trait TypedWord extends Word {

  /** Structured parameter declarations in user-facing order (deepest stack item first). */
  def parameters: IndexedSeq[Parameter]

  /** Output types produced by this word. Empty means the word does not push any results. */
  def outputs: IndexedSeq[DataType]

  override def signature: String = {
    val inputs = parameters.map { p =>
      if (p.name.isEmpty) p.dataType.name else s"${p.name}:${p.dataType.name}"
    }
    val output = outputs.map(_.name).mkString(" ")
    s"${inputs.mkString(" ")} -- $output"
  }

  override def matches(stack: List[Any]): Boolean = {
    val params = parameters
    if (stack.length < params.length) return false
    // Parameters are in user-facing order (deepest first).
    // Stack list has top-of-stack first. Reverse parameters to align.
    val n = params.length
    var i = 0
    while (i < n) {
      if (params(n - 1 - i).dataType.extract(stack(i)).isEmpty) return false
      i += 1
    }
    true
  }

  /**
    * Extracts typed parameters from the stack and delegates to
    * [[execute(context:Context,params:IndexedSeq[Any])*]]. The consumed items are removed
    * from the context stack before the call. After execution, [[transformResult]] is called
    * to allow post-processing.
    */
  final override def execute(context: Context): Context = {
    val params = parameters
    val n = params.length
    val extracted = new Array[Any](n)
    var i = 0
    while (i < n) {
      extracted(i) = params(n - 1 - i).dataType.extract(context.stack(i)).get
      i += 1
    }
    val args = ArraySeq.unsafeWrapArray(extracted).reverse
    val remainingStack = context.stack.drop(n)
    val result = execute(context.copy(stack = remainingStack), args)
    transformResult(context.stack.take(n), args, result)
  }

  /**
    * Post-process the result of execution. Called after
    * [[execute(context:Context,params:IndexedSeq[Any])*]] with the original raw stack
    * values (top-of-stack first), the extracted parameters, and the result context.
    * Override to implement cross-cutting concerns like style passthrough. Default
    * implementation returns the result unchanged.
    *
    * @param rawStackValues
    *     The original stack values consumed by this word, in stack order
    *     (top-of-stack first).
    * @param params
    *     The extracted parameters in user-facing order (deepest first).
    * @param result
    *     The context returned by execute.
    */
  protected def transformResult(
    rawStackValues: List[Any],
    params: IndexedSeq[Any],
    result: Context
  ): Context = result

  /**
    * Execute the word with extracted parameters. The context stack has already had
    * the consumed items removed. `params` are in user-facing order (deepest first),
    * with values already coerced by their DataType extractors.
    */
  def execute(context: Context, params: IndexedSeq[Any]): Context
}
