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
import munit.FunSuite

class TypedWordSuite extends FunSuite {

  //
  // DataType extraction
  //

  test("AnyType: extracts anything") {
    assertEquals(DataType.AnyType.extract("hello"), Some("hello"))
    assertEquals(DataType.AnyType.extract(42), Some(42))
    assertEquals(DataType.AnyType.extract(List(1, 2)), Some(List(1, 2)))
  }

  test("IntType: extracts Int from Int") {
    assertEquals(DataType.IntType.extract(42), Some(42))
  }

  test("IntType: extracts Int from String") {
    assertEquals(DataType.IntType.extract("42"), Some(42))
  }

  test("IntType: returns None for non-Int") {
    assertEquals(DataType.IntType.extract("abc"), None)
    assertEquals(DataType.IntType.extract(3.14), None)
  }

  test("DoubleType: extracts Double from Double") {
    assertEquals(DataType.DoubleType.extract(3.14), Some(3.14))
  }

  test("DoubleType: extracts Double from String") {
    assertEquals(DataType.DoubleType.extract("3.14"), Some(3.14))
  }

  test("DoubleType: returns None for non-Double") {
    assertEquals(DataType.DoubleType.extract("abc"), None)
  }

  test("StringType: extracts String") {
    assertEquals(DataType.StringType.extract("hello"), Some("hello"))
  }

  test("StringType: returns None for non-String") {
    assertEquals(DataType.StringType.extract(42), None)
  }

  //
  // TypedWord: matches
  //

  private val swapWord = new TypedWord {
    def name: String = "swap"
    def summary: String = "Swap the top two items"
    def examples: List[String] = List("a,b,:swap")
    override def parameters: List[Parameter] = List(
      Parameter("a", "first item", DataType.AnyType),
      Parameter("b", "second item", DataType.AnyType)
    )
    override def outputType: Option[DataType] = Some(DataType.AnyType)
    def execute(context: Context): Context = {
      context.stack match {
        case a :: b :: rest => context.copy(stack = b :: a :: rest)
        case _              => invalidStack
      }
    }
  }

  private val addIntsWord = new TypedWord {
    def name: String = "add-ints"
    def summary: String = "Add two integers"
    def examples: List[String] = List("1,2,:add-ints")
    override def parameters: List[Parameter] = List(
      Parameter("a", "first operand", DataType.IntType),
      Parameter("b", "second operand", DataType.IntType)
    )
    override def outputType: Option[DataType] = Some(DataType.IntType)
    def execute(context: Context): Context = {
      context.stack match {
        case Extractors.IntType(b) :: Extractors.IntType(a) :: rest =>
          context.copy(stack = (a + b) :: rest)
        case _ => invalidStack
      }
    }
  }

  test("matches: returns true when stack has matching types") {
    assert(swapWord.matches(List("a", "b")))
    assert(swapWord.matches(List(1, 2, 3))) // extra items ok
    assert(addIntsWord.matches(List("2", "1")))
    assert(addIntsWord.matches(List(2, 1)))
  }

  test("matches: returns false when stack too shallow") {
    assert(!swapWord.matches(List("a")))
    assert(!swapWord.matches(Nil))
    assert(!addIntsWord.matches(List(1)))
  }

  test("matches: returns false when types don't match") {
    assert(!addIntsWord.matches(List("abc", "1")))
    assert(!addIntsWord.matches(List("1", "abc")))
  }

  //
  // TypedWord: signature
  //

  test("signature: generated from named parameters") {
    assertEquals(swapWord.signature, "a:Any b:Any -- Any")
  }

  test("signature: generated with typed parameters") {
    assertEquals(addIntsWord.signature, "a:Int b:Int -- Int")
  }

  test("signature: unnamed parameter uses type only") {
    val word = new TypedWord {
      def name: String = "test"
      def summary: String = ""
      def examples: List[String] = Nil
      override def parameters: List[Parameter] = List(
        Parameter("", "anonymous", DataType.IntType)
      )
      override def outputType: Option[DataType] = None
      def execute(context: Context): Context = context
    }
    assertEquals(word.signature, "Int -- *")
  }

  //
  // TypedWord: end-to-end execution
  //

  test("execute: swap word works end-to-end") {
    val interpreter = Interpreter(List(swapWord))
    val result = interpreter.execute("a,b,:swap")
    assertEquals(result.stack, List("a", "b"))
  }

  test("execute: add-ints word with string coercion") {
    val interpreter = Interpreter(List(addIntsWord))
    val result = interpreter.execute("3,4,:add-ints")
    assertEquals(result.stack, List(7))
  }

  //
  // Word default parameters
  //

  test("Word: parameters defaults to Nil") {
    val word = new Word {
      def name: String = "test"
      def signature: String = "-- test"
      def summary: String = ""
      def examples: List[String] = Nil
      def matches(stack: List[Any]): Boolean = true
      def execute(context: Context): Context = context
    }
    assertEquals(word.parameters, Nil)
    assertEquals(word.outputType, None)
  }
}