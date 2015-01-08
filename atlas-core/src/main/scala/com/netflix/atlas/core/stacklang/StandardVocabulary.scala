/*
 * Copyright 2015 Netflix, Inc.
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


object StandardVocabulary extends Vocabulary {

  val words = List(
    Call,
    Clear,
    Drop,
    Dup,
    Each,
    Format,
    Get,
    Map,
    Over,
    ReverseRot,
    Rot,
    Set,
    Swap,
    ToList,

    Macro("2over", List(":over", ":over"), List("a,b")),
    Macro("fcall", List(":get", ":call"), List("duplicate,(,:dup,),:set,a,duplicate")),
    Macro("sset", List(":swap", ":set"), List("a,b"))
  )

  /** A word defined as a sequence of other commands. */
  case class Macro(name: String, body: List[Any], examples: List[String] = Nil) extends Word {

    override def matches(stack: List[Any]): Boolean = true

    override def execute(context: Context): Context = {
      context.interpreter.execute(body, context)
    }

    override def summary: String =
      s"""
        |Shorthand equivalent to writing: `${Interpreter.toString(body.reverse)}`
      """.stripMargin.trim

    override def signature: String = "? -- ?"
  }

  /** Pop a list off the stack and execute it as a program. */
  object Call extends Word {
    override def name: String = "call"

    override def matches(stack: List[Any]): Boolean = stack match {
      case (_: List[_]) :: _ => true
      case _                 => false
    }

    override def execute(context: Context): Context = {
      context.stack match {
        case (vs: List[_]) :: stack =>
          context.interpreter.execute(vs, context.copy(stack = stack))
        case _ => invalidStack
      }
    }

    override def summary: String =
      """
        |Pop a list off the stack and execute it as a program.
      """.stripMargin.trim

    override def signature: String = "? List -- ?"

    override def examples: List[String] = List("(,a,)")
  }

  /** Remove all items from the stack. */
  object Clear extends SimpleWord {
    override def name: String = "clear"

    protected def matcher: PartialFunction[List[Any], Boolean] = { case _ => true }

    protected def executor: PartialFunction[List[Any], List[Any]] = { case _ => Nil }

    override def summary: String =
      """
        |Remove all items from the stack.
      """.stripMargin.trim

    override def signature: String = "* -- <empty>"

    override def examples: List[String] = List("a,b,c")
  }

  /** Remove the item on the top of the stack. */
  object Drop extends SimpleWord {
    override def name: String = "drop"

    protected def matcher: PartialFunction[List[Any], Boolean] = { case vs => vs.nonEmpty }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case _ :: s => s
    }

    override def summary: String =
      """
        |Remove the item on the top of the stack.
      """.stripMargin.trim

    override def signature: String = "a -- "

    override def examples: List[String] = List("a,b,c", "ERROR:")
  }

  /** Duplicate the item on the top of the stack. */
  object Dup extends SimpleWord {
    override def name: String = "dup"

    protected def matcher: PartialFunction[List[Any], Boolean] = { case vs => vs.nonEmpty }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case s @ v :: _ => v :: s
    }

    override def summary: String =
      """
        |Duplicate the item on the top of the stack.
      """.stripMargin.trim

    override def signature: String = "a -- a a"

    override def examples: List[String] = List("a", "a,b", "ERROR:")
  }

  /** For each item in a list push it on the stack and apply a function. */
  object Each extends Word {
    override def name: String = "each"

    override def matches(stack: List[Any]): Boolean = stack match {
      case (_: List[_]) :: (_: List[_]) :: _ => true
      case _                                 => false
    }

    override def execute(context: Context): Context = {
      context.stack match {
        case (f: List[_]) :: (vs: List[_]) :: stack =>
          vs.reverse.foldLeft(context.copy(stack = stack)) { (c, v) =>
            c.interpreter.execute(f, c.copy(stack = v :: c.stack))
          }
        case _ => invalidStack
      }
    }

    override def summary: String =
      """
        |For each item in a list push it on the stack and apply a function.
      """.stripMargin.trim

    override def signature: String = "items:List f:List -- f(items[0]) ... f(items[N])"

    override def examples: List[String] = List("(,a,b,),(,:dup,)")
  }

  /** Format a string. */
  object Format extends SimpleWord {
    override def name: String = "format"

    protected def matcher: PartialFunction[List[Any], Boolean] = {
      case (_: List[_]) :: (_: String) :: _ => true
    }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case (vs: List[_]) :: (s: String) :: stack => s.format(vs: _*) :: stack
    }

    override def summary: String =
      """
        |Format a string using a printf [style pattern]
        |(https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html).
      """.stripMargin.trim

    override def signature: String = "pattern:String args:List -- str:String"

    override def examples: List[String] = List("foo%s,(,bar,)")
  }

  /** Get the value of a variable and push it on the stack. */
  object Get extends Word {
    override def name: String = "get"

    override def matches(stack: List[Any]): Boolean = stack match {
      case (_: String) :: _ => true
      case _                => false
    }

    override def execute(context: Context): Context = {
      context.stack match {
        case (k: String) :: _ => context.copy(stack = context.variables(k) :: context.stack.tail)
        case _                => invalidStack
      }
    }

    override def summary: String =
      """
        |Get the value of a variable and push it on the stack.
      """.stripMargin.trim

    override def signature: String = "k -- vars[k]"

    override def examples: List[String] = List("k,v,:set,k")
  }

  /** Create a new list by applying a function to all elements of a list. */
  object Map extends Word {
    override def name: String = "map"

    override def matches(stack: List[Any]): Boolean = stack match {
      case (_: List[_]) :: (_: List[_]) :: _ => true
      case _                                 => false
    }

    override def execute(context: Context): Context = {
      context.stack match {
        case (f: List[_]) :: (vs: List[_]) :: stack =>
          val init = context.copy(stack = stack)
          val res = vs.foldLeft(List.empty[Any] -> init) { case ((rs, c), v) =>
            val rc = c.interpreter.execute(f, c.copy(stack = v :: c.stack))
            (rc.stack.head :: rs) -> rc.copy(stack = rc.stack.tail)
          }
          res._2.copy(stack = res._1.reverse :: res._2.stack)
        case _ => invalidStack
      }
    }

    override def summary: String =
      """
        |Create a new list by applying a function to all elements of a list.
      """.stripMargin.trim

    override def signature: String = "items:List f:List -- List(f(items[0]) ... f(items[N]))"

    override def examples: List[String] = List("(,a%s,b%s,),(,(,.netflix.com,),:format,)")
  }

  /** Copy the item in the second position on the stack to the top. */
  object Over extends SimpleWord {
    override def name: String = "over"

    protected def matcher: PartialFunction[List[Any], Boolean] = {
      case (_: Any) :: (_: Any) :: _ => true
    }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case s @ _ :: v :: _ => v :: s
    }

    override def summary: String =
      """
        |Copy the item in the second position on the stack to the top.
      """.stripMargin.trim

    override def signature: String = "a b -- a b a"

    override def examples: List[String] = List("a,b")
  }

  /** Rotate the stack so that the item at the bottom is now at the top. */
  object Rot extends SimpleWord {
    override def name: String = "rot"

    protected def matcher: PartialFunction[List[Any], Boolean] = { case vs => vs.nonEmpty }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case s => s.last :: s.take(s.size - 1)
    }

    override def summary: String =
      """
        |Rotate the stack so that the item at the bottom is now at the top.
      """.stripMargin.trim

    override def signature: String = "a ... b -- ... b a"

    override def examples: List[String] = List("a,b,c,d")
  }

  /** Rotate the stack so that the item at the top is now at the bottom. */
  object ReverseRot extends SimpleWord {
    override def name: String = "-rot"

    protected def matcher: PartialFunction[List[Any], Boolean] = { case vs => vs.nonEmpty }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case v :: vs => vs ::: List(v)
    }

    override def summary: String =
      """
        |Rotate the stack so that the item at the top is now at the bottom.
      """.stripMargin.trim

    override def signature: String = "* a b -- b * a"

    override def examples: List[String] = List("a,b,c,d")
  }

  /** Set the value of a variable. */
  object Set extends Word {
    override def name: String = "set"

    override def matches(stack: List[Any]): Boolean = stack match {
      case (_: Any) :: (_: String) :: _ => true
      case _                            => false
    }

    override def execute(context: Context): Context = {
      context.stack match {
        case (v: Any) :: (k: String) :: vs =>
          Context(context.interpreter, vs, context.variables + (k -> v))
        case _ => invalidStack
      }
    }

    override def summary: String =
      """
        |Set the value of a variable.
      """.stripMargin.trim

    override def signature: String = "k v -- "

    override def examples: List[String] = List("k,v")
  }

  /** Swap the top two items on the stack. */
  object Swap extends SimpleWord {
    override def name: String = "swap"

    protected def matcher: PartialFunction[List[Any], Boolean] = {
      case (_: Any) :: (_: Any) :: _ => true
    }

    protected def executor: PartialFunction[List[Any], List[Any]] = {
      case (b: Any) :: (a: Any) :: vs => a :: b :: vs
    }

    override def summary: String =
      """
        |Swap the top two items on the stack.
      """.stripMargin.trim

    override def signature: String = "a b -- b a"

    override def examples: List[String] = List("a,b")
  }

  /** Pop all items off the stack and push them as a list. */
  object ToList extends Word {
    override def name: String = "list"

    override def matches(stack: List[Any]): Boolean = true

    override def execute(context: Context): Context = {
      context.copy(stack = List(context.stack))
    }

    override def summary: String =
      """
        |Pop all items off the stack and push them as a list.
      """.stripMargin.trim

    override def signature: String = "* -- List(*)"

    override def examples: List[String] = List("a,b", "")
  }
}
