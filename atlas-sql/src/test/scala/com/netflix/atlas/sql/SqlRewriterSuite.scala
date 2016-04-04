/*
 * Copyright 2014-2016 Netflix, Inc.
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
package com.netflix.atlas.sql

import com.netflix.atlas.core.model.DataExpr
import com.netflix.atlas.core.model.DataVocabulary
import com.netflix.atlas.core.model.EvalContext
import com.netflix.atlas.core.model.ModelExtractors
import com.netflix.atlas.core.model.Query
import com.netflix.atlas.core.stacklang.Interpreter
import org.scalatest.FunSuite

class SqlRewriterSuite extends FunSuite with SqlRewriter {

  private val interpreter = Interpreter(DataVocabulary.allWords)

  private val step = 60000L
  private val start = 1459702560000L
  private val context = EvalContext(start, start + 60 * step, step)

  def queryExpr(s: String): Query = {
    interpreter.execute(s).stack match {
      case (q: Query) :: _ => q
      case _ => throw new IllegalArgumentException("expected Query")
    }
  }

  def dataExpr(s: String): DataExpr = {
    interpreter.execute(s).stack match {
      case ModelExtractors.DataExprType(e) :: _ => e
      case _ => throw new IllegalArgumentException("expected DataExpr")
    }
  }

  private def format(sql: String): String = {
    //sql.split(' ').map(s => if (keywords contains s) s"\n$s" else s).mkString(" ").trim
    sql
  }

  test("tags: list values for key") {
    val sql = format(tagValues(context, Query.Not(Query.HasKey("atlas.dstype")), "nf_zone"))
    println(sql)
  }

  test("tags/zone?verbose=1") {
    val sql = format(tagValuesWithCount(context, Query.And(Query.Equal("nf.app", "api"), Query.HasKey("class")), "class"))
    println(sql)
  }

  test("graph?q=nf.node,i-8a838609,:eq,:all") {
    val sql = graphDataExpr(context, DataExpr.All(Query.And(Query.Equal("nf.node", "i-8a838609"), Query.Equal("name", "ssCpuUser"))))
    println(sql)
  }

  test("graph?q=nf.node,i-8a838609,:eq,:sum") {
    val sql = graphDataExpr(context, DataExpr.GroupBy(DataExpr.Max(Query.And(Query.Equal("nf.app", "atlas_expr"), Query.Equal("name", "CpuRawUser"))), List("nf.node")))
    println(sql)
  }

  test("query - :true") {
    assert(rewriteQuery(Query.True) === "true")
  }

  test("query - :false") {
    assert(rewriteQuery(Query.False) === "false")
  }

  test("query - :eq") {
    assert(rewriteQuery(Query.Equal("a", "b")) === "tags['a'] = 'b'")
  }

  test("query - :lt") {
    assert(rewriteQuery(Query.LessThan("a", "b")) === "tags['a'] < 'b'")
  }

  test("query - :le") {
    assert(rewriteQuery(Query.LessThanEqual("a", "b")) === "tags['a'] <= 'b'")
  }

  test("query - :gt") {
    assert(rewriteQuery(Query.GreaterThan("a", "b")) === "tags['a'] > 'b'")
  }

  test("query - :ge") {
    assert(rewriteQuery(Query.GreaterThanEqual("a", "b")) === "tags['a'] >= 'b'")
  }

  test("query - :in") {
    assert(rewriteQuery(Query.In("a", List("b", "c", "d"))) === "tags['a'] IN ('b', 'c', 'd')")
  }

  test("query - :has") {
    assert(rewriteQuery(Query.HasKey("a")) === "tags['a'] IS NOT NULL")
  }

  test("query - :not") {
    val sql = rewriteQuery(Query.Not(Query.Equal("a", "1")))
    assert(sql === "NOT (tags['a'] = '1')")
  }

  test("query - :and") {
    val sql = rewriteQuery(Query.And(Query.Equal("a", "1"), Query.Equal("b", "2")))
    assert(sql === "(tags['a'] = '1' AND tags['b'] = '2')")
  }

  test("query - :or") {
    val sql = rewriteQuery(Query.Or(Query.Equal("a", "1"), Query.Equal("b", "2")))
    assert(sql === "(tags['a'] = '1' OR tags['b'] = '2')")
  }
}
