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
import com.typesafe.config.ConfigFactory
import org.scalatest.FunSuite

class PrestoSqlRewriterSuite extends FunSuite {

  private val config = ConfigFactory.load()
  private val rewriter = new PrestoSqlRewriter(config.getConfigList("atlas.sql.rewriters").get(0))

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

  test("tags/name?q=nf.app,atlas_expr,:eq") {
    val expected =
      """
        |SELECT DISTINCT name AS value
        |FROM atlas.atlas
        |WHERE ((dateint = 20160403 AND hour >= 16) OR (dateint > 20160403)) AND ((dateint = 20160403 AND hour <= 17) OR (dateint < 20160403))
        |  AND nf_app = 'atlas_expr'
        |ORDER BY name
      """.stripMargin.trim
    println(expected)
    val expr = queryExpr("nf.app,atlas_expr,:eq")
    val sql = rewriter.tagValues(context, expr, "name")
    assert(sql.trim === expected)
  }

  test("tags/nf.node?q=nf.app,atlas_expr,:eq") {
    val expected =
      """
        |SELECT DISTINCT nf_node AS value
        |FROM atlas.atlas
        |WHERE ((dateint = 20160403 AND hour >= 16) OR (dateint > 20160403)) AND ((dateint = 20160403 AND hour <= 17) OR (dateint < 20160403))
        |  AND nf_app = 'atlas_expr'
        |ORDER BY nf_node
      """.stripMargin.trim
    println(expected)
    val expr = queryExpr("nf.app,atlas_expr,:eq")
    val sql = rewriter.tagValues(context, expr, "nf.node")
    assert(sql.trim === expected)
  }

  test("tags/name?q=nf.app,atlas_expr,:eq?verbose=1") {
    val expected =
      """
        |SELECT name AS value, COUNT(name) AS count
        |FROM atlas.atlas
        |WHERE ((dateint = 20160403 AND hour >= 16) OR (dateint > 20160403)) AND ((dateint = 20160403 AND hour <= 17) OR (dateint < 20160403))
        |  AND nf_app = 'atlas_expr'
        |GROUP BY name
      """.stripMargin.trim
    val expr = queryExpr("nf.app,atlas_expr,:eq")
    val sql = rewriter.tagValuesWithCount(context, expr, "name")
    println(sql)
    assert(sql.trim === expected)
  }

  test("graph?q=nf.app,atlas_expr,:eq,name,CpuRawUser,:eq,:and,:max,(,nf.node,),:by") {
    val expected =
      """
        |SELECT s10.datetime, MAX(s10.value) AS value, s10.name, s10.nf_app, s10.nf_node
        |FROM (
        |SELECT s1.datetime, s1.value, s1.tags, s1.name, s1.nf_ami, s1.nf_app, s1.nf_asg, s1.nf_cluster, s1.nf_node, s1.nf_region, s1.nf_zone
        |FROM (
        |  SELECT (s0.dateint * 10000 + s0.hour * 100 + s0.minute) AS datetime, s0.value, s0.tags, s0.name, s0.nf_ami, s0.nf_app, s0.nf_asg, s0.nf_cluster, s0.nf_node, s0.nf_region, s0.nf_zone
        |  FROM (
        |    SELECT dateint, hour, (minute - 1) AS minute, cast(value as double) AS value, tags, name, nf_ami, nf_app, nf_asg, nf_cluster, nf_node, nf_region, nf_zone
        |    FROM atlas.atlas
        |    CROSS JOIN UNNEST(cast(cast("values" as json) as array<varchar>)) WITH ORDINALITY AS t (value, minute)
        |    WHERE ((dateint = 20160403 AND hour >= 16) OR (dateint > 20160403)) AND ((dateint = 20160403 AND hour <= 17) OR (dateint < 20160403))
        |      AND (nf_app = 'atlas_expr' AND name = 'CpuRawUser')) AS s0
        |  WHERE NOT IS_NAN(s0.value)) AS s1
        |WHERE s1.datetime >= 201604031656 AND s1.datetime < 201604031756
        |     ) AS s10
        |GROUP BY s10.datetime, s10.name, s10.nf_app, s10.nf_node
        |ORDER BY s10.datetime
      """.stripMargin.trim
    val expr = dataExpr("nf.app,atlas_expr,:eq,name,CpuRawUser,:eq,:and,:max,(,nf.node,),:by")
    val sql = rewriter.graphDataExpr(context, expr)
    assert(sql.trim === expected)
  }

  test("graph?q=nf.app,atlas_expr,:eq,name,http.req.attempt,:eq,:and,:max,(,client,status,),:by") {
    val expected =
      """
        |SELECT s10.datetime, MAX(s10.value) AS value, s10.tags['client'] AS client, s10.name, s10.nf_app, s10.tags['status'] AS status
        |FROM (
        |SELECT s1.datetime, s1.value, s1.tags, s1.name, s1.nf_ami, s1.nf_app, s1.nf_asg, s1.nf_cluster, s1.nf_node, s1.nf_region, s1.nf_zone
        |FROM (
        |  SELECT (s0.dateint * 10000 + s0.hour * 100 + s0.minute) AS datetime, s0.value, s0.tags, s0.name, s0.nf_ami, s0.nf_app, s0.nf_asg, s0.nf_cluster, s0.nf_node, s0.nf_region, s0.nf_zone
        |  FROM (
        |    SELECT dateint, hour, (minute - 1) AS minute, cast(value as double) AS value, tags, name, nf_ami, nf_app, nf_asg, nf_cluster, nf_node, nf_region, nf_zone
        |    FROM atlas.atlas
        |    CROSS JOIN UNNEST(cast(cast("values" as json) as array<varchar>)) WITH ORDINALITY AS t (value, minute)
        |    WHERE ((dateint = 20160403 AND hour >= 16) OR (dateint > 20160403)) AND ((dateint = 20160403 AND hour <= 17) OR (dateint < 20160403))
        |      AND (nf_app = 'atlas_expr' AND name = 'http.req.attempt')) AS s0
        |  WHERE NOT IS_NAN(s0.value)) AS s1
        |WHERE s1.datetime >= 201604031656 AND s1.datetime < 201604031756
        |     ) AS s10
        |GROUP BY s10.datetime, s10.tags['client'], s10.name, s10.nf_app, s10.tags['status']
        |ORDER BY s10.datetime
      """.stripMargin.trim
    val expr = dataExpr("nf.app,atlas_expr,:eq,name,http.req.attempt,:eq,:and,:max,(,client,status,),:by")
    val sql = rewriter.graphDataExpr(context, expr)
    println(sql)
    assert(sql.trim === expected)
  }

  test("graph?q=nf.app,atlas_expr,:eq,name,http.req.attempt,:eq,:and,:max,(,client,status,),:by,2,:head") {
    val expected =
      """
        |SELECT s10.datetime, MAX(s10.value) AS value, s10.tags['client'] AS client, s10.name, s10.nf_app, s10.tags['status'] AS status
        |FROM (
        |SELECT s1.datetime, s1.value, s1.tags, s1.name, s1.nf_ami, s1.nf_app, s1.nf_asg, s1.nf_cluster, s1.nf_node, s1.nf_region, s1.nf_zone
        |FROM (
        |  SELECT (s0.dateint * 10000 + s0.hour * 100 + s0.minute) AS datetime, s0.value, s0.tags, s0.name, s0.nf_ami, s0.nf_app, s0.nf_asg, s0.nf_cluster, s0.nf_node, s0.nf_region, s0.nf_zone
        |  FROM (
        |    SELECT dateint, hour, (minute - 1) AS minute, cast(value as double) AS value, tags, name, nf_ami, nf_app, nf_asg, nf_cluster, nf_node, nf_region, nf_zone
        |    FROM atlas.atlas
        |    CROSS JOIN UNNEST(cast(cast("values" as json) as array<varchar>)) WITH ORDINALITY AS t (value, minute)
        |    WHERE ((dateint = 20160403 AND hour >= 16) OR (dateint > 20160403)) AND ((dateint = 20160403 AND hour <= 17) OR (dateint < 20160403))
        |      AND (nf_app = 'atlas_expr' AND name = 'http.req.attempt')) AS s0
        |  WHERE NOT IS_NAN(s0.value)) AS s1
        |WHERE s1.datetime >= 201604031656 AND s1.datetime < 201604031756
        |     ) AS s10
        |GROUP BY s10.datetime, s10.tags['client'], s10.name, s10.nf_app, s10.tags['status']
        |ORDER BY s10.datetime
        |LIMIT 2
      """.stripMargin.trim
    val expr = dataExpr("nf.app,atlas_expr,:eq,name,http.req.attempt,:eq,:and,:max,(,client,status,),:by,2,:head")
    val sql = rewriter.graphDataExpr(context, expr)
    println(sql)
    assert(sql.trim === expected)
  }

}
