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

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SQLContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSuite


class RewriteSuite extends FunSuite with BeforeAndAfterAll {

  val conf = new SparkConf().setMaster("local").setAppName("test")
  val sc = new SparkContext(conf)
  val context = SQLContext.getOrCreate(sc)
  val data = context.read.json("atlas-sql/src/test/resources/data.json").registerTempTable("atlas")

  override def afterAll(): Unit = {
    sc.stop()
  }

  test("md5") {
    val results = context.sql("select name from atlas")
    results.foreach { row =>
      println(row)
    }
  }

  test("aggr") {
    val results = context.sql("select name, explode(values) as v from atlas")
    results.foreach { row =>
      println(row)
    }
  }

  test("filter nan") {
    val results = context.sql(
      """
        |select name, v
        |from (
        |  select name, explode(values) as v from atlas) foo
        |where cast(v as string) != 'NaN'
      """.stripMargin)
    results.foreach { row =>
      println(row)
    }
  }

  test("up vs read timeout") {
    val results = context.sql(
      """
        |select name, dateint, hour, minute, sum(s1.value) as value
        |from (
        |  (
        |    select tags['id'] as name, dateint, hour, minute, value
        |    from atlas.atlas_p
        |    lateral view posexplode(values) t as minute, value
        |    where dateint > 20151201
        |      and name = 'NumErrors'
        |      and tags['class'] = 'NIWSErrorStats'
        |      and (
        |        tags['id'] = 'api-allrequests-ErrorCode-NIWSErrorCode-CONNECT_EXCEPTION'
        |        or
        |        tags['id'] = 'api-allrequests-ErrorCode-NIWSErrorCode-READ_TIMEOUT_EXCEPTION'
        |      )
        |  ) union all (
        |    select name, dateint, hour, minute, value
        |    from atlas.atlas_p
        |    lateral view posexplode(values) t as minute, value
        |    where dateint > 20151201
        |      and name = 'DiscoveryStatus_api_UP'
        |  )
        |) s1
        |where cast(s1.value as string) != 'NaN'
        |group by name, dateint, hour, minute
        |order by dateint, hour, minute;
      """.stripMargin)
    results.foreach { row =>
      println(row)
    }
  }

  test("aggr2") {
    val results = context.sql("select name, i, v from atlas LATERAL view posexplode(values) t AS i, v")
    results.foreach { row =>
      println(row)
    }
  }
}
