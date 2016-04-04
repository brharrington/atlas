package com.netflix.atlas.sql

import com.netflix.atlas.core.model.EvalContext
import com.netflix.atlas.core.model.Query
import com.typesafe.config.Config

/**
  * Created by brharrington on 4/3/16.
  */
class PrestoSqlRewriter(config: Config) extends SqlRewriter {
  override val tableName: String = config.getString("table")

  override val keyColumns: Map[String, String] = {
    import scala.collection.JavaConversions._
    val columns = config.getConfigList("key-columns").toList
    columns.map(c => c.getString("key") -> c.getString("column")).toMap
  }

  /**
    * Presto doesn't handle NaN values in the array and gives a JSON parsing failure.
    *
    * ```
    * Caused by: com.fasterxml.jackson.core.JsonParseException: Current token (VALUE_STRING) not
    * numeric, can not use numeric value accessors
    * ```
    *
    * To work around this problem we cast to an array of strings and then cast back to a
    * double in the projection list of the select clause.
    */
  override protected def unnest(context: EvalContext, query: Query): String = {
    val columns = "tags" :: keyColumns.values.toList.sortWith(_ < _)
    val cols = columns.mkString(", ")
    val s0cols = columns.map(c => "s0." + c).mkString(", ")
    val s1cols = columns.map(c => "s1." + c).mkString(", ")
    s"""
       |SELECT s1.datetime, s1.value, $s1cols
       |FROM (
       |  SELECT (s0.dateint * 10000 + s0.hour * 100 + s0.minute) AS datetime, s0.value, $s0cols
       |  FROM (
       |    SELECT dateint, hour, (minute - 1) AS minute, cast(value as double) AS value, $cols
       |    FROM $tableName
       |    CROSS JOIN UNNEST(cast(cast("values" as json) as array<varchar>)) WITH ORDINALITY AS t (value, minute)
       |    WHERE ${initialTimeFilter(context)}
       |      AND ${rewriteQuery(query)}) AS s0
       |  WHERE ${isNotNaN("s0.value")}) AS s1
       |WHERE ${unnestedTimeFilter(context, "s1.")}
     """.stripMargin
  }
}
