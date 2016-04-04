package com.netflix.atlas.sql

import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import com.netflix.atlas.core.model.DataExpr
import com.netflix.atlas.core.model.DataExpr.AggregateFunction
import com.netflix.atlas.core.model.EvalContext
import com.netflix.atlas.core.model.Expr
import com.netflix.atlas.core.model.Query
import com.netflix.atlas.core.util.StringMatcher

/**
  * Rewrites an Atlas stack langauge expression to SQL. This is mostly intended for
  * mapping Atlas queries to long term storage sources that can be queried using
  * HiveQL, SparkSQL, and Presto.
  *
  * ```
  * dateint
  * hour
  * name    string
  * nf_app  string
  * nf_cluster
  * nf_asg
  * nf_ami
  * nf_node
  * nf_region
  * nf_zone
  * tags        Map[String, String]
  * values      Array[Double]
  * ```
  */
trait SqlRewriter {

  private val dateIntFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
  private val hourFormatter = DateTimeFormatter.ofPattern("HH")
  private val minuteFormatter = DateTimeFormatter.ofPattern("mm")

  def tagKeys(context: EvalContext, query: Query): String = {
    s"SELECT DISTINCT todo AS key FROM $tableName WHERE ${initialTimeFilter(context)} AND ${rewriteQuery(query)}"
  }

  def tagValues(context: EvalContext, query: Query, key: String): String = {
    val k = rewriteTagKey(key)
    s"""
       |SELECT DISTINCT $k AS value
       |FROM $tableName
       |WHERE ${initialTimeFilter(context)}
       |  AND ${rewriteQuery(query)}
       |ORDER BY $k
     """.stripMargin
  }

  def tagValuesWithCount(context: EvalContext, query: Query, key: String): String = {
    val k = rewriteTagKey(key)
    s"""
       |SELECT $k AS value, COUNT($k) AS count
       |FROM $tableName
       |WHERE ${initialTimeFilter(context)}
       |  AND ${rewriteQuery(query)}
       |GROUP BY $k
     """.stripMargin
  }

  def graphDataExpr(context: EvalContext, expr: DataExpr): String = rewriteDataExpr(context, expr)

  protected def notSupported(expr: Expr): Nothing = {
    throw new UnsupportedOperationException(s"unsupported expression: $expr")
  }

  protected def tableName: String = "atlas.atlas"

  /**
    * If any of the tag keys are explicitly broken out into separate columns, then those need
    * to be specified. For the map the key is the value the user would enter in a stack
    * expression and the value is the name of the column.
    */
  protected def keyColumns: Map[String, String] = Map("name" -> "name")

  /**
    * By default it does nothing. The default restrictions on tag key/values should work fine
    * in SQL.
    */
  protected def escape(v: String): String = v

  protected def dateInt(t: Long): String = {
    ZonedDateTime.ofInstant(Instant.ofEpochMilli(t), ZoneOffset.UTC).format(dateIntFormatter)
  }

  protected def dateIntAndHour(t: Long, hourOp: String, dtOp: String): String = {
    val dt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(t), ZoneOffset.UTC)
    val d = dt.format(dateIntFormatter)
    val h = dt.format(hourFormatter)
    s"((dateint = $d AND hour $hourOp $h) OR (dateint $dtOp $d))"
  }

  protected def dateHourMinute(t: Long, prefix: String, op: String): String = {
    val dt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(t), ZoneOffset.UTC)
    val d = dt.format(dateIntFormatter)
    val h = dt.format(hourFormatter)
    val m = dt.format(minuteFormatter)
    s"${prefix}datetime $op $d$h$m"
  }

  /**
    * This is the first time filter that will get applied as part of the WHERE
    * clause. For models where each row contains data for a longer time span
    * this should be used to restrict to the row and `unnestedTimeFilter` will
    * get used to filter after unnesting the values.
    */
  protected def initialTimeFilter(context: EvalContext): String = {
    dateIntAndHour(context.start, ">=", ">") + " AND " + dateIntAndHour(context.end, "<=", "<")
  }

  protected def unnestedTimeFilter(context: EvalContext, prefix: String = ""): String = {
    dateHourMinute(context.start, prefix, ">=") + " AND " + dateHourMinute(context.end, prefix, "<")
  }

  protected def rewriteTagKey(k: String): String = {
    keyColumns.getOrElse(k, s"tags['${escape(k)}']")
  }

  protected def rewriteTagValue(v: String): String = s"'${escape(v)}'"

  protected def selectKeyAs(k: String): String = {
    val simpleKey = k.replaceAll("[.-]", "_")
    keyColumns.getOrElse(k, s"tags['${escape(k)}'] AS $simpleKey")
  }

  protected def rewriteNotQuery(query: Query.Not): String = s"NOT (${rewriteQuery(query.q)})"

  protected def rewriteAndQuery(query: Query.And): String = {
    val q1 = rewriteQuery(query.q1)
    val q2 = rewriteQuery(query.q2)
    s"($q1 AND $q2)"
  }

  protected def rewriteOrQuery(query: Query.Or): String = {
    val q1 = rewriteQuery(query.q1)
    val q2 = rewriteQuery(query.q2)
    s"($q1 OR $q2)"
  }

  protected def rewriteKeyValueQuery(query: Query.ScalarKeyValueQuery, op: String): String = {
    rewriteTagKey(query.k) + op + rewriteTagValue(query.v)
  }

  protected def rewriteInQuery(query: Query.In): String = {
    val vs = query.vs.map(rewriteTagValue).mkString("(", ", ", ")")
    rewriteTagKey(query.k) + " IN " + vs
  }

  protected def rewriteHasKeyQuery(query: Query.HasKey): String = {
    rewriteTagKey(query.k) + " IS NOT NULL"
  }

  protected def rewritePrefixQuery(k: String, prefix: String): String = {
    rewriteTagKey(k) + " LIKE '" + escape(prefix) + "%'"
  }

  protected def rewriteIndexOfQuery(k: String, substr: String): String = {
    rewriteTagKey(k) + " LIKE '%" + escape(substr) + "%'"
  }

  /**
    * Maps regular expression queries to the appropriate like clause. Right now it only
    * supports simple prefix and substring modes that can be expressed using LIKE. Support
    * for arbitrary regex and ignore case modes will take some additional investigation.
    */
  protected def rewritePatternQuery(query: Query.PatternQuery): String = query.pattern match {
    case StringMatcher.All                => rewriteTagKey(query.k) + " LIKE '%'"
    case StringMatcher.StartsWith(prefix) => rewritePrefixQuery(query.k, prefix)
    case StringMatcher.IndexOf(substr)    => rewriteIndexOfQuery(query.k, substr)
    case _                                => notSupported(query)
  }

  /**
    * Ideally the query expression would be rewritten so true/false constants are no longer
    * present except at the root level. At the root true is often used as the default for
    * tag queries if the user did not specify a more explicit restriction. For example, show
    * all available tag keys.
    */
  protected def rewriteTrueQuery: String = "true"

  /**
    * Ideally the query expression would be rewritten so true/false constants are no longer
    * present except at the root level. At the root true is often used as the default for
    * tag queries if the user did not specify a more explicit restriction. For example, show
    * all available tag keys.
    */
  protected def rewriteFalseQuery: String = "false"

  protected def rewriteQuery(query: Query): String = query match {
    case q: Query.Not              => rewriteNotQuery(q)
    case q: Query.And              => rewriteAndQuery(q)
    case q: Query.Or               => rewriteOrQuery(q)
    case q: Query.Equal            => rewriteKeyValueQuery(q, " = ")
    case q: Query.LessThan         => rewriteKeyValueQuery(q, " < ")
    case q: Query.LessThanEqual    => rewriteKeyValueQuery(q, " <= ")
    case q: Query.GreaterThan      => rewriteKeyValueQuery(q, " > ")
    case q: Query.GreaterThanEqual => rewriteKeyValueQuery(q, " >= ")
    case q: Query.In               => rewriteInQuery(q)
    case q: Query.HasKey           => rewriteHasKeyQuery(q)
    case q: Query.PatternQuery     => rewritePatternQuery(q)
    case Query.True                => rewriteTrueQuery
    case Query.False               => rewriteFalseQuery
  }

  /**
    * How to filter out NaN values seems to vary quite a bit. The default implementation
    * assumes an IS_NAN function.
    */
  protected def isNotNaN(k: String): String = s"NOT IS_NAN($k)"

  /**
    * Expand the values array so we have a simple table one row per datapoint. By default
    * we assume the SQL engine supports an UNNEST WITH ORDINALITY clause that will index
    * starting at 1. As an example see the following guide for postgres:
    *
    * http://michael.otacoo.com/postgresql-2/postgres-9-4-feature-highlight-with-ordinality/
    *
    * After expanding the date we need to perform additional time filtering for cases where
    * we only need a subset of the nested values.
    */
  protected def unnest(context: EvalContext, query: Query): String = {
    val columns = "value" :: "tags" :: keyColumns.values.toList.sortWith(_ < _)
    val cols = columns.mkString(", ")
    val s0cols = columns.map(c => "s0." + c).mkString(", ")
    val s1cols = columns.map(c => "s1." + c).mkString(", ")
    s"""
       |SELECT s1.datetime, $s1cols
       |FROM (
       |  SELECT (s0.dateint * 10000 + s0.hour * 100 + s0.minute) AS datetime, $s0cols
       |  FROM (
       |    SELECT dateint, hour, (minute - 1) AS minute, $cols
       |    FROM $tableName
       |    CROSS JOIN UNNEST("values") WITH ORDINALITY AS t (value, minute)
       |    WHERE ${initialTimeFilter(context)}
       |      AND ${rewriteQuery(query)}) AS s0
       |  WHERE ${isNotNaN("s0.value")}) AS s1
       |WHERE ${unnestedTimeFilter(context, "s1.")}
     """.stripMargin
  }

  protected def rewriteFetchAll(context: EvalContext, q: Query): String = unnest(context, q)

  protected def toAggr(af: AggregateFunction): String = af match {
    case DataExpr.Consolidation(f, _) => toAggr(f)
    case _: DataExpr.Sum              => "SUM"
    case _: DataExpr.Count            => "COUNT"
    case _: DataExpr.Min              => "MIN"
    case _: DataExpr.Max              => "MAX"
  }

  protected def rewriteGroupBy(
      context: EvalContext,
      af: DataExpr.AggregateFunction,
      keys: List[String]): String = {
    val aggr = toAggr(af)
    val q = af.query
    val ks = (Query.exactKeys(q) ++ keys).toList.sortWith(_ < _)

    val selectCols = if (ks.isEmpty) "" else {
      ks.sortWith(_ < _).map(k => "s10." + selectKeyAs(k)).mkString(", ", ", ", "")
    }
    val cols = if (ks.isEmpty) "" else {
      ks.sortWith(_ < _).map(k => "s10." + rewriteTagKey(k)).mkString(", ", ", ", "")
    }

    val inner = s"""
       |SELECT s10.datetime, $aggr(s10.value) AS value$selectCols
       |FROM (${unnest(context, q)}) AS s10
       |GROUP BY s10.datetime$cols
       |ORDER BY s10.datetime
     """.stripMargin

    // If we have an explicit group by in the stack expression we need to ensure that all
    // values are present. SQL will keep undefined columns in the output with a value of NULL.
    if (keys.isEmpty) inner else {
      val condition = keys.map(k => s"$k IS NOT NULL").mkString(" AND ")
      val s11cols = ks.sortWith(_ < _).map(k => "s11." + rewriteTagKey(k)).mkString(", ", ", ", "")
      s"""
         |SELECT s11.datetime, s11.value$s11cols
         |FROM ($inner) AS s11
         |WHERE $condition
       """.stripMargin
    }
  }

  /**
    * Basic rewriting for data expressions. Offsets and consolidation are currently ignored.
    */
  protected def rewriteDataExpr(context: EvalContext, expr: DataExpr): String = expr match {
    case DataExpr.All(q, _)             => rewriteFetchAll(context, q)
    case DataExpr.GroupBy(af, ks)       => rewriteGroupBy(context, af, ks)
    case DataExpr.Head(de, n)           => rewriteDataExpr(context, de).trim + s"\nLIMIT $n"
    case af: DataExpr.AggregateFunction => rewriteGroupBy(context, af, Nil)
  }
}
