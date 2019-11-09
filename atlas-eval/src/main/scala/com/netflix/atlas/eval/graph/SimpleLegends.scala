package com.netflix.atlas.eval.graph

import com.netflix.atlas.core.model.Query
import com.netflix.atlas.core.model.Query.KeyValueQuery
import com.netflix.atlas.core.model.StyleExpr

/**
  * Helper to analyze a set of expressions and try to automatically set a reasonable
  * human readable legend.
  */
private[graph] object SimpleLegends {

  def generate(exprs: List[StyleExpr]): List[StyleExpr] = {
    // Extract key/value pairs from all expressions
    val kvs = exprs
      .map(e => e -> extractKeyValues(e))
      .filterNot(_._2.isEmpty)
      .toMap

    if (kvs.isEmpty) {
      exprs
    } else {
      // Figure out the unique set
      val common = kvs.values.reduce(intersect)
      exprs.map { expr =>
        if (hasExplicitLegend(expr)) {
          expr
        } else {
          val kv = kvs(expr)
          val uniq = diff(kv, common)
          if (uniq.nonEmpty)
            generateLegend(expr, uniq)
          else if (common.nonEmpty)
            generateLegend(expr, common)
          else
            expr
        }
      }
    }
  }

  private def hasExplicitLegend(expr: StyleExpr): Boolean = {
    expr.settings.contains("legend")
  }

  private def withLegend(expr: StyleExpr, legend: String): StyleExpr = {
    val label = if (expr.offset > 0L) s"$legend (offset=$$atlas.offset)" else legend
    expr.copy(settings = expr.settings + ("legend" -> label))
  }

  private def keyValues(query: Query): Map[String, String] = {
    query match {
      case Query.And(q1, q2)            => keyValues(q1) ++ keyValues(q2)
      case Query.Equal(k, v)            => Map(k -> v)
      case Query.LessThan(k, v)         => Map(k -> v)
      case Query.LessThanEqual(k, v)    => Map(k -> v)
      case Query.GreaterThan(k, v)      => Map(k -> v)
      case Query.GreaterThanEqual(k, v) => Map(k -> v)
      case Query.Regex(k, v)            => Map(k -> v)
      case Query.RegexIgnoreCase(k, v)  => Map(k -> v)
      case Query.Not(q: KeyValueQuery)  => keyValues(q).map(t => t._1 -> s"!${t._2}")
      case _                            => Map.empty
    }
  }

  private def generateLegend(expr: StyleExpr, kv: Map[String, String]): StyleExpr = {
    if (expr.expr.isGrouped) {
      val fmt = expr.expr.finalGrouping.mkString("$", " $", "")
      withLegend(expr, fmt)
    } else if (kv.contains("name")) {
      withLegend(expr, kv("name"))
    } else {
      val legend = kv.toList.sortWith(_._1 < _._1).map(_._2).mkString(" ")
      withLegend(expr, legend)
    }
  }

  private def extractKeyValues(expr: StyleExpr): Map[String, String] = {
    val dataExprs = expr.expr.dataExprs
    if (dataExprs.isEmpty)
      Map.empty
    else
      dataExprs.map(de => keyValues(de.query)).reduce(intersect)
    /*expr.expr.dataExprs.distinct match {
      case de :: Nil => keyValues(de.query)
      case _ => Map.empty
    }*/
  }

  private def intersect(m1: Map[String, String], m2: Map[String, String]): Map[String, String] = {
    (m1.toSet intersect m2.toSet).toMap
  }

  private def diff(m1: Map[String, String], m2: Map[String, String]): Map[String, String] = {
    (m1.toSet diff m2.toSet).toMap
  }
}
