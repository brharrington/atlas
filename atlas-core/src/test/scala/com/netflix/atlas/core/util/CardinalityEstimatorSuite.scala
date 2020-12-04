package com.netflix.atlas.core.util

import java.util.UUID

import org.openjdk.jol.info.GraphLayout
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Using

class CardinalityEstimatorSuite extends AnyFunSuite {

  // just to get warnings printed up front
  GraphLayout.parseInstance(this)

  private def check(name: String, estimator: CardinalityEstimator, values: Seq[AnyRef]): Unit = {
    var errorSum = 0.0
    values.zipWithIndex.foreach {
      case (v, i) =>
        estimator.update(v)
        //if (i % 10 == 0) {
          val actual = i + 1
          val estimate = estimator.cardinality
          val percentError = 100.0 * math.abs(estimate - actual) / actual
          errorSum += percentError
          //println(s"actual = $actual, estimate = $estimate, percent error = $percentError")
        //}
    }
    val avgPctError = errorSum / values.size
    val footprint = GraphLayout.parseInstance(estimator).totalSize()
    println(f"$name%25s: $avgPctError%8.2f%% $footprint%8d bytes")
  }

  /*private def sha(r: AnyRef): Int = {
    Hash.sha1(r.toString).intValue()
  }*/

  private def check(name: String, values: Seq[String]): Unit = {
    import CardinalityEstimator._
    println(name)
    println("-" * 51)
    check("custom default", hyperLogLog(_.hashCode()), values)
    check("custom lowbias", hyperLogLog(r => Hash.lowbias32(r.hashCode())), values)
    //check("custom sha1", hyperLogLog(sha), values)
    check("cpc string", cpcString, values)
    check("cpc lowbias", cpcLowBias, values)
    check("hll string", hllString, values)
    check("hll lowbias", hllLowBias, values)
    println()
    println()
  }

  test("int strings") {
    check("int strings", (0 until 100_000).map(_.toString))
  }

  test("uuids") {
    check("uuids", (0 until 100_000).map(_ => UUID.randomUUID().toString))
  }

  test("words") {
    import Streams._
    val values = Using.resource(fileIn("/usr/share/dict/words")) { in =>
      lines(in).toList
    }
    check("words", values)
  }
}
