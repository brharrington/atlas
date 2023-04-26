package com.netflix.atlas.chart.graphics

import com.netflix.atlas.chart.model.LineDef
import com.netflix.atlas.chart.model.LineStyle
import com.netflix.atlas.core.model.TagKey
import com.netflix.spectator.api.histogram.PercentileBuckets

import scala.collection.immutable.ArraySeq

case class Heatmap(
             lines: List[LineDef],
             xaxis: TimeAxis,
             yaxis: ValueAxis,
             y1: Int,
             y2: Int
             ) {
  require(lines.nonEmpty)

  val step: Long = lines.head.data.data.step

  private val start = xaxis.start
  private val end = xaxis.end

  private val minValue = yaxis.min
  private val maxValue = yaxis.max

  val ticks: ArraySeq[ValueTick] = ArraySeq.unsafeWrapArray(yaxis.ticks(y1, y2).toArray)

  private val counts: Array[Array[Double]] = computeCounts()

  val (minCount: Double, maxCount: Double) = {
    var min = Double.MaxValue
    var max = Double.MinValue
    var i = 0
    while (i < counts.length) {
      var j = 0
      while (j < counts(i).length) {
        val v = counts(i)(j)
        min = math.min(min, v)
        max = math.max(max, v)
        j += 1
      }
      i += 1
    }
    min -> max
  }

  private def findBucket(value: Double): Int = {
    // When using explicit bounds, some values may not be visible
    if (value < minValue || value > maxValue) {
      return -1
    }

    // Find the matching bucket
    var i = 0
    while (i < ticks.length) {
      if (value < ticks(i).v)
        return i
      i += 1
    }
    ticks.length
  }

  private def computeWeight(mn: Double, mx: Double, cellMin: Double, cellMax: Double): Double = {
    if (cellMax < mn || cellMin > mx) {
      // No overlap, use a weight of zero
      0.0
    } else {
      val lower = math.max(mn, cellMin)
      val upper = math.min(mx, cellMax)
      (upper - lower) / (mx - mn)
    }
  }

  private def updateCounts(mn: Double, mx: Double, cnt: Double, counts: Array[Double]): Unit = {
    var cellMin = minValue
    var i = 0
    while (i < ticks.length) {
      val cellMax = ticks(i).v
      if (cellMax > mx) {
        // Stop early once passed the max of the bucket range
        return
      }
      counts(i) += cnt * computeWeight(mn, mx, cellMin, cellMax)
      cellMin = cellMax
      i += 1
    }
    counts(i) += cnt * computeWeight(mn, mx, cellMin, maxValue)
  }

  private def computeCounts(): Array[Array[Double]] = {
    val w = ((end - start) / step).toInt
    val h = ticks.length + 1
    val counts = Array.fill(w, h)(0.0)

    lines.foreach { line =>
      val pctRange = Heatmap.percentileBucketRange(line.data.tags)
      val ts = line.data.data
      var t = start
      while (t < end) {
        val x = ((t - start) / step).toInt
        val v = ts(t)
        if (!v.isNaN) {
          if (pctRange.isDefined) {
            // For percentile, spread the amount from the value across the cells in the
            // graph that overlap the range of the percentile bucket
            val (mn, mx) = pctRange.get
            val cnt = v * step / 1000.0
            updateCounts(mn, mx, cnt, counts(x))
          } else {
            // For normal lines, just update the counts based on the position of the value
            val y = findBucket(v)
            if (y >= 0) counts(x)(y) += 1.0
          }
        }
        t += step
      }
    }
    counts
  }

  def numberOfValueBuckets: Int = ticks.length + 1

  def count(t: Long, y: Int): Double = {
    val x = ((t - start) / step).toInt
    counts(x)(y)
  }
}

object Heatmap {

  private val percentileRanges: Map[String, (Double, Double)] = {
    val builder = Map.newBuilder[String, (Double, Double)]
    val n = PercentileBuckets.length()
    var min = 0L
    var i = 0
    while (i < n) {
      val k = String.format("%04X", i)
      val max = PercentileBuckets.get(i)
      builder += s"D$k" -> (min.toDouble -> max.toDouble)
      builder += s"T$k" -> (min / 1e9 -> max / 1e9)
      min = max
      i += 1
    }
    builder.result()
  }

  def isPercentileHeatmap(line: LineDef): Boolean = {
    line.lineStyle == LineStyle.HEATMAP && line.data.tags.contains(TagKey.percentile)
  }

  def percentileBucketRange(tags: Map[String, String]): Option[(Double, Double)] = {
    tags.get(TagKey.percentile).flatMap { s =>
      percentileRanges.get(s)
    }
  }
}
