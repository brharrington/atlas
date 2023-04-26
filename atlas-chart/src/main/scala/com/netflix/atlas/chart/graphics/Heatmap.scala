/*
 * Copyright 2014-2023 Netflix, Inc.
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
package com.netflix.atlas.chart.graphics

import com.netflix.atlas.chart.model.HeatmapDef
import com.netflix.atlas.chart.model.LineDef
import com.netflix.atlas.chart.model.LineStyle
import com.netflix.atlas.chart.model.Palette
import com.netflix.atlas.chart.model.PlotBound
import com.netflix.atlas.chart.model.TickLabelMode
import com.netflix.atlas.core.model.TagKey
import com.netflix.spectator.api.histogram.PercentileBuckets

import java.awt.Color
import scala.collection.immutable.ArraySeq

case class Heatmap(
                  settings: HeatmapDef,
  lines: List[LineDef],
  xaxis: TimeAxis,
  yaxis: ValueAxis,
  canvasHeight: Int
) {

  require(lines.nonEmpty)

  val step: Long = lines.head.data.data.step

  private val start = xaxis.start
  private val end = xaxis.end

  private val minValue = yaxis.min
  private val maxValue = yaxis.max

  val yTicks: ArraySeq[ValueTick] = ArraySeq.unsafeWrapArray(yaxis.ticks(0, canvasHeight).toArray)

  val palette: Palette = settings.palette.getOrElse {
    Palette.gradient(lines.head.color)
  }

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

  private val colorScale = Scales.factory(settings.colorScale)(
    settings.lower.lower(hasArea = false, minCount),
    settings.upper.upper(hasArea = false, maxCount),
    palette.colorArray.size,
    0
  )

  def colorTicks(mode: TickLabelMode): ArraySeq[ValueTick] = {
    val numTicks = palette.colorArray.length
    val ticks = mode match {
      case TickLabelMode.BINARY   => Ticks.binary(minCount, maxCount, numTicks)
      case TickLabelMode.DURATION => Ticks.duration(minCount, maxCount, numTicks)
      case _                      => Ticks.value(minCount, maxCount, numTicks, settings.colorScale)
    }
    ArraySeq.from(ticks)
  }

  private def boundLower(count: Double): Double = {
    settings.lower match {
      case PlotBound.Explicit(v) if count < v => v
      case _ => count
    }
  }

  private def boundUpper(count: Double): Double = {
    settings.upper match {
      case PlotBound.Explicit(v) if count > v => v
      case _ => count
    }
  }

  private def boundedCount(count: Double): Double = {
    boundUpper(boundLower(count))
  }

  private def lookupColor(i: Int): Color = {
    // The default palette lookup will go back to the first color if the index exceeds the
    // last index of hte palette's color array. For heatmaps that is not desirable and should
    // just use the last color.
    val idx = if (i >= palette.colorArray.size) i - 1 else i
    palette.colorArray(idx)
  }

  private def findBucket(value: Double): Int = {
    // When using explicit bounds, some values may not be visible
    if (value < minValue || value > maxValue) {
      return -1
    }

    // Find the matching bucket
    var i = 0
    while (i < yTicks.length) {
      if (value < yTicks(i).v)
        return i
      i += 1
    }
    yTicks.length
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
    while (i < yTicks.length) {
      val cellMax = yTicks(i).v
      counts(i) += cnt * computeWeight(mn, mx, cellMin, cellMax)
      if (cellMax > mx) {
        // Stop early once passed the max of the bucket range
        return
      }
      cellMin = cellMax
      i += 1
    }
    counts(i) += cnt * computeWeight(mn, mx, cellMin, maxValue)
  }

  private def computeCounts(): Array[Array[Double]] = {
    val w = ((end - start) / step).toInt
    val h = yTicks.length + 1
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
            if (cnt > 0.0) updateCounts(mn, mx, cnt, counts(x))
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

  def numberOfValueBuckets: Int = yTicks.length + 1

  def count(t: Long, y: Int): Double = {
    val x = ((t - start) / step).toInt
    counts(x)(y)
  }

  def color(t: Long, y: Int): Option[Color] = {
    val c = count(t, y)
    if (c > 0.0)
      Some(color(c))
    else
      None
  }

  def color(c: Double): Color = {
    lookupColor(colorScale(boundedCount(c)))
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
      builder += s"T$k" -> (min / 1e9    -> max / 1e9)
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
