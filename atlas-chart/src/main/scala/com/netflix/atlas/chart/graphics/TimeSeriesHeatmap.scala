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

import java.awt.Graphics2D

import com.netflix.atlas.core.model.TimeSeq

/**
  * Draws a time series as a stepped line.
  *
  * @param style
  *     Style to use for drawing the line.
  * @param data
  *     Lines that should be converted into a heatmap.
  * @param xaxis
  *     Axis used to create the X scale.
  * @param yaxis
  *     Axis used to create the Y scale.
  */
case class TimeSeriesHeatmap(style: Style, data: List[TimeSeq], xaxis: TimeAxis, yaxis: ValueAxis)
  extends Element {

  import TimeSeriesHeatmap._

  def draw(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    if (data.isEmpty) return
    style.configure(g)
    val step = data.head.step
    val xscale = xaxis.scale(x1, x2)
    val yscale = yaxis.scale(y1, y2)

    val ticks = yaxis.ticks(y1, y2).toArray
    val counts = computeCounts(data, xaxis.start, xaxis.end, step, ticks)

    var t = xaxis.start
    while (t < xaxis.end) {
      val px1 = xscale(t - step)
      val px2 = xscale(t)

      val cx = ((t - xaxis.start) / step).toInt
      var cy = 0
      while (cy <= ticks.length) {
        val count = counts(cx)(cy)
        if (count > 0) {
          val py1 = if (cy == 0) y1 else yscale(ticks(cy - 1).v)
          val py2 = if (cy == ticks.length) y2 else yscale(ticks(cy).v)
          g.fillRect(px1, py2, px2 - px1, py1 - py2)
        }
        cy += 1
      }

      t += step
    }
  }
}

object TimeSeriesHeatmap {

  private def findBucket(ticks: Array[ValueTick], value: Double): Int = {
    // positive vs negative, limited bounds
    var i = 0
    while (i < ticks.length) {
      if (value < ticks(i).v)
        return i
      i += 1
    }
    ticks.length
  }

  private def computeCounts(data: List[TimeSeq], start: Long, end: Long, step: Long, ticks: Array[ValueTick]): Array[Array[Int]] = {
    val w = ((end - start) / step).toInt
    val h = ticks.length + 1
    val counts = Array.fill(w, h)(0)
    var t = start
    while (t < end) {
      val x = ((t - start) / step).toInt
      data.foreach { ts =>
        val v = ts(t)
        if (!v.isNaN) {
          val y = findBucket(ticks, v)
          counts(x)(y) += 1
        }
      }
      t += step
    }
    counts
  }
}
