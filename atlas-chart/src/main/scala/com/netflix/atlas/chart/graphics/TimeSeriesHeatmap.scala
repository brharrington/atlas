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
import com.netflix.atlas.chart.model.Palette
import com.netflix.atlas.chart.model.PlotBound

import java.awt.Color
import java.awt.Graphics2D

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
case class TimeSeriesHeatmap(settings: HeatmapDef, heatmap: Heatmap)
  extends Element {

  private val palette = settings.palette.getOrElse {
    Palette.gradient(heatmap.lines.head.color)
  }

  private val colorScale = Scales.factory(settings.colorScale)(
    settings.lower.lower(hasArea = false, heatmap.minCount),
    settings.upper.upper(hasArea = false, heatmap.maxCount),
    palette.colorArray.size ,
    0
  )
  println("MIN " + heatmap.minCount)
  println("MAX " + heatmap.maxCount)
  println("MAX2 " + settings.upper.upper(hasArea = false, heatmap.maxCount))

  private def boundLower(count: Double): Double = {
    settings.lower match {
      case PlotBound.Explicit(v) if count < v => v
      case _                                  => count
    }
  }

  private def boundUpper(count: Double): Double = {
    settings.upper match {
      case PlotBound.Explicit(v) if count > v => v
      case _                                  => count
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

  def draw(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    val step = heatmap.step
    val xaxis = heatmap.xaxis
    val yaxis = heatmap.yaxis
    val xscale = xaxis.scale(x1, x2)
    val yscale = yaxis.scale(y1, y2)

    var t = heatmap.xaxis.start
    while (t < xaxis.end) {
      val px1 = xscale(t - step)
      val px2 = xscale(t)

      var i = 0
      while (i < heatmap.numberOfValueBuckets) {
        val count = heatmap.count(t, i)
        if (count > 0.0) {
          val c = lookupColor(colorScale(boundedCount(count)))
          g.setColor(c)
          val py1 = if (i == 0) y2 else yscale(heatmap.ticks(i - 1).v)
          val py2 = if (i == heatmap.ticks.length) y1 else yscale(heatmap.ticks(i).v)
          g.fillRect(px1, py2, px2 - px1, py1 - py2)
        }
        i += 1
      }

      t += step
    }
  }
}
