/*
 * Copyright 2014-2017 Netflix, Inc.
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

import com.netflix.atlas.chart.model.PlotDef
import com.netflix.atlas.chart.model.TickLabelMode
import com.netflix.atlas.core.util.UnitPrefix

sealed trait ValueXAxis extends Element with FixedHeight {

  import ValueXAxis._

  override def height: Int = 10 + Constants.smallFontDims.height

  def plotDef: PlotDef

  def min: Double
  def max: Double

  val style: Style = Style(color = plotDef.getAxisColor)
  val label: Option[Text] = plotDef.ylabel.map { str => Text(str, style = style) }

  val valueScale: Scales.DoubleFactory = Scales.factoryForXAxis(plotDef.scale)

  def scale(x1: Int, x2: Int): Scales.DoubleScale = valueScale(min, max, x1, x2)

  def ticks(x1: Int, x2: Int): List[ValueTick] = {
    val numTicks = (x2 - x1) / minTickLabelHeight
    Ticks.value(min, max, numTicks)
    plotDef.tickLabelMode match {
      case TickLabelMode.BINARY  => Ticks.binary(min, max, numTicks)
      case _                     => Ticks.value(min, max, numTicks)
    }
  }

  protected def tickPrefix(v: Double): UnitPrefix = {
    plotDef.tickLabelMode match {
      case TickLabelMode.OFF     => UnitPrefix.one
      case TickLabelMode.DECIMAL => UnitPrefix.decimal(v)
      case TickLabelMode.BINARY  => UnitPrefix.binary(v)
    }
  }

  protected def tickLabelFmt: String = {
    plotDef.tickLabelMode match {
      case TickLabelMode.OFF     => ""
      case TickLabelMode.DECIMAL => "%.1f%s"
      case TickLabelMode.BINARY  => "%.0f%s"
    }
  }
}

case class BottomValueXAxis(
  plotDef: PlotDef,
  min: Double,
  max: Double,
  alpha: Int = 40) extends ValueXAxis {

  import ValueXAxis._

  def draw(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    val txtH = Constants.smallFontDims.height
    val labelPadding = ValueXAxis.tickLabelWidth / 2

    // Horizontal line across the bottom of the chart. The main horizontal line for the axis is
    // made faint so it is easier to see lines in the chart that are directly against the axis.
    style.withAlpha(alpha).configure(g)
    g.drawLine(x1, y1, x2, y1)


    style.configure(g)
    val xscale = scale(x1, x2)
    val majorTicks = ticks(x1, x2).filter(_.major)
    majorTicks.foreach { tick =>
      val px = xscale(tick.v)
      if (px >= x1 && px <= x2) {
        // Vertical tick mark
        g.drawLine(px, y1, px, y1 + 4)

        // Label for the tick mark
        val txt = Text(tick.label, font = Constants.smallFont, style = style)
        txt.draw(g, px - labelPadding, y1 + txtH / 2, px + labelPadding, y1 + txtH)
      }
    }
  }
}

object ValueXAxis {

  val labelHeight = Constants.normalFontDims.height

  /**
    * Width of value tick labels. The assumption is a monospace font with 7 characters. The 7 is
    * for:
    *
    * - `[sign][3digits][decimal point][1digit][suffix]`: e.g., `-102.3K`
    * - `-1.0e-5`
    */
  val tickLabelWidth = Constants.smallFontDims.width * 7

  val tickMarkLength = 4

  val minTickLabelHeight = Constants.smallFontDims.height * 3
}
