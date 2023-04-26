package com.netflix.atlas.chart.graphics

import com.netflix.atlas.chart.model.PlotDef

import java.awt.Color
import java.awt.Graphics2D

// show stats behavior
// responsive sizing tests
// tick labels
// indentation
case class HeatmapLegendEntry(styles: Styles, plot: PlotDef, data: Heatmap) extends Element with FixedHeight {

  override def height: Int = {
    ChartSettings.normalFontDims.height * 3
  }

  override def draw(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    // Label
    val label = data.settings.label.getOrElse("Heatmap")
    val txt = Text(label, alignment = TextAlignment.LEFT, style = styles.text)
    val truncated = txt.truncate(x2 - x1)
    truncated.draw(g, x1, y1, x2, y2)

    // Color scale
    val colors = data.palette.colorArray
    val d = ChartSettings.normalFontDims.height - 4
    val w = d * 4
    val offset = y1 + ChartSettings.normalFontDims.height

    // Color boxes
    g.setColor(Color.WHITE)
    g.fillRect(x1, offset, w * colors.length, d)
    var i = 0
    while (i < colors.length) {
      g.setColor(colors(i))
      g.fillRect(x1 + i * w, offset, w, d)
      i += 1
    }

    // Axis lines
    g.setColor(styles.line.color)
    g.drawLine(x1, offset + d, x1 + w * colors.length, offset + d)

    // Vertical ticks for color axis
    var x = x1
    while (x <= x1 + w * colors.length) {
      g.drawLine(x, offset + d, x, offset + d + 4)
      x += w
    }
  }
}
