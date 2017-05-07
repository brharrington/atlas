package com.netflix.atlas.chart.graphics

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D

import com.netflix.atlas.chart.GraphConstants
import com.netflix.atlas.chart.model.BoxAndWhiskerDef
import com.netflix.atlas.chart.model.PlotDef
import com.netflix.atlas.chart.model.PointShape

/**
  * Created by brharrington on 5/6/17.
  */
case class BoxAndWhiskerGraph(graphDef: BoxAndWhiskerDef)
  extends Element with FixedHeight with FixedWidth {

  private val min = math.min(graphDef.min, graphDef.outliers.map(_.v).min)
  private val max = math.max(graphDef.max, graphDef.outliers.map(_.v).max)

  private val xAxis = BottomValueXAxis(PlotDef(Nil), min, max)

  override def height: Int = {
    val max = GraphConstants.MaxHeight
    val h = if (graphDef.height > max) max else {
      val min = GraphConstants.MinCanvasHeight
      if (graphDef.height < min)  min else graphDef.height
    }
    if (graphDef.onlyGraph || graphDef.layout.isFixedHeight) h else h + xAxis.height
  }

  override def width: Int = {
    val max = GraphConstants.MaxWidth
    if (graphDef.width > max) max else {
      val min = GraphConstants.MinCanvasWidth
      if (graphDef.width < min)  min else graphDef.width
    }
  }

  private def clip(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    g.setClip(x1, y1, x2 - x1, y2 - y1)
    g.setColor(Color.WHITE)
    g.fillRect(x1, y1, x2 - x1, y2 - y1)
  }

  override def draw(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int): Unit = {

    val leftOffset = BoxAndWhiskerGraph.minSidePadding
    val rightOffset = BoxAndWhiskerGraph.minSidePadding

    val xAxisH = if (graphDef.onlyGraph) 10 else xAxis.height
    val xGrid = ValueXGrid(xAxis)

    val chartEnd = y2 - xAxisH

    val yOffset = chartEnd / 10

    val prevClip = g.getClip
    clip(g, x1 + leftOffset, y1, x2 - rightOffset, chartEnd + 1)

    // Draw the box
    val xscale = xAxis.scale(x1 + leftOffset, x2 - rightOffset)
    val q1x = xscale(graphDef.q1)
    val q3x = xscale(graphDef.q3)
    g.setColor(graphDef.boxFillColor)
    g.fillRect(q1x, y1 + yOffset, q3x - q1x, chartEnd - y1 - 2 * yOffset)
    g.setColor(graphDef.boxColor)
    g.drawRect(q1x, y1 + yOffset, q3x - q1x, chartEnd - y1 - 2 * yOffset)

    // Draw median
    val q2x = xscale(graphDef.q2)
    g.setColor(graphDef.medianLineColor)
    g.drawLine(q2x, y1 + yOffset, q2x, chartEnd - yOffset)

    // Draw whiskers
    val middleY = y1 + yOffset + ((chartEnd - yOffset) - (y1 + yOffset)) / 2
    val minX = xscale(graphDef.min)
    val maxX = xscale(graphDef.max)
    g.setColor(graphDef.whiskerLineColor)
    g.drawLine(minX, middleY, maxX, middleY)
    g.drawLine(minX, y1 + yOffset, minX, chartEnd - yOffset)
    g.drawLine(maxX, y1 + yOffset, maxX, chartEnd - yOffset)

    xGrid.draw(g, x1 + leftOffset, y1, x2 - rightOffset, chartEnd)
    g.setClip(prevClip)

    // Draw points, this is done after clipping to avoid cutting off the last
    // point
    g.setStroke(new BasicStroke(1.0f))
    val py = middleY - graphDef.pointWidth / 2
    val ph = graphDef.pointWidth
    graphDef.outliers.foreach { point =>
      val x = xscale(point.v)
      val px = x - graphDef.pointWidth / 2
      val pw = graphDef.pointWidth
      g.setColor(point.color)
      point.shape match {
        case PointShape.CIRCLE =>
          if (point.fill)
            g.fillOval(px, py, pw, ph)
          g.drawOval(px, py, pw, ph)
        case PointShape.SQUARE =>
          if (point.fill)
            g.fillRect(px, py, pw, ph)
          g.drawRect(px, py, pw, ph)
        case PointShape.TRIANGLE =>
          val xs = Array(px, px + pw / 2, px + pw)
          val ys = Array(py + ph, py, py + ph)
          if (point.fill)
            g.fillPolygon(xs, ys, 3)
          g.drawPolygon(xs, ys, 3)
      }
    }

    xAxis.draw(g, x1 + leftOffset, chartEnd + 1, x2 - rightOffset, y2)
  }

}

object BoxAndWhiskerGraph {
  /**
    * Allow at least 4 small characters on the side to prevent the final tick mark label
    * from getting truncated.
    */
  private val minSidePadding = Constants.smallFontDims.width * 4
}
