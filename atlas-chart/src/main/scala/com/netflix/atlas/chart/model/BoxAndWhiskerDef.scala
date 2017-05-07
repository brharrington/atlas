package com.netflix.atlas.chart.model

import java.awt.Color
import java.awt.Font
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.image.RenderedImage
import java.nio.file.Files
import java.nio.file.Paths

import com.netflix.atlas.chart.GraphConstants
import com.netflix.atlas.chart.graphics.BottomValueXAxis
import com.netflix.atlas.chart.graphics.BoxAndWhiskerGraph
import com.netflix.atlas.chart.graphics.Constants
import com.netflix.atlas.chart.graphics.Element
import com.netflix.atlas.chart.graphics.HorizontalPadding
import com.netflix.atlas.chart.graphics.Legend
import com.netflix.atlas.chart.graphics.Text
import com.netflix.atlas.chart.graphics.TextAlignment
import com.netflix.atlas.config.ConfigManager
import com.netflix.atlas.core.util.PngImage
import com.netflix.atlas.core.util.UnitPrefix

/**
  * Definition of a simple box and whisker chart.
  */
case class BoxAndWhiskerDef(
  min: Double,
  max: Double,
  q1: Double,
  q2: Double,
  q3: Double,
  outliers: List[PointDef],
  boxColor: Color = Color.BLACK,
  boxFillColor: Color = Color.CYAN,
  medianLineColor: Color = Color.BLACK,
  whiskerLineColor: Color = Color.BLACK,
  pointWidth: Int = 10,
  width: Int = 400,
  height: Int = 200,
  layout: Layout = Layout.CANVAS,
  zoom: Double = 1.0,
  title: Option[String] = None,
  legendType: LegendType = LegendType.LABELS_WITH_STATS,
  onlyGraph: Boolean = false) {

  private val renderingHints = {
    import scala.collection.JavaConverters._
    val config = ConfigManager.current.getConfig("atlas.chart.rendering-hints")
    config.entrySet.asScala.toList.map { entry =>
      val k = getField(entry.getKey).asInstanceOf[RenderingHints.Key]
      val v = getField(entry.getValue.unwrapped.asInstanceOf[String])
      k -> v
    }
  }

  private def getField(name: String): AnyRef = {
    classOf[RenderingHints].getField(name).get(null)
  }

  /** Returns true if text should be shown. */
  def showText: Boolean = {
    width >= Constants.minWidthForText
  }

  private def format(v: Double): String = {
    UnitPrefix.decimal(v).format(v, "%9.3f%1s", "%8.1e ")
  }

  def asImage: RenderedImage = {
    val aboveCanvas = List.newBuilder[Element]
    title.foreach { str =>
      if (showText)
        aboveCanvas += Text(str, font = Constants.largeFont).truncate(width)
    }
    aboveCanvas += HorizontalPadding(5)

    val belowCanvas = List.newBuilder[Element]

    legendType match {
      case LegendType.LABELS_ONLY =>
        belowCanvas += Legend(PlotDef(outliers, axisColor = Some(Color.BLACK)), None, showStats = false, 50)
      case LegendType.LABELS_WITH_STATS =>
        belowCanvas += Text("Stats",
          font = Constants.normalFont.deriveFont(Font.BOLD),
          alignment = TextAlignment.LEFT)
        belowCanvas += Text("   min: " + format(min), alignment = TextAlignment.LEFT)
        belowCanvas += Text("    q1: " + format(q1), alignment = TextAlignment.LEFT)
        belowCanvas += Text("    q2: " + format(q2), alignment = TextAlignment.LEFT)
        belowCanvas += Text("    q3: " + format(q3), alignment = TextAlignment.LEFT)
        belowCanvas += Text("   max: " + format(max), alignment = TextAlignment.LEFT)
        belowCanvas += Legend(PlotDef(outliers, axisColor = Some(Color.BLACK)), Some("Outliers"), showStats = false, 50)
      case LegendType.OFF =>
    }

    val above = aboveCanvas.result()
    val below = belowCanvas.result()

    val parts = List.newBuilder[Element]
    parts ++= above

    val graph = BoxAndWhiskerGraph(this)
    parts += graph

    if (!layout.isFixedHeight)
      parts ++= below
    val elements = parts.result()

    val imgWidth = width
    val imgHeight = computeHeight(elements, imgWidth)

    val zoomFactor = if (zoom > GraphConstants.MaxZoom) GraphConstants.MaxZoom else zoom
    val zoomWidth = (imgWidth * zoomFactor).toInt
    val zoomHeight = (imgHeight * zoomFactor).toInt
    val image = new BufferedImage(zoomWidth, zoomHeight, BufferedImage.TYPE_INT_ARGB)
    val g = image.createGraphics()
    renderingHints.foreach(h => g.setRenderingHint(h._1, h._2))
    g.scale(zoomFactor, zoomFactor)

    g.setColor(Constants.backgroundColor)
    g.fillRect(0, 0, imgWidth, imgHeight)

    var y = 0
    elements.foreach { element =>
      val h = element.getHeight(Constants.refGraphics, imgWidth)
      element.draw(g, 0, y, imgWidth, y + h)
      y += h
    }

    image
  }

  private def computeHeight(elements: List[Element], w: Int): Int = {
    elements.foldLeft(0) { (acc, e) => acc + e.getHeight(Constants.refGraphics, w) }
  }

  def asPng: Array[Byte] = {
    PngImage(asImage).toByteArray
  }
}

object BoxAndWhiskerDef {
  def main(args: Array[String]): Unit = {
    val palette = Palette.fromResource("armytage").iterator
    val boxDef1 = BoxAndWhiskerDef(
        min = 0.0,
        max = 40.0e6,
        q1 = 20.2e6,
        q2 = 25.5e6,
        q3 = 35.6e6,
        outliers = List(
          PointDef(-6.0e6, "one", color = palette.next()),
          PointDef(49.0e6, "two", shape = PointShape.SQUARE, fill = false, color = palette.next()),
          PointDef(40.0e6, "three", shape = PointShape.SQUARE, fill = false, color = palette.next()),
          PointDef(45.0e6, "four", shape = PointShape.SQUARE, fill = false, color = palette.next()),
          PointDef(90.0e6, "five", shape = PointShape.TRIANGLE, fill = false, color = palette.next()),
          PointDef(38.0e6, "six", shape = PointShape.TRIANGLE, color = palette.next())),
      height = 50
    )
    Files.write(Paths.get("box/t1.png"), boxDef1.asPng)

    val boxDef2 = BoxAndWhiskerDef(
      min = 0.0,
      max = 40.0,
      q1 = 20.2,
      q2 = 25.5,
      q3 = 35.6,
      outliers = List(
        PointDef(49.0, "foo", shape = PointShape.SQUARE, fill = false)),
      height = 50,
      title = Some("A useful title"),
      legendType = LegendType.LABELS_ONLY
    )
    Files.write(Paths.get("box/t2.png"), boxDef2.asPng)

    val boxDef3 = BoxAndWhiskerDef(
      min = 0.0,
      max = 40.0,
      q1 = 20.2,
      q2 = 25.5,
      q3 = 35.6,
      outliers = List(
        PointDef(49.0, "foo", shape = PointShape.TRIANGLE, color = Color.RED)),
      height = 250,
      title = Some("A useful title"),
      legendType = LegendType.LABELS_ONLY
    )
    Files.write(Paths.get("box/t3.png"), boxDef3.asPng)

    val boxDef4 = BoxAndWhiskerDef(
      min = 0.0,
      max = 40.0,
      q1 = 20.2,
      q2 = 30.2,
      q3 = 35.6,
      boxColor = Color.ORANGE,
      boxFillColor = Color.GREEN,
      medianLineColor = Color.BLACK,
      whiskerLineColor = Color.RED,
      outliers = List(
        PointDef(49.0, "foo", shape = PointShape.SQUARE, fill = false)),
      height = 50,
      title = Some("A useful title"),
      legendType = LegendType.LABELS_ONLY
    )
    Files.write(Paths.get("box/t4.png"), boxDef4.asPng)

    val boxDef5 = BoxAndWhiskerDef(
      min = 0.0,
      max = 40.0,
      q1 = 20.2,
      q2 = 30.2,
      q3 = 35.6,
      boxFillColor = Color.LIGHT_GRAY,
      medianLineColor = Color.RED,
      outliers = List(
        PointDef(49.0, "foo", shape = PointShape.TRIANGLE, color = Color.RED)),
      height = 50,
      title = Some("A useful title"),
      legendType = LegendType.LABELS_ONLY
    )
    Files.write(Paths.get("box/t5.png"), boxDef5.asPng)
  }
}
