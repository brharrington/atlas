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
package com.netflix.atlas.chart

import com.netflix.atlas.chart.model.GraphDef
import com.netflix.atlas.chart.model.HeatmapDef
import com.netflix.atlas.chart.model.LineDef
import com.netflix.atlas.chart.model.LineStyle
import com.netflix.atlas.chart.model.Palette
import com.netflix.atlas.chart.model.PlotDef
import com.netflix.atlas.core.model.ArrayTimeSeq
import com.netflix.atlas.core.model.DsType
import com.netflix.atlas.core.model.TimeSeries
import munit.FunSuite

import java.awt.Color
import java.time.Instant

class JsonCodecSuite extends FunSuite {

  private def graphDef(hints: Set[String]): GraphDef = {
    val step = 60_000L
    GraphDef(
      plots = List(PlotDef(List(LineDef(TimeSeries.noData(step))))),
      startTime = Instant.ofEpochMilli(0L),
      endTime = Instant.ofEpochMilli(step),
      renderingHints = hints
    )
  }

  test("rendering hint: none") {
    val gdef = graphDef(Set.empty)
    val str = JsonCodec.encode(gdef)
    assert(str.contains(""""type":"graph-image""""))
  }

  test("rendering hint: no-image") {
    val gdef = graphDef(Set("no-image"))
    val str = JsonCodec.encode(gdef)
    assert(!str.contains(""""type":"graph-image""""))
  }

  private def generateHeatmap(
    isPercentile: Boolean = false,
    heatmapDef: Option[HeatmapDef]
  ): GraphDef = {
    val dpsA = new Array[Double](60)
    java.util.Arrays.fill(dpsA, 1)
    val dpsB = new Array[Double](60)
    java.util.Arrays.fill(dpsB, 10)
    val timeseries = if (isPercentile) {
      List(
        LineDef(
          TimeSeries(
            Map("percentile" -> "T0042"),
            new ArrayTimeSeq(DsType.Gauge, 0L, 60_000, dpsA)
          ),
          lineStyle = LineStyle.HEATMAP
        ),
        LineDef(
          TimeSeries(
            Map("percentile" -> "T0048"),
            new ArrayTimeSeq(DsType.Gauge, 0L, 60_000, dpsB)
          ),
          lineStyle = LineStyle.HEATMAP
        )
      )
    } else {
      List(
        LineDef(
          TimeSeries(
            Map("series" -> "0"),
            new ArrayTimeSeq(DsType.Gauge, 0L, 60_000, dpsA)
          ),
          lineStyle = LineStyle.HEATMAP
        ),
        LineDef(
          TimeSeries(
            Map("series" -> "1"),
            new ArrayTimeSeq(DsType.Gauge, 0L, 60_000, dpsB)
          ),
          lineStyle = LineStyle.HEATMAP
        )
      )
    }

    val plotDef = PlotDef(
      timeseries,
      scale = PlotDef(List.empty).scale,
      heatmap = heatmapDef
    )

    GraphDef(
      List(plotDef),
      Instant.ofEpochMilli(0L),
      Instant.ofEpochMilli(3_600_000)
    )
  }

  test("v2 heatmap") {
    val gdef = generateHeatmap(heatmapDef =
      Some(
        HeatmapDef(
          palette = Some(Palette.fromArray("heatmap", Array(Color.RED)))
        )
      )
    )
    val str = JsonCodec.encode(gdef)
    assert(str.contains(""""type":"heatmap""""))
    assert(str.contains(""""data":{"type":"heatmap""""))
    assert(str.contains(""""label":"10.0""""))
    val obtained = JsonCodec.decode(str)
    assertEquals(obtained, gdef)
  }

  test("v2 percentile heatmap") {
    val gdef = generateHeatmap(
      true,
      Some(
        HeatmapDef(
          palette = Some(Palette.fromArray("heatmap", Array(Color.RED)))
        )
      )
    )
    val str = JsonCodec.encode(gdef)
    assert(str.contains(""""type":"heatmap""""))
    assert(str.contains(""""data":{"type":"heatmap""""))
    assert(str.contains(""""label":"percentile=T0048""""))
    val obtained = JsonCodec.decode(str)
    assertEquals(obtained, gdef)
  }
}
