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
package com.netflix.atlas.chart.model

import com.netflix.atlas.chart.model.PlotBound

/**
  * A configuration used to compute and optionally plot a heatmap.
  *
  * @param colorScale
  *     The color scale to use for counts within a cell.
  * @param upper
  *     An optional upper boundary for the cell count.
  * @param lower
  *     An optional lower boundary for the cell count.
  * @param palette
  *     An optional palette to use for the heatmap
  * @param legend
  *     A string to use for the legend.
  */
case class HeatmapDef(
  colorScale: Scale = Scale.LINEAR,
  upper: PlotBound = PlotBound.AutoData,
  lower: PlotBound = PlotBound.AutoData,
  palette: Option[Palette] = None,
  legend: Option[String] = None
)
