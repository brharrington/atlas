package com.netflix.atlas.chart.graphics

import com.netflix.atlas.chart.model.LineDef
import com.netflix.atlas.chart.model.LineStyle
import com.netflix.atlas.core.model.TagKey

trait Heatmap {

  def minCount: Int

  def maxCount: Int

  def numberOfValueBuckets: Int

  def count(t: Long, y: Int): Int
}

object Heatmap {

  def isPercentileHeatmap(line: LineDef): Boolean = {
    line.lineStyle == LineStyle.HEATMAP && line.data.tags.contains(TagKey.percentile)
  }

  def percentileBucketRange(tags: Map[String, String]): Option[(Double, Double)] = {
    None
    /*tags.get(TagKey.percentile).map { s =>
      if (s.startsWith("T"))
    }*/
  }
}
