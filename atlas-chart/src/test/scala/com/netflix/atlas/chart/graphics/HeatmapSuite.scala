package com.netflix.atlas.chart.graphics

import com.netflix.atlas.core.model.TagKey
import munit.FunSuite

class HeatmapSuite extends FunSuite {

  test("percentileBucketRange: timer") {
    val tags = Map(TagKey.percentile -> "T0042")
    assertEquals(Heatmap.percentileBucketRange(tags), Some(4.915E-5 -> 5.4611E-5))
  }

  test("percentileBucketRange: distribution summary") {
    val tags = Map(TagKey.percentile -> "D0042")
    assertEquals(Heatmap.percentileBucketRange(tags), Some(49150.0 -> 54611.0))
  }

  test("percentileBucketRange: unknown") {
    val tags = Map(TagKey.percentile -> "foo")
    assertEquals(Heatmap.percentileBucketRange(tags), None)
  }
}
