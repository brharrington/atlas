package com.netflix.atlas.core.util

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.infra.Blackhole

/**
  * jmh:run -wi 10 -i 10 -f1 -t1 -prof gc .*CardinalityEstimators.*
  */
@State(Scope.Thread)
class CardinalityEstimators {

  private val N_MASK = 1024 * 1024 - 1
  private val N = N_MASK + 1
  private val strings = (0 until N).map(_.toString).toArray
  private var i = 0

  private val custom = CardinalityEstimator.hyperLogLog
  private val cpc = CardinalityEstimator.cpcString
  private val hll = CardinalityEstimator.hllString

  def update(estimator: CardinalityEstimator, bh: Blackhole): Unit = {
    estimator.update(strings(i))
    i = (i + 1) & N_MASK
    // assume we'll need to check the estimate to see if a rollup is needed after every update
    bh.consume(estimator.cardinality)
  }

  @Benchmark
  def custom(bh: Blackhole): Unit = {
    update(custom, bh)
  }

  @Benchmark
  def cpc(bh: Blackhole): Unit = {
    update(cpc, bh)
  }

  @Benchmark
  def hll(bh: Blackhole): Unit = {
    update(hll, bh)
  }
}
