package com.netflix.atlas.core.util

import org.apache.datasketches.cpc.CpcSketch
import org.apache.datasketches.hll.HllSketch

trait CardinalityEstimator {
  def update(obj: AnyRef): Unit
  def cardinality: Int
}

object CardinalityEstimator {

  def hyperLogLog: CardinalityEstimator = {
    new HyperLogLog(r => Hash.lowbias32(r.hashCode()))
  }

  def hyperLogLog(h: AnyRef => Int): CardinalityEstimator = {
    new HyperLogLog(h)
  }

  // http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf
  private class HyperLogLog(h: AnyRef => Int) extends CardinalityEstimator {

    // 2**b, for b == 4
    private val m = 16

    // for m = 16
    private val alpha = 0.673

    // registers
    private val M = new Array[Int](m)

    // for large range check
    private val L = math.pow(2, 32)

    // cached estimate
    private var estimate = -1

    private def positionOfFirstOneBit(x: Int): Int = {
      // Ignore bits used for address and then remove address offset - 1
      Integer.numberOfLeadingZeros(x & 0x0FFFFFFF) - 3
    }

    override def update(obj: AnyRef): Unit = {
      val x = h(obj)
      val j = x >>> 28 // address of register to update
      val p = positionOfFirstOneBit(x)
      if (p > M(j)) {
        M(j) = p
        estimate = -1
      }
    }

    private def computeCardinalityEstimate: Int = {
      var sum = 0.0
      var V = 0 // registers that are zero
      var i = 0
      while (i < m) {
        sum += math.pow(2.0, -M(i))
        if (M(i) == 0) {
          V += 1
        }
        i += 1
      }
      val E = ((alpha * m * m) / sum).toInt

      if (E < 2.5 * m && V != 0) {
        // small range correction
        (m * math.log(m / V)).toInt
      } else if (E > L / 30) {
        // large range correction
        (-L * math.log(1.0 - E / L)).toInt
      } else {
        E
      }
    }

    override def cardinality: Int = {
      if (estimate < 0) {
        estimate = computeCardinalityEstimate
      }
      estimate
    }
  }

  //
  // Sketches using toString as input
  //

  def cpcString: CardinalityEstimator = {
    new CpcString
  }

  private class CpcString extends CardinalityEstimator {
    private val sketch = new CpcSketch()

    override def update(obj: AnyRef): Unit = {
      sketch.update(obj.toString)
    }

    override def cardinality: Int = sketch.getEstimate.toInt
  }

  def hllString: CardinalityEstimator = {
    new HllString
  }

  private class HllString extends CardinalityEstimator {
    private val sketch = new HllSketch()

    override def update(obj: AnyRef): Unit = {
      sketch.update(obj.toString)
    }

    override def cardinality: Int = sketch.getEstimate.toInt
  }

  //
  // Sketches but using the lowbias hash as initial input
  //

  def cpcLowBias: CardinalityEstimator = {
    new CpcLowBias
  }

  private class CpcLowBias extends CardinalityEstimator {
    private val sketch = new CpcSketch()

    override def update(obj: AnyRef): Unit = {
      sketch.update(Hash.lowbias32(obj.hashCode()))
    }

    override def cardinality: Int = sketch.getEstimate.toInt
  }

  def hllLowBias: CardinalityEstimator = {
    new HllLowBias
  }

  private class HllLowBias extends CardinalityEstimator {
    private val sketch = new HllSketch()

    override def update(obj: AnyRef): Unit = {
      sketch.update(Hash.lowbias32(obj.hashCode()))
    }

    override def cardinality: Int = sketch.getEstimate.toInt
  }
}
