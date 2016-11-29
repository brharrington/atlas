/*
 * Copyright 2014-2016 Netflix, Inc.
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
package com.netflix.atlas.core.util

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

import com.netflix.atlas.core.util.ConcMap.Counters
import com.netflix.atlas.core.util.ConcMap.Index
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Threads
import org.openjdk.jmh.infra.Blackhole

/**
  */
@State(Scope.Benchmark)
class ConcMap {

  @Benchmark
  def random(c: Counters, i: Index, bh: Blackhole): Unit = {
    i.next += 1
    val k = c.keys(i.next % c.keys.length)
    bh.consume(c.map.computeIfAbsent(k, s => new AtomicLong(0L)))
  }

  @Benchmark
  def one(c: Counters, bh: Blackhole): Unit = {
    bh.consume(c.map.computeIfAbsent(c.keys(0), s => new AtomicLong(0L)))
  }
}

object ConcMap {
  @State(Scope.Benchmark)
  class Counters {
    import scala.collection.JavaConverters._

    val map = new ConcurrentHashMap[String, AtomicLong](1024, 0.75f, 1024)

    (0 until 100000).foreach { i =>
      val id = UUID.randomUUID()
      map.put(id.toString, new AtomicLong(0L))
    }

    val keys = map.keySet().asScala.toArray
  }

  @State(Scope.Thread)
  class Index {
    var next: Int = 0
  }
}
