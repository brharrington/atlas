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

import java.util.concurrent.atomic.AtomicLong

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Threads
import org.openjdk.jmh.infra.Blackhole

/**
  * Quick check of overhead for determining location info.
  *
  * ```
  * > jmh:run -wi 10 -i 10 -f1 -t1 .*LocationInfo.*
  * ...
  * [info] Benchmark                          Mode  Cnt          Score         Error  Units
  * [info] LocationInfo.testStatusQuo        thrpt   10  111518653.168 ± 5098351.719  ops/s
  * [info] LocationInfo.testUsingStackTrace  thrpt   10      65726.307 ±    3662.065  ops/s
  * [info] LocationInfo.testUsingSunClasses  thrpt   10    3778815.665 ±  113435.390  ops/s
  * ```
  */
@State(Scope.Thread)
class LocationInfo {

  private val counter = new AtomicLong(0L)

  def inc(): Long = {
    counter.incrementAndGet()
  }

  private def getName(path: String): String = {
    val i = path.lastIndexOf('/')
    if (i < 0) path else path.substring(i + 1)
  }

  def incLocUsingStackTrace(): Long = {
    val trace = Thread.currentThread.getStackTrace
    val caller = Class.forName(trace(1).getClassName)
    val url = caller.getProtectionDomain.getCodeSource.getLocation
    val jar = getName(url.getPath)
    // use jar as part of return so it isn't optimized away
    counter.incrementAndGet() + jar.length
  }

  def incLocUsingSunClasses(): Long = {
    val caller = sun.reflect.Reflection.getCallerClass(1)
    val url = caller.getProtectionDomain.getCodeSource.getLocation
    var jar: String = getName(url.getPath)
    // use jar as part of return so it isn't optimized away
    counter.incrementAndGet() + jar.length
  }

  @Threads(1)
  @Benchmark
  def testStatusQuo(bh: Blackhole): Unit = {
    bh.consume(inc())
  }

  @Threads(1)
  @Benchmark
  def testUsingStackTrace(bh: Blackhole): Unit = {
    bh.consume(incLocUsingStackTrace())
  }

  @Threads(1)
  @Benchmark
  def testUsingSunClasses(bh: Blackhole): Unit = {
    bh.consume(incLocUsingSunClasses())
  }
}
