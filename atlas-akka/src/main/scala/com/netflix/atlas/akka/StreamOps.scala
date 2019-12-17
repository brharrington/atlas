/*
 * Copyright 2014-2019 Netflix, Inc.
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
package com.netflix.atlas.akka

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit

import akka.Done
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.ActorMaterializerSettings
import akka.stream.Attributes
import akka.stream.FlowShape
import akka.stream.Graph
import akka.stream.Inlet
import akka.stream.Materializer
import akka.stream.Outlet
import akka.stream.OverflowStrategy
import akka.stream.QueueOfferResult
import akka.stream.SourceShape
import akka.stream.Supervision
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.stream.stage.GraphStage
import akka.stream.stage.GraphStageLogic
import akka.stream.stage.GraphStageWithMaterializedValue
import akka.stream.stage.InHandler
import akka.stream.stage.OutHandler
import com.netflix.spectator.api.NoopRegistry
import com.netflix.spectator.api.Registry
import com.netflix.spectator.api.Timer
import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

/**
  * Utility functions for commonly used operations on Akka streams. Most of these are for
  * the purpose of getting additional instrumentation into the behavior of the stream.
  */
object StreamOps extends StrictLogging {

  /**
    * Creates an instance of the materializer with a supervision strategy that will provide
    * some insight into exceptions.
    *
    * @param system
    *     Actor system to use for materializing the streams.
    * @param registry
    *     Registry to use for reporting metrics.
    */
  def materializer(system: ActorSystem, registry: Registry): ActorMaterializer = {
    val settings = ActorMaterializerSettings(system)
      .withSupervisionStrategy(t => {
        registry
          .counter("akka.stream.exceptions", "error", t.getClass.getSimpleName)
          .increment()
        logger.warn(s"exception from stream stage", t)
        Supervision.Stop
      })
    ActorMaterializer(settings)(system)
  }

  /**
    * Wraps a source queue and adds monitoring for the results of items offered to the queue.
    * This can be used to detect if items are being dropped or offered after the associated
    * stream has been closed.
    *
    * **Warning:** can have high memory use if the incoming data rate for the queue is not
    * limited based on the future returned from `SourceQueueWithComplete.offer`.
    *
    * @param registry
    *     Spectator registry to manage metrics for this queue.
    * @param id
    *     Dimension used to distinguish a particular queue usage.
    * @param size
    *     Number of enqueued items to allow before triggering the overflow strategy.
    * @param strategy
    *     How to handle items that come in while the queue is full.
    */
  def queue[T](
    registry: Registry,
    id: String,
    size: Int,
    strategy: OverflowStrategy
  ): Source[T, SourceQueueWithComplete[T]] = {
    Source
      .queue[T](size, strategy)
      .mapMaterializedValue(q => new WrappedSourceQueueWithComplete[T](registry, id, q))
  }

  private final class WrappedSourceQueueWithComplete[T](
    registry: Registry,
    id: String,
    queue: SourceQueueWithComplete[T]
  ) extends SourceQueueWithComplete[T] {

    private implicit val ec = scala.concurrent.ExecutionContext.global

    private val baseId = registry.createId("akka.stream.offeredToQueue", "id", id)
    private val enqueued = registry.counter(baseId.withTag("result", "enqueued"))
    private val dropped = registry.counter(baseId.withTag("result", "droppedQueueFull"))
    private val closed = registry.counter(baseId.withTag("result", "droppedQueueClosed"))
    private val failed = registry.counter(baseId.withTag("result", "droppedQueueFailure"))
    private val completed = registry.counter(baseId.withTag("result", "droppedStreamCompleted"))

    override def complete(): Unit = queue.complete()

    override def fail(ex: Throwable): Unit = queue.fail(ex)

    override def watchCompletion(): Future[Done] = queue.watchCompletion()

    override def offer(elem: T): Future[QueueOfferResult] = {
      queue.offer(elem).andThen {
        case Success(QueueOfferResult.Enqueued)    => enqueued.increment()
        case Success(QueueOfferResult.Dropped)     => dropped.increment()
        case Success(QueueOfferResult.QueueClosed) => closed.increment()
        case Success(QueueOfferResult.Failure(_))  => failed.increment()
        case Failure(t)                            => completed.increment()
      }
    }
  }

  /**
    * Creates a queue source based on an `ArrayBlockingQueue`. Values offered to the queue
    * will be emitted by the source. This can be used as an alternative to `Source.queue`
    * or `Source.actorRef` that can have unbounded memory use if the consumer cannot keep
    * up with the rate of data being offered ([#25798]).
    *
    * [#25798]: https://github.com/akka/akka/issues/25798
    *
    * @param registry
    *     Spectator registry to manage metrics for this queue.
    * @param id
    *     Dimension used to distinguish a particular queue usage.
    * @param size
    *     Number of enqueued items to allow before triggering the overflow strategy.
    * @return
    *     Source that emits values offered to the queue.
    */
  def blockingQueue[T](registry: Registry, id: String, size: Int): Source[T, SourceQueue[T]] = {
    val queueSupplier = () => new SourceQueue[T](registry, id, new ArrayBlockingQueue[T](size))
    Source.fromGraph(new QueueSource[T](queueSupplier))
  }

  /** Bounded queue for submitting elements to a stream. */
  class SourceQueue[T] private[akka] (registry: Registry, id: String, queue: BlockingQueue[T]) {

    private val baseId = registry.createId("akka.stream.offeredToQueue", "id", id)
    private val enqueued = registry.counter(baseId.withTag("result", "enqueued"))
    private val dropped = registry.counter(baseId.withTag("result", "droppedQueueFull"))
    private val closed = registry.counter(baseId.withTag("result", "droppedQueueClosed"))

    private[akka] var push: T => Unit = _

    @volatile private var pushImmediately: Boolean = false

    @volatile private var completed: Boolean = false

    private def increment(result: Boolean): Boolean = {
      (if (result) enqueued else dropped).increment()
      result
    }

    /**
      * Add the value into the queue if there is room. Returns true if the value was successfully
      * enqueued.
      */
    def offer(value: T): Boolean = {
      if (completed) {
        closed.increment()
        false
      } else {
        if (pushImmediately) {
          synchronized {
            if (pushImmediately) {
              pushImmediately = false
              push(value)
              increment(true)
            } else {
              increment(queue.offer(value))
            }
          }
        } else {
          increment(queue.offer(value))
        }
      }
    }

    private[akka] def poll(): T = {
      val value = queue.poll()
      if (value == null)
        pushImmediately = true
      value
    }

    /**
      * Indicate that the use of the queue is complete. This will allow the associated stream
      * to finish processing elements and then shutdown. Any new elements offered to the queue
      * will be dropped.
      */
    def complete(): Unit = {
      completed = true
      // push null to indicate to the graph stage that it is complete, otherwise it can hang
      // if it has already been pulled
      if (pushImmediately && queue.isEmpty) push(null.asInstanceOf[T])
    }

    /** Check if the queue is done, i.e., it has been completed and the queue is empty. */
    def isDone: Boolean = completed && queue.isEmpty
  }

  private class QueueSource[T](queueSupplier: () => SourceQueue[T])
      extends GraphStageWithMaterializedValue[SourceShape[T], SourceQueue[T]] {

    private val out = Outlet[T]("QueueSource")

    override val shape: SourceShape[T] = SourceShape(out)

    override def createLogicAndMaterializedValue(
      attrs: Attributes
    ): (GraphStageLogic, SourceQueue[T]) = {
      val queue = queueSupplier()
      val logic = new GraphStageLogic(shape) with OutHandler {
        override def onPull(): Unit = {
          if (queue.isDone) {
            complete(out)
          } else {
            val value = queue.poll()
            if (value != null) push(out, value)
          }
        }

        setHandler(out, this)
        queue.push = {
          val callback = getAsyncCallback[T](v => if (v == null) complete(out) else push(out, v))
          callback.invoke
        }
      }
      logic -> queue
    }
  }

  /**
    * Stage that measures the flow of data through the stream. It will keep track of three
    * meters:
    *
    * - `akka.stream.numEvents`: a simple counter indicating the number of events that flow
    *   through the stream.
    *
    * - `akka.stream.downstreamDelay`: a timer measuring the delay for the downstream to
    *   request a new element from the stream after an item is pushed. This can be used to
    *   understand if the downstream cannot keep up and is introducing significant back
    *   pressure delays.
    *
    * - `akka.stream.upstreamDelay`: a timer measuring the delay for the upstream to push
    *   a new element after one has been requested.
    *
    * @param registry
    *     Spectator registry to manage metrics.
    * @param id
    *     Dimension used to distinguish a particular usage.
    */
  def monitorFlow[T](registry: Registry, id: String): Flow[T, T, NotUsed] = {
    Flow[T].via(new MonitorFlow[T](registry, id))
  }

  private final class MonitorFlow[T](registry: Registry, id: String)
      extends GraphStage[FlowShape[T, T]] {

    private val numEvents = registry.counter("akka.stream.numEvents", "id", id)
    private val upstreamTimer = registry.timer("akka.stream.upstreamDelay", "id", id)
    private val downstreamTimer = registry.timer("akka.stream.downstreamDelay", "id", id)

    private val in = Inlet[T]("MonitorBackpressure.in")
    private val out = Outlet[T]("MonitorBackpressure.out")

    override val shape: FlowShape[T, T] = FlowShape(in, out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = {

      new GraphStageLogic(shape) with InHandler with OutHandler {

        private var upstreamStart: Long = -1L
        private var downstreamStart: Long = -1L

        override def onPush(): Unit = {
          upstreamStart = record(upstreamTimer, upstreamStart)
          push(out, grab(in))
          numEvents.increment()
          downstreamStart = registry.clock().monotonicTime()
        }

        override def onPull(): Unit = {
          downstreamStart = record(downstreamTimer, downstreamStart)
          pull(in)
          upstreamStart = registry.clock().monotonicTime()
        }

        private def record(timer: Timer, start: Long): Long = {
          if (start > 0L) {
            val delay = registry.clock().monotonicTime() - start
            timer.record(delay, TimeUnit.NANOSECONDS)
          }
          -1L
        }

        setHandlers(in, out, this)
      }
    }
  }

  /**
    * Map operation that passes in the materializer for the graph stage.
    *
    * @param f
    *     Function that maps the input value to a new value.
    * @return
    *     Flow that applies the mapping function to each value.
    */
  def map[V1, V2](f: (V1, Materializer) => V2): Flow[V1, V2, NotUsed] = {
    Flow[V1].via(new MapFlow(f))
  }

  private final class MapFlow[V1, V2](f: (V1, Materializer) => V2)
      extends GraphStage[FlowShape[V1, V2]] {

    private val in = Inlet[V1]("MapFlow.in")
    private val out = Outlet[V2]("MapFlow.out")

    override val shape: FlowShape[V1, V2] = FlowShape(in, out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = {

      new GraphStageLogic(shape) with InHandler with OutHandler {

        override def onPush(): Unit = {
          push(out, f(grab(in), materializer))
        }

        override def onPull(): Unit = {
          pull(in)
        }

        setHandlers(in, out, this)
      }
    }
  }

  /**
    * Flat map operation that passes in the materializer for the graph stage.
    *
    * @param f
    *     Function that maps the input value to a new value.
    * @return
    *     Flow that applies the mapping function to each value.
    */
  def flatMapConcat[V1, V2, M](
    f: (V1, Materializer) => Graph[SourceShape[V2], M]
  ): Flow[V1, V2, NotUsed] = {
    map(f).flatMapConcat(src => src)
  }

  /**
    * Maintains a sub-flow for each member of a cluster. The set of sub-flows can change
    * dynamically based on the members present in the input cluster definition.
    *
    * ```
    * [C] ---> f(C) ---> [I1] -> client(M1) -> [O1] --+---> [O]
    *               |                                 |
    *               |--> [I2] -> client(M2) -> [O2] --|
    *               |                                 |
    *               +--> [I3] -> client(M3) -> [O3] --+
    * ```
    *
    * TODO, queueing and backpressure
    * Use-cases
    * - subscribe to all members of a cluster
    * - publish to all members of a cluster
    *
    * @param f
    *     Function that converts an input representing a cluster and associated data into
    *     a map to a key for a member and the data that should be forwarded to that member.
    *     The full set of members should be present with each message or the sub-flow for
    *     missing members will be shutdown.
    * @param client
    *     Function that creates a sub-flow for a member of a cluster. The input data for
    *     the member will be emitted to the sub-flow. If a failure occurs for the client
    *     flow, then it will be recreated continually until the member is removed from the
    *     cluster definition.
    * @tparam C
    *     Represents a cluster and associated input data.
    * @tparam M
    *     Key identifying a member of a cluster.
    * @tparam I
    *     Input data to forward to the sub-flow for a member.
    * @tparam O
    *     Output data that will be flattened after the mapping.
    * @return
    *     Overall flow that performs the grouping and merges the output data from the
    *     cluster.
    */
  def clusterGroupBy[C <: AnyRef, M <: AnyRef, I <: AnyRef, O <: AnyRef](
    f: C => Map[M, I],
    client: M => Flow[I, O, NotUsed]
  ): Flow[C, O, NotUsed] = {

    Flow[C]
      .via(new ClusterGroupBy[C, M, I, O](f, client))
      .flatMapMerge(Int.MaxValue, sources => Source(sources))
      .flatMapMerge(Int.MaxValue, source => source)
  }

  private final class ClusterGroupBy[C <: AnyRef, M <: AnyRef, I <: AnyRef, O <: AnyRef](
    f: C => Map[M, I],
    client: M => Flow[I, O, NotUsed]
  ) extends GraphStage[FlowShape[C, List[Source[O, NotUsed]]]] {

    private val in = Inlet[C]("ClusterGroupBy.in")
    private val out = Outlet[List[Source[O, NotUsed]]]("ClusterGroupBy.out")

    override def shape: FlowShape[C, List[Source[O, NotUsed]]] = FlowShape(in, out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = {
      new GraphStageLogic(shape) with InHandler with OutHandler {

        private val registry = new NoopRegistry
        private val membersSources = mutable.AnyRefMap.empty[M, MemberQueue[I]]

        override def onPush(): Unit = {
          val cluster = grab(in)
          val members = f(cluster)
          val keys = members.keySet
          val current = membersSources.keySet

          val removed = current -- keys
          if (removed.nonEmpty) {
            logger.debug(s"members removed: $removed")
          }
          removed.foreach { m =>
            membersSources.remove(m).foreach { ref =>
              logger.debug(s"stopping $m")
              ref.queue.complete()
            }
          }

          val added = keys -- current
          if (added.nonEmpty) {
            logger.debug(s"members added: $added")
          }
          val sources = added
            .toList
            .map { m =>
              // TODO: restart handling etc
              val queue = new SourceQueue[I](registry, "_", new ArrayBlockingQueue[I](100))
              membersSources += m -> new MemberQueue(queue, null.asInstanceOf[I])
              Source
                .fromGraph(new QueueSource[I](() => queue))
                .mapMaterializedValue(_ => NotUsed)
                .via(client(m))
            }

          // Push down the value for all entries
          members.foreachEntry { (k, v) =>
            membersSources(k).offer(v)
          }

          push(out, sources)
        }

        override def onPull(): Unit = {
          pull(in)
        }

        override def onUpstreamFinish(): Unit = {
          membersSources.values.foreach(_.queue.complete())
          super.onUpstreamFinish()
        }

        setHandlers(in, out, this)
      }
    }
  }

  private final class MemberQueue[V](val queue: SourceQueue[V], var last: V) {
    def offer(value: V): Unit = {
      if (value != last) {
        logger.trace(s"previous = [$last], current = [$value]")
        queue.offer(value)
        last = value
      }
    }
  }
}
