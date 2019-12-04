package com.netflix.atlas.eval.stream

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.Attributes
import akka.stream.FlowShape
import akka.stream.Inlet
import akka.stream.Outlet
import akka.stream.SourceShape
import akka.stream.ThrottleMode
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.RestartSource
import akka.stream.scaladsl.Source
import akka.stream.stage.GraphStage
import akka.stream.stage.GraphStageLogic
import akka.stream.stage.InHandler
import akka.stream.stage.OutHandler
import com.netflix.atlas.akka.StreamOps
import com.netflix.spectator.api.NoopRegistry
import com.netflix.spectator.api.Registry
import com.netflix.spectator.api.Timer
import org.reactivestreams.Publisher
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Failure

class FooSuite extends AnyFunSuite {

  private def dataStream(k: String): Flow[String, String, NotUsed] = {
    Flow[String]
      .zipWithIndex
      .map {
        //case (h, i) if h == "c" && i == 3 =>
        //  throw new RuntimeException("failure")
        case t => s"$k ==> $t"
      }
  }


  test("foo") {
    implicit val system = ActorSystem("test")
    implicit val materializer = StreamOps.materializer(system, new NoopRegistry)

    val hostsSrc = Source(
      List(
        List("a", "b", "c"),
        List("a", "bb", "c"),
        List("a", "c"),
        List("a", "c", "d"),
        List("a", "c", "e"),
        List("a", "e", "f"),
        List("b", "e", "f"),
      )
    )

    val future = hostsSrc
      .throttle(1, 5.seconds, 1, ThrottleMode.Shaping)
      .via(clusterGroupBy(_.map(v => v.substring(0, 1) -> v).toMap, dataStream))
      .runForeach(println)

    Await.result(future, Duration.Inf)
  }

  // m: C => Set[I]
  // k: I => K
  // client: K => Flow[I, O, NotUsed]
  // how to pass to datasources to client flow
  private def clusterGroupBy[C <: AnyRef, K <: AnyRef, I <: AnyRef, O <: AnyRef](
    f: C => Map[K, I],
    client: K => Flow[I, O, NotUsed]
  ): Flow[C, O, NotUsed] = {

    Flow[C]
      .via(new ClusterGroupBy[C, K, I, O](f, client))
      .flatMapMerge(Int.MaxValue, sources => Source(sources))
      .flatMapMerge(Int.MaxValue, source => source)
  }

  private final class ClusterGroupBy[C <: AnyRef, K <: AnyRef, I <: AnyRef, O <: AnyRef](
    f: C => Map[K, I],
    client: K => Flow[I, O, NotUsed]
  ) extends GraphStage[FlowShape[C, List[Source[O, NotUsed]]]] {

    private val in = Inlet[C]("ClusterGroupBy.in")
    private val out = Outlet[List[Source[O, NotUsed]]]("ClusterGroupBy.out")

    override def shape: FlowShape[C, List[Source[O, NotUsed]]] = FlowShape(in, out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = {
      new GraphStageLogic(shape) with InHandler with OutHandler {

        private val membersSources = mutable.AnyRefMap.empty[K, MemberQueue[I]]

        override def onPush(): Unit = {
          val cluster = grab(in)
          val members = f(cluster)
          val keys = members.keySet
          val current = membersSources.keySet

          val removed = current -- keys
          if (removed.nonEmpty) {
            println(s"members removed: $removed")
          }
          removed.foreach { m =>
            membersSources.remove(m).foreach { ref =>
              println(s"stopping $m")
              ref.queue.complete()
            }
          }

          val added = keys -- current
          if (added.nonEmpty) {
            println(s"members added: $added")
          }
          val sources = added
            .toList
            .map { m =>
              // TODO: restart handling etc
              val queue = new SourceQueue[I](new ArrayBlockingQueue[I](1))
              membersSources += m -> new MemberQueue(queue, null.asInstanceOf[I])
              Source
                .fromGraph(new QueueSource[I](queue))
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

        setHandlers(in, out, this)
      }
    }
  }

  class MemberQueue[V](val queue: SourceQueue[V], var last: V) {
    def offer(value: V): Unit = {
      if (value != last) {
        println(s"previous = [$last], current = [$value]")
        queue.offer(value)
        last = value
      }
    }
  }

  class SourceQueue[T](queue: BlockingQueue[T]) {

    private[stream] var push: T => Unit = _

    @volatile private var pushImmediately: Boolean = false

    @volatile private var completed: Boolean = false

    private def increment(result: Boolean): Boolean = {
      result
    }

    /**
      * Add the value into the queue if there is room. Returns true if the value was successfully
      * enqueued.
      */
    def offer(value: T): Boolean = {
      if (completed) {
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

    private[stream] def poll(): T = {
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

  private final class QueueSource[V](queue: SourceQueue[V]) extends GraphStage[SourceShape[V]] {

    private val out = Outlet[V]("QueueSource")
    override val shape: SourceShape[V] = SourceShape(out)

    override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = {
      new GraphStageLogic(shape) with OutHandler {
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
          val callback = getAsyncCallback[V](v => if (v == null) complete(out) else push(out, v))
          callback.invoke
        }
      }
    }

  }
}
