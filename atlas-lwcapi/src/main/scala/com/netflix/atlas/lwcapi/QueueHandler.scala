package com.netflix.atlas.lwcapi

import akka.stream.QueueOfferResult
import akka.stream.scaladsl.SourceQueueWithComplete
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.Future

class QueueHandler(id: String, queue: SourceQueueWithComplete[SSERenderable]) extends StrictLogging {
  def offer(msg: SSERenderable): Future[QueueOfferResult] = {
    logger.trace(s"enqueuing message for $id: ${msg.toSSE}")
    queue.offer(msg)
  }

  def complete(): Unit = {
    logger.debug(s"queue complete for $id")
    queue.complete()
  }

  override def toString: String = s"QueueHandler($id)"
}
