package com.netflix.atlas.standalone

import java.time.Duration
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeUnit
import javax.inject.Inject

import com.google.inject.AbstractModule
import com.google.inject.Module
import com.google.inject.multibindings.Multibinder
import com.netflix.iep.service.AbstractService
import com.netflix.iep.service.Service
import com.netflix.spectator.api.Registry
import com.netflix.spectator.api.histogram.PercentileTimer
import com.netflix.spectator.impl.Scheduler

class Issue629Service @Inject() (registry: Registry) extends AbstractService {

  private val scheduler = new Scheduler(registry, "issue629", 2)
  private val baseId = registry.createId("issue629")

  override def startImpl(): Unit = {
    val options = new Scheduler.Options()
      .withFrequency(Scheduler.Policy.FIXED_DELAY, Duration.ofSeconds(1))
      .withStopOnFailure(false)
    scheduler.schedule(options, () => {
      (0 until 10).foreach { i =>
        val latency = ThreadLocalRandom.current().nextInt(10, 10000)
        val id = baseId.withTag("node", f"i-$i%02d")
        PercentileTimer.get(registry, id).record(latency, TimeUnit.MILLISECONDS)
      }
    })
  }

  override def stopImpl(): Unit = scheduler.shutdown()
}

object Issue629Service {
  def newGuiceModule: Module = {
    new AbstractModule {
      override def configure(): Unit = {
        val serviceBinder = Multibinder.newSetBinder(binder, classOf[Service])
        serviceBinder.addBinding().to(classOf[Issue629Service])
      }
    }
  }
}
