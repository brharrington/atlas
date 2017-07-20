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
  private val baseId = registry.createId("http.server.requestDurationPercentile")
    .withTag("deployment.aws.availability-zone", "us-west-1c")
    .withTag("deployment.aws.instance-id", "i-09862cd0a39d51782")
    .withTag("deployment.aws.instance-type", "m4.2xlarge")
    .withTag("deployment.aws.local-ipv4", "172.18.92.144")
    .withTag("deployment.canary.enabled", "false")
    .withTag("deployment.environment", "prod")
    .withTag("deployment.stack.name", "agora-directory-prod-201707181516")
    .withTag("http.request.method", "GET")
    .withTag("registration.application.name", "core-v1")
    .withTag("registration.application.service.boundedContext", "IAM")
    .withTag("registration.application.service.name", "directoryDS")


  override def startImpl(): Unit = {
    val options = new Scheduler.Options()
      .withFrequency(Scheduler.Policy.FIXED_DELAY, Duration.ofSeconds(1))
      .withStopOnFailure(false)
    scheduler.schedule(options, () => {
      (0 until 10).foreach { i =>
        val latency = ThreadLocalRandom.current().nextInt(10, 10000)

        val id1 = baseId
        PercentileTimer.get(registry, id1).record(latency, TimeUnit.MILLISECONDS)

        val id2 = baseId
          .withTag("eureka.status", "UP")
        PercentileTimer.get(registry, id2).record(latency, TimeUnit.MILLISECONDS)

        val id3 = id2
          .withTag("http.response.status", "200")
        PercentileTimer.get(registry, id3).record(latency, TimeUnit.MILLISECONDS)

        val id4 = id3
          .withTag("http.clientName", "UNKNOWN")
          .withTag("http.request.rootBoundedContext", "undefined")
          .withTag("http.resourceMethod", "PingResource._getPing")
        PercentileTimer.get(registry, id4).record(latency, TimeUnit.MILLISECONDS)
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
