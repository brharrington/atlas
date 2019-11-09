package com.netflix.atlas.eval.graph

import akka.http.scaladsl.model.Uri
import com.netflix.atlas.core.model.CustomVocabulary
import com.netflix.atlas.core.model.DataExpr
import com.netflix.atlas.core.model.ModelExtractors
import com.netflix.atlas.core.model.Query
import com.netflix.atlas.core.model.Query.KeyValueQuery
import com.netflix.atlas.core.model.StyleExpr
import com.netflix.atlas.core.stacklang.Interpreter
import com.netflix.atlas.core.util.Streams
import com.netflix.atlas.json.Json
import com.typesafe.config.ConfigFactory
import org.scalatest.FunSuite

class SimpleLegendsSuite extends FunSuite {

  private val notSet = "__NOT_SET__"

  private val interpreter = Interpreter(new CustomVocabulary(ConfigFactory.load()).allWords)

  private def eval(str: String): List[StyleExpr] = {
    interpreter.execute(str).stack
      .map {
        case ModelExtractors.PresentationType(t) => t//.copy(settings = t.settings - "legend")
      }
      .reverse
      .flatMap(_.perOffset)
  }

  private def legends(str: String): List[String] = {
    //try {
      SimpleLegends.generate(eval(str)).map(_.settings.getOrElse("legend", notSet))
    //} catch {
    //  case _: Exception => Nil
    //}
  }

  test("honor explicit legend") {
    assert(legends("name,cpu,:eq,:sum,foo,:legend") === List("foo"))
  }

  test("just math") {
    assert(legends("4,5,:add,10,:mul") === List(notSet))
  }

  test("prefer just name") {
    assert(legends("name,cpu,:eq,:sum") === List("cpu"))
    assert(legends("name,cpu,:eq,id,user,:eq,:and,:sum") === List("cpu"))
  }

  test("use group by keys") {
    assert(legends("name,cpu,:eq,:sum,(,app,id,),:by") === List("$app $id"))
  }

  test("name with math") {
    assert(legends("name,cpu,:eq,:sum,4,:add,6,:mul,:abs") === List("cpu"))
  }

  test("name regex") {
    assert(legends("name,cpu,:re,:sum") === List("cpu"))
  }

  test("name not present") {
    assert(legends("id,user,:eq,:sum") === List("user"))
  }

  test("name with offsets") {
    val expr = "name,cpu,:eq,:sum,(,0h,1w,),:offset"
    assert(legends(expr) === List("cpu", "cpu (offset=$atlas.offset)"))
  }

  test("name with dist avg") {
    assert(legends("name,cpu,:eq,:dist-avg") === List("cpu"))
  }

  test("name not clause") {
    assert(legends("name,cpu,:eq,:not,:sum") === List("!cpu"))
  }

  test("name with node avg") {
    assert(legends("name,cpu,:eq,:node-avg") === List("cpu"))
  }

  test("group by with offsets") {
    val expr = "name,cpu,:eq,:sum,(,id,),:by,(,0h,1w,),:offset"
    assert(legends(expr) === List("$id", "$id (offset=$atlas.offset)"))
  }

  test("complex: same name and math") {
    assert(legends("name,cpu,:eq,:sum,:dup,:add") === List("cpu"))
  }

  test("complex: not clause") {
    val expr = "name,cpu,:eq,:dup,id,user,:eq,:and,:sum,:swap,id,user,:eq,:not,:and,:sum"
    assert(legends(expr) === List("user", "!user"))
  }

  test("complex: different names and math") {
    assert(legends("name,cpu,:eq,:sum,name,disk,:eq,:sum,:and") === List(notSet))
  }

  test("multi: different names") {
    assert(legends("name,cpu,:eq,:sum,name,disk,:eq,:sum") === List("cpu", "disk"))
  }

  test("multi: same name further restricted") {
    val vs = legends("name,cpu,:eq,:sum," +
        "name,cpu,:eq,id,user,:eq,:and,:sum," +
        "name,cpu,:eq,id,system,:eq,:and,:sum," +
        "name,cpu,:eq,id,idle,:eq,:and,:sum,"
    )
    assert(vs === List("cpu", "user", "system", "idle"))
  }

  test("multi: same name with math") {
    val vs = legends("name,cpu,:eq,:sum,:dup,4,:add")
    assert(vs === List("cpu", "cpu"))
  }

  ignore("uris") {
    val lines = Streams.scope(Streams.fileIn("/Users/brharrington/repos/atlas-oss/uris")) { in =>
      Streams.lines(in).toList
    }

    val queries = lines
      .map(Uri.apply)
      .flatMap(_.query().get("q"))

    val results = queries
      .map { q => q -> legends(q) }
      .toMap

    val totalUniq = results.size
    val totalExprs = results.map(_._2.size).sum
    val totalEmpty = results.count(_._2.isEmpty)

    val totalDflt = results.flatMap(_._2).count(_ == notSet)
    val dfltPct = 100.0 * totalDflt / totalExprs

    println(s"lines   ${lines.size}")
    println(s"queries ${queries.size}")
    println(s"uniq    $totalUniq")
    println(s"exprs   $totalExprs")
    println(s"empty   $totalEmpty")
    println(s"dflt    $totalDflt ($dfltPct%)")

    Streams.scope(Streams.fileOut("/Users/brharrington/repos/atlas-oss/legends")) { out =>
      Json.encode(out, results)
    }
  }
}
