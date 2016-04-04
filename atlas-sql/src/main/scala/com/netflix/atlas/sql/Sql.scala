package com.netflix.atlas.sql

/**
  * Created by brharrington on 4/3/16.
  */
object Sql {
  case class Query(
    select: Select,
    from: From,
    unnest: Option[Unnest],
    where: Option[Where],
    groupBy: Option[GroupBy],
    orderBy: Option[OrderBy],
    limit: Option[Limit]) {

    override def toString: String = {
      val others = List(unnest, where, groupBy, orderBy, limit).flatten.mkString("\n")
      s"""
         |$select
         |$from
         |$others
       """.stripMargin.trim
    }
  }

  case class Select(columns: List[Column]) {
    override def toString: String = {
      "SELECT " + columns.mkString(", ")
    }
  }

  case class Column(name: String, alias: Option[String]) {
    override def toString: String = {
      alias.fold(name)(a => s"$name AS $a")
    }
  }

  sealed trait From {
    def alias: Option[String]
  }

  case class FromQuery(q: Query, alias: Option[String] = None) extends From {
    override def toString: String = s"FROM ($q)"
  }

  case class FromTable(table: String) extends From {
    def alias: Option[String] = None
    override def toString: String = s"FROM $table"
  }

  case class Unnest(name: String)

  case class Where()
  case class GroupBy()
  case class OrderBy()
  case class Limit()
}
