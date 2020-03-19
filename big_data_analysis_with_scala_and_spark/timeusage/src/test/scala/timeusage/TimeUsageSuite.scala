package timeusage

import org.apache.spark.sql.{Column, ColumnName, DataFrame, Row}
import org.junit.{Assert, Test}
import org.junit.Assert.assertEquals

import scala.util.Random

class TimeUsageSuite {
  import TimeUsage._
  @Test def `classifiedColumns on simple input`: Unit = {
    val input = List("t0100", "t180100", "t180500", "t02000", "t189900", "someInput")
    val res = classifiedColumns(input)
    val ans = (List(new Column("t0100"), new Column("t180100")), List(new Column("t180500")),
      List(new Column("t02000"), new Column("t189900")))

    println(res)

    assertEquals(res._1.toSet, ans._1.toSet)
    assertEquals(res._2.toSet, ans._2.toSet)
    assertEquals(res._3.toSet, ans._3.toSet)
  }
}
