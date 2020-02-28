package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.junit._
import org.junit.Assert.assertEquals
import java.io.File
import StackOverflow._

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {
  import StackOverflowSuite._


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  trait TestRDDs {
    val rdd0 = sc.parallelize(Seq(
      Posting(1, 100, None, None, 0, None),
      Posting(2, 201, None, Some(100), 1, None),
      Posting(2, 202, None, Some(100), 2, None),
      Posting(1, 101, None, None, 0, None),
      Posting(2, 204, None, Some(101), 1, None),
      Posting(1, 102, None, None, 0, None),
      Posting(2, 205, None, None, 0, None),
    ))

  }

  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  @Test def `groupedPostings on simple case`: Unit = {
    new TestRDDs {
      val res = groupedPostings(rdd0).collect().toList
      assert(res.size == 2)
      assert(res.map(_._2.size).reduce(_ + _) == 3)
    }
  }

  @Test def `scoredPostings on simple case`: Unit = {
    new TestRDDs {
      val res = scoredPostings(groupedPostings(rdd0))
      val r100 = res.filter(r => r._1.id == 100).collect().toList.head
      val r101 = res.filter(r => r._1.id == 101).collect().toList.head
      assertEquals(r100._2, 2)
      assertEquals(r101._2, 1)
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
