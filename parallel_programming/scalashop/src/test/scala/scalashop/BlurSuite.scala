package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  import VerticalBoxBlur.fromToPair

  @Test def `fromToPair test`: Unit = {
    val res0 = List((0, 2), (2, 4), (4, 6))
    assertEquals(res0, fromToPair(6, 3))

    val res1 = List((0, 2), (2, 4), (4, 5))
    assertEquals(res1, fromToPair(5, 3))

  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
