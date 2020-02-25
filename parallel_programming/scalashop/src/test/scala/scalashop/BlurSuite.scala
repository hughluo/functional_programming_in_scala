package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  import VerticalBoxBlur.fromToPairs

  @Test def `fromToPair test`: Unit = {
    val res0 = List((0, 2), (2, 4), (4, 6))
    assertEquals(res0, fromToPairs(6, 3))

    val res1 = List((0, 2), (2, 4), (4, 5))
    assertEquals(res1, fromToPairs(5, 3))

    val res2 = List((0, 6))
    assertEquals(res2, fromToPairs(6, 1))

  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
