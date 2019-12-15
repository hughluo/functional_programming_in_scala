package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   */
    @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains all elements that are in both set`: Unit = {
    new TestSets {
      val u12 = union(s1, s2)
      val u13 = union(s1, s3)
      val s = intersect(u12, u13)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  @Test def `diff contains different of two given set`: Unit = {
    new TestSets {
      val u12 = union(s1, s2)
      val u23 = union(s2, s3)
      val s = diff(u12, u23)
      assert(contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(!contains(s, 3), "diff 3")
    }
  }

  @Test def `filter contains the subset of s which p holds`: Unit = {
    new TestSets {
      val u12 = union(s1, s2)
      val s = filter(u12, elem => elem == 1)
      assert(contains(s, 1), "filter 1")
      assert(!contains(s, 2), "filter 2")
      assert(!contains(s, 3), "filter 3")
    }
  }

  @Test def `forall check whether all bounded integer within s satisfy p`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(forall(s, elem => elem < 3), "forall 1")
      assert(!forall(s, elem => elem >= 3), "forall 2")
    }
  }

  @Test def `exists check whether there is a bounded integer within s satisfy p`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(exists(s, elem => elem < 2), "exists 1")
      assert(!exists(s, elem => elem >= 3), "exists 2")
    }
  }

  @Test def `map transfrom a set by applying given function`: Unit = {
    new TestSets {
      val s = map(union(s2, s3), elem => elem - 1)
      assert(contains(s, 1), "map 1")
      assert(contains(s, 2), "map 1")
      assert(!contains(s, 3), "map 3")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
