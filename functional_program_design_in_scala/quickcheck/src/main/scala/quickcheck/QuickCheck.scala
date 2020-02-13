package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  def abPropHelper(a: Int, b: Int): Boolean = {
    val ha = insert(a, empty)
    val hab = insert(b, ha)
    val res = findMin(hab)
    if (a > b) res == b else res == a
  }

  property("ab") = forAll { (a: Int, b: Int) =>
    abPropHelper(a, b)
  }

  property("empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val res = deleteMin(h)
    isEmpty(res)
  }

  def listToMinHeap(xs: List[Int]): H = xs match {
    case Nil => empty
    case x :: rest => insert(x, listToMinHeap(rest))
  }

  def minHeapToSortedList(h: H): List[Int] = {
    if (isEmpty(h)) List()
    else {
      val x = findMin(h)
      x :: minHeapToSortedList(deleteMin(h))
    }
  }

  def sortPropHelper(xs: List[Int]): Boolean = {
    val h = listToMinHeap(xs)
    val res = minHeapToSortedList(h)
    res == res.sorted
  }

  property("sort") = forAll { (xs: List[Int]) =>
    sortPropHelper(xs)
  }

  property("commutative") = forAll { (a: Int, b: Int) =>
    val lab = minHeapToSortedList(listToMinHeap(List(a, b)))
    val lba = minHeapToSortedList(listToMinHeap(List(b, a)))
    lab == lba
  }


  def meldPropHelper(h1: H, h2: H): Boolean = {
    val h12 = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) isEmpty(h12)
    else {
      val equalToH1Min = if (isEmpty(h1)) false else findMin(h12) == findMin(h1)
      val equalToH2Min = if (isEmpty(h2)) false else findMin(h12) == findMin(h2)
      equalToH1Min || equalToH2Min
    }
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    meldPropHelper(h1, h2)
  }

  def deleteMinPropHelper(h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val min1 = findMin(h)
      val h1 = deleteMin(h)
      if (isEmpty(h1)) true else min1 < findMin(h1)
    }
  }

  property("deleteMin1") = forAll { (h: H) =>
    deleteMinPropHelper(h)
  }

}
