package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  trait TestEncodeDecode {
    val ls = List('a', 'b', 'a', 'c', 'a')
    val tree = createCodeTree(ls)
    val res_a = List(1)
    val res_b = List(0, 0)
    val res_c = List(0, 1)
    val res_abc = List(1, 0, 0, 0, 1)
  }

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `test times`: Unit = {
    val charList = List('a', 'b', 'a')
    assertEquals(List(('a', 2), ('b', 1)), times(charList))
  }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `decodeChar test`: Unit = {
    val ls = List('a', 'b', 'a', 'c', 'a')
    val tree = Huffman.createCodeTree(ls)
    val res_a = decodeChar(tree, List(1))
    val res_b = decodeChar(tree, List(0, 0))
    val res_c = decodeChar(tree, List(0, 1))
    assertEquals(('a', List()), res_a)
    assertEquals(('b', List()), res_b)
    assertEquals(('c', List()), res_c)
  }

  @Test def `decode test`: Unit = {
    new TestEncodeDecode {
      val a = decode(tree, res_a)
      val b = decode(tree, res_b)
      val c = decode(tree, res_c)
      val abc = decode(tree, res_abc)
      assertEquals(List('a'), a)
      assertEquals(List('b'), b)
      assertEquals(List('c'), c)
      assertEquals(List('a', 'b', 'c'), abc)
    }
  }

  @Test def `encodeChar test`: Unit = {
    val ls = List('a', 'b', 'a', 'c', 'a')
    val tree = Huffman.createCodeTree(ls)
    val res_a = encodeChar(tree)('a')
    val res_b = encodeChar(tree)('b')
    val res_c = encodeChar(tree)('c')
    assertEquals(List(1), res_a)
    assertEquals(List(0, 0), res_b)
    assertEquals(List(0, 1), res_c)
  }

  @Test def `encode test`: Unit = {
    new TestEncodeDecode {
      val encoded_abc = encode(tree)(List('a', 'b', 'c'))
      assertEquals(res_abc, encoded_abc)
    }
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `quickEncode test`: Unit = {
    new TestEncodeDecode {
      val encoded_abc = quickEncode(tree)(List('a', 'b', 'c'))
      assertEquals(res_abc, encoded_abc)
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
