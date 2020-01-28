package patmat

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, cs, _) => cs
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = timesAcc(chars, List())

  def timesAcc(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = chars match {
    case x::xs => timesAcc(xs, timesAccHelper(x, acc))
    case List() => acc
  }

  def timesAccHelper(char: Char, acc: List[(Char, Int)]): List[(Char, Int)] = {
    if (isCharInPairs(char, acc)) updateTimesAcc(char, acc, List())
    else (char, 1)::acc
  }
    
  def isCharInPairs(char: Char, pairs: List[(Char, Int)]): Boolean = pairs match {
    case x::xs if (x._1==char) => true
    case x::xs if (x._1!=char) => isCharInPairs(char, xs)
    case List() => false
  }

  def updateTimesAcc(char: Char, old: List[(Char, Int)], acc: List[(Char, Int)]): List[(Char, Int)] = old match {
    case x::xs => updateTimesAcc(char, xs, updateTimesPair(char, x)::acc)
    case List() => acc match {
      case List() => List((char, 1))
      case _ => acc
    }
  }

  def updateTimesPair(char: Char, pair: (Char, Int)): (Char, Int) = if (char == pair._1) (pair._1, pair._2 + 1) else pair

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    val orderedFreqList = sortFreqList(freqs)
    convertFreqListToLeafList(orderedFreqList)
  }

  def sortFreqList(freqs: List[(Char, Int)]): List[(Char, Int)] = {
    if (freqs.length < 2) freqs
    else {
      val pivot = freqs.head._2
      sortFreqList (freqs filter (_._2 < pivot)) ++ (freqs filter (_._2 == pivot)) ++ sortFreqList (freqs filter(_._2 > pivot))
    }
  }

  def convertFreqListToLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.map( f => Leaf(f._1, f._2))

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case x::Nil => true
    case _ => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => trees
    case x1::Nil => trees
    case (l1 @ Leaf(c1, w1)) :: (l2 @ Leaf(c2, w2)) :: xs => Fork(l1, l2, List(c1, c2), w1 + w2) :: xs
    case (f1 @ Fork(_, _, ls1, w1)) :: (f2 @ Fork(_, _, ls2, w2)) :: xs => Fork(f1, f2, ls1:::ls2, w1 + w2) :: xs
    case (l1 @ Leaf(c1, w1)) :: (f2 @ Fork(_, _, ls, w2)) :: xs => Fork(l1, f2, c1::ls, w1 + w2) :: xs
    case (f1 @ Fork(_, _, ls, w1)) :: (l2 @ Leaf(c2, w2)) :: xs => Fork(f1, l2, c2::ls, w1 + w2) :: xs
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = 
    if (done(trees)) trees
    else {
      until(done, merge)(merge(trees))
    }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val trees = makeOrderedLeafList(times(chars))
    val sing: List[CodeTree] = until(singleton, combine)(trees)
    sing.head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = decodeAcc(tree, bits, List())

  def decodeAcc(root: CodeTree, bits: List[Bit], acc:List[Char]): List[Char] = bits match {
    case Nil => acc.reverse
    case _ => {
      val (char, rest_bits) = decodeChar(root, bits)
      decodeAcc(root, rest_bits, char::acc)
    }
  }


  // decode one char, return char and rest bits
  def decodeChar(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = tree match {
    case Leaf(c, _) => (c, bits)
    case Fork(l, r, _, _) => bits match {
      case 0::xs => decodeChar(l, xs)
      case 1::xs => decodeChar(r, xs)
    }
    
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = encodeAcc(tree)(text)(List())

  def encodeAcc(tree: CodeTree)(text: List[Char])(acc: List[Bit]): List[Bit] = text match {
    case Nil => acc
    case x::xs => {
      val charEncoded: List[Bit] = encodeChar(tree)(x)
      encodeAcc(tree)(xs)(acc++charEncoded)
    }
  }


  def encodeChar(tree: CodeTree)(text: Char): List[Bit] = {
    encodeCharAcc(tree)(text)(List())  
  }

  def encodeCharAcc(tree: CodeTree)(text: Char)(acc: List[Bit]): List[Bit] = tree match {
    case Leaf(c, _) => acc.reverse
    case Fork(left @ Fork(_, _, csLeft, _), right, _, _) => {
      if (csLeft.contains(text)) encodeCharAcc(left)(text)(0::acc)
      else encodeCharAcc(right)(text)(1::acc)
    } 
    case Fork(left @ Leaf(c, _), right, _, _) => {
      if (c == text) encodeCharAcc(left)(text)(0::acc)
      else encodeCharAcc(right)(text)(1::acc)
    } 
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case (charCurr, ls)::xs => {
      if (char == charCurr) ls
      else codeBits(xs)(char) 
    }
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Fork(_, _, chars, _) => convertAcc(tree)(chars)(List())
  }
  def convertAcc(tree: CodeTree)(chars: List[Char])(acc: CodeTable): CodeTable = chars match {
    case Nil => acc
    case x::xs => {
      val entry = (x, encodeChar(tree)(x))
      convertAcc(tree)(xs)(entry::acc)
    }
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a++b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    quickEncodeAcc(table)(text)(List())
  }

  def quickEncodeAcc(table: CodeTable)(text: List[Char])(acc: List[Bit]): List[Bit] = text match {
    case Nil => acc
    case x::xs => {
      val encoded = codeBits(table)(x)
      quickEncodeAcc(table)(xs)(acc++encoded)
    }
  }

}

object Huffman extends Huffman

object Main extends App {

  println(Huffman.decodedSecret)

}
