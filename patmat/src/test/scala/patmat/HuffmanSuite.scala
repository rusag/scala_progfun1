package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val table1 = List(('a', List(0,0)), ('b',List(0,1)), ('d', List(1)))
	}
//

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("Singleton for Nil") {
    assert(singleton(Nil) === false)
  }

  test("Singleton for one element") {
    assert(singleton(List(Leaf('t', 2))) === true)
  }

  test("Singleton for two elements") {
    assert(singleton(List(Leaf('t', 2), Leaf('b',3))) === false)
  }

  test("Singleton for many elements") {
    assert(singleton(List(Leaf('a', 1), Leaf('t', 2), Leaf('b',3),  Leaf('z',5), Leaf('w',7))) === false)
  }

  test("times for frequencies") {
    assert(times(List('h','e','l','l','o')) === List(('e',1), ('h',1), ('l',2), ('o',1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

//  test("createCodeTree for some characters") {
//    assert(createCodeTree(List('h','e','l','l','o')) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
//  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode a longer text") {
    new TestTrees {
      assert(decode(t1, List(0,1,0,0,0,0,1,1,0,1,0,1,1,0)) === "abaaaabbababba".toList)
    }
  }

  test("single encode a char") {
    new TestTrees {
      assert(single_encode(t2, Nil, 'b') === List(0,1))
    }
  }

  test("encode a longer text") {
    new TestTrees {
      assert(encode(t1)("abaaaabbababba".toList) === List(0,1,0,0,0,0,1,1,0,1,0,1,1,0))
    }
  }

  test("encode with a bigger tree") {
    new TestTrees {
      assert(encode(t2)("abbd".toList) === List(0,0,0,1,0,1,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abaaaabbababba".toList)) === "abaaaabbababba".toList)
    }
  }

  test("codeBits char 1") {
    new TestTrees {
      assert(codeBits(table1)('d') === List(1))
    }
  }
  test("codeBits char 2") {
    new TestTrees {
      assert(codeBits(table1)('b') === List(0,1))
    }
  }

  test("codeBits Nil") {
    new TestTrees {
      assert(codeBits(Nil)('a') === Nil)
    }
  }

  test("Convert short codetree t1") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("Convert long codetree t1") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0,0)) , ('b', List(0,1)), ('d', List(1))))
    }
  }

}
