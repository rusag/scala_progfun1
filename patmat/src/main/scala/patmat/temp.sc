import patmat.Huffman.{Bit, CodeTree, Fork, Leaf, chars, combine, createCodeTree, makeOrderedLeafList, singleton, times, until}

//def createCodeTree(chars: List[Char]): CodeTree = {
//  until(singleton, combine)(makeOrderedLeafList(times(chars))).head
//}

val t1 = List(Leaf('a', 1), Leaf('t', 2), Leaf('b',3),  Leaf('z',5), Leaf('w',7))
val t2 = "hello".toList
val t3 = List(Leaf('a', 1))
val t4 = Nil
val t5 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

def single_encode(tree: CodeTree, bits: List[Bit],  ch: Char): List[Bit] =
  tree match {
    case Leaf(_ , _) => bits
    case Fork(l, r, _, _) => if (chars(l) contains ch) single_encode(l, 0 :: bits, ch) else single_encode(r, 1 :: bits, ch)
  }

def encode(tree: CodeTree)(text: List[Char]): List[Bit] = text match {
  case Nil => Nil
  case x::Nil => single_encode(tree, Nil, x)
  case x::xs => single_encode(tree, Nil, x) ::: encode(tree)(xs)
}

//encode(createCodeTree(t2))("tat".toList)

//encode(t5)("abbd".toList)

single_encode(t5,Nil ,'b')

//=== List(0,0,0,1,0,1,1)

//times(t2)
//makeOrderedLeafList(times(t2))
//
//combine(makeOrderedLeafList(times(t2)))
//combine(combine(combine(makeOrderedLeafList(times(t2)))))

//singleton(t1)
//singleton(t4)
//singleton(t3)

//until(singleton, combine)(makeOrderedLeafList(times(t2))).head


createCodeTree(t2)