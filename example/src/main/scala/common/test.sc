

def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => y*y :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map(x=>x*x)


val sqlist = List(0,1,2,3)
val l2 = List("a", "a", "a", "b", "c", "c", "a")

squareList(sqlist)
squareList2(sqlist)




def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))


pack(l2)
encode(l2 )
//List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))