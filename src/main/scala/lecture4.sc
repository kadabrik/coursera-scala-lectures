def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => (y * y) :: squareList(ys)
  }

squareList(List(1, 2, 3))

def squareListMap(xs: List[Int]): List[Int] =
  xs map (y => y * y)

squareListMap(List(1, 2, 3))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    xs1 span (y => y == x) match {
      case (a, b) => (x :: a) :: pack(b)
    }
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] = {
  def count[T](ys: List[List[T]]): List[(T, Int)] = {
    ys match {
      case Nil => Nil
      case ys1 :: ys2 => (ys1.head, ys1.length) :: count(ys2)
    }
  }

  count(pack(xs))
}

encode(List("a", "a", "a", "b", "c", "c", "a"))
