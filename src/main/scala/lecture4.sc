def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => (y * y) :: squareList(ys)
  }

squareList(List(1, 2, 3))

def squareListMap(xs: List[Int]): List[Int] =
  xs map (y => y * y)

squareListMap(List(1, 2, 3))
