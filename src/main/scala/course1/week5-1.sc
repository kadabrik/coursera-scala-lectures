def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

init(List(1, 2, 3))

def removeAt[T](n: Int, xs: List[T]): List[T] = {
  if (n < 0 || n >= xs.length) throw new Error("n index out of bounds")
  n match {
    case 0 => xs.tail
    case _ => xs.head :: removeAt(n - 1, xs.tail)
  }
}

removeAt(1, List(1, 2, 3))

def removeAtAlt[T](n: Int, xs: List[T]): List[T] = {
  (xs take n) ++ (xs drop n + 1)
}

removeAtAlt(1, List(1, 2, 3))

def flatten(xs: List[Any]): List[Any] = {
  xs match {
    case Nil => xs
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))
