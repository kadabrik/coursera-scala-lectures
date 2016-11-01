trait Generator[+T] {
  self => // an alias for ”this”.
  def generate: T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = for (x <- integers) yield x > 0

def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
  x <- t
  y <- u
} yield (x, y)

val randPair = pairs(integers, integers).generate

// Tree generator

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree

case class Leaf(x: Int) extends Tree

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) treeLeaf else treeInnerNode
} yield tree

def treeLeaf = for {
  leafVal <- integers
} yield Leaf(leafVal)

def treeInnerNode = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

trees.generate
