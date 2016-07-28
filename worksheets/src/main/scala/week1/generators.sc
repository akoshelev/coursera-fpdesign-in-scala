trait Generator[+T] {

  self =>
  // an alias for ”this”.

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }

}

// integers
val integers = new Generator[Int] {
  val rand = new java.util.Random

  def generate = rand.nextInt()
}

// booleans
val booleans = integers map (i => i > 0)

// api
def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

// lists generator
def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)

def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

lists.generate

// tree generator
trait Tree

case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  node <- if (isLeaf) leafs else inners
} yield node

def inners = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

def leafs = for {
  i <- integers
} yield Leaf(i)

trees.generate


// test function
def test[T](g: Generator[T], numTimes: Int = 100)
           (test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test failed for "+value)
  }
  println("passed "+numTimes+" tests")
}

def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
  x => u map { y => (x, y) } }

test(pairs(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}
