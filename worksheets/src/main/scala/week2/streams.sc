val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

Stream(1, 2, 3)

(1 to 1000).toStream

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  println(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10)

1 #:: Stream.empty

streamRange(1, 10).take(3).toList


def from(n: Int): Stream[Int] = n #:: from(n + 1)

val nats = from(0)
val m4s = nats map (_ * 4)

(m4s take 1000).toList

