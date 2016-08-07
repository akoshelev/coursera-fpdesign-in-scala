package quickcheck

import java.util.NoSuchElementException

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

//  override def ord = scala.math.Ordering.Int.reverse

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(i, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("remove1") = forAll{ a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("insert1") = forAll { a: Int =>
    val h = insert(a, empty)
    !isEmpty(h)
  }

  property("insert2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insert2delete1") = forAll { (a: Int, b: Int) =>
    val h = deleteMin(insert(b, insert(a, empty)))
    findMin(h) == Math.max(a, b)
  }

  property("meld2") = forAll{ (h1: H, h2: H) =>
    val min = Math.min(findMin(h1), findMin(h2))
    findMin(meld(h1, h2)) == min
  }

  def getListOfVal(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: getListOfVal(deleteMin(h))
  }

  def emptyHeap(h: H): H = {
    if (isEmpty(h)) h
    else emptyHeap(deleteMin(h))
  }

  def buildHeap(l: List[Int]): H = {
    l match {
      case Nil => empty
      case x :: xs => meld(insert(x, empty), buildHeap(xs))
    }
  }

  property("findMin/deleteMin create a sorted list of values") = forAll{ l1: List[Int] =>
    val h = buildHeap(l1)
    val l2 = getListOfVal(h)
    l2 == l1.sorted
  }

  property("deleteMin of empty") = throws(classOf[NoSuchElementException])(deleteMin(empty))
  property("findMin of empty") = throws(classOf[NoSuchElementException])(findMin(empty))
  property("empty.isEmpty") = isEmpty(empty)

  property("deleteMin of arb heap eventually empties it") = forAll { h1: H =>
    isEmpty(emptyHeap(h1))
  }
}
