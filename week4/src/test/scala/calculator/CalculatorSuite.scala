package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }


  test("Expressions corresponding to every variable should be correctly evaluated") {
    val aToTheUltimateAnswer: Map[String, Signal[Expr]] = Map("a" -> Signal(Literal(42)))
    assert(Calculator.eval(Ref("a"), aToTheUltimateAnswer) == 42)
    assert(Calculator.eval(Ref("b"), aToTheUltimateAnswer).isNaN)

    val refA: Var[Expr] = Var(Literal(5))
    val refE: Var[Expr] = Var(Literal(2))
    val references: Map[String, Signal[Expr]] = Map(
      ("a", Var(refA())),                             // a = 5
      ("b", Var(Times(Ref("a"), Literal(7)))),        // b = a * 7
      ("c", Var(Divide(Ref("b"), Ref("d")))),         // c = b / d
      ("d", Var(Divide(Ref("a"), Ref("e")))),         // d = a / e
      ("e", Var(refE()))                              // e = 2
    )
    val computedValues = Calculator.computeValues(references)
    assert(computedValues("a")() == 5)
    assert(computedValues("b")() == 35)
    assert(computedValues("c")() == 14)
    assert(computedValues("d")() == 2.5)
    assert(computedValues("e")() == 2)

    refA() = Literal(7)
    refE() = Plus(Ref("a"), Ref("a"))
    assert(computedValues("a")() == 7)
    assert(computedValues("b")() == 49)
    assert(computedValues("c")() == 98)
    assert(computedValues("d")() == 0.5)
    assert(computedValues("e")() == 14)
  }

  test("Reference not found") {
    assert(Calculator.eval(Ref("a"), Map()).isNaN)
  }

  test("Simple reference to literal") {
    val mapA: Map[String, Signal[Expr]] = Map("a" -> Signal(Literal(1)))
    assert(Calculator.eval(Ref("a"), mapA) == 1)
    assert(Calculator.computeValues(mapA)("a")() == 1)
  }

  test("Division by zero") {
    val references: Map[String, Signal[Expr]] = Map(
      ("a", Signal(Literal(1))),
      ("b", Signal(Literal(0))),
      ("c", Var(Divide(Ref("a"), Ref("b"))))         // c = a / b
    )
    assert(Calculator.computeValues(references)("c")().isNaN)
  }

  test("Normal expressions evaluate when cycles are present") {
    val references: Map[String, Signal[Expr]] = Map(
      ("a", Var(Ref("b"))),
      ("b", Var(Ref("a"))),
      ("c", Var(Minus(Literal(7), Literal(2))))
    )
    val computedValues = Calculator.computeValues(references)
    assert(computedValues("a")().isNaN)
    assert(computedValues("b")().isNaN)
    assert(computedValues("c")() == 5)
  }
}
