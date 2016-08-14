package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = new Signal[Double](b() * b() - 4 * a() * c())


//Δ = b² - 4ac
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    new Signal[Set[Double]](delta() match {
      case x if x < 0 => Set()
      case y =>
        val q = Math.sqrt(y)
        val x1 = (-1 * b() + q) / (2 * a())
        val x2 = (-1 * b() - q) / (2 * a())
        Set(x1, x2)
    })
//  (-b ± √Δ) / 2a
}
