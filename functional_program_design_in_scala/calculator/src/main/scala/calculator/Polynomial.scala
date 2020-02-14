package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val bv = b()
    val av = a()
    val cv = c()
    bv * bv - 4 * av * cv
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val av = a()
    val bv = b()
    val dv = delta()

    if (dv < 0) Set()
    else {
      val sqrDelta = scala.math.pow(dv, 1/2)
      val root1 = (- bv + sqrDelta) / (2 * av)
      val root2 = (- bv - sqrDelta) / (2 * av)
      if (dv == 0) Set(root1) else Set(root1, root2)
    }
  }
}
