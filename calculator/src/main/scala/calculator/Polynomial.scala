package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal.Var(b() * b() - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal.Var {
      if (delta() < 0) Set()
      else {
        val r1 = (-b() - Math.sqrt(delta())) / (2 * a())
        val r2 = (-b() + Math.sqrt(delta())) / (2 * a())
        if (r1 == r2) Set(r1)
        else Set(r1, r2)
      }
    }
