package org.mikesajak.raytracer.math

import org.scalactic.Prettifier
import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * Created by mike on 08.10.15.
 */
trait Vector4Matchers {
  import scala.language.implicitConversions

  case class Ulps(value: Int) {
    require(value > 0 && value < 4 * 1024 * 1024)
  }

  case class Precision(value: Float) {
    require(value.signum >= 0, "precision must be zero or greater, but was " + value)
  }

  implicit class FloatPlusOrMinusUlpsWrapper(pivot: Float) {
    def +-(tolerance: Ulps) = FloatUlpSpread(pivot, tolerance)
  }

  implicit class FloatPlusOrMinusPrecisionWrapper(pivot: Float) {
    def +-(tolerance: Precision) = FloatPrecisionSpread(pivot, tolerance)
  }

  implicit class Vector4PlusOrMinusWrapper(pivot: Vector4) {
    def +-(tolerance: Float) = Vector4Spread(pivot, tolerance)
  }

  implicit class Vector4PlusOrMinusUlpsWrapper(pivot: Vector4) {
    def +-(tolerance: Ulps) = Vector4UlpSpread(pivot, tolerance)
  }

  implicit class Vector4PlusOrMinusPrecisionWrapper(pivot: Vector4) {
    def +-(tolerance: Precision) = Vector4PrecisionSpread(pivot, tolerance)
  }

  trait SpreadCompare[A, B] {
    def isWithin(n: A): Boolean
    def diff(n: A): A

    def ===(n: A): Boolean = isWithin(n)
    def !==(n: A): Boolean = !isWithin(n)

    def pivot: A
    def tolerance: B

    override def toString: String = Prettifier.default(pivot) + " +- " + Prettifier.default(tolerance)
  }

  case class Vector4Spread(pivot: Vector4, tolerance: Float) extends SpreadCompare[Vector4, Float] {
    require(tolerance.signum >= 0, "tolerance must be zero or greater, but was " + tolerance)

    override def isWithin(n: Vector4) =
      (0 until 4).forall(i => (pivot(i) - n(i)).abs <= tolerance)

    override def diff(n: Vector4) =
      Vector4.tabulate(i => pivot.x - n.x) // todo: make sure it's correct direction
  }

  case class Vector4UlpSpread(pivot: Vector4, tolerance: Ulps) extends SpreadCompare[Vector4, Ulps] {
    override def isWithin(n: Vector4): Boolean =
      (0 until 4) forall (i => FloatComparison.almostEqual2sComplement(pivot(i), n(i), tolerance.value))

    override def diff(n: Vector4) =
      Vector4.tabulate(i => FloatComparison.ulpsDiff(pivot(i), n(i))) // todo: make sure it's correct direction
  }

  case class Vector4PrecisionSpread(pivot: Vector4, tolerance: Precision) extends SpreadCompare[Vector4, Precision] {
    override def isWithin(n: Vector4): Boolean = {
      (0 until 4).forall((i => FloatComparison.almostEqualRelOrAbs(pivot(i), n(i), tolerance.value)))
    }

    override def diff(n: Vector4) =
      Vector4.tabulate(i => FloatComparison.relativeDiff(pivot(i), n(i)))
  }

  case class FloatUlpSpread(pivot: Float, tolerance: Ulps) extends SpreadCompare[Float, Ulps] {
    override def isWithin(n: Float) = FloatComparison.almostEqual2sComplement(pivot, n, tolerance.value)

    override def diff(n: Float) = pivot - n // todo: make sure it's correct direction
  }

  case class FloatPrecisionSpread(pivot: Float, tolerance: Precision) extends SpreadCompare[Float, Precision] {
    override def isWithin(n: Float) = FloatComparison.almostEqualRelOrAbs(pivot, n, tolerance.value)

    override def diff(n: Float) = FloatComparison.relativeDiff(pivot, n)
  }

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal (Vector4(1,2,3) +- 1)
   *               ^
   * </pre>
   */
  def equal[T](spread: SpreadCompare[T, _]): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        MatchResult(
          spread.isWithin(left),
          "{0} did not equal {1} plus or minus {2} by {3}",
          "{0} equaled {1} plus or minus {2} by {3}",
          Vector(left, spread.pivot, spread.tolerance, spread.diff(left))
        )
      }
      override def toString: String = "equal (" + Prettifier.default(spread) + ")"
    }
  }


}
