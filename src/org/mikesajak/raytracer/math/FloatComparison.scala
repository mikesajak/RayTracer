package org.mikesajak.raytracer.math

/**
 * Created by mike on 10.10.15.
 */
object FloatComparison {

  def relativeDiff(a: Float, b: Float) =
    if (math.abs(b) > math.abs(a) && b != 0)
      math.abs((a - b) / b)
    else if (a != 0)
      math.abs((a - b) / a)
    else
      a - b

  def almostEqualRelative(a: Float, b: Float, maxRelativeError: Float) = {
    if (a == b) true
    else {
      val relativeError = relativeDiff(a, b)
      if (relativeError <= maxRelativeError) true
      else {
        println(s"Error: a=$a, b=$b, maxRelError=$maxRelativeError, actualError=$relativeError")
        false
      }
    }
  }

  def almostEqualRelOrAbs(a: Float, b: Float, maxRelError: Float): Boolean = almostEqualRelOrAbs(a, b, maxRelError, maxRelError)

  def almostEqualRelOrAbs(a: Float, b: Float, maxRelError: Float, maxAbsError: Float) = {
    if (math.abs(a - b) < maxAbsError) true
    else almostEqualRelative(a, b, maxRelError)
  }


  def almostEqual2sComplement(a: Float, b: Float, maxUlps: Int) = {
    require(maxUlps > 0 && maxUlps < 4 * 1024 * 1024)

    val diff = math.abs(ulpsDiff(a, b))

    diff <= maxUlps
  }

  def ulpsDiff(a: Float, b: Float) = {
    val aInt = toIntBits(a)
    val bInt = toIntBits(b)

    aInt - bInt
  }

  private def toIntBits(a: Float) = {
    val intBits = java.lang.Float.floatToIntBits(a)
    if (intBits >= 0) intBits
    else 0x80000000 - intBits
  }
}
