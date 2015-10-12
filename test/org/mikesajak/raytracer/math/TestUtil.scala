package org.mikesajak.raytracer.math

import org.scalacheck.Gen

/**
 * Created by mike on 11.10.15.
 */
object TestUtil {
  val Epsilon = 0.0001f
  val MaxValue = 1 / Epsilon

  def vecGen(min: Float, max: Float, w: Float) =
    for (x <- Gen.choose(min, max);
         y <- Gen.choose(min, max);
         z <- Gen.choose(min, max);
         v = Vector4(x, y, z, w))
      yield v

  def dirVecGen(min: Float, max: Float) = vecGen(min, max, 0.0f)
  def dirVecGen: Gen[Vector4] = dirVecGen(-MaxValue, MaxValue)

  def ptVecGen(min: Float, max: Float) = vecGen(min, max, 1.0f)
  def ptVecGen: Gen[Vector4] = ptVecGen(-MaxValue, MaxValue)

  val angleDegGen = for (theta <- Gen.choose(0.0f, 360.0f)) yield theta
  val angleRadGen = for (theta <- Gen.choose(0.0f, (2 * math.Pi).toFloat)) yield theta
}
