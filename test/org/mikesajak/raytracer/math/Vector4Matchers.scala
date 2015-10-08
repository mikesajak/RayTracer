package org.mikesajak.raytracer.math

import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * Created by mike on 08.10.15.
 */
trait Vector4Matchers {
  class Vector4WithToleranceMatcher(expected: Vector4, tolerance: Float) extends Matcher[Vector4] {
    override def apply(actual: Vector4) = {
      MatchResult((actual.x - expected.x).abs < tolerance
                    && (actual.y - expected.y).abs < tolerance
                    && (actual.z - expected.z).abs < tolerance
                    && (actual.w - expected.w).abs < tolerance,
                  s"Vector4 $actual did not equal expected value $expected with tolerance +-$tolerance",
                  s"Vector4 $actual equals expected value $expected with tolerance +-$tolerance")
    }
  }

  def equals_+-(expected: Vector4, tolerance: Float) = new Vector4WithToleranceMatcher(expected, tolerance)
}
