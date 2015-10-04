package org.mikesajak.raytracer.math

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by mike on 04.10.15.
 */
class TransformTest extends FlatSpec with Matchers {
  "Translation" should "move point vector by specified amount" in {
    val T = Matrix44.translation(1,2,3)

    val v = Vector4(1,1,1,1)

    val v1 = v * T

    v1 should equal(Vector4(2,3,4,1))
  }

  it should "not move direction vector" in {
    val T = Matrix44.translation(1,2,3)

    val v = Vector4(1,1,1,0)

    val v1 = v * T

    v1 should equal(Vector4(1,1,1,0))
  }

  "Rotation" should "rotate ..." ignore {

  }
}
