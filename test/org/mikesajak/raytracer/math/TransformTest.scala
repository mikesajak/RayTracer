package org.mikesajak.raytracer.math

import org.mikesajak.raytracer.math.TestUtil._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
/**
 * Created by mike on 11.10.15.
 */
class TransformTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Transform.combine" should "accumulate both forward and inverse matrix and they should cancel out" in {
    val T = new Transform(Matrix44.identity(), Matrix44.identity())

    forAll((dirVecGen, "tx1"), (dirVecGen, "axis2"),
      (angleDegGen, "angle2"), (dirVecGen, "sx3"), (dirVecGen, "axis4"), (angleDegGen, "angle4")) {
      (tx1: Vector4, axis2: Vector4, angle2: Float, sx3: Vector4, axis4: Vector4, angle4: Float) =>

        T.combine(Transform.translation(tx1))
        T.combine(Transform.rotation(math.toRadians(angle2).toFloat, axis2))
        T.combine(Transform.scale(sx3))
        T.combine(Transform.rotation(math.toRadians(angle4).toFloat, axis4))

        val M = Matrix44.identity()
        M *= T.matrix
        M *= T.inverse

        M should equal(Matrix44.identity())
    }
  }
}
