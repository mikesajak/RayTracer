package org.mikesajak.raytracer.math

import org.mikesajak.raytracer.AABB
import org.mikesajak.raytracer.math.TestUtil._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by mike on 13.10.15.
 */
class AABBTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "isInside" should "return true for any point inside bounds" in {
    val aabb = new AABB(Vector4(-100, -100, -100, 1), Vector4(100, 100, 100, 1))

    forAll((ptVecGen(-100, 100), "pt")) { pt: Vector4 =>
      aabb.isInside(pt) should equal (true)
    }
  }

  it should "return false for any point outside bounds" in {
    val aabb = new AABB(Vector4(-100, -100, -100, 1), Vector4(100, 100, 100, 1))

    forAll((ptVecGen(-1000, -99.99f), "pt")) { pt: Vector4 =>
      aabb.isInside(pt) should equal (false)
    }

    forAll((ptVecGen(99.99f, 1000), "pt")) { pt: Vector4 =>
      aabb.isInside(pt) should equal (false)
    }
  }

  "intersect with AABB" should "return None for AABB with overlapping x and y but not z" in {
    forAll((ptVecGen(-100, 0.0f), "min1"), (ptVecGen(0.0f, 100), "max1")) { (min1: Vector4, max1: Vector4) =>
      val aabb1 = new AABB(min1, max1)
      for (i <- 0 until 3) withClue(s"Coordinate out of bounds: $i") {
        forAll((ptVecGen(-100, 0.0f), "min2"), (ptVecGen(0.0f, 100), "max2")) { (min2: Vector4, max2: Vector4) =>
          min2(i) += 201.0f
          max2(i) += 201.0f

          val aabb2 = new AABB(min2, max2)

          aabb1.intersect(aabb2) should equal(None)
        }
      }
    }
  }
}
