package org.mikesajak.raytracer.math

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by mike on 04.10.15.
 */
class TransformTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with Vector4Matchers {
  val EPSILON = 0.00001f

  val MaxValue = math.sqrt(Float.MaxValue/2).toFloat / 10

  val dirVecGen = for (x <- Gen.choose(-1/EPSILON, 1/EPSILON);
                       y <- Gen.choose(-1/EPSILON, 1/EPSILON);
                       z <- Gen.choose(-1/EPSILON, 1/EPSILON);
                       v = Vector4(x,y,z,0))
                       yield v
  val ptVecGen = for (x <- Gen.choose(-1/EPSILON, 1/EPSILON);
                      y <- Gen.choose(-1/EPSILON, 1/EPSILON);
                      z <- Gen.choose(-1/EPSILON, 1/EPSILON);
                      v = Vector4(x,y,z,1)) yield v
  val angleDegGen = for (theta <- Gen.choose(0.0f, 360.0f)) yield theta
  val angleRadGen = for (theta <- Gen.choose(0.0f, (2 * math.Pi).toFloat)) yield theta

  "Translation" should "move point vector by specified amount" in {
    forAll((dirVecGen, "t"), (ptVecGen, "v")) { (t: Vector4, v: Vector4) =>
      val T = Matrix44.translation(t)

      val vExpected = Vector4(v.x+t.x, v.y+t.y, v.z+t.z, 1)

      val v1 = v * T

      v1 should equal(vExpected)
    }
  }

  it should "not move direction vector" in {
    forAll((dirVecGen, "t"), (dirVecGen, "v")) { (t: Vector4, v: Vector4) =>
      val T = Matrix44.translation(t)

      val vOrig = new Vector4(v)

      val v1 = v * T

      v1 should equal(vOrig)
    }
  }


  "Rotation" should "rotate ax vector by 90deg around ay axis to get az vector" in {
    val R = Matrix44.rotation(math.toRadians(-90).toFloat, Vector4(0,1,0))
    val v = Vector4(1,0,0)

    val v1 = v * R

    v1 should equals_+-(Vector4(0,0,1), EPSILON)
  }

  it should "produce perpendicular vector for rotation by 90deg by any axis" in {
    forAll((dirVecGen, "axis"), (dirVecGen, "v")) { (axis: Vector4, v: Vector4) =>
      axis.normalize()
      v.normalize()
      val R = Matrix44.rotation(math.toRadians(90).toFloat, axis)

      val v1 = v * R

      val dot = v * v1
      withClue(s"result=$v1") {
        dot should equal(1.0f +- EPSILON)
      }
    }
  }

  it should "not change length of rotated vector" in {
    forAll((dirVecGen, "axis"), (angleDegGen, "angle"), (dirVecGen, "v")) { (axis: Vector4, angle: Float, v: Vector4) =>
      axis.normalize()
      val R = Matrix44.rotation(math.toRadians(angle).toFloat, axis)

      val v1 = v * R

      withClue(s"result=$v1") {
        v1.length should equal (v.length +- 0.1f)
      }

    }
  }

//  it should "produce vector perpendicular to axis of rotation for any rotation by any axis" in {
//    forAll((angleGen, "angle"), (dirVecGen, "axis"), (dirVecGen, "v")) { (angle: Float, axis: Vector4, v: Vector4) =>
//      axis.normalize()
//      v.normalize()
//      val R = Matrix44.rotation(math.toRadians(angle).toFloat, axis)
//
//      val v1 = v * R
//
//      val dot = axis * v1
//      dot should equal (0.0f +- EPSILON)
//
//      val cross = axis cross v1
//      cross.length should equal(1.0f +- EPSILON)
//    }
//  }

  // todo: more rotation tests

  "Scale" should "scale dir vector by specified amounts" in {
    forAll((dirVecGen, "s"), (dirVecGen, "v")) { (s: Vector4, v: Vector4) =>
      val S = Matrix44.scale(s)

      val v1 = v * S

      v1 should equals_+-(Vector4(v.x*s.x, v.y*s.y, v.z*s.z, 0), EPSILON)
    }
  }

  it should "scale point vector by specified amounts" in {
    forAll((dirVecGen, "s"), (ptVecGen, "v")) { (s: Vector4, v: Vector4) =>
      val S = Matrix44.scale(s)

      val v1 = v * S

      v1 should equals_+-(Vector4(v.x*s.x, v.y*s.y, v.z*s.z, 1), EPSILON)
    }
  }

  // todo: more scale tests
}
