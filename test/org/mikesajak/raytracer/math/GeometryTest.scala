package org.mikesajak.raytracer.math

import org.mikesajak.raytracer.math.TestUtil._
import org.mikesajak.raytracer.{Ray, Sphere}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by mike on 14.10.15.
 */
class GeometryTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Sphere ray intersection" should "todo" ignore {
    val sphere = new Sphere(Vector4(0, 0, 0), 1)
    forAll((ptVecGen(-1.0f, 1.0f), "rayTo")) { rayTo =>
      val rayOrigin = Vector4(-10, 0, 0)
      val ray = Ray(rayOrigin, (rayTo - rayOrigin).normalize())

      val intersection = sphere.intersect(ray)

      withClue(s"ray=$ray, intersection=$intersection") {

        intersection should not equal (None)

        val pt = intersection.get.point

        (pt - sphere.center).length should equal(1.0f +- Epsilon)
      }

    }
  }

  "Translation and scale of a ray" should "todo" ignore {
    val T = new Transform() combine Transform.translation(Vector4(-1,-1,-1)) combine Transform.scale(Vector4(0.5f, 0.5f, 0.5f))
    forAll((Gen.chooseNum(0.0f, 0.5f), "r")) { r =>
      val ray = Ray(Vector4(-10, 0, 0), Vector4(-1+r, -1+r, -1+r))

      val sphere = new Sphere(Vector4(0,0,0), 1)


    }
  }

}
