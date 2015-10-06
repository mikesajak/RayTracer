package org.mikesajak.raytracer.math

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mike on 03.10.15.
 */
class Vector4Test extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  val EPSILON = 0.00001f

  "No-arg constructor" should "create vector with zeroes" in {
    val v = new Vector4()

    v.x should equal (0)
    v.y should equal (0)
    v.z should equal (0)
    v.w should equal (0)
  }

  "Constructor with 4 values" should "create vector with provided values" in {
    val v = new Vector4(1,2,3,4)

    v.x should equal (1)
    v.y should equal (2)
    v.z should equal (3)
    v.w should equal (4)
  }

  "Constructor with 3 values" should "create vector with provided values and infer w==0" in {
    val v = new Vector4(1,2,3)

    v.x should equal (1)
    v.y should equal (2)
    v.z should equal (3)
    v.w should equal (0)
  }

  "copy constructor" should "create new vector from the original one" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(v1)

    (v2.x, v2.y, v2.z, v2.w) should equal ((v1.x, v1.y, v1.z, v1.w))
  }

  it should "create independent copy" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(v1)

    v1.x = 5
    v1.y = 6
    v1.z = 7
    v1.w = 8

    (v2.x, v2.y, v2.z, v2.w) should equal ((1, 2, 3, 4))
  }

  "apply/update" should "allow access by index" in {
    val v = new Vector4(1,2,3,4)

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v(i) should equal (i+1)
    }

    for (i <- 0 until 4) {
      v(i) = 10 - i
    }

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v(i) should equal (10-i)
    }
  }

  "equals" should "return true for identical vectors" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,3,4)

    v1.equals(v2) should equal (true)
  }

  it should "return false for difference on x coordinate" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(2,2,3,4)

    v1.equals(v2) should equal (false)
  }
  it should "return false for difference on y coordinate" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,3,3,4)

    v1.equals(v2) should equal (false)
  }
  it should "return false for difference on z coordinate" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,4,4)

    v1.equals(v2) should equal (false)
  }
  it should "return false for difference on w coordinate" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,3,5)

    v1.equals(v2) should equal (false)
  }

  "toTuple" should "create new tuple with all vector values" in {
    val v1 = new Vector4(1,2,3,4)

    val t = v1.toTuple
    t should equal ((1, 2, 3, 4))
  }

  "set" should "assign vector values from provided numbers" in {
    val v1 = new Vector4(1,2,3,4)

    v1.set(5,6,7,8)

    v1 should equal(Vector4(5,6,7,8))
  }

  "fill" should "set all elements to passed value" in {
    val v = new Vector4(1,2,3,4)

    v.fill(9)

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v(i) should equal (9)
    }
  }

  "zero" should "zero all elements" in {
    val v = new Vector4(1,2,3,4)

    v.zero()

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v(i) should equal (0)
    }
  }

  "inverse" should "inverse all values" in {
    val v = new Vector4(1,2,3,4)

    v.inverse()

    v.x should equal (-1)
    v.y should equal (-2)
    v.z should equal (-3)

    v.w should equal (4)
  }

  "operator +=" should "add another vector value-by-value" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,3,4)

    v1 += v2

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v1(i) should equal (if (i < 3) (i+1)*2 else i+1)
      v2(i) should equal (i+1)
    }
  }

  "operator +" should "create new vector and add values one-by-one" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,3,4)

    val v3 = v1 + v2

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v3(i) should equal (if (i < 3) (i+1)*2 else i+1)
      v1(i) should equal (i+1)
      v2(i) should equal (i+1)
    }
  }

  "operator -=" should "subtract another vector value-by-value" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,3,4)

    v1 -= v2

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v1(i) should equal (if (i < 3) 0 else 4)
      v2(i) should equal (i+1)
    }
  }

  "operator -" should "create new vector and subtract values one-by-one" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(1,2,3,4)

    val v3 = v1 - v2

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v3(i) should equal (if (i < 3) 0 else 4)
      v1(i) should equal (i+1)
      v2(i) should equal (i+1)
    }
  }

  "operator *= by scalar" should "multiply all vector values by scalar" in {
    val v1 = new Vector4(1,2,3,4)

    v1 *= 10

    for (i <- 0 until 4) withClue(s"i=$i:") {
      v1(i) should equal (if (i < 3) 10*(i+1) else 4)
    }
  }

  "operator * by scalar" should "create new vector and multiply its values by scalar" in {
    forAll("x", "y", "z", "w") { (x: Float, y: Float, z: Float, w: Float) =>
      val v1 = new Vector4(x, y, z, w)

      val v2 = v1 * 10

      v2 should equal(Vector4(x*10, y*10, z*10, w))
    }
  }

  it should "not modify original vector" in {
    forAll("x", "y", "z", "w") { (x: Float, y: Float, z: Float, w: Float) =>
      val v1 = new Vector4(x, y, z, w)

      val v2 = v1 * 10

      v1 should equal(Vector4(x, y, z, w))
    }
  }

  "operator * by vector (dot product)" should "calculate dot product of 2 vectors" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(10, 100, 1000, 10000)

    val d = v1 * v2

    d should equal (10 + 200 + 3000)
  }

  it should "not modify original vectors" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(10, 100, 1000, 10000)

    val d = v1 * v2

    v1 should equal (Vector4(1,2,3,4))
    v2 should equal (Vector4(10, 100, 1000, 10000))
  }

  "operator dot" should "calculate dot product of 2 vectors" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(10, 100, 1000, 10000)

    val d = v1 dot v2

    d should equal (10 + 200 + 3000)
  }

  it should "not modify original vectors" in {
    val v1 = new Vector4(1,2,3,4)
    val v2 = new Vector4(10, 100, 1000, 10000)

    val d = v1 dot v2

    v1 should equal(Vector4(1,2,3,4))
    v2 should equal(Vector4(10, 100, 1000, 10000))
  }

  "operator cross" should "produce z unit vector from x and y unit vectors" in {
    val v1 = new Vector4(1,0,0,0)
    val v2 = new Vector4(0,1,0,0)

    val c = v1 cross v2

    c should equal(Vector4(0,0,1,0))
  }

  it should "produce inverse z unit vector from y and x unit vectors" in {
    val v1 = new Vector4(1,0,0,0)
    val v2 = new Vector4(0,1,0,0)

    val c = v2 cross v1

    c should equal(Vector4(0,0,-1,0))
  }

  it should "produce unit x vector from z and y unit vectors" in {
    val v1 = Vector4(0, 1, 0, 0)
    val v2 = Vector4(0, 0, 1, 0)

    val c = v1 cross v2

    c should equal(Vector4(1, 0, 0, 0))
  }

  it should "produce unit y vector from z and x unit vectors" in {
    val v1 = Vector4(1, 0, 0, 0)
    val v2 = Vector4(0, 0, 1, 0)

    val c = v2 cross v1

    c should equal(Vector4(0, 1, 0, 0))
  }

  it should "produce zero vector for cross product with itself" in {
    val v1 = Vector4(1,2,3,0)

    val c = v1 cross v1

    c should equal (Vector4(0,0,0,0))
  }

  it should "not modify the original vectors" in {
    val v1 = Vector4(1, 2, 3, 4)
    val v2 = Vector4(5, 6, 7, 8)

    val c = v1 cross v2

    v1 should equal (Vector4(1, 2, 3, 4))
    v2 should equal (Vector4(5, 6, 7, 8))
  }

  "crossAndSet operator" should "produce cross product of 2 vectors and set the first vector to it" in {
    val v1 = Vector4(1,0,0,0)
    val v2 = Vector4(0,1,0,0)

    v1.crossAndSet(v2)

    v1 should equal (Vector4(0,0,1,0))
  }

  it should "not modify the second vector" in {
    val v1 = Vector4(1,0,0,0)
    val v2 = Vector4(0,1,0,0)

    v1.crossAndSet(v2)

    v2 should equal (Vector4(0,1,0,0))
  }

  "d2 operator" should "compute square length of a vector" in {
    val v1 = new Vector4(1,2,3,4)

    v1.d2 should equal ((1.0f*1 + 2*2 + 3*3) +- EPSILON)
  }

  it should "not modify the vector" in {
    val v1 = new Vector4(1,2,3,4)

    val d2 = v1.d2

    v1 should equal (Vector4(1,2,3,4))
  }

  "d operator" should "compute length of a vector" in {
    val v = new Vector4(1,2,3,4)

    v.d should equal (scala.math.sqrt(1*1 + 2*2 + 3*3).toFloat +- EPSILON)
  }

  it should "not modify the vector" in {
    val v1 = new Vector4(1,2,3,4)

    val d = v1.d

    v1 should equal (Vector4(1,2,3,4))
  }

  "length operator" should "compute length of a vector" in {
    val v = new Vector4(1,2,3,4)

    v.length should equal (scala.math.sqrt(1*1 + 2*2 + 3*3).toFloat +- EPSILON)
  }

  it should "not modify the vector" in {
    val v1 = new Vector4(1,2,3,4)

    val d = v1.length

    v1 should equal (Vector4(1,2,3,4))
  }

  "normalize operator" should "scale the vector to unit length" in {
    val v = Vector4(1,2,3,4)

    v.normalize()

    v.length should equal(1.0f +- EPSILON)
  }

  it should "not change direction of a vector" in {
    val v1 = Vector4(1,2,3,4)
    val v2 = new Vector4(v1)

    v1.normalize()

    val c = v1 cross v2
    val d = v1 dot v2

    c should equal (Vector4(0,0,0,0))
    d should equal (v2.length +- EPSILON)
  }

  it should "scale any generated vector to unit length" in {
    forAll("x", "y", "z", "w") { (x: Float, y: Float, z: Float, w: Float) =>
      if ((x < -EPSILON && x > EPSILON) && (y < -EPSILON || y > EPSILON) && (z < -EPSILON || z > EPSILON)) {
        val v = Vector4(x, y, z, w)

        v.normalize()

        v.length should equal(1.0f +- EPSILON)
      }
    }
  }
}
