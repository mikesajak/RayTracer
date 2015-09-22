package org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
class Matrix44(a00: Float, a01: Float, a02: Float, a03: Float,
               a10: Float, a11: Float, a12: Float, a13: Float,
               a20: Float, a21: Float, a22: Float, a23: Float,
               a30: Float, a31: Float, a32: Float, a33: Float) {
  def data = Array(
    Vector4(a00, a01, a02, a03),
    Vector4(a10, a11, a12, a13),
    Vector4(a20, a21, a22, a23),
    Vector4(a30, a31, a32, a33)
  )

  def this(a: Float) = this(a,a,a,a,  a,a,a,a,  a,a,a,a,  a,a,a,a)

  def this(c0: Vector4, c1: Vector4, c2: Vector4, c3: Vector4) =
    this(c0.x, c0.y, c0.z, c0.w,
      c1.x, c1.y, c1.z, c1.w,
      c2.x, c2.y, c2.z, c2.w,
      c3.x, c3.y, c3.z, c3.w)


  def this(m: Matrix44) = this(m.data(0), m.data(1), m.data(2), m.data(3))

  def this() = this(1,0,0,0,
    0,1,0,0,
    0,0,1,0,
    0,0,0,1)

  def apply(idx: Int) = data(idx)

  def set(a00: Float, a01: Float, a02: Float, a03: Float,
          a10: Float, a11: Float, a12: Float, a13: Float,
          a20: Float, a21: Float, a22: Float, a23: Float,
          a30: Float, a31: Float, a32: Float, a33: Float) = {
    data(0).set(a00, a01, a02, a03)
    data(1).set(a10, a11, a12, a13)
    data(2).set(a20, a21, a22, a23)
    data(3).set(a30, a31, a32, a33)
    this
  }

  def set(c0: Vector4, c1: Vector4, c2: Vector4, c3: Vector4) =
    set(c0.x, c0.y, c0.z, c0.w,
      c1.x, c1.y, c1.z, c1.w,
      c2.x, c2.y, c2.z, c2.w,
      c3.x, c3.y, c3.z, c3.w)

  def set(m: Matrix44) = set(m.data(0), m.data(1), m.data(2), m.data(3))

  def :=(m: Matrix44) = set(m)
  def :=(t: Tuple4[Vector4, Vector4, Vector4, Vector4]) = set(t._1, t._2, t._3, t._4)
  def :=(t: Tuple16[Float, Float, Float, Float,  Float, Float, Float, Float,  Float, Float, Float, Float,  Float, Float, Float, Float]) =
    set(t._1,  t._2,   t._3,  t._4,
      t._5,  t._6,   t._7,  t._8,
      t._9,  t._10,  t._11, t._12,
      t._13, t._14,  t._15, t._16)

  def fill(a: Float) = {
    data(0).fill(a)
    data(1).fill(a)
    data(2).fill(a)
    data(3).fill(a)
    this
  }

  def zero() = fill(0)
  def ones() = fill(1)

  def identity() = {
    data(0).set(1,0,0,0)
    data(1).set(0,1,0,0)
    data(2).set(0,0,1,0)
    data(3).set(0,0,0,1)
    this
  }

  def +=(m: Matrix44) = {
    data(0) += m.data(0)
    data(1) += m.data(1)
    data(2) += m.data(2)
    data(3) += m.data(3)
    this
  }
  def +(m: Matrix44) = new Matrix44(this) += m

  def -=(m: Matrix44) = {
    data(0) -= m.data(0)
    data(1) -= m.data(1)
    data(2) -= m.data(2)
    data(3) -= m.data(3)
    this
  }
  def -(m: Matrix44) = new Matrix44(this) -= m

  def *=(a: Float) = {
    data(0) *= a
    data(1) *= a
    data(2) *= a
    data(3) *= a
    this
  }
  def *(a: Float) = new Matrix44(this) *= a

  def *=(m: Matrix44) = this := Matrix44.mul(this, m)

  def *(m: Matrix44) = Matrix44.mul(this, m)
}

object Matrix44 {
  def apply() = new Matrix44()

  def apply(a00: Float, a01: Float, a02: Float, a03: Float,
            a10: Float, a11: Float, a12: Float, a13: Float,
            a20: Float, a21: Float, a22: Float, a23: Float,
            a30: Float, a31: Float, a32: Float, a33: Float) =
    new Matrix44(a00, a01, a02, a03,
                 a10, a11, a12, a13,
                 a20, a21, a22, a23,
                 a30, a31, a32, a33)

  def apply(c0: Vector4, c1: Vector4, c2: Vector4, c3: Vector4) =
    new Matrix44(c0.x, c1.x, c2.x, c3.x,
                 c0.y, c1.y, c2.y, c3.y,
                 c0.z, c1.z, c2.z, c3.z,
                 c0.w, c1.w, c2.w, c3.w)

  def outerProd(v1: Vector4, v2: Vector4) =
      Matrix44(v1.x*v2.x, v1.x*v2.y, v1.x*v2.z, v1.x*v2.w,
               v1.y*v2.x, v1.y*v2.y, v1.y*v2.z, v1.y*v2.w,
               v1.z*v2.x, v1.z*v2.y, v1.z*v2.z, v1.z*v2.w,
               v1.w*v2.x, v1.w*v2.y, v1.w*v2.z, v1.w*v2.w)
}