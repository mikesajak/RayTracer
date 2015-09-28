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
  def update(idx: Int, col: Vector4) = data(idx) := col
  def at(idx: Int) = apply(idx)

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

  def set(c0: Vector4, c1: Vector4, c2: Vector4, c3: Vector4): Matrix44 =
    set(c0.x, c0.y, c0.z, c0.w,
        c1.x, c1.y, c1.z, c1.w,
        c2.x, c2.y, c2.z, c2.w,
        c3.x, c3.y, c3.z, c3.w)

  def set(m: Matrix44): Matrix44 = set(m.data(0), m.data(1), m.data(2), m.data(3))

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

  def transpose() = {
    def swap(ax: Int, ay: Int, bx: Int, by: Int) = {
      val tmp = data(ay)(ax)
      data(ay)(ax) = data(by)(bx)
      data(by)(bx) = tmp
    }

    swap(0,1, 1,0)
    swap(0,2, 2,0)
    swap(0,3, 3,0)

    swap(1,2, 2,1)
    swap(1,3, 3,1)

    swap(2,3, 3,2)

    this
  }

  def inverse() = {
    data(0)(0) = data(1)(2)*data(2)(3)*data(3)(1) - data(1)(3)*data(2)(2)*data(3)(1) + data(1)(3)*data(2)(1)*data(3)(2) - data(1)(1)*data(2)(3)*data(3)(2) - data(1)(2)*data(2)(1)*data(3)(3) + data(1)(1)*data(2)(2)*data(3)(3)
    data(0)(1) = data(0)(3)*data(2)(2)*data(3)(1) - data(0)(2)*data(2)(3)*data(3)(1) - data(0)(3)*data(2)(1)*data(3)(2) + data(0)(1)*data(2)(3)*data(3)(2) + data(0)(2)*data(2)(1)*data(3)(3) - data(0)(1)*data(2)(2)*data(3)(3)
    data(0)(2) = data(0)(2)*data(1)(3)*data(3)(1) - data(0)(3)*data(1)(2)*data(3)(1) + data(0)(3)*data(1)(1)*data(3)(2) - data(0)(1)*data(1)(3)*data(3)(2) - data(0)(2)*data(1)(1)*data(3)(3) + data(0)(1)*data(1)(2)*data(3)(3)
    data(0)(3) = data(0)(3)*data(1)(2)*data(2)(1) - data(0)(2)*data(1)(3)*data(2)(1) - data(0)(3)*data(1)(1)*data(2)(2) + data(0)(1)*data(1)(3)*data(2)(2) + data(0)(2)*data(1)(1)*data(2)(3) - data(0)(1)*data(1)(2)*data(2)(3)
    data(1)(0) = data(1)(3)*data(2)(2)*data(3)(0) - data(1)(2)*data(2)(3)*data(3)(0) - data(1)(3)*data(2)(0)*data(3)(2) + data(1)(0)*data(2)(3)*data(3)(2) + data(1)(2)*data(2)(0)*data(3)(3) - data(1)(0)*data(2)(2)*data(3)(3)
    data(1)(1) = data(0)(2)*data(2)(3)*data(3)(0) - data(0)(3)*data(2)(2)*data(3)(0) + data(0)(3)*data(2)(0)*data(3)(2) - data(0)(0)*data(2)(3)*data(3)(2) - data(0)(2)*data(2)(0)*data(3)(3) + data(0)(0)*data(2)(2)*data(3)(3)
    data(1)(2) = data(0)(3)*data(1)(2)*data(3)(0) - data(0)(2)*data(1)(3)*data(3)(0) - data(0)(3)*data(1)(0)*data(3)(2) + data(0)(0)*data(1)(3)*data(3)(2) + data(0)(2)*data(1)(0)*data(3)(3) - data(0)(0)*data(1)(2)*data(3)(3)
    data(1)(3) = data(0)(2)*data(1)(3)*data(2)(0) - data(0)(3)*data(1)(2)*data(2)(0) + data(0)(3)*data(1)(0)*data(2)(2) - data(0)(0)*data(1)(3)*data(2)(2) - data(0)(2)*data(1)(0)*data(2)(3) + data(0)(0)*data(1)(2)*data(2)(3)
    data(2)(0) = data(1)(1)*data(2)(3)*data(3)(0) - data(1)(3)*data(2)(1)*data(3)(0) + data(1)(3)*data(2)(0)*data(3)(1) - data(1)(0)*data(2)(3)*data(3)(1) - data(1)(1)*data(2)(0)*data(3)(3) + data(1)(0)*data(2)(1)*data(3)(3)
    data(2)(1) = data(0)(3)*data(2)(1)*data(3)(0) - data(0)(1)*data(2)(3)*data(3)(0) - data(0)(3)*data(2)(0)*data(3)(1) + data(0)(0)*data(2)(3)*data(3)(1) + data(0)(1)*data(2)(0)*data(3)(3) - data(0)(0)*data(2)(1)*data(3)(3)
    data(2)(2) = data(0)(1)*data(1)(3)*data(3)(0) - data(0)(3)*data(1)(1)*data(3)(0) + data(0)(3)*data(1)(0)*data(3)(1) - data(0)(0)*data(1)(3)*data(3)(1) - data(0)(1)*data(1)(0)*data(3)(3) + data(0)(0)*data(1)(1)*data(3)(3)
    data(2)(3) = data(0)(3)*data(1)(1)*data(2)(0) - data(0)(1)*data(1)(3)*data(2)(0) - data(0)(3)*data(1)(0)*data(2)(1) + data(0)(0)*data(1)(3)*data(2)(1) + data(0)(1)*data(1)(0)*data(2)(3) - data(0)(0)*data(1)(1)*data(2)(3)
    data(3)(0) = data(1)(2)*data(2)(1)*data(3)(0) - data(1)(1)*data(2)(2)*data(3)(0) - data(1)(2)*data(2)(0)*data(3)(1) + data(1)(0)*data(2)(2)*data(3)(1) + data(1)(1)*data(2)(0)*data(3)(2) - data(1)(0)*data(2)(1)*data(3)(2)
    data(3)(1) = data(0)(1)*data(2)(2)*data(3)(0) - data(0)(2)*data(2)(1)*data(3)(0) + data(0)(2)*data(2)(0)*data(3)(1) - data(0)(0)*data(2)(2)*data(3)(1) - data(0)(1)*data(2)(0)*data(3)(2) + data(0)(0)*data(2)(1)*data(3)(2)
    data(3)(2) = data(0)(2)*data(1)(1)*data(3)(0) - data(0)(1)*data(1)(2)*data(3)(0) - data(0)(2)*data(1)(0)*data(3)(1) + data(0)(0)*data(1)(2)*data(3)(1) + data(0)(1)*data(1)(0)*data(3)(2) - data(0)(0)*data(1)(1)*data(3)(2)
    data(3)(3) = data(0)(1)*data(1)(2)*data(2)(0) - data(0)(2)*data(1)(1)*data(2)(0) + data(0)(2)*data(1)(0)*data(2)(1) - data(0)(0)*data(1)(2)*data(2)(1) - data(0)(1)*data(1)(0)*data(2)(2) + data(0)(0)*data(1)(1)*data(2)(2)

    this *= 1/det()
  }

  def det() = {
    data(0)(3)*data(1)(2)*data(2)(1)*data(3)(0) - data(0)(2)*data(1)(3)*data(2)(1)*data(3)(0) - data(0)(3)*data(1)(1)*data(2)(2)*data(3)(0) + data(0)(1)*data(1)(3)*data(2)(2)*data(3)(0)+
    data(0)(2)*data(1)(1)*data(2)(3)*data(3)(0) - data(0)(1)*data(1)(2)*data(2)(3)*data(3)(0) - data(0)(3)*data(1)(2)*data(2)(0)*data(3)(1) + data(0)(2)*data(1)(3)*data(2)(0)*data(3)(1)+
    data(0)(3)*data(1)(0)*data(2)(2)*data(3)(1) - data(0)(0)*data(1)(3)*data(2)(2)*data(3)(1) - data(0)(2)*data(1)(0)*data(2)(3)*data(3)(1) + data(0)(0)*data(1)(2)*data(2)(3)*data(3)(1)+
    data(0)(3)*data(1)(1)*data(2)(0)*data(3)(2) - data(0)(1)*data(1)(3)*data(2)(0)*data(3)(2) - data(0)(3)*data(1)(0)*data(2)(1)*data(3)(2) + data(0)(0)*data(1)(3)*data(2)(1)*data(3)(2)+
    data(0)(1)*data(1)(0)*data(2)(3)*data(3)(2) - data(0)(0)*data(1)(1)*data(2)(3)*data(3)(2) - data(0)(2)*data(1)(1)*data(2)(0)*data(3)(3) + data(0)(1)*data(1)(2)*data(2)(0)*data(3)(3)+
    data(0)(2)*data(1)(0)*data(2)(1)*data(3)(3) - data(0)(0)*data(1)(2)*data(2)(1)*data(3)(3) - data(0)(1)*data(1)(0)*data(2)(2)*data(3)(3) + data(0)(0)*data(1)(1)*data(2)(2)*data(3)(3)
  }

  override def toString =
    s"[${at(0)} ${at(1)} ${at(2)} ${at(3)}]"
}

object Matrix44 {
  def apply() = new Matrix44()

  def apply(a: Float) = new Matrix44(a,a,a,a,
                                      a,a,a,a,
                                      a,a,a,a,
                                      a,a,a,a)

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

  def identity() = new Matrix44(1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                0,0,0,1)

  def transpose(m: Matrix44) = new Matrix44(m).transpose()
  def inverse(m: Matrix44) = new Matrix44(m).inverse()

  def mul(m1: Matrix44, m2: Matrix44) = {
    val r = Matrix44(0)
    for (i <- 0 until 4;
         j <- 0 until 4;
         k <- 0 until 4) {
      r(i)(j) = m1(i)(j) * m2(i)(k)
    }
    r
  }

  def outerProd(v1: Vector4, v2: Vector4) =
      Matrix44(v1.x*v2.x, v1.x*v2.y, v1.x*v2.z, v1.x*v2.w,
               v1.y*v2.x, v1.y*v2.y, v1.y*v2.z, v1.y*v2.w,
               v1.z*v2.x, v1.z*v2.y, v1.z*v2.z, v1.z*v2.w,
               v1.w*v2.x, v1.w*v2.y, v1.w*v2.z, v1.w*v2.w)

  def translation(t: Vector4): Matrix44 = translation(t.x, t.y, t.z)
  def translation(tx: Float, ty: Float, tz: Float) =
    Matrix44(1, 0, 0, tx,
             0, 1, 0, ty,
             0, 0, 1, tz,
             0, 0, 0, 1)

  def scale(s: Vector4): Matrix44 = scale(s.x, s.y, s.z)
  def scale(sx: Float, sy: Float, sz: Float) =
    Matrix44(sx,  0,  0,  0,
             0, sy,  0,  0,
             0,  0, sz,  0,
             0,  0,  0,  1)

  def rotation(theta: Float, ax: Float, ay: Float, az: Float): Matrix44 = rotation(theta, Vector4(ax, ay, az))
  def rotation(theta: Float, axis: Vector4) = {
    val cosTheta = math.cos(theta).toFloat
    val R = Matrix44()
    R *= cosTheta
    R += (crossProdMatrix(axis) *= math.sin(theta).toFloat)
    R += (Matrix44.outerProd(axis, axis) *= (1 - cosTheta))
    R
  }

  def crossProdMatrix(u: Vector4) =
    Matrix44(   0, -u.x,  u.y,  0,
              u.z,    0, -u.x,  0,
             -u.y,  u.x,   0,   0,
                0,    0,   0,   0)

  def lookAt(eye: Vector4, at: Vector4, up: Vector4) = {
    val w = (eye - at).normalize()//new Vector4(eye).normalize()
    val u = (new Vector4(up) cross w).normalize()
    val v = (new Vector4(w) cross u).normalize()

    Matrix44(u.x, u.y, u.z, -(eye*u),
             v.x, v.y, v.z, -(eye*v),
             w.x, w.y, w.z, -(eye*w),
             0,   0,   0,     1)
  }
}