package org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
class Matrix44(a00: Float, a01: Float, a02: Float, a03: Float,
               a10: Float, a11: Float, a12: Float, a13: Float,
               a20: Float, a21: Float, a22: Float, a23: Float,
               a30: Float, a31: Float, a32: Float, a33: Float) {
  val data = Array[Float](a00, a01, a02, a03,
                          a10, a11, a12, a13,
                          a20, a21, a22, a23,
                          a30, a31, a32, a33)

  def this(a: Float) = this(a,a,a,a,  a,a,a,a,  a,a,a,a,  a,a,a,a)

  def this(r0: Vector4, r1: Vector4, r2: Vector4, r3: Vector4) =
    this(r0.x, r0.y, r0.z, r0.w,
         r1.x, r1.y, r1.z, r1.w,
         r2.x, r2.y, r2.z, r2.w,
         r3.x, r3.y, r3.z, r3.w)
//    this(c0.x, c1.x, c2.x, c3.w,
//         c0.y, c1.y, c2.y, c3.w,
//         c0.z, c1.z, c2.z, c3.w,
//         c0.w, c1.w, c2.w, c3.w)


  def this(m: Matrix44) = this(m.data(0),  m.data(1),  m.data(2),  m.data(3),
                               m.data(4),  m.data(5),  m.data(6),  m.data(7),
                               m.data(8),  m.data(9),  m.data(10), m.data(11),
                               m.data(12), m.data(13), m.data(14), m.data(15))

  def this() = this(1,0,0,0,
                    0,1,0,0,
                    0,0,1,0,
                    0,0,0,1)

//  def apply(col: Int) = {
//    val ofs = col*4
//    Vector4(data(col), data(4+ofs), data(8+ofs), data(12+ofs))
//  }
//
//  def update(colIdx: Int, col: Vector4) = {
//    val ofs = colIdx*4
//    data(4+ofs) = col(0)
//    data(4+ofs) = col(1)
//    data(8+ofs) = col(2)
//    data(12+ofs) = col(3)
//  }

  def apply(col: Int, row: Int) = data(4*col + row)
  def update(col: Int, row: Int, value: Float) = data(4*col+row) = value
  def at(col: Int, row: Int) = apply(col, row)
//  def at(idx: Int) = apply(idx)

  def set(a00: Float, a01: Float, a02: Float, a03: Float,
          a10: Float, a11: Float, a12: Float, a13: Float,
          a20: Float, a21: Float, a22: Float, a23: Float,
          a30: Float, a31: Float, a32: Float, a33: Float) = {
    data(0)  = a00; data(1)  = a01; data(2)  = a02; data(3)  = a03
    data(4)  = a10; data(5)  = a11; data(6)  = a12; data(7)  = a13
    data(8)  = a20; data(9)  = a21; data(10) = a22; data(11) = a23
    data(12) = a30; data(13) = a31; data(14) = a32; data(15) = a33
    this
  }

  def set(c0: Vector4, c1: Vector4, c2: Vector4, c3: Vector4): Matrix44 =
    set(c0.x, c0.y, c0.z, c0.w,
        c1.x, c1.y, c1.z, c1.w,
        c2.x, c2.y, c2.z, c2.w,
        c3.x, c3.y, c3.z, c3.w)

  def set(m: Matrix44): Matrix44 = {
    Array.copy(m.data, 0, data, 0, 16)
    this
  }

  def :=(m: Matrix44) = set(m)
  def :=(t: Tuple4[Vector4, Vector4, Vector4, Vector4]) = set(t._1, t._2, t._3, t._4)
  def :=(t: Tuple16[Float, Float, Float, Float,  Float, Float, Float, Float,  Float, Float, Float, Float,  Float, Float, Float, Float]) =
    set(t._1,  t._2,   t._3,  t._4,
        t._5,  t._6,   t._7,  t._8,
        t._9,  t._10,  t._11, t._12,
        t._13, t._14,  t._15, t._16)

  def fill(a: Float) = set(a,a,a,a, a,a,a,a, a,a,a,a, a,a,a,a)

  def zero() = fill(0)
  def ones() = fill(1)

  def identity() =
    set(1,0,0,0,
        0,1,0,0,
        0,0,1,0,
        0,0,0,1)

  def +=(m: Matrix44) = {
    data(0)  += m.data(0);  data(1)  += m.data(1);  data(2)  += m.data(2);  data(3)  += m.data(3)
    data(4)  += m.data(4);  data(5)  += m.data(5);  data(6)  += m.data(6);  data(7)  += m.data(7)
    data(8)  += m.data(8);  data(9)  += m.data(9);  data(10) += m.data(10); data(11) += m.data(11)
    data(12) += m.data(12); data(13) += m.data(13); data(14) += m.data(14); data(15) += m.data(15)
    this
  }
  def +(m: Matrix44) = new Matrix44(this) += m

  def -=(m: Matrix44) = {
    data(0)  -= m.data(0);  data(1)  -= m.data(1);  data(2)  -= m.data(2);  data(3)  -= m.data(3)
    data(4)  -= m.data(4);  data(5)  -= m.data(5);  data(6)  -= m.data(6);  data(7)  -= m.data(7)
    data(8)  -= m.data(8);  data(9)  -= m.data(9);  data(10) -= m.data(10); data(11) -= m.data(11)
    data(12) -= m.data(12); data(13) -= m.data(13); data(14) -= m.data(14); data(15) -= m.data(15)
    this
  }
  def -(m: Matrix44) = new Matrix44(this) -= m

  def *=(a: Float) = {
    data(0)  *= a; data(1)  *= a; data(2)  *= a; data(3)  *= a
    data(4)  *= a; data(5)  *= a; data(6)  *= a; data(7)  *= a
    data(8)  *= a; data(9)  *= a; data(10) *= a; data(11) *= a
    data(12) *= a; data(13) *= a; data(14) *= a; data(15) *= a
    this
  }
  def *(a: Float) = new Matrix44(this) *= a

  def *=(m: Matrix44) = this := Matrix44.mul(this, m)

  def *(m: Matrix44) = Matrix44.mul(this, m)

  def transpose() = {
    def swap(ax: Int, ay: Int, bx: Int, by: Int) = {
      val tmp = this(ay, ax)
      this(ay, ax) = this(by, bx)
      this(by, bx) = tmp
    }

    swap(0,1, 1,0)
    swap(0,2, 2,0)
    swap(0,3, 3,0)

    swap(1,2, 2,1)
    swap(1,3, 3,1)

    swap(2,3, 3,2)

    this
  }

//  def inverse() = {
//    data(0)(0) = data(1)(2)*data(2)(3)*data(3)(1) - data(1)(3)*data(2)(2)*data(3)(1) + data(1)(3)*data(2)(1)*data(3)(2) - data(1)(1)*data(2)(3)*data(3)(2) - data(1)(2)*data(2)(1)*data(3)(3) + data(1)(1)*data(2)(2)*data(3)(3)
//    data(0)(1) = data(0)(3)*data(2)(2)*data(3)(1) - data(0)(2)*data(2)(3)*data(3)(1) - data(0)(3)*data(2)(1)*data(3)(2) + data(0)(1)*data(2)(3)*data(3)(2) + data(0)(2)*data(2)(1)*data(3)(3) - data(0)(1)*data(2)(2)*data(3)(3)
//    data(0)(2) = data(0)(2)*data(1)(3)*data(3)(1) - data(0)(3)*data(1)(2)*data(3)(1) + data(0)(3)*data(1)(1)*data(3)(2) - data(0)(1)*data(1)(3)*data(3)(2) - data(0)(2)*data(1)(1)*data(3)(3) + data(0)(1)*data(1)(2)*data(3)(3)
//    data(0)(3) = data(0)(3)*data(1)(2)*data(2)(1) - data(0)(2)*data(1)(3)*data(2)(1) - data(0)(3)*data(1)(1)*data(2)(2) + data(0)(1)*data(1)(3)*data(2)(2) + data(0)(2)*data(1)(1)*data(2)(3) - data(0)(1)*data(1)(2)*data(2)(3)
//    data(1)(0) = data(1)(3)*data(2)(2)*data(3)(0) - data(1)(2)*data(2)(3)*data(3)(0) - data(1)(3)*data(2)(0)*data(3)(2) + data(1)(0)*data(2)(3)*data(3)(2) + data(1)(2)*data(2)(0)*data(3)(3) - data(1)(0)*data(2)(2)*data(3)(3)
//    data(1)(1) = data(0)(2)*data(2)(3)*data(3)(0) - data(0)(3)*data(2)(2)*data(3)(0) + data(0)(3)*data(2)(0)*data(3)(2) - data(0)(0)*data(2)(3)*data(3)(2) - data(0)(2)*data(2)(0)*data(3)(3) + data(0)(0)*data(2)(2)*data(3)(3)
//    data(1)(2) = data(0)(3)*data(1)(2)*data(3)(0) - data(0)(2)*data(1)(3)*data(3)(0) - data(0)(3)*data(1)(0)*data(3)(2) + data(0)(0)*data(1)(3)*data(3)(2) + data(0)(2)*data(1)(0)*data(3)(3) - data(0)(0)*data(1)(2)*data(3)(3)
//    data(1)(3) = data(0)(2)*data(1)(3)*data(2)(0) - data(0)(3)*data(1)(2)*data(2)(0) + data(0)(3)*data(1)(0)*data(2)(2) - data(0)(0)*data(1)(3)*data(2)(2) - data(0)(2)*data(1)(0)*data(2)(3) + data(0)(0)*data(1)(2)*data(2)(3)
//    data(2)(0) = data(1)(1)*data(2)(3)*data(3)(0) - data(1)(3)*data(2)(1)*data(3)(0) + data(1)(3)*data(2)(0)*data(3)(1) - data(1)(0)*data(2)(3)*data(3)(1) - data(1)(1)*data(2)(0)*data(3)(3) + data(1)(0)*data(2)(1)*data(3)(3)
//    data(2)(1) = data(0)(3)*data(2)(1)*data(3)(0) - data(0)(1)*data(2)(3)*data(3)(0) - data(0)(3)*data(2)(0)*data(3)(1) + data(0)(0)*data(2)(3)*data(3)(1) + data(0)(1)*data(2)(0)*data(3)(3) - data(0)(0)*data(2)(1)*data(3)(3)
//    data(2)(2) = data(0)(1)*data(1)(3)*data(3)(0) - data(0)(3)*data(1)(1)*data(3)(0) + data(0)(3)*data(1)(0)*data(3)(1) - data(0)(0)*data(1)(3)*data(3)(1) - data(0)(1)*data(1)(0)*data(3)(3) + data(0)(0)*data(1)(1)*data(3)(3)
//    data(2)(3) = data(0)(3)*data(1)(1)*data(2)(0) - data(0)(1)*data(1)(3)*data(2)(0) - data(0)(3)*data(1)(0)*data(2)(1) + data(0)(0)*data(1)(3)*data(2)(1) + data(0)(1)*data(1)(0)*data(2)(3) - data(0)(0)*data(1)(1)*data(2)(3)
//    data(3)(0) = data(1)(2)*data(2)(1)*data(3)(0) - data(1)(1)*data(2)(2)*data(3)(0) - data(1)(2)*data(2)(0)*data(3)(1) + data(1)(0)*data(2)(2)*data(3)(1) + data(1)(1)*data(2)(0)*data(3)(2) - data(1)(0)*data(2)(1)*data(3)(2)
//    data(3)(1) = data(0)(1)*data(2)(2)*data(3)(0) - data(0)(2)*data(2)(1)*data(3)(0) + data(0)(2)*data(2)(0)*data(3)(1) - data(0)(0)*data(2)(2)*data(3)(1) - data(0)(1)*data(2)(0)*data(3)(2) + data(0)(0)*data(2)(1)*data(3)(2)
//    data(3)(2) = data(0)(2)*data(1)(1)*data(3)(0) - data(0)(1)*data(1)(2)*data(3)(0) - data(0)(2)*data(1)(0)*data(3)(1) + data(0)(0)*data(1)(2)*data(3)(1) + data(0)(1)*data(1)(0)*data(3)(2) - data(0)(0)*data(1)(1)*data(3)(2)
//    data(3)(3) = data(0)(1)*data(1)(2)*data(2)(0) - data(0)(2)*data(1)(1)*data(2)(0) + data(0)(2)*data(1)(0)*data(2)(1) - data(0)(0)*data(1)(2)*data(2)(1) - data(0)(1)*data(1)(0)*data(2)(2) + data(0)(0)*data(1)(1)*data(2)(2)
//
//    this *= 1/det()
//  }
//
//  def det() = {
//    data(0)(3)*data(1)(2)*data(2)(1)*data(3)(0) - data(0)(2)*data(1)(3)*data(2)(1)*data(3)(0) - data(0)(3)*data(1)(1)*data(2)(2)*data(3)(0) + data(0)(1)*data(1)(3)*data(2)(2)*data(3)(0)+
//    data(0)(2)*data(1)(1)*data(2)(3)*data(3)(0) - data(0)(1)*data(1)(2)*data(2)(3)*data(3)(0) - data(0)(3)*data(1)(2)*data(2)(0)*data(3)(1) + data(0)(2)*data(1)(3)*data(2)(0)*data(3)(1)+
//    data(0)(3)*data(1)(0)*data(2)(2)*data(3)(1) - data(0)(0)*data(1)(3)*data(2)(2)*data(3)(1) - data(0)(2)*data(1)(0)*data(2)(3)*data(3)(1) + data(0)(0)*data(1)(2)*data(2)(3)*data(3)(1)+
//    data(0)(3)*data(1)(1)*data(2)(0)*data(3)(2) - data(0)(1)*data(1)(3)*data(2)(0)*data(3)(2) - data(0)(3)*data(1)(0)*data(2)(1)*data(3)(2) + data(0)(0)*data(1)(3)*data(2)(1)*data(3)(2)+
//    data(0)(1)*data(1)(0)*data(2)(3)*data(3)(2) - data(0)(0)*data(1)(1)*data(2)(3)*data(3)(2) - data(0)(2)*data(1)(1)*data(2)(0)*data(3)(3) + data(0)(1)*data(1)(2)*data(2)(0)*data(3)(3)+
//    data(0)(2)*data(1)(0)*data(2)(1)*data(3)(3) - data(0)(0)*data(1)(2)*data(2)(1)*data(3)(3) - data(0)(1)*data(1)(0)*data(2)(2)*data(3)(3) + data(0)(0)*data(1)(1)*data(2)(2)*data(3)(3)
//  }

  override def toString =
    s"[[${at(0,0)}, ${at(1,0)}, ${at(2,0)}, ${at(3,0)}], " +
    s"[${at(0,1)}, ${at(1,1)}, ${at(2,1)}, ${at(3,1)}], " +
    s"[${at(0,2)}, ${at(1,2)}, ${at(2,2)}, ${at(3,2)}], " +
    s"[${at(0,3)}, ${at(1,3)}, ${at(2,3)}, ${at(3,3)}]]"

  override def equals(obj: scala.Any) = obj match {
    case m: Matrix44 =>
      var i = 0
      var result = true
      while (result && i < data.length) {
        result = data(i) == m.data(i)
        i += 1
      }
      result
    case _ => false
  }

  override def hashCode() = {
    var i = 0
    var result = 31
    while (i < data.length) {
      result = result * 37 + java.lang.Float.floatToIntBits(data(i))
      i += 1
    }
    result
  }
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
    new Matrix44(c0, c1, c2, c3)

  def fill(a: Float) = new Matrix44(a,a,a,a,
                                    a,a,a,a,
                                    a,a,a,a,
                                    a,a,a,a)

  def tabulate(f: (Int, Int) => Float) =
    new Matrix44(f(0,0), f(1,0), f(2,0), f(3,0),
                 f(0,1), f(1,1), f(2,1), f(3,1),
                 f(0,2), f(1,2), f(2,2), f(3,2),
                 f(0,3), f(1,3), f(2,3), f(3,3))


  def identity() = new Matrix44(1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                0,0,0,1)

  def transpose(m: Matrix44) = new Matrix44(m).transpose()
//  def inverse(m: Matrix44) = new Matrix44(m).inverse()

  def mul(M1: Matrix44, M2: Matrix44) = {
    val R = Matrix44.fill(0)
    for (row <- 0 until 4;
         col <- 0 until 4;
         i <- 0 until 4) {
      R(col, row) += M1(i,row) * M2(col, i)
    }
    R
  }


  // algebraic operations

  /**
   * Compute outer product (Matrix44) of 2 vectors
   * @param v1
   * @param v2
   * @return Matrix44 representing the outer product
   */
  def outerProd(v1: Vector4, v2: Vector4) =
    Matrix44(v1.x*v2.x, v1.x*v2.y, v1.x*v2.z, v1.x*v2.w,
             v1.y*v2.x, v1.y*v2.y, v1.y*v2.z, v1.y*v2.w,
             v1.z*v2.x, v1.z*v2.y, v1.z*v2.z, v1.z*v2.w,
             v1.w*v2.x, v1.w*v2.y, v1.w*v2.z, v1.w*v2.w)


  /**
   * Compute matrix representing translation
   * @param t translation offset
   * @return result matrix
   */
  def translation(t: Vector4): Matrix44 = translation(t.x, t.y, t.z)
  def translation(tx: Float, ty: Float, tz: Float) =
    Matrix44(1, 0, 0, tx,
             0, 1, 0, ty,
             0, 0, 1, tz,
             0, 0, 0, 1)

  /**
   * Compute matrix representing scale
   * @param s scale coefficients (3d)
   * @return result matrix
   */
  def scale(s: Vector4): Matrix44 = scale(s.x, s.y, s.z)
  def scale(sx: Float, sy: Float, sz: Float) =
    Matrix44(sx, 0,  0,  0,
             0, sy,  0,  0,
             0,  0, sz,  0,
             0,  0,  0,  1)

  /**
   * Computer matrix representing rotation by specified angle around specified axis
   * @param theta angle of rotation
   * @param ax
   * @param ay
   * @param az
   * @return result matrix
   */
  def rotation(theta: Float, ax: Float, ay: Float, az: Float): Matrix44 = rotation(theta, Vector4(ax, ay, az))
  def rotation(theta: Float, axis: Vector4) = {
    val cosTheta = math.cos(theta).toFloat
    val sinTheta = math.sin(theta).toFloat
    val R = Matrix44()
    //    R *= cosTheta
    val A = crossProdMatrix(axis)
    R += A * sinTheta
    R += (A * A) * (1 - cosTheta)
    //    R += (crossProdMatrix(axis) *= sinTheta)
    //    R += (Matrix44.outerProd(axis, axis) *= (1 - cosTheta))
    R
  }

  /**
   * Compute matrix representing a cross product operation with specified vector
   * @param u a vector to cross product
   * @return result matrix
   */
  def crossProdMatrix(u: Vector4) =
    Matrix44(   0, -u.z,  u.y,  0,
              u.z,    0, -u.x,  0,
             -u.y,  u.x,    0,  0,
                0,    0,    0,  0)

  /**
   * Compute matrix representing lookAt transformation
   * (rotation by angle defined by eye, at and up vectors and translation to the eye position)
   * @param eye
   * @param at
   * @param up
   * @return result matrix
   */
  def lookAt(eye: Vector4, at: Vector4, up: Vector4) = {
    val w = (eye - at).normalize()//new Vector4(eye).normalize()
    val u = (up cross w).normalize()
    val v = (w cross u).normalize()

    Matrix44(u.x, u.y, u.z, -(eye*u),
             v.x, v.y, v.z, -(eye*v),
             w.x, w.y, w.z, -(eye*w),
             0,   0,   0,        1)
  }


}