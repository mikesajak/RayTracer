package org.mikesajak.raytracer.math

import org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
class Vector4(x0: Float, y0: Float, z0: Float, w0: Float = 0) {
  val data = Array[Float](x0, y0, z0, w0)

  def this(v: Vector4) = this(v.x, v.y, v.z, v.w)
  def this() = this(0,0,0,0)

  def x = data(0)
  def x_=(x1: Float) = data(0) = x1

  def y = data(1)
  def y_=(y1: Float) = data(1) = y1

  def z = data(2)
  def z_=(z1: Float) = data(2) = z1

  def w = data(3)
  def w_=(w1: Float) = data(3) = w1

  def apply(idx: Int) = data(idx)
  def update(idx: Int, a: Float) = data(idx) = a

  def set(x0: Float, y0: Float, z0: Float, w0: Float) = {
    data(0) = x0
    data(1) = y0
    data(2) = z0
    data(3) = w0
    this
  }

  def :=(v: Vector4) = set(v.x, v.y, v.z, v.w)
  def :=(t: Tuple4[Float, Float, Float, Float]) = set(t._1, t._2, t._3, t._4)

  def fill(a: Float) = {
    data(0) = a
    data(1) = a
    data(2) = a
    data(3) = a
    this
  }

  def inverse() = {
    data(0) = -data(0)
    data(1) = -data(1)
    data(2) = -data(2)
//    data(3) = -data(3)
    this
  }

  def zero() = fill(0)
  def ones() = fill(1)

  def length = d
  def d2 = (data(0) * data(0) + data(1)*data(1) + data(2)*data(2))
  def d = {
    val dd = d2
    scala.math.sqrt(dd).toFloat
  }

  def normalize() = {
    val len = length
    data(0) /= len
    data(1) /= len
    data(2) /= len
    this
  }

  def +=(v: Vector4) = {
    data(0) += v.data(0)
    data(1) += v.data(1)
    data(2) += v.data(2)
//    data(3) += v.data(3)
    this
  }
  def +(v: Vector4) = new Vector4(this) += v

  def -=(v: Vector4) = {
    data(0) -= v.data(0)
    data(1) -= v.data(1)
    data(2) -= v.data(2)
//    data(3) -= v.data(3)
    this
  }
  def -(v: Vector4) = new Vector4(this) -= v

  def *=(a: Float) = {
    data(0) *= a
    data(1) *= a
    data(2) *= a
//    data(3) *= a
    this
  }

  def *(a: Float) = new Vector4(this) *= a

  def *=(m: Matrix44) = 
    set(data(0)*m(0,0) + data(1)*m(0,1) + data(2)*m(0,2) + data(3)*m(0,3),
        data(0)*m(1,0) + data(1)*m(1,1) + data(2)*m(1,2) + data(3)*m(1,3),
        data(0)*m(2,0) + data(1)*m(2,1) + data(2)*m(2,2) + data(3)*m(2,3),
        data(0)*m(3,0) + data(1)*m(3,1) + data(2)*m(3,2) + data(3)*m(3,3))

  def *(m: Matrix44) = new Vector4(this) *= m

  def dot(v: Vector4) = x*v.x + y*v.y + z*v.z// + w*v.w
  def *(v: Vector4) = this dot v

  def cross(v: Vector4) = new Vector4(this).crossAndSet(v)

  def crossAndSet(v: Vector4) =
    set(y*v.z - z*v.y,
        z*v.x - x*v.z,
        x*v.y - y*v.x,
        0)

  override def toString = s"[$x, $y, $z, $w](len=$length)"

  override def equals(o: Any) = o match {
    case v: Vector4 => x == v.x && y == v.y && z == v.z && w == v.w
    case _ => false
  }

  override def hashCode = 31 + 7*x.hashCode + 19*y.hashCode + 23*z.hashCode + 47*w.hashCode

  def toTuple = (x,y,z,w)
  def equals_+-(v: Vector4, epsilon: Float) = {
    def equals_+-(a: Float, b: Float, epsilon: Float) = scala.math.abs(a-b) < epsilon

    equals_+-(x, v.x, epsilon) && equals_+-(y, v.y, epsilon) && equals_+-(z, v.z, epsilon) && equals_+-(w, v.w, epsilon)
  }

}

object Vector4 {
  def apply() = new Vector4(0,0,0,0)
  def apply(x: Float, y: Float, z: Float, w: Float = 0) = new Vector4(x,y,z,w)

  def fill(a: Float) = new Vector4(a,a,a,a)
  def tabulate(f: (Int => Float)) = new Vector4(f(0), f(1), f(2), f(3))

  def normalize(v: Vector4) = new Vector4(v).normalize()
  def inverse(v: Vector4) = new Vector4(v).inverse()

  def cross(v1: Vector4, v2: Vector4) = new Vector4(v1).crossAndSet(v2)

  def dist(v1: Vector4, v2: Vector4) = {
    val dx = v1.x - v2.x
    val dy = v1.y - v2.y
    val dz = v1.z - v2.z
    scala.math.sqrt(dx * dx + dy * dy + dz * dz).toFloat
  }
}
