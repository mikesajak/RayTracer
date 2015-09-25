package org.mikesajak.raytracer.math

import org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
class Vector4(x0: Float, y0: Float, z0: Float, w0: Float = 0) {
  val data = Array[Float](4)

  def this(v: Vector4) = this(v.x, v.y, v.z, v.w)

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
    data(3) = -data(3)
    this
  }

  def zero() = fill(0)
  def ones() = fill(1)

  def length = d
  def d2 = this dot (this)
  def d = scala.math.sqrt(d2).toFloat

  def normalize() = Vector4.normalize(this)

  def +=(v: Vector4) = {
    data(0) += v.data(0)
    data(1) += v.data(1)
    data(2) += v.data(2)
    data(3) += v.data(3)
    this
  }
  def +(v: Vector4) = new Vector4(this) += v

  def -=(v: Vector4) = {
    data(0) -= v.data(0)
    data(1) -= v.data(1)
    data(2) -= v.data(2)
    data(3) -= v.data(3)
    this
  }
  def -(v: Vector4) = new Vector4(this) -= v

  def *=(a: Float) = {
    data(0) *= a
    data(1) *= a
    data(2) *= a
    data(3) *= a
    this
  }

  def *(a: Float) = new Vector4(this) *= a

  def *=(m: Matrix44) = 
    set(data(0)*m(0)(0) + data(1)*m(0)(1) + data(2)*m(0)(2) + data(3)*m(0)(3),
        data(0)*m(1)(0) + data(1)*m(1)(1) + data(2)*m(1)(2) + data(3)*m(1)(3),
        data(0)*m(2)(0) + data(1)*m(2)(1) + data(2)*m(2)(2) + data(3)*m(2)(3),
        data(0)*m(3)(0) + data(1)*m(3)(1) + data(2)*m(3)(2) + data(3)*m(3)(3))

  def *(m: Matrix44) = new Vector4(this) *= m

  def dot(v: Vector4) = x*v.x + y*v.y + z*v.z + w*v.w
  def *(v: Vector4) = this dot v

  def cross(v: Vector4) =
    set(y*v.z - z*v.y,
        z*v.x - x*v.z,
        x*v.y - y*v.z,
        0)
}

object Vector4 {
  def apply() = new Vector4(0,0,0,0)
  def apply(x: Float, y: Float, z: Float, w: Float = 0) = new Vector4(x,y,z,w)

  def normalize(v: Vector4) = v * (1.0f / v.length)

  def cross(v1: Vector4, v2: Vector4) = new Vector4(v1).cross(v2)
}