package org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
case class Transform(matrix: Matrix44, inverse: Matrix44) {
  def this(t: Transform) = this(new Matrix44(t.matrix), new Matrix44(t.inverse))

  def this() = this(Matrix44.identity(), Matrix44.identity())

//  def :=(t: Transform) = {
//    matrix := t.matrix
//    inverse := t.inverse
//    this
//  }

  def combine(t: Transform) =
    new Transform(matrix * t.matrix, t.inverse * inverse)

}

object Transform {
  def translation(t: Vector4) =
    Transform(Matrix44.translation(t), Matrix44.translation(Vector4.inverse(t)))

  def rotation(angle: Float, axis: Vector4) =
    Transform(Matrix44.rotation(angle, axis), Matrix44.rotation(-angle, axis))

  def scale(s: Vector4) =
    Transform(Matrix44.scale(s), Matrix44.scale(1/s.x, 1/s.y, 1/s.z))

  def combine(t1: Transform, t2: Transform) = new Transform(t1).combine(t2)
}
