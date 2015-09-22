package org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
object Transform {
  def translationMatrix(tx: Float, ty: Float, tz: Float) =
      Matrix44(1, 0, 0, tx,
               0, 1, 0, ty,
               0, 0, 1, tz,
               0, 0, 0, 1)

  def scaleMatrix(sx: Float, sy: Float, sz: Float) =
      Matrix44(sx,  0,  0,  0,
                0, sy,  0,  0,
                0,  0, sz,  0,
                0,  0,  0,  1)

  def rotationMatrix(theta: Float, axis: Vector4) = {
    val cosTheta = math.cos(theta).toFloat
    val R = Matrix44() *= cosTheta
    R += crossProdMatrix(axis) *= math.sin(theta).toFloat
    R += Matrix44.outerProd(axis, axis) *= (1 - cosTheta)
    R
  }

  def crossProdMatrix(u: Vector4) =
      Matrix44(   0, -u.x,  u.y,  0,
                u.z,    0, -u.x,  0,
               -u.y,  u.x,   0,   0,
                  0,    0,   0,   0)

  def lookAt(eye: Vector4, at: Vector4, up: Vector4) = {
    // todo
  }
}
