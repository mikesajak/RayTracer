package org.mikesajak.raytracer.math

/**
 * Created by SG0220070 on 9/22/2015.
 */
object Transform {
  def translation(t: Vector4) = translation(t.x, t.y, t.z)
  def translation(tx: Float, ty: Float, tz: Float) =
      Matrix44(1, 0, 0, tx,
               0, 1, 0, ty,
               0, 0, 1, tz,
               0, 0, 0, 1)

  def scale(s: Vector4) = scale(s.x, s.y, s.z)
  def scale(sx: Float, sy: Float, sz: Float) =
      Matrix44(sx,  0,  0,  0,
                0, sy,  0,  0,
                0,  0, sz,  0,
                0,  0,  0,  1)

  def rotation(theta: Float, axis: Vector4) = {
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
    val w = new Vector4(eye).normalize()
    val u = (new Vector4(up) cross w).normalize()
    val v = new Vector4(w).normalize()

    Matrix44(u.x, u.y, u.z, eye*u,
             v.x, v.y, v.z, eye*v,
             w.x, w.y, w.z, eye*w,
               0,   0,   0,     1)
  }
}
