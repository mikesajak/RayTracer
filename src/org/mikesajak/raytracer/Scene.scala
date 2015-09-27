package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.{Matrix44, Transform, Vector4}

/**
 * Created by SG0220070 on 9/22/2015.
 */
class Camera(val eye: Vector4, val at: Vector4, private val up0: Vector4, val fovy: Float) {
  val lookAtMatrix = Matrix44.lookAt(eye, at, up0)

  def origin = eye
  val ax = {
    val ax0 = new Vector4(lookAtMatrix(0))
    ax0(3) = 0
    ax0
  }

  val ay = {
    val ay0 = new Vector4(lookAtMatrix(1))
    ay0(3) = 0
    ay0
  }

  val az = {
    val az0 = new Vector4(lookAtMatrix(2))
    az0(3) = 0
    az0
  }

  def dir = lookAtMatrix(2)
  def up = lookAtMatrix(1)
  def left = lookAtMatrix(0)

  override def toString =
    s"Camera(eye=$eye, at=$at, up=$up0, fovy=$fovy)"
}

case class Color4(r: Float, g: Float, b: Float, a: Float = 1.0f) {
  override def toString =s"Color4(r=$r, g=$g, b=$b, a=$a)"

  def apply(idx: Int) = idx match {
    case 0 => r
    case 1 => g
    case 2 => b
    case 3 => a
  }

  def argb = pack(2,1,0,3)
  def pack(i1: Int, i2: Int, i3: Int, i4: Int) =
    (apply(i1) * 255).toInt | ((apply(i2) * 255).toInt << 8) | ((apply(i3) * 255).toInt << 16) | ((apply(i4) * 255).toInt << 24)
}

case class Material(ambient: Color4, diffuse: Color4, specular: Color4, shininess: Float, emission: Color4) {
  override def toString =
    s"Material(ambient=$ambient, diffuse=$diffuse, specular=$specular, shininess=$shininess, emission=$emission"
}

case class Model(geometry: Geometry, transformation: Transform, material: Material)

case class Attenuation(const: Float, linear: Float, quadratic: Float)

trait Light

case class DirLight(dir: Vector4, color: Color4, attenuation: Attenuation) extends Light{
  override def toString =
    s"DirLight(dir=$dir, color=$color, attenuation=$attenuation)"
}
case class PointLight(pos: Vector4, color: Color4, attenuation: Attenuation) extends Light {
  override def toString =
    s"PointLight(pos=$pos, color=$color, attenuation=$attenuation"
}

class Scene {
  var camera: Camera = _

  var models = collection.mutable.Seq[Model]()
  var lights = collection.mutable.Seq[Light]()

  def modelsCloseTo(ray: Ray) = {
    // todo: optimize to select only models on a grid, or in octtree, or bsp...
    // for now just return all models
    models
  }

  override def toString = {
    val modelsStr = (models foldLeft new StringBuilder)((acc,m) => acc ++= s"\n    $m")
    val lightsStr = (lights foldLeft new StringBuilder)((acc,l) => acc ++= s"\n    $l")
    s"Scene(\n  camera: $camera, \n  models:$modelsStr, \n  lights:$lightsStr\n)"
  }

}
