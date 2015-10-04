package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.{Matrix44, Transform, Vector4}

/**
 * Created by SG0220070 on 9/22/2015.
 */
class Camera(val eye: Vector4, val at: Vector4, private val up0: Vector4, val fovy: Float) {
  val lookAtMatrix = Matrix44.lookAt(eye, at, up0)

  def origin = eye
  val ax = Vector4(lookAtMatrix(0,0), lookAtMatrix(0,1), lookAtMatrix(0,2), 0)
  val ay = Vector4(lookAtMatrix(1,0), lookAtMatrix(1,1), lookAtMatrix(1,2), 0)
  val az = Vector4(lookAtMatrix(2,0), lookAtMatrix(2,1), lookAtMatrix(2,2), 0)

  def dir = az
  def up = ay
  def left = ax

  override def toString =
    s"Camera(eye=$eye, at=$at, up=$up0, fovy=$fovy)"
}

case class Material(ambient: Color4, diffuse: Color4, specular: Color4, shininess: Float, emission: Color4) {
  override def toString =
    s"Material(ambient=$ambient, diffuse=$diffuse, specular=$specular, shininess=$shininess, emission=$emission"
}

case class Model(geometry: Geometry, transformation: Transform, material: Material)

case class Attenuation(constant: Float, linear: Float, quadratic: Float) {
  def calculate(dist: Float)=
    1.0f / (constant + linear*dist + quadratic*dist*dist)
}

abstract class Light(val color: Color4) {
  def directionTo(p: Vector4): Vector4
  def colorIntensity(p: Vector4): Color4
}

case class DirLight(dir: Vector4, override val color: Color4) extends Light(color) {
  dir.w = 0 // make sure that homogeneous coordinate is correct for direction vector
  dir.normalize()

  override def directionTo(p: Vector4) = new Vector4(dir)
  override def colorIntensity(p: Vector4) = new Color4(color)

  override def toString =
    s"DirLight(dir=$dir, color=$color)"
}
case class PointLight(pos: Vector4, override val color: Color4,attenuation: Attenuation) extends Light(color) {
  pos.w = 1 // make sure that homogeneous coordinate is correct for position vector

  override def directionTo(p: Vector4) = (p - pos).normalize()
  override def colorIntensity(p: Vector4) = color * attenuation.calculate((p - pos).length)

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
