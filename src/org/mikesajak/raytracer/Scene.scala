package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.Vector4

/**
 * Created by SG0220070 on 9/22/2015.
 */

case class Color4(r: Float, g: Float, b: Float, a: Float = 1.0)

case class Material(ambient: Color4, diffuse: Color4, specular: Color4, shininess: Float, emmision: Color4)

class Model(material: Material, geometry: Geometry)

case class Attenuation(const: Float, linear: Float, quadratic: Float)

case class DirLight(dir: Vector4, color: Color4, attenuation: Attenuation)
case class PosLight(pos: Vector4, color: Color4, attenuation: Attenuation)


class Scene {
  val vertices = Array[Vector4]()
  val triangles = Array[Tuple3[Int, Int, Int]]()
  val spheres = Array[Sphere]()


}
