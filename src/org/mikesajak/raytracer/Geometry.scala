package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.{Matrix44, Vector4}

/**
 * Created by mike on 23.09.15.
 */
trait Geometry {
  def intersect(ray: Ray): Option[RayIntersection]
}

case class Sphere(center: Vector4, radius: Float) extends Geometry {
  def intersect2(ray: Ray) = {
    // ray: P = P0 + P1*t
    // sphere: (P-C)*(P-C) - r^2 = 0
    // t^2*(P1*P1) + 2*t*P1*(P0-C) + (P0-C)*(P0-C) - r^2 = 0
    // discr = b^2 - a*c
    val rayToCenter = ray.origin - center
    val a = ray.dir.d2
    val b = 2 * (ray.dir * rayToCenter)
    val c = (ray.origin - rayToCenter).d2 - radius*radius
    val discr = b*b - 4*a*c

    if (discr <= 0) None
    else {
      val discrRoot = scala.math.sqrt(discr).toFloat

      val t0 = (-b - discrRoot) / (2*a)
      val t1 = (-b + discrRoot) / (2*a)

      if (t0 > 0 || t1 > 0) {
        val t =
          if (t0 > 0 && t1 > 0) t0.min(t1)
          else if (t0 > 0) t0
          else t1


        val point = ray.point(t)
        val normal = (point - center).normalize()

        Some(new RayIntersection(ray, t, normal))
      }
      else None
    }
  }

  override def intersect(ray: Ray) = {
    // ray: P = P0 + P1*t
    // sphere: (P-C)*(P-C) - r^2 = 0
    // t^2*(P1*P1) + 2*t*P1*(P0-C) + (P0-C)*(P0-C) - r^2 = 0
    // discr = b^2 - a*c
    val v = ray.origin - center
//    val a = ray.dir.d2
    val b = 2 * (ray.dir * v)
    val c = v.d2 - radius*radius
    val discr = b*b - 4*c

    if (discr <= 0) None
    else {
      val discrRoot = scala.math.sqrt(discr).toFloat

      val t0 = (-b - discrRoot) / 2
      val t1 = (-b + discrRoot) / 2

      if (t0 > 0 || t1 > 0) {
        val t =
          if (t0 > 0 && t1 > 0) t0.min(t1)
          else if (t0 > 0) t0
          else t1


        val point = ray.point(t)
        val normal = (point - center).normalize()

        Some(new RayIntersection(ray, t, normal))
      }
      else None
    }
  }
}

case class Triangle(p1: Vector4, p2: Vector4, p3: Vector4) extends Geometry {
  def normal = {
    //((p2 - p1) cross (p3 - p1)).normalize()
    val n = p2 - p1
    n.crossAndSet(p3 - p1)
    n.normalize()
  }

  val EPSILON = 0.00001f

  override def intersect(ray: Ray): Option[RayIntersection] = {
    // Moller-Trumbore algorithm

    // find vectors for two edges sharing p1
    val e1 = p2 - p1
    val e2 = p3 - p1

    // start calculating determinant - also used to calculate u parameter
    val p = ray.dir cross e2

    // if determinant is near zero, ray lies in plane of the triangle
    val det = e1 * p

    // not culling
    if (det > -EPSILON && det < EPSILON) None // the intersection lies outside of the triangle
    else {
      val invDet = 1.0f / det

      // calculate distance from v1 to ray origin
      val t = ray.origin - p1

      // calculate u parameter and test bound
      val u = t * p * invDet

      // the intersection lays outside of the triangle

      if (u < 0.0f || u > 1.0f) None
      else {
        // prepare to test v parameter
        val q = t cross e1

        // calculate v parameter and test bound
        val v = ray.dir * q * invDet

        // the intersection lies outside of the triangle
        if (v < 0.0f || u + v > 1.0f) None
        else {
          val tRay = e2 * q * invDet

          if (tRay > EPSILON) // ray intersection
            Some(new RayIntersection(ray, tRay, normal))
          else None
        }
      }
    }
  }

}



case class IndexedTriangle(vertices: Seq[Vector4], i1: Int, i2: Int, i3: Int) extends Geometry {
  def p1 = vertices(i1)
  def p2 = vertices(i2)
  def p3 = vertices(i3)

  def normal = ((p2 - p1) cross (p3 - p1)).normalize()
  override def intersect(ray: Ray): Option[RayIntersection] = {
    None
  }
}

case class Mesh(triangles: Seq[Triangle]) extends Geometry {
  override def intersect(ray: Ray) = {
    val intersections =
      for (tri <- triangles;
           intersection <- tri.intersect(ray))
        yield intersection

    if (intersections.nonEmpty) {
      val closest = intersections.min(Ordering.by((i: RayIntersection) => i.t))//(i.point - ray.origin).length))
      Some(closest)
    }
    else None
  }
}


case class Ray(origin: Vector4, dir: Vector4) {
  def this(ray: Ray) = this(new Vector4(ray.origin), new Vector4(ray.dir))

  origin.w = 1
  dir.w = 0
  def point(t: Float) = {
    val p = new Vector4(dir)
    p *= t
    p += origin
    p
  }

  def transform(m: Matrix44) = {
    origin *= m
    dir *= m
//    dir.normalize()
    this
  }
}

object Ray {
  def transform(ray: Ray, m: Matrix44) = new Ray(ray).transform(m)
}

case class Intersection(point: Vector4, normal: Vector4)
class RayIntersection(val ray: Ray, val t: Float, override val normal: Vector4) extends Intersection(ray.point(t), normal) {
//  def point = ray.point(t)

  override def toString = s"Intersection(t=$t, point=$point, normal=$normal)"
}


