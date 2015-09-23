package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.Vector4

/**
 * Created by mike on 23.09.15.
 */
trait Geometry {
  def intersect(ray: Ray): Option[Intersection]
}

case class Sphere(center: Vector4, radius: Float) extends Geometry {
  override def intersect(ray: Ray) = {
    // ray: P = P0 + P1*t
    // sphere: (P-C)*(P-C) - r^2 = 0
    // t^2*(P1*P1) + 2*t*P1*(P0-C) + (P0-C)*(P0-C) - r2 = 0
    // discr = b^2 - a*c
    val rayToCenter = ray.origin - center
    val a = ray.dir.d2
    val b = ray.dir*rayToCenter*2
    val c = (ray.origin - rayToCenter).d2
    val discr = b*b - 4*a*c

    if (discr <= 0) None
    else {
      val discrRoot = scala.math.sqrt(discr).toFloat

      val t0 = (-b - discrRoot) / (2*a)
      val t1 = (-b + discrRoot) / (2*a)

      val t =
        if (t0 > 0 && t1 > 0) t0.min(t1)
        else if (t0 > 0) t0
        else t1

      val point = ray.point(t)
      val normal = (point - center).normalize()

      Some(Intersection(point, normal))
    }
  }
}

case class Triangle(p1: Vector4, p2: Vector4, p3: Vector4) extends Geometry {
  def normal = ((p2 - p1) cross (p3 - p1)).normalize()
  override def intersect(ray: Ray): Option[Intersection] = ??? // TODO
}

case class Mesh(triangles: Seq[Triangle]) extends Geometry {
  override def intersect(ray: Ray) = {
    val intersections =
      for (tri <- triangles;
           intersection <- tri.intersect(ray))
        yield intersection

    if (intersections.nonEmpty) {
      val closest = intersections.min(Ordering.by((i: Intersection) => (i.point - ray.origin).length))
      Some(closest)
    } else None
  }
}


case class Ray(origin: Vector4, dir: Vector4) {
  def point(t: Float) = {
    val p = new Vector4(dir)
    p *= t
    p += origin
    p
  }
}

case class Intersection(point: Vector4, normal: Vector4)


