package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.{Matrix44, Transform, Vector4}

/**
 * Created by mike on 26.09.15.
 */
class RayTracer {

  def process(config: Config, scene: Scene, pixelOutput: PixelOutput) = {

//    def pixmap = new Pixmap(config.size._1, config.size._2)
//    pixelOutput.setSize(config.size._1, config.size._2)

    val pixelBuffer = new PixelOutput {
      var list = List[(Int, Int, Color4)]()

      override def setPixel(x: Int, y: Int, color: Color4) = list ::= (x, y, color)
    }

    val pixels =
      for (y <- 0 until config.size._2;
           x <- 0 until config.size._1)
        yield (x,y)

    val stats = new Stats
    pixels.par foreach { case pixel@(x,y) =>
      val ray = mkRay(x,y, config.size._1, config.size._2, scene.camera)

      val intersection = intersect(ray, scene)

      val color = intersection.map{ case (m,i) => findColor(i, ray, m, scene) } getOrElse Color4(0,0,0)

      pixelOutput.setPixel(x, y, color)
    }

    println(s"Statistics: $stats")

//    for ((x,y,color) <- pixelBuffer.list) pixelOutput.setPixel(x, y, color)

  }

  def mkRay(x: Int, y: Int, width: Int, height: Int, camera: Camera) = {
    // make ray go through the middle of rectangle representing a pixel on camera screen instead of a corner
    val i = y + 0.5f
    val j = x + 0.5f

    val aspect = width / height.toFloat
    val fovx = scala.math.toRadians(camera.fovy * aspect)
    val fovy = scala.math.toRadians(camera.fovy)
    val halfWidth = width/2.0f
    val halfHeight = height/2.0f

//    val alpha = scala.math.tan(fovx/2).toFloat * ((j - halfWidth)/halfWidth)
    var alpha = scala.math.tan(fovx/2).toFloat * ((j - halfWidth)/halfWidth)
    var beta = scala.math.tan(fovy/2).toFloat * ((halfHeight - i) / halfHeight)

    val w = (camera.eye - camera.at).normalize()
    val u = (camera.up cross w).normalize()
    val v = w cross u

    u *= alpha
    v *= beta

//    val dir = (u + v - w).normalize()
    val dir = u + v
    dir -= w
    dir.normalize()

    Ray(camera.eye, dir)
  }

  def traceRay(ray: Ray, scene: Scene, config: Config): Color4 = {
    Color4(1,0,0)
  }

  ///////////////////
  def intersect(ray: Ray, scene: Scene): Option[(Model, Intersection)] = {
    val intersections =
      for (model <- scene.modelsCloseTo(ray);
           modelSpaceRay = Ray.transform(ray, model.transformation.inverse);
           modelSpaceIntersection <- model.geometry.intersect(modelSpaceRay);
           intersection = transform(modelSpaceIntersection, model.transformation))
        yield (model, intersection)

    if (intersections.nonEmpty) Some(intersections.minBy{ case (m, i) => Vector4.dist(scene.camera.origin, i.point) })
    else None
  }

  def transform(intersection: Intersection, t: Transform) =
    Intersection(intersection.point * t.matrix, intersection.normal * Matrix44.transpose(t.inverse))

  val EPSILON = 0.1f

  def findColor(intersection: Intersection, cameraRay: Ray, model: Model, scene: Scene): Color4 = {
    val constantColor = model.material.ambient + model.material.emission
    val shadingColor =
      (scene.lights foldLeft Color4(0,0,0)) { (colorAcc, light) =>
        val dirToLight = light.directionTo(intersection.point).inverse()

        //move intersection point slightly towards the light to avoid positioning it under the surface because of numerical error
        val point = Ray(intersection.point, dirToLight).point(EPSILON)
        val ray = Ray(point, dirToLight)

        val visible = intersect(ray, scene) == None
        if (visible) {
          val pointColor = Color4(0,0,0)

          // add diffuse term
          val lightDirCosine = dirToLight * intersection.normal
          if (lightDirCosine > 0)
//            pointColor += model.material.diffuse * lightDirCosine
            pointColor.addScaled(model.material.diffuse, lightDirCosine)

          // add specular term
          val halfVec = (dirToLight - cameraRay.dir).normalize()
          // note: camera ray is ray _from_ camera, but for half vector I need to add dir to light and dir _to_ eye
          // so instead of inverting cameraRay I just subtract it
          val halfVecNCosine = (halfVec * intersection.normal)
          if (halfVecNCosine > 0) {
            val shininess = scala.math.pow(halfVecNCosine, model.material.shininess).toFloat
//            pointColor += model.material.specular * shininess
              pointColor.addScaled(model.material.specular, shininess)
          }

          val lightIntensity = light.colorIntensity(intersection.point)
//          colorAcc += lightIntensity * pointColor
          colorAcc.addScaled(pointColor, lightIntensity)
        }

        colorAcc
    }

    constantColor += shadingColor
  }

}
