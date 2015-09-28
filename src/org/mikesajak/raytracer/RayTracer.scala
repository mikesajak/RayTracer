package org.mikesajak.raytracer

import java.util.concurrent.TimeUnit

import com.google.common.base.Stopwatch
import org.mikesajak.raytracer.math.{Transform, Matrix44, Vector4}

/**
 * Created by mike on 26.09.15.
 */
class RayTracer {

  class Stats {
    var statsMap = collection.mutable.Map[String, AverageCalc]()

    def accumulate(name: String, value: Long) = {
      val avgCalc = statsMap.getOrElseUpdate(name, new AverageCalc)
      avgCalc.acc(value)
    }

    def get(name: String) = statsMap.get(name)

    override def toString = statsMap.toString

  }
  
  class AverageCalc {
    private var avg0 = 0.0
    private var count = 0
    
    def acc(x: Double) = {
      count += 1
      avg0 += (x - avg0) / count
    }
    
    def avg = avg0
    def numSamples = count
    
    override def toString = s"{average=$avg0, num samples=$count}"
  }

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
    pixels foreach { case pixel@(x,y) =>
      val startTime = System.nanoTime()
      val ray = mkRay(x,y, config.size._1, config.size._2, scene.camera)
      val rayTime = System.nanoTime()
      stats.accumulate("ray", rayTime - startTime)

      val intersection = intersect(ray, scene)
      val intersectionTime = System.nanoTime()
      stats.accumulate("intersection", intersectionTime - rayTime)

      val color = intersection.map(i => findColor(i)) getOrElse Color4(0,0,0)
      val lightingTime = System.nanoTime()
      stats.accumulate("lighting", lightingTime - intersectionTime)
      stats.accumulate("ray tracing", lightingTime - startTime)

      pixelOutput.setPixel(x, y, color)
      val pixelTime = System.nanoTime()
      stats.accumulate("set pixel", pixelTime - lightingTime)
      stats.accumulate("whole pixel", pixelTime - startTime)
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
    var alpha = scala.math.tan(fovx/2).toFloat * ((halfWidth - j)/halfWidth)
    var beta = scala.math.tan(fovy/2).toFloat * ((halfHeight - i) / halfHeight)

    val w = (camera.eye - camera.at).normalize()
    val u = (new Vector4(camera.up) cross w).normalize()
    val v = new Vector4(w) cross u

    u *= alpha
    v *= beta

    val dir = (u + v - w).normalize()
    Ray(camera.eye, dir)
  }

  def traceRay(ray: Ray, scene: Scene, config: Config): Color4 = {
    Color4(1,0,0)
  }

  ///////////////////
  def intersect(ray: Ray, scene: Scene): Option[Intersection] = {
    val intersections =
      for (model <- scene.modelsCloseTo(ray);
           modelSpaceRay = Ray.transform(ray, model.transformation.inverse);
           modelSpaceIntersection <- model.geometry.intersect(modelSpaceRay);
           intersection = transform(modelSpaceIntersection, model.transformation))
        yield intersection

    if (intersections.nonEmpty) Some(intersections.minBy(i => Vector4.dist(scene.camera.origin, i.point)))
    else None
  }

  def transform(intersection: Intersection, t: Transform) =
    Intersection(intersection.point * t.matrix, intersection.normal * Matrix44.transpose(t.inverse))

  def findColor(intersection: Intersection): Color4 = {
    // todo:
    Color4(1,0,0)
  }

}
