package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.{Transform, Matrix44, Vector4}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by mike on 22.09.15.
 */
class ConfigParser {
  /*
  (Format)
  # comment
  blank lines
  command parameter1 parameter2 ...

  (General)
  size width height - first command of the config
  maxdepth depth (default 5)
  output filename

  (Camera)
  camera lookfromx lookfromy lookfromz lookatx lookaty lookatz upx upy upz fov
     fov == fovy

  (Geometry)
  sphere x y z radius
  maxverts number
  maxvertsnorms number
  vertex x y z
  vertexnormal x y z nx ny nz
  tri v1 v2 v3
  trinormal v1 v2 v3

    (vertexnormal, trinormal, maxvertnorms - not used in grader)
    (vertex tri sphere maxverts - used in grader)

  (Transformations)
  translate x y z
  rotate x y z angle
  scale x y z
  pushTransform
  popTransform

  (Lights)
  directional x y z r g b
  point x y z r g b
  attenuation const linear quadratic (default 1,0,0)
  ambient r g b (default 0.2, 0.2, 0.2

  (Materials)
  diffuse r g b
  specular r g b
  shininess s
  emission r g b
   */

  class Config(size: (Int, Int), outFilename: String, maxDepth: Int)

  sealed trait TransformCmd
  case class PushTransform() extends TransformCmd
  case class PopTransform() extends TransformCmd

  trait TransformOp extends TransformCmd {
    def matrix: Matrix44
    def invMatrix: Matrix44
  }
  case class MatrixTransformOp(matrix: Matrix44, invMatrix: Matrix44) extends TransformOp

  case class TranslateOp(t: Vector4)
    extends MatrixTransformOp(Transform.translation(t), Transform.translation(Vector4.inverse(t)))

  case class RotateOp(angle: Float, axis: Vector4)
    extends MatrixTransformOp(Transform.rotation(angle, axis), Transform.rotation(-angle, axis))

  case class ScaleOp(s: Vector4)
    extends MatrixTransformOp(Transform.scale(s), Transform.scale(new Vector4(1/s.x, 1/s.y, 1/s.z)))



  def parse(filename: String) = parse(Source.fromFile(filename))

  class Builder {
    var size: (Int, Int) = _
    var maxDepth = 5
    var outFile: String = _

    var camera: Camera = _
    var vertices = ArrayBuffer[Vector4]()
    var faces = ArrayBuffer[(Int, Int, Int)]()
    var spheres = ArrayBuffer[Sphere]()

    var transformCmds = ArrayBuffer[TransformCmd]()
  }

  def parse(input: Source) = {
    val builder = new Builder()

    val CommentMatch = "#.*".r
    val SizeMatch = raw"size (\d+) (\d+)".r
    val MaxDepthMatch = raw"maxdepth (\d+)".r
    val OutputMatch = raw"output (\S+)".r

    val CameraMatch = raw"camera (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+))".r

    val MaxVertsMatch = raw"maxverts (\d+)".r
    val VertexMatch = raw"vertex (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+))".r
    val TriMatch = raw"tri (\d+) (\d+) (\d+)".r

    val SphereMatch = raw"sphere (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+))".r

    val PushTransformMatch = "pushTransform".r
    val PopTransformMatch = "popTransform".r
    val TranslationMatch = raw"translate (\d+) (\d+) (\d+)".r
    val RotationMatch = raw"rotate (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+))".r
    val ScaleMatch = raw"scale (\d+(?:\.\d+)) (\d+(?:\.\d+)) (\d+(?:\.\d+))".r



    for (line <- input.getLines();
          tokens = line.split(" ")) yield {
      tokens.head match {

        case CommentMatch => // ignore
        case SizeMatch(x, y) => builder.size = (x.toInt,y.toInt)
        case MaxDepthMatch(d) => builder.maxDepth = d.toInt
        case OutputMatch(name) => builder.outFile = name

        case CameraMatch(eyex, eyey, eyez, atx, aty, atz, upx, upy, upz, fovy) =>
          builder.camera = Camera(Vector4(eyex.toFloat, eyey.toFloat, eyez.toFloat),
                                  Vector4(atx.toFloat, aty.toFloat, atz.toFloat),
                                  Vector4(upx.toFloat, upy.toFloat, upz.toFloat),
                                  fovy.toFloat)

        case MaxVertsMatch(n) => println(s"Ignoring maxverts: $n")
        case VertexMatch(x, y, z) => builder.vertices += Vector4(x.toFloat, y.toFloat, z.toFloat)
        case TriMatch(a, b, c) => builder.faces += ((a.toInt, b.toInt, c.toInt))

        case SphereMatch(x, y, z, r) => builder.spheres += Sphere(x.toFloat, y.toFloat, z.toFloat, r.toFloat)


      }
    }
  }

}

object ConfigParser {
  def main(args: Array[String]): Unit = {
    new ConfigParser().parse(Source.fromString(
      """
        |size 256 256
        |
        |translate 1 2 3
        |
        |rotate 1 2 3 4
      """.stripMargin))
  }
}
