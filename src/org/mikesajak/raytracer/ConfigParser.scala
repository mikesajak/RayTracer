package org.mikesajak.raytracer

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


  trait Command {
    def name: String
//    def params: Seq[String]
  }



  case class Directive(name: String)
  case class Size(name: String, width: Int, height: Int) extends Command
  case class IntCommand(name: String, value: Int)
  case class Color(name: String, r: Float, g: Float, b: Float)

  def parse(filename: String) = parse(Source.fromFile(filename))

  def parse(input: Source) = {
    for (line <- input.getLines();
          tokens = line.split(" ")) yield {
      tokens.head match {
        case "size" => Size("size", tokens(1).toInt, tokens(2).toInt)
        case "maxdepth" => IntCommand("maxdepth", tokens(1).toInt)
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
