package org.mikesajak.raytracer

import org.mikesajak.raytracer.math.{Transform, Matrix44, Vector4}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by mike on 22.09.15.
 */
object ConfigParser {
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

  def parse(filename: String): (Config, Scene) = parse(Source.fromFile(filename))

  def parse(input: Source): (Config, Scene) = {
    val configBuilder= Config.builder()
    val sceneBuilder = new SceneBuilder()

    val CommentMatch = "#.*".r
    val EmptyMatch = raw"\s*".r


    val float2 = raw"([-+]?\d+(?:\.\d+)?|\.\d+)"
    val float = raw"([-+]?(?:(?:\d+)|(?:\d+\.\d+)|(?:\.\d+)))"
    val int = raw"(\d+)"

    def from(tokens: String*) =
      (tokens.tail foldLeft tokens.head)((acc, token) => acc + "\\s+" + token).r


    val SizeMatch = from("size", int, int)
    val MaxDepthMatch = from("maxdepth",int)
    val OutputMatch = from("output", raw"(\S+)")

    val CameraMatch = from("camera", float, float, float, float, float, float, float, float, float, float)

    val MaxVertsMatch = from("maxverts", int)
    val MaxVertNormsMatch = from("maxvertnorms", int)
    val VertexMatch = from("vertex", float, float, float)
    val VertexNormalMatch = from("vertexnormal", float, float, float)
    val TriMatch = from("tri", int, int, int)
    val TriNormalMatch = from("trinormal", int, int, int)

    val SphereMatch = from("sphere", float, float, float, float)

    val PushTransformMatch = "pushTransform".r
    val PopTransformMatch = "popTransform".r
    val TranslationMatch = from("translate", float, float, float)
    val RotationMatch = from("rotate", float, float, float, float)
    val ScaleMatch = from("scale", float, float, float)

    val DirLightMatch = from("directional", float, float, float, float, float,float)
    val PointLightMatch= from("point", float, float, float, float, float, float)
    val AttenuationMatch = from("attenuation", float, float, float)

    val AmbientMatch = from("ambient", float, float, float)
    val DiffuseMatch = from("diffuse", float, float, float)
    val SpecularMatch = from("specular", float, float, float)
    val ShininessMatch = from("shininess", float)
    val EmissionMatch = from("emission", float, float, float)

    for (rawLine <- input.getLines();
         line = rawLine.trim) {
      line match {

        case CommentMatch() => // ignore
        case EmptyMatch() => // ignore

        case SizeMatch(x, y) => configBuilder.size = (x.toInt,y.toInt)
        case MaxDepthMatch(d) => configBuilder.maxDepth = d.toInt
        case OutputMatch(name) => configBuilder.outFile = name

        case CameraMatch(eyex, eyey, eyez, atx, aty, atz, upx, upy, upz, fovy) =>
          sceneBuilder.camera = new Camera(Vector4(eyex.toFloat, eyey.toFloat, eyez.toFloat, 1),
                                           Vector4(atx.toFloat, aty.toFloat, atz.toFloat, 1),
                                           Vector4(upx.toFloat, upy.toFloat, upz.toFloat, 0),
                                           fovy.toFloat)

        // geometry
        case MaxVertsMatch(n) => println(s"Ignoring maxverts: $n")
        case MaxVertNormsMatch(n) => println(s"Ignoring maxvertnorms: $n")

        case VertexMatch(x, y, z) => sceneBuilder.addVertex(Vector4(x.toFloat, y.toFloat, z.toFloat, 1))
        case VertexNormalMatch(x, y, z) => sceneBuilder.addNormal(Vector4(x.toFloat, y.toFloat, z.toFloat))
        case TriMatch(a, b, c) => sceneBuilder.addFace(a.toInt, b.toInt, c.toInt)
        case TriNormalMatch(a, b, c) => sceneBuilder.addFaceNormal(a.toInt, b.toInt, c.toInt)
        case SphereMatch(x, y, z, r) => sceneBuilder.addSphere(Vector4(x.toFloat, y.toFloat, z.toFloat, 1), r.toFloat)

        // lights & materials
        case DirLightMatch(dx, dy, dz, r, g, b) =>
          val dir = Vector4(dx.toFloat, dy.toFloat, dz.toFloat, 0)
          val color = Color4(r.toFloat, g.toFloat, b.toFloat)
          sceneBuilder.addDirLight(dir, color)

        case PointLightMatch(px, py, pz, r, g, b) =>
          val pos = Vector4(px.toFloat, py.toFloat, pz.toFloat, 1)
          val color = Color4(r.toFloat, g.toFloat, b.toFloat)
          sceneBuilder.addPosLight(pos, color)

        case AmbientMatch(r, g, b) => sceneBuilder.ambient = Color4(r.toFloat, g.toFloat, b.toFloat)
        case DiffuseMatch(r, g, b) => sceneBuilder.diffuse = Color4(r.toFloat, g.toFloat, b.toFloat)
        case SpecularMatch(r, g, b) => sceneBuilder.specular = Color4(r.toFloat, g.toFloat, b.toFloat)
        case ShininessMatch(s) => sceneBuilder.shininess = s.toFloat
        case EmissionMatch(r, g, b) => sceneBuilder.emission = Color4(r.toFloat, g.toFloat, b.toFloat)

        // transformations
        case PushTransformMatch() => sceneBuilder.pushTransform()

        case PopTransformMatch() => sceneBuilder.popTransform()

        case TranslationMatch(tx, ty, tz) =>
          val t = Vector4(tx.toFloat, ty.toFloat, tz.toFloat)
          sceneBuilder.addTransform(Transform.translation(t))

        case RotationMatch(ax, ay, az, angle) =>
          val axis = Vector4(ax.toFloat, ay.toFloat, az.toFloat)
          sceneBuilder.addTransform(Transform.rotation(angle.toFloat, axis))

        case ScaleMatch(sx, sy, sz) =>
          val s = Vector4(sx.toFloat, sy.toFloat, sz.toFloat)
          sceneBuilder.addTransform(Transform.scale(s))

        case _ => println(s"*** No match for line: $line")

      }
    }

    (configBuilder.build(), sceneBuilder.scene)
  }

  class SceneBuilder(val scene: Scene) {
    def this() = this(new Scene())

    private val vertices = ArrayBuffer[Vector4]()
    private val vertexNormals = ArrayBuffer[Vector4]()
    private val faces = ArrayBuffer[(Int, Int, Int)]()
    private val faceNormals = ArrayBuffer[(Int, Int, Int)]()
    private val spheres = ArrayBuffer[Sphere]()

    private var transformationStack = List[Transform]()
    private var curTransform = new Transform(Matrix44.identity(), Matrix44.identity())

    private var curAttenuation = new Attenuation(1,0,0)
    private var curAmbient = new Color4(0.2f, 0.2f, 0.2f)
    private var curDiffuse = new Color4(0,0,0)
    private var curSpecular = new Color4(0,0,0)
    private var curShininess = 0.0f
    private var curEmission = new Color4(0,0,0)

    def camera = scene.camera
    def camera_=(cam: Camera) = {
      println(s"Camera=$cam")
      scene.camera = cam
    }

    def addVertex(v:Vector4)= {
      println(s"Adding vertex: $v")
      vertices += v
    }

    def addNormal(n: Vector4) = {
      println(s"Adding normal: $n")
      vertexNormals += n
    }

    def addFace(v1: Int, v2: Int, v3: Int) = {
      println(s"Adding face: ($v1, $v2, $v3)")
      faces += ((v1, v2, v3))
      // first try - simple model with single triangle
      scene.models :+= Model(Triangle(vertices(v1), vertices(v2), vertices(v3)), transform, material)
    }

    def addFaceNormal(n1: Int, n2: Int, n3: Int) = {
      println(s"Adding face normals: ($n1, $n2, $n3)")
      faceNormals += ((n1, n2, n3))
    }

    def addSphere(center: Vector4, radius: Float) = {
      println(s"Adding sphere: center=$center, radius=$radius")
      //      spheres += Sphere(center, radius)
      scene.models :+= Model(new Sphere(center, radius), transform, material)
    }

    def attenuation = curAttenuation
    def attenuation_=(a: Attenuation) = {
      println(s"Attenuation=$a")
      curAttenuation = a
    }

    def ambient = curAmbient
    def ambient_=(a: Color4) = {
      println(s"Ambient color=$a")
      curAmbient = a
    }

    def diffuse = curDiffuse
    def diffuse_=(d: Color4) = {
      println(s"Diffuse color=$d")
      curDiffuse = d
    }

    def specular = curSpecular
    def specular_=(s: Color4) = {
      println(s"Specular color=$s")
      curSpecular = s
    }

    def shininess = curShininess
    def shininess_=(s: Float) = {
      println(s"Shininess=$s")
      curShininess = s
    }

    def emission = curEmission
    def emission_=(e: Color4) = {
      println(s"Emission color=$e")
      curEmission = e
    }

    def pushTransform() = {
      println(s"Push transform: $curTransform")
      transformationStack ::= new Transform(curTransform)
    }

    def popTransform() = {
      print(s"Pop transform prev: $curTransform")
      curTransform = transformationStack.head
      transformationStack = transformationStack.tail
      println(s" cur: $curTransform")
    }

    def material =
      Material(curAmbient, curDiffuse, curSpecular, curShininess, curEmission)

    //    def setTransform(t: Transform) = curTransform := t

    def addTransform(t: Transform) = {
      println(s"Add transform: $t")
      curTransform = curTransform combine t
    }

    def transform = new Transform(curTransform)

    def addDirLight(dir: Vector4, color: Color4) = {
      println(s"Add directional light: dir=$dir, color=$color")
      val lightDir = dir * curTransform.matrix
      scene.lights :+= DirLight(lightDir, color)
    }

    def addPosLight(pos: Vector4, color: Color4) = {
      println(s"Add positional light: pos=$pos, color=$color")
      val lightPos = pos * curTransform.matrix
      scene.lights :+= PointLight(lightPos, color, attenuation)
    }
  }



  def main(args: Array[String]): Unit = {
    val (config, scene) = ConfigParser.parse(Source.fromURL(getClass.getResource("/simple-sphere.test")))

    println(config)
    println(scene)
  }
}
