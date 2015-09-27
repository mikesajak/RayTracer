package org.mikesajak.raytracer.ui

import java.io.File

import akka.actor.{ActorRef, Props, Actor, ActorSystem}
import com.google.common.base.Stopwatch
import org.mikesajak.raytracer._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scalafx.Includes._
import scalafx.application.{Platform, JFXApp}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.image.PixelWriter
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.{Node, Scene}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.input.KeyCombination
import scalafx.scene.layout.{HBox, BorderPane, VBox}
import scalafx.scene.text.{Font, TextFlow, Text}
import collection.JavaConversions._
import scalafx.stage.{WindowEvent, FileChooser}
import scalafx.stage.FileChooser.ExtensionFilter

/**
 * Created by mike on 26.09.15.
 */
object RayTracerApp extends JFXApp {

  case class RenderRequest(cfg: Config, scene: org.mikesajak.raytracer.Scene, pixelOutput: PixelOutput)
  case class Done()

  class RayTracerActor extends Actor {
    def receive = {
      case RenderRequest(cfg, scene, pixelOutput) =>
        val rt = new RayTracer()
        rt.process(cfg, scene, pixelOutput)
        sender ! Done

      case x@_ => println(s"Received unknown message: $x")
    }
  }

  val actorSystem = ActorSystem("RayTracerActorSystem")

  val rayTracerActor = actorSystem.actorOf(Props[RayTracerActor])

  val fileChooser = new FileChooser {
    title = "Open scene definition file"
    extensionFilters ++= Seq(
      new ExtensionFilter("Test scene definition file (*.test)", "*.test"),
      new ExtensionFilter("All files", "*.*")
    )
  }

  private val borderStyle = "" +
    "-fx-background-color: white;" +
    "-fx-border-color: black;" +
    "-fx-border-width: 1;" +
    "-fx-border-radius: 5;" +
    "-fx-padding: 6;"

  val canvas = new Canvas(200, 200) {
    def clear(c: Color): Unit = {
      val gc = graphicsContext2D
      gc.fill = c
      gc.fillRect(0, 0, width.value, height.value)
    }
  }

  val canvasPane = new ScrollPane {
    content = canvas
  }

  var renderConfig: Config = _
  var sceneDef: org.mikesajak.raytracer.Scene = _


  class CanvasPixelOutput(pixelWriter: PixelWriter) extends PixelOutput {

    var list = List[(Int, Int, Color4)]()
    var count = 0

    override def setPixel(x: Int, y: Int, color: Color4) = {
      synchronized {
        list ::= (x, y, color)
        count += 1
        if (count > 1000)
          flush()
      }
//      println(s"setPixel($x, $y, $color/${color.argb}})")
    }

    def flush(): Unit = {
      val tmpList = list
      list = List()
      Platform.runLater {
        for ((x, y, color) <- tmpList)
        pixelWriter.setArgb(x, y, color.argb)
      }
    }
  }

  val canvasPixelSink = new CanvasPixelOutput(canvas.graphicsContext2D.pixelWriter)

  val summaryPane = new HBox(20)

  val startRenderingButton = new Button("Start ray tracing") {
    onAction = (ae: ActionEvent) => {
      canvas.width = renderConfig.size._1
      canvas.height = renderConfig.size._2

      text = "Processing..."
      disable = true
      canvas.clear(Color.Gray)

      guiUpdaterActor ! RenderRequest(renderConfig, sceneDef, canvasPixelSink)
//
//      val rayTracer = new RayTracer()
//      rayTracer.process(renderConfig, sceneDef, new CanvasPixelOutput(canvas.graphicsContext2D.pixelWriter))


//      text = "Start ray tracing"

    }

    disable = true
  }

  val renderingTimeText = new TextFlow(new Text("Rendering time: n/a"))

  class GuiUpdaterActor extends Actor {
    var stopwatch: Stopwatch = _
    def receive = {
      case r: RenderRequest =>
        stopwatch = Stopwatch.createStarted()
        rayTracerActor ! r
      case Done =>
        Platform.runLater {
                            canvasPixelSink.flush
                            startRenderingButton.text = "Start ray tracing"
                            startRenderingButton.disable = false
                            renderingTimeText.children = Seq(new Text(s"Rendering time: $stopwatch"))
                            stopwatch.stop()
                            stopwatch = null
                          }
    }
  }
  val guiUpdaterActor = actorSystem.actorOf(Props[GuiUpdaterActor])

  def closeHandler() = {
    Platform.exit()
    System.exit(0)
  }

  stage = new PrimaryStage {
    title = "Simple RayTracer example"
    width = 800
    height = 700

    onCloseRequest = (we: WindowEvent) => closeHandler()


    scene = new Scene {
      root = new BorderPane {
        top = createMenus()
        center = new VBox {
          spacing = 5
          padding = Insets(10, 10, 0, 10)

          children ++= List(
            summaryPane,
            startRenderingButton,
            canvasPane
          )
        }
        bottom = new HBox {
          spacing = 5
          padding = Insets(0, 10, 0, 10)

          children ++= List(
            renderingTimeText
          )
        }
      }
    }

    updateSummary(renderConfig, sceneDef)
    canvas.clear(Color.Gray)


  }

  def createMenus() = new MenuBar {
    menus = List(
      new Menu("File") {
        items = List(
          new MenuItem("Open...") {
            accelerator = KeyCombination.keyCombination("Ctrl +O")
            onAction = {
              e: ActionEvent =>
                val file = fileChooser.showOpenDialog(stage)
                if (file != null) {
                  try {
                    val (cfg, sc) = ConfigParser.parse(Source.fromFile(file))
                    renderConfig = cfg
                    sceneDef = sc

                    updateSummary(cfg, sc)
                    startRenderingButton.disable = false

//                    configSummary.text = s"size: ${cfg.size}, raytracing max depth: ${cfg.maxDepth}, output file: ${cfg.outFile}"
                    // TODO: show config and scene summary on gui
                  } catch {
                    case e: Exception =>
                      e.printStackTrace()

                      new Alert(AlertType.Error) {
                        initOwner(stage)
                        title = "Error"
                        headerText = "An error occurred during parsing of input file"
                        contentText = s"Error details: $e"
                      }.showAndWait()
                  }

                }
            }
          },
          new SeparatorMenuItem(),
          new MenuItem("Exit") {
            onAction = (ae: ActionEvent) => closeHandler()
          }
        )
      }
    )
  }

  def updateSummary(cfg: Config, sceneDef: org.mikesajak.raytracer.Scene) = {
    summaryPane.children = List(
      new TextFlow { children = createConfigSummary(cfg) },
      new TextFlow { children = createCameraSummary(if (sceneDef != null) sceneDef.camera else null) },
      new TextFlow { children = createSceneSummary(sceneDef) }
    )
  }

  def createConfigSummary(cfg: Config) = {
    val size = if (cfg == null) "n/a" else cfg.size.toString
    val maxDepth = if (cfg == null) "n/a" else cfg.maxDepth.toString
    val outFile = if (cfg == null) "n/a" else cfg.outFile

    Seq(
      new Text("Config summary: ") {font = new Font("Arial", 18)},
      new Text(s"\n   Size: $size"),
      new Text(s"\n   Maximum depth: $maxDepth"),
      new Text(s"\n   Output file: $outFile")
    )
  }

  def createCameraSummary(camera: Camera) = {
    val eye = if (camera == null) "n/a" else s"${camera.eye}"
    val ax = if (camera == null) "n/a" else s"${camera.ax}"
    val ay =  if (camera == null) "n/a" else s"${camera.ay}"
    val az =  if (camera == null) "n/a" else s"${camera.az}"
    Seq(
      new Text("Scene summary: ") {font = new Font("Arial", 18)},
      new Text(s"\n   Camera:\n" +
                 s"      eye=$eye\n" +
                 s"      ax=$ax\n" +
                 s"      ay=$ay\n" +
                 s"      az=$az")
    )
  }

  def createSceneSummary(sceneDef: org.mikesajak.raytracer.Scene) = {
    val numDirLights = if (sceneDef == null) "n/a" else sceneDef.lights.filter(_.isInstanceOf[DirLight]).size.toString
    val numPointLights = if (sceneDef == null) "n/a" else sceneDef.lights.filter(_.isInstanceOf[PointLight]).size.toString

    val numSpheres = if (sceneDef == null) "n/a" else sceneDef.models.filter(_.geometry.isInstanceOf[Sphere]).size.toString
    val numTriangles = if (sceneDef == null) "n/a" else sceneDef.models.filter(_.geometry.isInstanceOf[Triangle]).size.toString

    Seq(
      new Text("Scene summary: ") {font = new Font("Arial", 18)},
      new Text(s"\n   Lights:\n" +
                 s"      Directional: $numDirLights" +
                 s"      Positional: $numPointLights"),
      new Text(s"\n   Models:\n" +
                 s"      Spheres: $numSpheres" +
                 s"      Triangles: $numTriangles")
    )
  }

//  def setPixmap(pixmap: Pixmap): Unit = {
//    canvas.width = pixmap.width
//    canvas.height = pixmap.height
//
//    val gc = canvas.graphicsContext2D
//
//    for (y <- 0 until pixmap.width;
//         x <- 0 until pixmap.height) {
//      gc.pixelWriter.setArgb(x, y, pixmap(x, y).argb)
//    }
//
//  }

}
