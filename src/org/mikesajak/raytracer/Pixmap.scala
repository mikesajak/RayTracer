package org.mikesajak.raytracer

/**
 * Created by mike on 26.09.15.
 */
trait PixelOutput {
  def setPixel(x: Int, y: Int, color: Color4)
}

class Pixmap(val width: Int, val height: Int) extends PixelOutput {
  private val data = Array.tabulate(height, width)((x,y) => Color4(0,0,0,1))

  def apply(x: Int, y: Int) = data(y)(x)
  def update(x: Int, y: Int, c: Color4) = data(y)(x) = c

  override def setPixel(x: Int, y: Int, c: Color4) = update(x, y, c)
}