package org.mikesajak.raytracer

/**
 * Created by mike on 30.09.15.
 */
case class Color4(var r: Float, var g: Float, var b: Float, var a: Float = 1.0f) {
  override def toString =s"Color4(r=$r, g=$g, b=$b, a=$a)"

  def this(c: Color4) = this(c.r, c.g, c.b, c.a)

  def apply(idx: Int) = idx match {
    case 0 => r
    case 1 => g
    case 2 => b
    case 3 => a
  }

  def argb = pack(2,1,0,3)
  def pack(i1: Int, i2: Int, i3: Int, i4: Int) =
    (apply(i1) * 255).toInt | ((apply(i2) * 255).toInt << 8) | ((apply(i3) * 255).toInt << 16) | ((apply(i4) * 255).toInt << 24)

  def +=(c: Color4) = {
    r += c.r
    g += c.g
    b += c.b
//    a += c.a
    clamp()
  }
  def +(c: Color4) = new Color4(this) += c

  def -=(c: Color4) = {
    r -= c.r
    g -= c.g
    b -= c.b
//    a -= c.a
    clamp()
  }
  def -(c: Color4) = new Color4(this) -= c

  def *=(s: Float) = {
    r *= s
    g *= s
    b *= s
//    a *= s
    clamp()
  }
  def *(s: Float) = new Color4(this) *= s

  def *=(c: Color4) = {
    r *= c.r
    g *= c.g
    b *= c.b
    clamp()
  }
  def *(c: Color4) = new Color4(this) *= c

  def addScaled(c: Color4, s: Float) = {
    r += c.r * s
    g += c.g * s
    b += c.b * s
    clamp()
  }

  def addScaled(c: Color4, s: Color4) = {
    r += c.r * s.r
    g += c.g * s.g
    b += c.b * s.b
    a += c.a * s.a
    clamp()
  }

  def clamp() = {
    if (r < 0) r = 0
    else if (r > 1) r = 1

    if (g < 0) g = 0
    else if (g > 1) g = 1

    if (b < 0) b = 0
    else if (b > 1) b = 1

    if (a < 0) a = 0
    else if (a > 1) a = 1

    this
  }
}