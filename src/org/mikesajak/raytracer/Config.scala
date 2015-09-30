package org.mikesajak.raytracer

/**
 * Created by mike on 26.09.15.
 */
case class Config(size: (Int, Int), maxDepth: Int, outFile: String,
                  parallelComputing: Boolean = true, epsilon: Float = 0.0001f)

object Config {
  class Builder {
    var size: (Int, Int) = _
    var maxDepth = 5
    var outFile: String = _

    def build() = new Config(size, maxDepth, outFile)
  }

  def builder() = new Builder()
}
