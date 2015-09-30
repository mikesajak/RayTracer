package org.mikesajak.raytracer

/**
 * Created by mike on 30.09.15.
 */
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