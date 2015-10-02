package org.mikesajak.raytracer.math

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mike on 02.10.15.
 */
class Matrix44Test extends FlatSpec with Matchers {

  "Matrix44" should "be created as identity" in {
    val m = new Matrix44()
    for (col <- 0 until 4;
         row <- 0 until 4) {
      withClue(s"Column: $col, row: $row") {
        m(col)(row) should equal(if (col == row) 1.0f else 0.0f)
      }
    }
  }

  // todo

  "Multiplication by identity" should "not change matrix" in {
    val I = Matrix44.identity()
    val M = Matrix44()
    for (col <- 0 until 4;
         row <- 0 until 4) {
      M(col)(row) = col * 4 + row
    }

    val R1 = M * I
    val R2 = I * M

    for (col <- 0 until 4;
         row <- 0 until 4) {
      withClue(s"R1, column: $col, row: $row:") {
        R1(col)(row) should equal(M(col)(row))
      }
      withClue(s"R2, column: $col, row: $row:") {
        R2(col)(row) should equal (M(col)(row))
      }
    }
  }
}
