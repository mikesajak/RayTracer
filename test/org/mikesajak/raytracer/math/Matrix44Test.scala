package org.mikesajak.raytracer.math

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mike on 02.10.15.
 */
class Matrix44Test extends FlatSpec with Matchers {

  "No-arg constructor" should "create identity matrix" in {
    val m = new Matrix44()
    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal(if (col == row) 1.0f else 0.0f)
    }
  }

  "all-values constructor" should "create matrix with all values specified one by one" in {
    val m = Matrix44( 1,  2,  3,  4,
                      5,  6,  7,  8,
                      9,  10, 11, 12,
                      13, 14, 15, 16)

    for (col <- 0 until 4;
         row <- 0 until 4;
         value = 1 + 4 * col + row) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal (value)
    }
  }

  "Row values constructor" should "create matrix from provided row vectors" in {
    val m = Matrix44(new Vector4( 1,  2,  3,  4),
                     new Vector4( 5,  6,  7,  8),
                     new Vector4( 9, 10, 11, 12),
                     new Vector4(13, 14, 15, 16))

    for (col <- 0 until 4;
         row <- 0 until 4;
         value = 1 + 4 * col + row) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal (value)
    }
  }

  it should "create copy of provided columns to be independent of their changes" in {
    val c1 = new Vector4( 1,  2,  3,  4)
    val c2 = new Vector4( 5,  6,  7,  8)
    val c3 = new Vector4( 9, 10, 11, 12)
    val c4 = new Vector4(13, 14, 15, 16)
    val m = Matrix44(c1, c2, c3, c4)

    c1 := (100, 100, 100, 100)
    c2 := (200, 200, 200, 200)
    c3 := (300, 300, 300, 300)
    c4 := (400, 400, 400, 400)

    for (col <- 0 until 4;
         row <- 0 until 4;
         value = 1 + 4 * col + row) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal (value)

      m(col, row) = 9999
    }

    c1 should equal (Vector4(100, 100, 100, 100))
    c2 should equal (Vector4(200, 200, 200, 200))
    c3 should equal (Vector4(300, 300, 300, 300))
    c4 should equal (Vector4(400, 400, 400, 400))
  }

  it should "allow access by column-row indexes" in {
    val m = Matrix44( 1,  2, 3,  4,
                      5,  6, 7,  8,
                      9, 10,11, 12,
                     13, 14,15, 16)

    for (col <- 0 until 4;
         row <- 0 until 4;
         value = 1 + 4 * col + row) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal (value)
    }

    for (col <- 0 until 4;
         row <- 0 until 4;
         value = col * 4 + row) {
      m(col, row) = 100 - value
    }

    for (col <- 0 until 4;
         row <- 0 until 4;
         value = 100 - (4 * col + row)) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal (value)
    }
  }

//  it should "allow access to column vector by index" in {
//    val m = Matrix44( 1,  2,  3,  4,
//                      5,  6,  7,  8,
//                      9, 10, 11, 12,
//                     13, 14, 15, 16)
//
//    for (col <- 0 until 4;
//         colVec = Vector4(col*4+1, col*4+2, col*4+3, col*4+4)) withClue(s"Column=$col:") {
//      m(col) should equal (colVec)
//    }
//  }

  "all-values set" should "set all individual matrix elements to provided values" in {
    val m = Matrix44( 1,  2,  3,  4,
                      5,  6,  7,  8,
                      9, 10, 11, 12,
                     13, 14, 15, 16)

    m.set( 10,  20,  30,  40,
           50,  60,  70,  80,
           90, 100, 110, 120,
          130, 140, 150, 160)

    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, R=$m") {
//      colVec = Vector4(10*(col*4+1), 10*(col*4+2), 10*(col*4+3), 10*(col*4+4))
      m(col, row) should equal (10*(col*4+row + 1))
    }
  }

  "fill" should "set all values to specified number" in {
    val m = Matrix44( 1,  2, 3,  4,
                      5,  6, 7,  8,
                      9, 10,11, 12,
                      13, 14,15, 16)

    m.fill(100)
    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal(100)
    }
  }

  "zero" should "set all values to 0" in {
    val m = Matrix44( 1,  2, 3,  4,
      5,  6, 7,  8,
      9, 10,11, 12,
      13, 14,15, 16)

    m.zero()
    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal(0)
    }
  }

  "ones" should "set all values to 1" in {
    val m = Matrix44( 1,  2, 3,  4,
                      5,  6, 7,  8,
                      9, 10,11, 12,
                      13, 14,15, 16)

    m.ones()
    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal(1)
    }
  }

  "identity" should "set values to identity matrix" in {
    val m = Matrix44( 1,  2, 3,  4,
                      5,  6, 7,  8,
                      9, 10,11, 12,
                      13, 14,15, 16)

    m.identity()

    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R=$m") {
      m(col, row) should equal(if (row == col) 1 else 0)
    }
  }


  "+ operator" should "produce new matrix as sum of provided matrices" in {
    val m1 = Matrix44( 1,  2,  3,  4,
                       5,  6,  7,  8,
                       9, 10, 11, 12,
                      13, 14, 15, 16)

    val m2 = Matrix44( 10,  20,  30,  40,
                       50,  60,  70,  80,
                       90, 100, 110, 120,
                      130, 140, 150, 160)

    val R = m1 + m2

    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R=$R") {
      R(col, row) should equal(m1(col, row) + m2(col, row))
    }
  }

  // todo

  "Multiplication by identity" should "not change matrix" in {
    val I = Matrix44.identity()
    val M = Matrix44()
    for (col <- 0 until 4;
         row <- 0 until 4) {
      M(col,row) = col * 4 + row
    }

    val R1 = M * I
    val R2 = I * M

    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R1=$R1") {
      R1(col,row) should equal (M(col,row))
    }

    for (col <- 0 until 4;
         row <- 0 until 4) withClue(s"Column=$col, row=$row, R2=$R2") {
      R2(col,row) should equal (M(col,row))
    }
  }
}
