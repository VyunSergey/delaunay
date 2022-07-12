import Delaunay._

import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class CircleSpec extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {
  property("circle should correctly work in") {
    val input = Table(
      ("px", "py", "qx", "qy", "rx", "ry", "div", "x0", "y0"),
      (-1.0, 0.0,  0.0, 1.0,   1.0, 0.0,   -4.0,  0.0,   0.0),
      ( 0.0, 1.0,  1.0, 2.0,   2.0, 1.0,   -4.0, -4.0,  -4.0),
      (-1.0, 1.0, -3.0,-1.0,  -1.0,-3.0,   16.0, -16.0, -16.0),
      (-1.0, 0.0,  0.0, 1.0,   1.0, 2.0,    0.0, -4.0,   4.0)
    )

    forAll (input) { (px: Double, py: Double,
                      qx: Double, qy: Double,
                      rx: Double, ry: Double,
                      divExp: Double, x0Exp: Double, y0Exp: Double) =>
      val (div, x0, y0) = circle(Array(px, py), Array(qx, qy), Array(rx, ry))
      // println(s"div=$div x0=$x0, y0=$y0")
      val (div1, x01, y01) = circle(Array(px, py), Array(rx, ry), Array(qx, qy))
      val (div2, x02, y02) = circle(Array(qx, qy), Array(rx, ry), Array(px, py))
      val (div3, x03, y03) = circle(Array(rx, ry), Array(px, py), Array(qx, qy))

      div should be (divExp)
      x0 should be (x0Exp)
      y0 should be (y0Exp)

      (x0 * div, y0 * div) should be (x01 * div1, y01 * div1)
      (x0 * div, y0 * div) should be (x02 * div2, y02 * div2)
      (x0 * div, y0 * div) should be (x03 * div3, y03 * div3)
    }
  }
}
