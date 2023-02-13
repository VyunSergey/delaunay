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
      val Circle(Point(x0, y0), div)    = circle(Point(px, py), Point(qx, qy), Point(rx, ry))
      // println(s"div=$div x0=$x0, y0=$y0")
      val Circle(Point(x01, y01), div1) = circle(Point(px, py), Point(rx, ry), Point(qx, qy))
      val Circle(Point(x02, y02), div2) = circle(Point(qx, qy), Point(rx, ry), Point(px, py))
      val Circle(Point(x03, y03), div3) = circle(Point(rx, ry), Point(px, py), Point(qx, qy))

      div should be (divExp)
      x0 should be (x0Exp)
      y0 should be (y0Exp)

      (x0 * div, y0 * div) should be (x01 * div1, y01 * div1)
      (x0 * div, y0 * div) should be (x02 * div2, y02 * div2)
      (x0 * div, y0 * div) should be (x03 * div3, y03 * div3)
    }
  }
}
