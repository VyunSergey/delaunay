import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class DelaunaySpec extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {
  property("Delaunay should correctly work in") {
    val input = Table(
      ("points", "triangles", "convex"),
      (Array(Array(0.0, 0.0), Array(0.0, 3.0), Array(1.0, 2.0)),
      Array((0, 1, 2)),
      Array(0, 1, 2)),
      (Array(Array(0.0, 0.0), Array(0.0, 3.0), Array(1.0, 2.0), Array(3.0, 1.0)),
        Array((0, 1, 2), (0, 2, 3), (1, 2, 3)),
        Array(0, 1, 3)),
      (Array(Array(0.0, 0.0), Array(0.0, 3.0), Array(1.0, 2.0), Array(3.0, 1.0), Array(4.0, 4.0)),
        Array((0, 1, 2), (0, 2, 3), (1, 2, 4), (2, 3, 4)),
        Array(0, 1, 3, 4)),
      (Array(Array(0.0, 0.0), Array(0.0, 3.0), Array(1.0, 2.0), Array(3.0, 1.0), Array(4.0, 4.0),
        Array(7.0, 3.0), Array(6.0, 6.0), Array(2.0, 6.0), Array(9.0, 5.0), Array(10.0, 10.0)),
        Array((0,1,2), (0,2,4), (1,2,3), (2,3,5), (2,4,5), (3,5,6), (3,6,9), (4,5,7), (5,6,7), (6,7,8), (6,8,9)),
        Array(0, 1, 3, 4, 7, 8, 9)),
      (Array(Array(0.0, 0.0), Array(3.0, 1.0), Array(2.0, 3.0), Array(6.0, 2.0),
        Array(9.0, 3.0), Array(4.0, 6.0), Array(7.0, 4.0)),
        Array((0,1,2), (0,1,3), (0,2,4), (0,4,6), (1,2,4), (1,3,4), (3,4,5), (3,5,6), (4,5,6)),
        Array(0, 3, 6)),
      (Array(Array(0.0, 0.0), Array(1.0, 1.0), Array(2.0, 2.0), Array(4.0, 5.0)),
        Array((0,1,2), (0,2,3)),
        Array(0, 1, 2, 3))
    )

    forAll (input) { (points: Array[Array[Double]],
                      trianglesExp: Array[(Int, Int, Int)],
                      convexExp: Array[Int]) =>
      val delaunay = Delaunay(points)
      val triangles = delaunay.getTriangulation()
      val convex = delaunay.getConvexHull()

      triangles should contain theSameElementsAs trianglesExp
      convex should contain theSameElementsAs convexExp
    }
  }
}
