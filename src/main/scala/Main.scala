import scala.io.Source
import scala.util.{Try, Using}

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Arguments: ${args.mkString("[", ", ", "]")}")

    val inputPoints: Option[Array[Array[Double]]] =
      Try(args.head.split(";").map(_.split(",").take(2).map(_.toDouble))).toOption
    println(s"Input Points: ${inputPoints.map(_.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]"))}")

    val filePoints: Option[Array[Array[Double]]] =
      Using(Source.fromFile(args.head)) { source =>
        source.getLines.toArray.tail.map(_.split(";").take(2).map(_.toDouble))
      }.toOption
    println(s"File Points: ${filePoints.map(_.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]"))}")

    val defaultPoints: Array[Array[Double]] = Array(
      Array(0.0, 0.0), Array(0.0, 3.0), Array(1.0, 2.0), Array(3.0, 1.0), Array(4.0, 4.0),
      Array(7.0, 3.0), Array(6.0, 6.0), Array(2.0, 6.0), Array(9.0, 5.0), Array(10.0, 10.0)
    )
    println(s"Default Points: ${defaultPoints.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")}")

    val delaunay = new Delaunay(inputPoints.orElse(filePoints).getOrElse(defaultPoints))

    val triangles = delaunay.getTriangulation(verbose = true)
    val convex = delaunay.getConvexHull(verbose = true)

    println(s"Points: ${delaunay.points.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")}")
    println(s"Convex Hull: ${convex.mkString("[", ", ", "]")}")
    println(s"Triangulation: ${triangles.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")}")
    triangles.foreach { case Array(i, j, k) =>
      println(
        delaunay.points(i).mkString("[", ", ", "]"),
        delaunay.points(j).mkString("[", ", ", "]"),
        delaunay.points(k).mkString("[", ", ", "]")
      )
    }
  }
}
