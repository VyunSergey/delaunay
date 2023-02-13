import scala.collection.mutable

import Delaunay._

class Delaunay(val points: Array[Array[Double]]) {
  // Граф триангуляцию Делоне в виде пары точек (p, q) -> Set - множество точек вершин треугольника на ребре (p, q)
  val graph: mutable.Map[(Int, Int), Set[Int]] =
    mutable.HashMap.empty[(Int, Int), Set[Int]].withDefaultValue(Set.empty[Int])

  // Вычислить триангуляцию Делоне множества точек points
  def getTriangulation: Array[Array[Int]] = {
    if (graph.isEmpty) build()
    graphToTriangles(graph)
  }

  // Вычислить выпуклую оболочку множества точек points
  def getConvexHull: Array[Int] = {
    if (graph.isEmpty) build()
    graphToConvexHull(graph)
  }

  private val convexPoints: Array[Convex] = Array.fill(points.length)(Convex(0, 0))
  private val recursionStack: mutable.Queue[(Int, Int)] = mutable.Queue.empty[(Int, Int)]

  private def build(): Unit = {
    if (points.length >= 3) {
      points.sortInPlaceBy(_.head)

      // Инициализация первой пары точек
      // У точки 0 слева и справа на границе следующей точкой является точка 1
      convexPoints(0) = Convex(left = 1, right = 1)
      // У точки 1 слева и справа на границе следующей точкой является точка 0
      convexPoints(1) = Convex(left = 0, right = 0)
      // У пары точек (0, 1) делаем вершину в точке 2
      graph += (0, 1) -> Set(2)

      // Начиная с точки 2 строим триангуляцию Делоне
      for (i <- 2 until points.length) AddPointToTriangulation(i)
    }
  }

  // Добавление новой внешней точки i в триангуляции Делоне:
  // Берется предыдущая последняя точка i - 1 для которой уже есть триангуляция
  // В цикле происходит обход всех точек на границе триангуляции: по часовой и против часовой
  // и для них добавляются новые треугольники с вершинами в точке i
  private def AddPointToTriangulation(i: Int): Unit = {
    val pointI: Point = Point(points(i))

    var prevHullPt: Int = -1
    var hullPt: Int = i - 1
    var nextHullPt: Int = convexPoints(hullPt).right

    var pointHull: Point = Point(points(hullPt))
    var pointHullNext: Point = Point(points(nextHullPt))

    var vector: Vector = vec(pointHull, pointI)
    var nextVector: Vector = vec(pointHullNext, pointI)

    // Двигаясь вправо от точки, пока ребра видимые, выполняем рекурсивное перестроение
    while(crossProduct(vector, nextVector) > -EPS && prevHullPt != nextHullPt) {
      val (left, right) = orderPair(hullPt, nextHullPt)
      FixTriangulation(left, right, i)

      prevHullPt = hullPt
      hullPt = nextHullPt
      nextHullPt = convexPoints(hullPt).right

      pointHullNext = Point(points(nextHullPt))

      vector = nextVector
      nextVector = vec(pointHullNext, pointI)
    }
    val newRight = hullPt

    prevHullPt = -1
    hullPt = i - 1
    nextHullPt = convexPoints(hullPt).left

    pointHull = Point(points(hullPt))
    pointHullNext = Point(points(nextHullPt))

    vector = vec(pointHull, pointI)
    nextVector = vec(pointHullNext, pointI)

    // Аналогично для движения влево, пока ребра видимые
    while(crossProduct(vector, nextVector) < EPS && prevHullPt != nextHullPt) {
      val (left, right) = orderPair(hullPt, nextHullPt)
      FixTriangulation(left, right, i)

      prevHullPt = hullPt
      hullPt = nextHullPt
      nextHullPt = convexPoints(hullPt).left

      pointHullNext = Point(points(nextHullPt))

      vector = nextVector
      nextVector = vec(pointHullNext, pointI)
    }
    val newLeft = hullPt

    convexPoints(i) = Convex(left = newLeft, right = newRight)

    // Справа от newLeft теперь i
    convexPoints(newLeft).right = i
    // Слева от newRight теперь i
    convexPoints(newRight).left = i
  }

  // Рекурсивная проверка и исправление триангуляции таким образом,
  // чтобы для всех треугольников выполнялось условие Делоне.
  // Проверка идет по четырехугольникам:
  //   (left, right) - центральное ребро
  //           inner - внутрення вершина
  //           outer - внешняя вершина
  private def FixTriangulation(left: Int, right: Int, outer: Int): Unit = {
    // Инициализация стека рекурсии хранить достаточно left и right
    recursionStack += ((left, right))

    while(recursionStack.nonEmpty) {
      val (left, right) = recursionStack.dequeue
      // Берем минимум в множестве, потому что outer > индекса любой добавленной точки
      val inner = graph((left, right)).min

      if (CheckDelaunayCondition(left, right, inner, outer)) {
        // Если менять ничего в четырехугольнике не надо, добавляем недостающие ребра
        graph += (right, outer) -> (graph((right, outer)) + left)
        graph += ( left, outer) -> (graph(( left, outer)) + right)
        graph += ( left, right) -> (graph(( left, right)) + outer)
      } else {
        // Иначе перестраиваем триангуляцию в четырехугольнике
        graph += ( left, outer) -> (graph(( left, outer)) - right + inner)
        graph += (right, outer) -> (graph((right, outer)) -  left + inner)
        graph += orderPair( left, inner) -> (graph(orderPair( left, inner)) - right + outer)
        graph += orderPair(right, inner) -> (graph(orderPair(right, inner)) -  left + outer)
        graph -= ((left, right))

        // И добавляем два новых рекурсивных вызова
        recursionStack += orderPair(left, inner)
        recursionStack += orderPair(right, inner)
      }
    }
  }

  // Проверка выполнения условия Делоне для трех вершин треугольника и внешней точки:
  // Условие Делоне - внешняя точка должна находиться за пределами описанной вокруг треугольника окружности
  private def CheckDelaunayCondition(left: Int, right: Int, inner: Int, outer: Int): Boolean = {
    // Проверка на то, что подан четырехугольник,
    // для треугольника условие Делоне выполняется автоматически
    if (inner == outer) return true

    // Первая точка окружности
    val p = Point(points(left))
    // Вторая точка окружности
    val q = Point(points(right))
    // Третья точка окружности
    val r = Point(points(inner))
    // Внешняя точка
    val s = Point(points(outer))

    // Находим координаты центра окружности по трем точкам
    val Circle(center, div) = circle(p, q, r)

    // Проверка на то, три точки не лежат на одной прямой
    if (div == 0) return true

    // Находим радиус окружности по трем точкам
    val pqrR = Math.pow(div * p.x - center.x, 2) + Math.pow(div * p.y - center.y, 2)
    // Находим радиус внешней точки
    val sR =   Math.pow(div * s.x - center.x, 2) + Math.pow(div * s.y - center.y, 2)

    // Если радиус внешней точки больше радиуса окружности,
    // то точка лежит за пределами окружности и условие Делоне выполняется
    sR - pqrR > -EPS
  }

  private def orderPair(left: Int, right: Int): (Int, Int) = {
    (Math.min(left, right), Math.max(left, right))
  }
}

object Delaunay {
  private val EPS: Double = 1e-9

  def apply(points: Array[Array[Double]] = Array.empty[Array[Double]]) = new Delaunay(points)

  final case class Point(x: Double, y: Double)

  object Point {
    def apply(arr: Array[Double]): Point = Point(arr(0), arr(1))
  }

  final case class Convex(var left: Int, var right: Int)

  final case class Vector(x: Double, y: Double)

  final case class Circle(center: Point, div: Double)

  // Функция построения триангуляции Делоне множества точек
  def graphToTriangles(graph: mutable.Map[(Int, Int), Set[Int]]): Array[Array[Int]] = {
    graph.flatMap { case ((i, j), set) =>
      set.map(k => List(i, j, k).sorted)
    }.collect {
      case i :: j :: k :: Nil => (i, j, k)
    }.toSet
      .toArray
      .sorted.map {
      case (i, j, k) => Array(i, j, k)
    }
  }

  // Функция построения выпуклой оболочки множества точек
  def graphToConvexHull(graph: mutable.Map[(Int, Int), Set[Int]]): Array[Int] = {
    graph.foldLeft(Set.empty[Int]) { case (res, ((i, j), set)) =>
      if (set.size < 2) res + i + j else res
    }.toArray.sorted
  }

  // Функция вычисления векторного произведения векторов
  def crossProduct(v1: Vector, v2: Vector): Double = {
    v1.x * v2.y - v1.y * v2.x
  }

  // Функция построения вектора по двум точкам
  def vec(p: Point, q: Point): Vector = {
    Vector(p.x - q.x, p.y - q.y)
  }

  // Функция построения описанной окружности вокруг треугольника по трем вершинам
  def circle(p: Point, q: Point, r: Point): Circle = {
    val pR = p.x * p.x + p.y * p.y
    val qR = q.x * q.x + q.y * q.y
    val rR = r.x * r.x + r.y * r.y

    // Нормирующий множитель, если он равен нулю, значит точки лежат на одной прямой
    val div = 2 * (p.x * (q.y - r.y) + q.x * (r.y - p.y) + r.x * (p.y - q.y))
    // Координата X0 центра окружности
    val x = -(p.y * (qR - rR) + q.y * (rR - pR) + r.y * (pR - qR))
    // Координата Y0 центра окружности
    val y =   p.x * (qR - rR) + q.x * (rR - pR) + r.x * (pR - qR)

    // Уравнение окружности имеет вид:
    // (X - X0 / div)^2 + (Y - Y0 / div)^2 = R^2, где
    // R^2 = (p.x - X0 / div)^2 + (p.y - Y0 / div)^2

    Circle(Point(x, y), div)
  }
}
