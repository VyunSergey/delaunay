import scala.collection.mutable

import Delaunay._

class Delaunay(val points: Array[Array[Double]]) {
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

  private val convexPoints: Array[Array[Int]] = Array.fill(points.length)(Array.fill(2)(0))
  private val recursionStack: mutable.Queue[(Int, Int)] = mutable.Queue.empty[(Int, Int)]

  private def build(): Unit = {
    if (points.length >= 3) {
      points.sortInPlaceBy(_.head)

      // Инициализация первого треугольника
      convexPoints(0) = Array(1, 1)
      convexPoints(1) = Array(0, 0)
      graph += (0, 1) -> (graph((0, 1)) + 2)

      for (i <- 2 until points.length) AddPointToTriangulation(i)
    }
  }

  // Добавление новой внешней точки i в триангуляции Делоне:
  // Берется предыдущая последняя точка i - 1 для которой уже есть триангуляция
  // В цикле происходит обход всех точек на границе триангуляции: по часовой и против часовой
  // и для них добавляются новые треугольники с вершинами в точке i
  private def AddPointToTriangulation(i: Int): Unit = {
    var prevHullPt: Int = -1
    var hullPt: Int = i - 1
    var nextHullPt: Int = convexPoints(hullPt)(1)
    var vector: Array[Double] = vec(points(hullPt), points(i))
    var nextVector: Array[Double] = vec(points(nextHullPt), points(i))

    // Двигаясь вправо от точки, пока ребра видимые, выполняем рекурсивное перестроение
    while(crossProduct(vector, nextVector) > -EPS && prevHullPt != nextHullPt) {
      val (left, right) = orderPair(hullPt, nextHullPt)
      FixTriangulation(left, right, i)

      prevHullPt = hullPt
      hullPt = nextHullPt
      nextHullPt = convexPoints(hullPt)(1)
      vector = nextVector
      nextVector = vec(points(nextHullPt), points(i))
    }
    val newRight = hullPt

    prevHullPt = -1
    hullPt = i - 1
    nextHullPt = convexPoints(hullPt)(0)
    vector = vec(points(hullPt), points(i))
    nextVector = vec(points(nextHullPt), points(i))

    // Аналогично для движения влево, пока ребра видимые
    while(crossProduct(vector, nextVector) < EPS && prevHullPt != nextHullPt) {
      val (left, right) = orderPair(hullPt, nextHullPt)
      FixTriangulation(left, right, i)

      prevHullPt = hullPt
      hullPt = nextHullPt
      nextHullPt = convexPoints(hullPt)(0)
      vector = nextVector
      nextVector = vec(points(nextHullPt), points(i))
    }
    val newLeft = hullPt

    convexPoints(i) = Array(newLeft, newRight)

    // Справа от newLeft теперь i
    convexPoints(newLeft)(1) = i
    // Слева от newRight теперь i
    convexPoints(newRight)(0) = i
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
        graph += (right, outer) -> (getByPair((right, outer)) + left)
        graph += ( left, outer) -> (getByPair(( left, outer)) + right)
        graph += ( left, right) -> (getByPair(( left, right)) + outer)
      } else {
        // Иначе перестраиваем триангуляцию в четырехугольнике
        graph += ( left, outer) -> (getByPair(( left, outer)) - right + inner)
        graph += (right, outer) -> (getByPair((right, outer)) -  left + inner)
        graph += orderPair( left, inner) -> (getByPair(orderPair( left, inner)) - right + outer)
        graph += orderPair(right, inner) -> (getByPair(orderPair(right, inner)) -  left + outer)
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
    val p = points(left)
    // Вторая точка окружности
    val q = points(right)
    // Третья точка окружности
    val r = points(inner)
    // Внешняя точка
    val s = points(outer)

    // Находим координаты центра окружности по трем точкам
    val (div, x0, y0) = circle(p, q, r)

    // Проверка на то, три точки не лежат на одной прямой
    if (div == 0) return true

    // Находим радиус окружности по трем точкам
    val pqrR = (div * p(0) - x0) * (div * p(0) - x0) + (div * p(1) - y0) * (div * p(1) - y0)
    // Находим радиус внешней точки
    val sR = (div * s(0) - x0) * (div * s(0) - x0) + (div * s(1) - y0) * (div * s(1) - y0)

    // Если радиус внешней точки больше радиуса окружности,
    // то точка лежит за пределами окружности и условие Делоне выполняется
    sR - pqrR > -EPS
  }

  private def getByPair(pair: (Int, Int)): Set[Int] = {
    graph(pair)
  }

  private def orderPair(left: Int, right: Int): (Int, Int) = {
    (Math.min(left, right), Math.max(left, right))
  }
}

object Delaunay {
  private val EPS: Double = 1e-9

  def apply(points: Array[Array[Double]] = Array.empty[Array[Double]]) = new Delaunay(points)

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
  def crossProduct(v1: Array[Double], v2: Array[Double]): Double = {
    v1(0) * v2(1) - v1(1) * v2(0)
  }

  // Функция построения вектора по двум точкам
  def vec(p: Array[Double], q: Array[Double]): Array[Double] = {
    Array(p(0) - q(0), p(1) - q(1))
  }

  // Функция построения описанной окружности вокруг треугольника по трем вершинам
  def circle(p: Array[Double], q: Array[Double], r: Array[Double]): (Double, Double, Double) = {
    val pR = p(0) * p(0) + p(1) * p(1)
    val qR = q(0) * q(0) + q(1) * q(1)
    val rR = r(0) * r(0) + r(1) * r(1)

    // Нормирующий множитель,
    // если он равен нулю, значит точки лежат на одной прямой
    val div = 2 * (p(0) * (q(1) - r(1)) + q(0) * (r(1) - p(1)) + r(0) * (p(1) - q(1)))
    // Координата X0 центра окружности
    val x0 = -(p(1) * (qR - rR) + q(1) * (rR - pR) + r(1) * (pR - qR))
    // Координата Y0 центра окружности
    val y0 =   p(0) * (qR - rR) + q(0) * (rR - pR) + r(0) * (pR - qR)

    // Уравнение окружности имеет вид:
    // (X - X0 / div)^2 + (Y - Y0 / div)^2 = R^2  , где
    // R^2 = (p(0) - X0 / div)^2 + (p(1) - Y0 / div)^2

    (div, x0, y0)
  }
}
