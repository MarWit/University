package figures

import numbers._

class Point(val x: Rational, val y: Rational)

trait Figure {
  def area: Double
  val description: String
  override def toString: String = s"$description ($area)"
}

class Rectangle(center: Point, width: Rational, height: Rational, rotation: Double = 0) extends Figure {
  override def area: Double = (width * height).toDouble
  override val description: String = "Rectangle"
}

object Rectangle {
  def newTopLeft(topLeft: Point, width: Rational, height: Rational, rotation: Double = 0) = {
    val center = new Point(topLeft.x + width / Rational(2), topLeft.y + height / Rational(2))
    new Rectangle(center, width, height, rotation)
  }
}

class Square(center: Point, width: Rational, rotation: Double = 0) extends Rectangle(center, width, width, rotation) {
  override val description: String = "Square"
}

object Square {
  def newTopLeft(topLeft: Point, width: Rational, rotation: Double = 0) = {
    val center = new Point(topLeft.x + width / Rational(2), topLeft.y + width / Rational(2))
    new Square(center, width, rotation)
  }
}

class Triangle(point: Point, right: Rational, bottom: Rational, angle: Double = math.Pi / 2, rotation: Double = 0) extends Figure {
  override def area: Double = (right * bottom).toDouble * math.sin(angle) / 2.0
  override val description: String = "Triangle"
}

object Triangle {
  def equilateral(point: Point, side: Rational, rotation: Double = 0) =
    new Triangle(point, side, side, math.Pi / 6, rotation)
}

object FigureUtils {
  def areaSum(figures: List[Figure]): Double = figures.map(_.area).sum
  def printAll(figures: List[Figure]): Unit = figures.foreach(println)
}
