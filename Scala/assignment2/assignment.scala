import numbers._
import figures._

object Assignment extends App {
  val rat1 = new Rational(44, 22)
  val rat2 = new Rational(12, 36)

  println(rat1 + rat2 + Rational.ZERO + Rational.ONE + Rational(7))
  assert(rat1.toString == "2")
  assert(rat2.toString == "1/3")
  assert((rat1 + rat2).toString == "2 1/3")

  val point = new Point(rat1, rat2)
  val square = new Square(point, Rational(5))
  val rectangle = new Rectangle(point, Rational(5), Rational(2))
  val eq_rectangle = new Rectangle(point, Rational(5), Rational(5))
  val triangle = new Triangle(point, Rational(4), Rational(3))

  println(square.description, square.area)
  println(rectangle.description, rectangle.area)
  println(eq_rectangle.description, eq_rectangle.area)
  println(triangle.description, triangle.area)

  assert(square.area == 5*5)
  assert(rectangle.area == 5*2)
  assert(eq_rectangle.area == square.area)
  assert(triangle.area == 0.5 * 4 * 3)

  val figures = List(square, rectangle, eq_rectangle, triangle)
  println("Area sum", FigureUtils.areaSum(figures))
  assert(FigureUtils.areaSum(figures) == List(square, rectangle, eq_rectangle, triangle).map(_.area).sum)
  FigureUtils.printAll(figures)
}
