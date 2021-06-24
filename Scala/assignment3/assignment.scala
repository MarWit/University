object Utils {
  def isSorted[A](as: Array[A], ordering: (A, A) => Boolean) =
    as.sliding(2)
      .forall{case Array(a, b) => ordering(a, b)}

  def isAscSorted[A](as: Array[A])(implicit ord: Ordering[A]) =
    isSorted[A](as, ord.lt(_,  _))

  def isDescSorted[A](as: Array[A])(implicit ord: Ordering[A]) =
    isSorted[A](as, ord.gt(_, _))

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def aux(xs: List[A], acc: B): B = xs match {
      case hd :: tl => aux(tl, f(acc, hd))
      case Nil => acc
    }
    aux(l, z)
  }

  def sum(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def length[A](l: List[A]) =
    foldLeft(l, 0) { (a, _) => a + 1 }

  def compose[A, B, C](f: A => B, g: B => C) =
    (a: A) => g(f(a))

  def repeated(f: Int => Int, n: Int) =
    (1 to n).foldLeft(n) {(a, _) => f(a)}
    //(x) => (1 to n).foldLeft(x) {(a, _) => f(a)}

  def curry[A, B, C](f: (A, B) => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C) =
    (a: A, b: B) => f(a)(b)

  def unSafe[T](ex: Exception)(block: => T) =
    try {
      block
    } catch {
      case _: Throwable => throw ex
    }
}

class MyException(s: String) extends Exception(s) {}

object Assigment extends App {
  val ar = Array(1, 2, 3, 4, 5)
  assert(Utils.isAscSorted(ar) == true)
  assert(Utils.isDescSorted(ar) == false)
  assert(Utils.isDescSorted(ar.reverse) == true)
  assert(Utils.foldLeft(ar.toList, 1)(_ * _) == 120)
  assert(Utils.sum(ar.toList) == 15)
  assert(Utils.length(ar.toList) == ar.length)

  val succ = (x: Int) => x + 1
  val plus_2 = Utils.compose(succ, succ)

  assert(plus_2(2) == 4)
  assert(Utils.repeated(succ, 10) == 20)

  val add_curry = Utils.curry((x: Int, y: Int) => x + y)
  assert(add_curry(13)(37) == (13 + 37))
  assert(Utils.uncurry(add_curry)(13, 37) == add_curry(13)(37))

  // val division = Utils.unSafe(new MyException("This will crash")) {
  //   21 / 0
  // }
}
