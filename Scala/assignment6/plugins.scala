package plugins

trait Pluginable {
  def plugin(s: String): String = s
}

trait Reverting extends Pluginable {
  override def plugin(s: String): String = super.plugin(s).reverse
}

trait LowerCasing extends Pluginable {
  override def plugin(s: String): String = super.plugin(s).toLowerCase
}

trait SingleSpacing extends Pluginable {
  override def plugin(s: String): String = super.plugin(s).replaceAll("  ", " ")
}

trait NoSpacing extends Pluginable {
  override def plugin(s: String): String = super.plugin(s).replaceAll(" ", "")
}

trait DuplicateRemoval extends Pluginable {
  override def plugin(s: String): String = {
    val newS: String = super.plugin(s)
    val counted = newS groupBy(identity) mapValues(_.size)
    newS.filter(counted(_) <= 1)
  }
}

trait Rotating extends Pluginable {
  override def plugin(s: String): String = {
    val newS: String = super.plugin(s)
    newS.last + newS.reverse.tail.reverse
  }
}

trait Doubling extends Pluginable {
  override def plugin(s: String): String = super.plugin(s).zipWithIndex.flatMap {
    case (e, i) => if((i + 1) % 2 == 0) List(e, e) else List(e)
  }.mkString
}

trait Shortening extends Pluginable {
  override def plugin(s: String): String = super.plugin(s).zipWithIndex.filter{ case (e, i) => i % 2 == 0}.map{ case (e, i) => e }.mkString
}

trait Chained extends Pluginable {
  val base: Pluginable
  val next: Pluginable

  override def plugin(s: String): String = {
    next.plugin(base.plugin(s))
  }
}
