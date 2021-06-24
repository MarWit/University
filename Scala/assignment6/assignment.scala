import plugins._

object Actions {
  val actionA: Pluginable = new Pluginable with SingleSpacing with Doubling with Shortening
  val actionB: Pluginable = new Pluginable with NoSpacing with Shortening with Doubling
  val actionC: Pluginable = new Pluginable with LowerCasing with Doubling
  val actionD: Pluginable = new Pluginable with DuplicateRemoval with Rotating
  val actionE: Pluginable = new Pluginable with NoSpacing with Shortening with Doubling with Reverting
  val actionF: Pluginable = new Chained with Rotating {
    val base = new Pluginable with Rotating
    val next = new Chained with Rotating {
      val base = new Pluginable with Rotating
      val next = new Pluginable with Rotating
    }
  }
  val actionG: Pluginable = new Pluginable with Chained {
    val base = actionA
    val next = actionB
  }
}

object Assignment extends App {
  val baseText = "Ala ma kota a kot ma ale"

  println(Actions.actionA.plugin(baseText))
  println(Actions.actionB.plugin(baseText))
  println(Actions.actionC.plugin(baseText))
  println(Actions.actionD.plugin(baseText))
  println(Actions.actionE.plugin(baseText))
  println(Actions.actionF.plugin("abcdefgh"))
  println(Actions.actionG.plugin("baseText"))
}
