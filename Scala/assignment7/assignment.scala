import money._

object Assignment extends App {
  val _10EUR = 10(`€`)
  val _10USD = 10(USD)

  val _20EUR = _10EUR + _10EUR
  val _10EURInUSD = _10EUR as $
  val comparision = _10EUR > _10USD

  println(_10EUR)
  println(_10USD)
  println(_20EUR)
  println(_10EUR + _10USD)
  println(5(zł) + 3(PLN) + 20.5($))
  println(_10EURInUSD)
  println("10EUR > 10USD", comparision)
}
