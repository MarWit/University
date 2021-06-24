import cards._
import deck._
import games._

object Assignment extends App {
  val exampleCard = Card(Hearts, Queen)
  println(s"exampleCard: $exampleCard")

  val dk = Deck()
  val dp = dk.pull()
  assert(dk.cards.tail == dp.cards)

  val dkp = dk.push(exampleCard)
  assert(dkp.cards.head == exampleCard)
  assert(dk.isStandard ==
    (cards.Colors.flatMap(c => cards.Ranks.map(r => dk.duplicatesOfCard(Card(c, r))))
                .forall(cnt => cnt == 1) && dk.cards.size == 52))

  assert(cards.Colors.iterator.map(dk.amountOfColor _).forall(_ == 13))

  val blackjack = Blackjack(3)
  blackjack.first21
}

