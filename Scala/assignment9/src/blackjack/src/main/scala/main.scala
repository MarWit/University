package blackjack

import cards._
import deck._
import rules._

object Main extends App {
  val exampleCard = Card(Hearts, Queen)
  println(s"exampleCard: ${exampleCard.toString}")

  val dk = Deck()
  val dp = dk.pull()
  assert(dk.cards.drop(1) == dp.cards)

  val dkp = dk.push(exampleCard)
  assert(dkp.cards.headOption.exists(_ == exampleCard))
  assert(dk.isStandard ==
    (cards.Colors.flatMap(c => cards.Ranks.map(r => dk.duplicatesOfCard(Card(c, r))))
      .forall(cnt => cnt == 1) && dk.cards.size == 52))

  assert(cards.Colors.iterator.map(dk.amountOfColor _).forall(_ == 13))

  val blackjack = Blackjack(3)
  blackjack.first21
}

