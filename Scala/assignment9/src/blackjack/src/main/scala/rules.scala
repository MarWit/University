package rules

import deck._
import cards._

class Blackjack(deck: Deck) {
  lazy val all21: List[List[Card]] = (1 to deck.cards.length).iterator
    .flatMap(deck.cards.combinations(_))
    .filter(_.map(pointsForCard).sum == 21)
    .toList
  def first21(): Unit = println((1 to deck.cards.length).iterator
    .flatMap(deck.cards.combinations(_))
    .filter(_.map(pointsForCard).sum == 21)
    .next
    .mkString(" | "))

  private def pointsForCard(card: Card) = card match {
    case Card(_, Ace) => 1
    case Card(_, Two) => 2
    case Card(_, Three) => 3
    case Card(_, Four) => 4
    case Card(_, Five) => 5
    case Card(_, Six) => 6
    case Card(_, Seven) => 7
    case Card(_, Eight) => 8
    case Card(_, Nine) => 9
    case Card(_, _) => 10
  }

}

object Blackjack {
  def apply(n: Int) =
    new Blackjack((1 to n).map(i => Deck()).reduce(_ ++ _))
}
