package deck

import cards._

class Deck(val cards: List[Card]) {
  def pull() = new Deck(cards drop (1))
  def push(card: Card) = new Deck(card :: cards)
  def push(color: Color, rank: Rank): Deck = push(Card(color, rank))
  val isStandard: Boolean = cards.distinct.size == cards.size && cards.size == 52
  def duplicatesOfCard(card: Card) = cards.count(_ == card)
  def amountOfColor(color: Color) = cards.count(_.color == color)
  def amountOfNumerical(numerical: Rank) = {
    require(numerical match {
      case Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten => true
      case _ => false
    }, "numerical should be beetween 2 and 10")

    cards.count(_.rank == numerical)
  }
  val amountWithNumerical = cards.count(_.rank match {
    case Ace | King | Queen | Jack => false
    case _ => true
  })
  def amountOfFace(face: Rank) = {
    require(face match {
      case Ace | King | Queen | Jack => true
      case _ => false
    }, "face can be either ace, king, queen or jack")

    cards.count(_.rank == face)
  }
  val amountWithFace = cards.count(_.rank match {
    case Ace | King | Queen | Jack => true
    case _ => false
  })
  def ++(deck: Deck) = new Deck(deck.cards ::: cards)
}

object Deck {
  def apply() = new Deck(scala.util.Random.shuffle(for (c <- cards.Colors; r <- cards.Ranks) yield Card(c, r)))
}
