package deck

import cards._

class Deck(val cards: List[Card]) {
  def pull() = new Deck(cards.tail)
  def push(card: Card) = new Deck(card :: cards)
  def push(color: Color, rank: Rank): Deck = push(Card(color, rank))
  val isStandard: Boolean = cards.distinct.size == 52
  def duplicatesOfCard(card: Card) = cards.count(_ == card)
  def amountOfColor(color: Color) = cards.count(_.color == color)
  def amountOfNumerical(numerical: Rank) = cards.count(_.rank == (numerical match {
    case Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten => numerical
    case _ => throw new IllegalArgumentException("numerical should be number between 2 and 10")
  }))
  val amountWithNumerical = cards.count(_.rank match {
    case Ace | King | Queen | Jack => false
    case _ => true
  })
  def amountOfFace(face: Rank) = cards.count(_.rank == (face match {
    case Ace | King | Queen | Jack => face
    case _ => throw new IllegalArgumentException("face can be either ace, kink, queen or jack")
  }))
  val amountWithFace = cards.count(_.rank match {
    case Ace | King | Queen | Jack => true
    case _ => false
  })
  def ++(deck: Deck) = new Deck(deck.cards ::: cards)
}

object Deck {
  def apply() = new Deck(scala.util.Random.shuffle(for(c <- cards.Colors; r <- cards.Ranks) yield Card(c, r)))
}
