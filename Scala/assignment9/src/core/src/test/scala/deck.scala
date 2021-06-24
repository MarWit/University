import org.scalatest._

import cards._
import deck._

class DeckTests extends FlatSpec {
  "Pull" should "remove one card" in {
    val deck = Deck()
    val nextDeck = deck.pull()
    assert(deck.cards.length == (nextDeck.cards.length + 1))
  }

  "Push" should "add new card on top of deck" in {
    val deck = Deck()
    val newCard = Card(Hearts, Ace)
    val newDeck = deck.push(newCard)

    assert((deck.cards.length + 1) == newDeck.cards.length)
  }

  "Deck" should "not be standard after adding additional card" in {
    val deck = Deck()
    val newCard = Card(Hearts, Ace)
    val newDeck = deck.push(newCard)

    assert(!newDeck.isStandard)
  }

  it should "have two instances of the same card" in {
    val deck = Deck()
    val newCard = Card(Hearts, Ace)
    val newDeck = deck.push(newCard)

    assert(newDeck.duplicatesOfCard(newCard) == 2)
  }

  it should "have twice as many cards when combining two decks" in {
    val deck1 = Deck()
    val deck2 = Deck()
    val newDeck = deck1 ++ deck2

    for (rank <- cards.Ranks; color <- cards.Colors) {
      assert(newDeck.duplicatesOfCard(Card(color, rank)) == 2)
    }
  }

  "Standard deck" should "have 13 cards of each color" in {
    val deck = Deck()

    for (color <- cards.Colors) {
      assert(deck.amountOfColor(color) == 13)
    }
  }

  it should "have 4 cards of each rank" in {
    val deck = Deck()

    for (rank <- cards.Ranks) {
      assert((rank match {
        case Ace | King | Queen | Jack => deck.amountOfFace(rank)
        case _ => deck.amountOfNumerical(rank)
      }) == 4)
    }
  }

}
