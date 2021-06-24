import org.scalatest._

import cards._

class CardsTests extends FlatSpec {
  "Card" should "show proper text for Clubs Seven" in {
    assert(Card(Clubs, Seven).toString == "Seven ♣")
  }

  "Card" should "show proper text for Diamonds Ace" in {
    assert(Card(Diamonds, Ace).toString == "Ace ♦")
  }
}
