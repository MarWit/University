package cards

sealed abstract class Color
case object Clubs extends Color
case object Diamonds extends Color
case object Hearts extends Color
case object Spades extends Color

sealed abstract class Rank
case object Two extends Rank
case object Three extends Rank
case object Four extends Rank
case object Five extends Rank
case object Six extends Rank
case object Seven extends Rank
case object Eight extends Rank
case object Nine extends Rank
case object Ten extends Rank
case object Jack extends Rank
case object Queen extends Rank
case object King extends Rank
case object Ace extends Rank

case class Card(color: Color, rank: Rank) {
  override def toString() =
    rank.getClass.getSimpleName.split("\\$").last ++ " " ++ (color match {
      case Clubs => "♣"
      case Diamonds => "♦"
      case Hearts => "♥"
      case Spades => "♠"
    })
}

package object cards {
  val Ranks = List[Rank](Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  val Colors = List[Color](Clubs, Diamonds, Hearts, Spades)
}
