package object money {
  trait Currency {
    override def toString = super.getClass.getSimpleName.dropRight(1)
  }

  case object $ extends Currency
  case object `€` extends Currency
  case object zł extends Currency
  val USD = $
  val EUR = `€`
  val PLN = zł

  implicit class CurrencyFromInt[T : Numeric](amount: T) {
    def apply(currency: Currency): Money = Money(implicitly[Numeric[T]].toDouble(amount), currency)
  }

  val conversion: Map[(Currency, Currency), BigDecimal] = Map(
    (PLN, USD) -> 0.259103,
    (PLN, EUR) -> 0.233711423,
    (USD, EUR) -> 0.902001993,
    (USD, PLN) -> 3.85946901,
    (EUR, USD) -> 1.108645,
    (EUR, PLN) -> 4.27878103
  )

  case class CurrencyConverter(
    conversion: Map[(Currency, Currency), BigDecimal]) {
      def convert(from: Currency, to: Currency): BigDecimal =
        if(from == to) 1 else conversion((from, to))
  }

  implicit val standardConversion = CurrencyConverter(conversion)

  case class Money(amount: BigDecimal, currency: Currency) {
    val currencyConverter = implicitly[CurrencyConverter]

    def +(money: Money) = {
      val newMoney = money as currency
      Money(amount + newMoney.amount, currency)
    }
    def -(money: Money) = {
      val newMoney = money as currency
      Money(amount - newMoney.amount, currency)
    }
    def *(mul: BigDecimal) = Money(amount * mul, currency)
    def /(mul: BigDecimal) = Money(amount / mul, currency)
    def as(cur: Currency) = Money(amount * currencyConverter.convert(currency, cur), cur)
    def <(money: Money) = {
      val newMoney = money as currency
      amount < newMoney.amount
    }
    def >(money: Money) = money < this

    override def toString: String = s"$amount$currency"
  }
}
