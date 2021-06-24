package orders

import pizzeria._

class Order(
  name: String,
  address: String,
  phone: String,
  pizzas: List[Pizza],
  drinks: List[drink.Drink],
  discount: Option[pizzeria.discount.Discount],
  specialInfo: Option[String]
) {
  require(phone.matches("^\\+(?:[0-9] ?){6,14}[0-9]$"), "invalid phone number")

  override def toString() = {
    val builder = new StringBuilder

    builder ++= s"Name: $name\n"
    builder ++= s"Address: $address\n"
    builder ++= s"Phone: $phone\n"
    builder ++= "Pizzas: " ++ pizzas.mkString(", ") ++ "\n"
    builder ++= "Drinks: " ++ drinks.map(_.getClass.getSimpleName.split("\\$")(0)).mkString(", ") ++ "\n"

    if(discount.isDefined) {
      builder ++= "Discount: " ++ discount.get.getClass.getSimpleName.split("\\$")(0) ++ "\n"
    }

    if(discount.isDefined) {
      builder ++= "Additional info: " ++ specialInfo.get ++ "\n"
    }

    builder ++= f"Price: $$$price%.2f"

    builder.toString
  }

  def extraMeatPrice: Option[Double] = {
    val withMeat = pizzas.filter(_.extraMeat.isDefined).map(_.extraMeat.get)
    Option.unless(withMeat.isEmpty)(withMeat.map(_.price)).map(_.sum)
  }
  def pizzasPrice: Option[Double] = Option.unless(pizzas.isEmpty)(pizzas.map(_.price)).map(_.sum)
  def drinksPrice: Option[Double] = Option.unless(drinks.isEmpty)(drinks.map(_.price)).map(_.sum)
  def priceByType(pizzaType: pizza.Type): Option[Double] =
    Option.unless(pizzas.isEmpty)(pizzas.filter(_.pizzaType == pizzaType).map(_.price)).filter(_.nonEmpty).map(_.sum)

  val price: Double = {
    import pizzeria.discount._

    val pizzaDiscount = discount.map(_ match {
      case Student => 0.95
      case Senior  => 0.93
    }).getOrElse(1.0)
    val drinksDiscount = discount.map(_ match {
      case Senior => 0.93
      case _ => 1.0
    }).getOrElse(1.0)

    pizzas.map(_.price).sum * pizzaDiscount + drinks.map(_.price).sum * drinksDiscount
  }
}
