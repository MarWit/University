package pizzeria

package object pizza {
  sealed abstract class Type(val price: Double);
  case object Margherita extends Type(5);
  case object Pepperoni extends Type(6.5);
  case object Funghi extends Type(7);
}

package object size {
  sealed abstract class Size(val multiplier: Double);
  case object Small extends Size(0.9);
  case object Regular extends Size(1.0);
  case object Large extends Size(1.5);
}

package object crust {
  sealed abstract class Crust;
  case object Thin extends Crust;
  case object Thick extends Crust;
}

package object topping {
  sealed abstract class Topping(val price: Double);
  case object Ketchup extends Topping(0.5);
  case object Garlic extends Topping(0.5);
}

package object meat {
  sealed abstract class Meat(val price: Double);
  case object Salami extends Meat(1);
}

package object drink {
  sealed abstract class Drink(val price: Double);
  case object Lemonade extends Drink(2);
}

package object discount {
  sealed abstract class Discount;
  case object Student extends Discount;
  case object Senior extends Discount;
}

case class Pizza(
  pizzaType: pizza.Type,
  size: pizzeria.size.Size,
  crust: pizzeria.crust.Crust,
  extraMeat: Option[meat.Meat],
  extraTopping: Option[topping.Topping]
) {
  override def toString() = {
    val getName = (c: java.lang.Object) => c.getClass.getSimpleName.split("\\$")(0)
    val builder = new StringBuilder

    builder ++= getName(size) + " " ++ getName(crust) ++ " " ++ getName(pizzaType)
    if(extraMeat.isDefined) {
      builder ++= " with extra " ++ getName(extraMeat.get)
    }

    if(extraTopping.isDefined) {
      builder ++= " topped with " ++ getName(extraTopping.get)
    }

    builder.toString
  }

  val price: Double =
    (pizzaType.price +
     extraMeat.map(_.price).getOrElse(0.0) +
     extraTopping.map(_.price).getOrElse(0.0)
    ) * size.multiplier
}
