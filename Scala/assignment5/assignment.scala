import pizzeria._
import orders._

object Assignment extends App {
  val p = new Pizza(pizza.Funghi, size.Large, crust.Thick, None, None);
  val p2 = new Pizza(pizza.Margherita, size.Large, crust.Thin, Some(meat.Salami), None);
  val p3 = new Pizza(pizza.Funghi, size.Large, crust.Thick, None, Some(topping.Ketchup));
  val o = new Order(
    "Jan Nowak",
    "Anywhere 1",
    "+48 333 333 3333",
    List(p, p2, p3),
    List(drink.Lemonade),
    Some(discount.Student),
    Some("Please knock")
  )

  println(o)
}
