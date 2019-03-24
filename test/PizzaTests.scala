import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class PizzaTests extends FunSuite with BeforeAndAfter {

  var items: Seq[String] = Seq("Ribeye", "NY Strip", "Chuck")

//    before {
//      items.++:("PRIME RIB")
//    }

  test("new pizza has zero toppings") {

    assert(items.size == 3)
  }

  test("adding one topping") {

    items = items :+ "t bone"
    assert(items.size == 4)
    assert(items.last === "t bone")
  }

  // mark that you want a test here in the future
  test("test pizza pricing")(pending)

//  after {
//    println(items.toString())
//  }

}
