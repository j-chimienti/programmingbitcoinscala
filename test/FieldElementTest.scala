import models.FieldElement
import org.scalatest._

class FieldElementTest extends FunSuite with BeforeAndAfter {

  test("Class creation") {

    val j = FieldElement(Some(2), 4)

    val jj = FieldElement(Some(3), 4)

    val result: FieldElement = jj + j

    assert(result == FieldElement(Some(1), 4))

    val a = FieldElement(Some(2), 31)
    val b = FieldElement(Some(15), 31)

    val result1: FieldElement = a + b

    assert(result1 == FieldElement(Some(17), 31))

  }

  test("ne") {

    val a = FieldElement(Some(2), 31)
    val b = FieldElement(Some(2), 31)
    val c = FieldElement(Some(15), 31)
    assert(a == b)
    assert(a != c)
  }

  test("add") {
    val a = FieldElement(Some(2), 31)
    val b = FieldElement(Some(15), 31)

    assert((a + b) == FieldElement(Some(17), 31))
    val a1 = FieldElement(Some(17), 31)
    val b1 = FieldElement(Some(21), 31)
    assert((a1 + b1) == FieldElement(Some(7), 31))
  }

  test("sub") {
    val a = FieldElement(Some(29), 31)
    val b = FieldElement(Some(4), 31)
    assert((a - b) == FieldElement(Some(25), 31))
    val a1 = FieldElement(Some(15), 31)
    val b1 = FieldElement(Some(30), 31)

    assert((a1 - b1) == FieldElement(Some(16), 31))

  }

  test("mul") {
    val a = FieldElement(Some(24), 31)
    val b = FieldElement(Some(19), 31)
    assert((a * b) == FieldElement(Some(22), 31))
  }

  test("pow") {
    val a = FieldElement(Some(17), 31)
    assert(a ** 3 == FieldElement(Some(15), 31))
    val a1 = FieldElement(Some(5), 31)
    val b1 = FieldElement(Some(18), 31)
    assert((a1 ** 5 * b1) == FieldElement(Some(16), 31))
  }

  test("div") {

    val fe1 = FieldElement(Some(2), 19)
    val fe2 = FieldElement(Some(7), 19)
    val fe3 = FieldElement(Some(5), 19)

    assert((fe1 / fe2) == FieldElement(Some(3), 19))

    assert((fe2 / fe3) == FieldElement(Some(9), 19))

    val a = FieldElement(Some(3), 31)
    val b = FieldElement(Some(24), 31)

    assert((a / b) == FieldElement(Some(4), 31))

    val a1 = FieldElement(Some(17), 31)

    assert((a1 ** -3) == FieldElement(Some(29), 31))

    val a2 = FieldElement(Some(4), 31)
    val b1 = FieldElement(Some(11), 31)

    assert((a2 ** -4 * b1) == FieldElement(Some(13), 31))
  }

}
