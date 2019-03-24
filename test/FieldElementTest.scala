import models.FieldElement
import org.scalatest._


class FieldElementTest extends FunSuite with BeforeAndAfter {



  test("element creation") {

    val j =  FieldElement(2, 4)

    val jj =  FieldElement(3, 4)

    val result: FieldElement = jj + j

    assert(result ==  FieldElement(1, 4))

    val a =  FieldElement(2, 31)
    val b =  FieldElement(15, 31)

    val result1: FieldElement = a + b

    assert(result1 ==  FieldElement(17, 31))


  }

  test("test_ne") {

    val a =  FieldElement(2, 31)
    val b =  FieldElement(2, 31)
    val c =  FieldElement(15, 31)
    assert(a == b)
    assert(a != c)
  }


  test("test_add") {
    val a =  FieldElement(2, 31)
    val b =  FieldElement(15, 31)

    assert((a + b) ==  FieldElement(17, 31))
    val a1 =  FieldElement(17, 31)
    val b1 =  FieldElement(21, 31)
    assert((a1 + b1) ==  FieldElement(7, 31))
  }


  test("test_sub") {
    val a =  FieldElement(29, 31)
    val b =  FieldElement(4, 31)
    assert((a - b) ==  FieldElement(25, 31))
    val a1 =  FieldElement(15, 31)
    val b1 =  FieldElement(30, 31)

    assert((a1 - b1) ==  FieldElement(16, 31))

  }


  test("test_mul") {
    val a =  FieldElement(24, 31)
    val b =  FieldElement(19, 31)
    assert((a*b) ==  FieldElement(22, 31))
  }

  test("test_pow") {
    val a =  FieldElement(17, 31)
    assert(a ** 3 ==  FieldElement(15, 31))
    val a1 =  FieldElement(5, 31)
    val b1 =  FieldElement(18, 31)
    assert((a1 ** 5 * b1) ==  FieldElement(16, 31))
  }


  test("test_div") {


    val fe1 = FieldElement(2, 19)
    val fe2 = FieldElement(7, 19)
    val fe3 = FieldElement(5, 19)

    assert((fe1 / fe2) == FieldElement(3, 19))

    assert((fe2 / fe3) == FieldElement(9, 19))

    val a =  FieldElement(3, 31)
    val b =  FieldElement(24, 31)

    assert((a / b) ==  FieldElement(4, 31))

    val a1 =  FieldElement(17, 31)


    assert((a1 ** -3) == FieldElement(29, 31))

    val a2 =  FieldElement(4, 31)
    val b1 =  FieldElement(11, 31)

    assert((a2 ** -4 * b1) ==  FieldElement(13, 31))
  }


}
