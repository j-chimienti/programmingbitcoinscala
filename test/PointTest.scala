import models.Point
import org.scalatest.{BeforeAndAfter, FunSuite}

class PointTest extends FunSuite with BeforeAndAfter{


  test("throws exception if point not on line") {

    assertThrows[RuntimeException](Point(Some(-2), Some(4), a=5, b=7))
    assertThrows[RuntimeException](Point(Some(-1), Some(-2), 5, 7))

  }
  test("test_ne") {
    val a = Point(Some(3), Some( -7), a = 5, b = 7)
    val b = Point(Some(18),  Some(77), a = 5, b = 7)
    assert(a != b)
    assert(!(a != a))
  }

  test("test_add") {
    val a = Point(None,  None, a = 5, b = 7)
    val b = Point(Some(2),  Some(5), a = 5, b = 7)
    val c = Point(Some(2),  Some(-5), a = 5, b = 7)
    assert(a + b == b)
    assert(b + a == b)
    assert(b + c == a)

  }
  test("test_add01") {
    val a = Point(Some(3),  Some(7), a = 5, b = 7)
    val b = Point(Some(-1),  Some(-1), a = 5, b = 7)
    assert(a + b == Point(Some(2),  Some(-5), a = 5, b = 7))
  }
  test("test_add02") {
    val a = Point(Some(-1),  Some(1), a = 5, b = 7)
    assert(a + a == Point(Some(18),  Some(-77), a = 5, b = 7))
  }


  test("test_on_curve") {



    Point(Some(3),  Some(-7), a = 5, b = 7)
    Point(Some(18),  Some(77), a = 5, b = 7)
  }
  test("test_add0") {

  val a = Point(None,  None, a=5, b=7)
  val b = Point(Some(2), Some(5), a=5, b=7)
  val c = Point(Some(2),  Some(-5), a=5, b=7)
  assert(a+b == b)
  assert(b+a == b)
  assert(b+c == a)
  }

  test("test_add1") {

  val a = Point(Some(3), Some(7), a=5, b=7)
  val b = Point(Some(-1),  Some(-1), a=5, b=7)
  assert(a+b == Point(Some(2), Some( -5), a=5, b=7))

  }
  test("test_add2") {

  val a = Point(Some(-1), Some(1), a=5, b=7)
  assert(a+a == Point(Some(18),  Some(-77), a=5, b=7))
  }


}
