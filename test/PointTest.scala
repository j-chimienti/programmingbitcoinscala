import models.Point
import org.scalatest.{BeforeAndAfter, FunSuite}

class PointTest extends FunSuite with BeforeAndAfter{


  test("throws exception if point not on line") {

    assertThrows[RuntimeException](Point(x= -2, y=4, a=5, b=7))
    assertThrows[RuntimeException](Point(-1, -2, 5, 7))

  }
  test("test_ne") {
    val a = Point(x = 3, y = -7, a = 5, b = 7)
    val b = Point(x = 18, y = 77, a = 5, b = 7)
    assert(a != b)
    assert(!(a != a))
  }

  test("test_add") {
    val a = Point(x = Double.PositiveInfinity, y = Double.PositiveInfinity, a = 5, b = 7)
    val b = Point(x = 2, y = 5, a = 5, b = 7)
    val c = Point(x = 2, y = -5, a = 5, b = 7)
    assert(a + b == b)
    assert(b + a == b)
    assert(b + c == a)

  }
  test("test_add01") {
    val a = Point(x = 3, y = 7, a = 5, b = 7)
    val b = Point(x = -1, y = -1, a = 5, b = 7)
    assert(a + b == Point(x = 2, y = -5, a = 5, b = 7))
  }
  test("test_add02") {
    val a = Point(x = -1, y = 1, a = 5, b = 7)
    assert(a + a == Point(x = 18, y = -77, a = 5, b = 7))
  }


  test("test_on_curve") {



    Point(x = 3, y = -7, a = 5, b = 7)
    Point(x = 18, y = 77, a = 5, b = 7)
  }
  test("test_add0") {

  val a = Point(x = Double.PositiveInfinity, y=Double.PositiveInfinity, a=5, b=7)
  val b = Point(x = 2, y=5, a=5, b=7)
  val c = Point(x = 2, y= -5, a=5, b=7)
  assert(a+b == b)
  assert(b+a == b)
  assert(b+c == a)
  }

  test("test_add1") {

  val a = Point(x = 3, y=7, a=5, b=7)
  val b = Point(x = -1, y= -1, a=5, b=7)
  assert(a+b == Point(x = 2, y= -5, a=5, b=7))

  }
  test("test_add2") {

  val a = Point(x = -1, y=1, a=5, b=7)
  assert(a+a == Point(x=18, y= -77, a=5, b=7))
  }


}
