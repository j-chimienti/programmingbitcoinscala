import models.{FieldElement, PointFE}
import org.scalatest._

class ECCTest extends FunSuite with BeforeAndAfter {

  test("test_on_curve") {

    // tests the following points whether they are on the curve or not
    // on curve y ^ 2 = x ^ 3 - 7 over F_223:
    val prime = 223
    val a = FieldElement(Some(0), prime)
    val b = FieldElement(Some(7), prime)

    val valid_points = Seq((192, 105), (17, 56), (1, 193))
    val invalid_points = Seq((200, 119), (42, 99))

    // iterate over valid points
    for (point <- valid_points) {

      val (x_raw, y_raw) = point
      val x = FieldElement(Some(x_raw), prime)
      val y = FieldElement(Some(y_raw), prime)
      // Creating the point should not result in an error
      PointFE(Some(x), Some(y), a, b)

    }
    // iterate over invalid points
    for (point <- invalid_points) {

      val (x_raw, y_raw) = point
      val x = FieldElement(Some(x_raw), prime)
      val y = FieldElement(Some(y_raw), prime)

      assertThrows[RuntimeException](PointFE(Some(x), Some(y), a, b))
    }
  }

  test("add") {

    // tests the following additions on curve y^2=x^3-7 over F_223:
    // (192,105) + (17,56)
    // (47,71) + (117,141)
    // (143,98) + (76,66)
    val prime = 223
    val a = FieldElement(Some(0), prime)
    val b = FieldElement(Some(7), prime)

    val additions: Seq[(Int, Int, Int, Int, Int, Int)] = Seq(
      // (x1, y1, x2, y2, x3, y3)
      (192, 105, 17, 56, 170, 142),
      (47, 71, 117, 141, 60, 139),
      (143, 98, 76, 66, 47, 71)
    )
    // iterate over the additions
    for ((x1_raw, y1_raw, x2_raw, y2_raw, x3_raw, y3_raw) <- additions) {
      val x1 = FieldElement(Some(x1_raw), prime)
      val y1 = FieldElement(Some(y1_raw), prime)
      val p1 = PointFE(Some(x1), Some(y1), a, b)
      val x2 = FieldElement(Some(x2_raw), prime)
      val y2 = FieldElement(Some(y2_raw), prime)
      val p2 = PointFE(Some(x2), Some(y2), a, b)
      val x3 = FieldElement(Some(x3_raw), prime)
      val y3 = FieldElement(Some(y3_raw), prime)
      val p3 = PointFE(Some(x3), Some(y3), a, b)
      // check that p1 + p2 == p3
      assert(p1 + p2 == p3)
    }
  }

  test("test_rmul") {

    val prime = 223
    val a = FieldElement(Some(0), prime)
    val b = FieldElement(Some(7), prime)

    val multiplications = Seq(
      // (coefficient, x1, y1, x2, y2)
      (2, 192, 105, 49, 71),
      (2, 143, 98, 64, 168),
      (2, 47, 71, 36, 111),
      (4, 47, 71, 194, 51),
      (8, 47, 71, 116, 55)
    )

    // iterate over the multiplications
    for ((s, x1_raw, y1_raw, x2_raw, y2_raw) <- multiplications) {

      val x1 = FieldElement(Some(x1_raw), prime)
      val y1 = FieldElement(Some(y1_raw), prime)
      val p1 = PointFE(Some(x1), Some(y1), a, b)

      val x2 = FieldElement(Some(x2_raw), prime)
      val y2 = FieldElement(Some(y2_raw), prime)
      val p2 = PointFE(Some(x2), Some(y2), a, b)

      // check that the product is equal to the expected point
      assert(p1 * s == p2)
    }
    val (s, x1_raw, y1_raw, x2_raw, y2_raw) = (21, 47, 71, None, None)

    val x1 = FieldElement(Some(x1_raw), prime)
    val y1 = FieldElement(Some(y1_raw), prime)
    val p1 = PointFE(Some(x1), Some(y1), a, b)
    val p2 = PointFE(x2_raw, y2_raw, a, b)

    // check that the product is equal to the expected point
    assert(p1 * s == p2)
  }

}
