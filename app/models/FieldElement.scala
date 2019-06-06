package models

case class FieldElement(num: Option[BigInt] = None, prime: BigInt)
    extends FieldElem[FieldElement] {

  def +(fieldElement: FieldElement): FieldElement =
    FieldElement(Some(add(fieldElement.num, fieldElement.prime)), prime)

  def add(_num: Option[BigInt], _prime: BigInt): BigInt = {
    assertEqPrimes(_prime)
    (num.get + _num.get).mod(prime)
  }

  def -(_fieldElem: FieldElement): FieldElement =
    FieldElement(Some(minus(_fieldElem)), prime)

  def minus(fieldElement: FieldElement): BigInt =
    minus(fieldElement.num, fieldElement.prime)

  def minus(_num: Option[BigInt], _prime: BigInt): BigInt = {
    assertEqPrimes(_prime)
    (num.get - _num.get).mod(prime)
  }

  def /(fieldElement: FieldElement): FieldElement =
    /(fieldElement.num, fieldElement.prime)

  def /(_num: Option[BigInt], _prime: BigInt): FieldElement = {
    assertEqPrimes(_prime)
    val result = (_num.get.modPow(prime - 2, prime) * num.get).mod(prime)
    FieldElement(Some(result), prime)
  }

  def *(_fieldElem: FieldElement): FieldElement = {
    assertEqPrimes(_fieldElem.prime)
    val result = mult(_fieldElem.num.get)
    FieldElement(Some(result), prime)
  }
  def *(coeff: Int): FieldElement = FieldElement(Some(mult(coeff)), prime)
  def **(_num: BigInt): FieldElement = FieldElement(Some(pow(_num)), prime)
  def pow(_num: BigInt): BigInt = num.get.modPow(_num.mod(prime - 1), prime)

}
