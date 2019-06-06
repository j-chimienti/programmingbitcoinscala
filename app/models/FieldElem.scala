package models

trait FieldElem[T] {

  if (num.isDefined && num.get > prime || num.get < 0)
    throw new RuntimeException(s"Num $num not in field range 0 to ${prime - 1}")

  def -(t: T): T
  def +(t: T): T
  def *(t: T): T
  def **(coeff: BigInt): T
  def num: Option[BigInt]
  def prime: BigInt
  def mult(coeff: BigInt): BigInt = {
    val n = num.get * coeff
    n.mod(prime)
  }

  def ==(obj: FieldElement): Boolean = num == obj.num && obj.prime == prime
  def equals(obj: FieldElement): Boolean = num == obj.num && obj.prime == prime

  def !=(_fieldEleem: FieldElement): Boolean =
    num != _fieldEleem.num || _fieldEleem.prime != prime

  final def assertEqPrimes(_prime: BigInt): Unit = {
    if (_prime != prime)
      throw new RuntimeException("Primes must be the same")
  }

  override def toString: String =
    s"FieldElement_$prime($num)"

}
