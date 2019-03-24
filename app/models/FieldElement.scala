package models

case class FieldElement(num: Long, prime: Long) {


  if (num > prime || num < 0) {

    throw new RuntimeException(s"Num $num not in field range 0 to ${prime - 1}")
  }

  @throws(classOf[RuntimeException])
  def +(_fieldElem: FieldElement): FieldElement = {


    assertEqPrimes(_fieldElem)
    val result = (BigInt(num) + _fieldElem.num).mod(prime)

    FieldElement(result.toLong, prime)
  }


  @throws(classOf[RuntimeException])
  def -(_fieldElem: FieldElement): FieldElement = {


    assertEqPrimes(_fieldElem)

    val result = (BigInt(num) - _fieldElem.num).mod(prime)
    FieldElement(result.toLong, prime)

  }

  @throws(classOf[RuntimeException])
  def *(_fieldElem: FieldElement): FieldElement = {

    assertEqPrimes(_fieldElem)
    val result = (BigInt(num) * _fieldElem.num).mod(prime)
    FieldElement(result.toLong, prime)
  }

  @throws(classOf[RuntimeException])
  def /(_fieldElem: FieldElement): FieldElement = {

    assertEqPrimes(_fieldElem)
    // num = (self.num * pow(other.num, self.prime - 2, self.prime)) % self.prime

    val result = (num * BigInt(_fieldElem.num).modPow(prime - 2, prime)).mod(prime)

    FieldElement(result.toLong, prime)

  }



  def **(_num: Int): FieldElement = {

    val result = BigInt(num).modPow(
      BigInt(_num).mod(prime - 1),
      prime
    )
    FieldElement(result.toLong, prime)
  }

  def ==(obj: FieldElement): Boolean = {

    num == obj.num && obj.prime == prime

  }

  def !=(_fieldEleem: FieldElement): Boolean = {

    num != _fieldEleem.num || _fieldEleem.prime != prime

  }


  override def toString: String = {

    s"FieldElement_$prime($num)"
  }


  private final def assertEqPrimes(_fieldElem: FieldElement): Unit = {

    if (_fieldElem.prime != prime) {

      throw new RuntimeException("Primes must be the same")
    }
  }

}



