package models

case class FieldElement(num: Option[Long] = None, prime: Long) {


  num match {


    case Some(number) =>

      if (number > prime || number < 0) {


        throw new RuntimeException(s"Num $num.get not in field range 0 to ${prime - 1}")
      }
    case None =>

  }
  @throws(classOf[RuntimeException])
  def +(_fieldElem: FieldElement): FieldElement = {


    assertEqPrimes(_fieldElem)
    val result = (BigInt(num.get) + _fieldElem.num.get).mod(prime)

    val _num = Some(result.toLong)
    FieldElement(_num, prime)
  }


  @throws(classOf[RuntimeException])
  def -(_fieldElem: FieldElement): FieldElement = {


    assertEqPrimes(_fieldElem)

    val result = (BigInt(num.get) - _fieldElem.num.get).mod(prime)
    FieldElement(Some(result.toLong), prime)

  }

  @throws(classOf[RuntimeException])
  def *(_fieldElem: FieldElement): FieldElement = {

    assertEqPrimes(_fieldElem)
    val result = (BigInt(num.get) * _fieldElem.num.get).mod(prime)
    FieldElement(Some(result.toLong), prime)
  }

  def *(coeff: Int) = {

    val n = num.get * BigInt(coeff)
    val _num = n.mod(prime).toLong

    FieldElement(Some(_num), prime)
  }

  @throws(classOf[RuntimeException])
  def /(_fieldElem: FieldElement): FieldElement = {

    assertEqPrimes(_fieldElem)
    // num = (self.num * pow(other.num, self.prime - 2, self.prime)) % self.prime

    val result = (BigInt(_fieldElem.num.get).modPow(prime - 2, prime) * num.get).mod(prime)

    FieldElement(Some(result.toLong), prime)

  }



  def **(_num: Int): FieldElement = {

    val result = BigInt(num.get).modPow(
      BigInt(_num).mod(prime - 1),
      prime
    )
    FieldElement(Some(result.toLong), prime)
  }

  def ==(obj: FieldElement): Boolean = {

    num == obj.num && obj.prime == prime

  }

  def equals(obj: FieldElement): Boolean = num == obj.num && obj.prime == prime

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



