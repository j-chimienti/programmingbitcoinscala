package models

object secp256kk1 {
  // y^2=x^3+7
  val A = BigInt(0)
  val B = BigInt(7)
  // the prime of the field = 2^256-2^32-977
  val P: BigInt = BigInt(2).pow(256) - BigInt(2).pow(32) - 977
  val G : S256Point = S256Point(
    Some(S256Field(Some(BigInt("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798", 16)))),
    Some(S256Field(Some(BigInt("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8", 16))))
  )
  val N = BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)
}
