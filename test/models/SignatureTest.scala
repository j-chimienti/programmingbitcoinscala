package models

import org.scalatest.WordSpec
import scala.util.Random

class SignatureTest extends WordSpec {

  "der" should {
    "create serialized Signature" in {
      val r = new Random
      val testcases = List(
        (BigInt(1), BigInt(2)),
        (BigInt(256, r), BigInt(255, r)),
        (BigInt(256, r), BigInt(255, r))
      )
      for ((r, s) <- testcases) {
        val sig = Signature(r, s)
        val der = sig.der
        val sig2 = Signature.parse(der)
        assert(sig2.r == r)
        assert(sig2.s == s)
      }

    }
  }
}
