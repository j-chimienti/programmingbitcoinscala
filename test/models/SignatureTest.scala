package models

import org.specs2.mutable.Specification
import scala.util.Random

class SignatureTest extends Specification {

  "SignatureTest" should {

    "der" in {
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
      ok
    }

  }
}
