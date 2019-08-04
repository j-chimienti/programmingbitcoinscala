package models

import org.scalatest.WordSpec

class PrivateKeyTest extends WordSpec {

  "PrivateKeyTest" should {

    "get key" in {

      val pk =
        "7ecd4c3b77ea6afa6698248c595fee1d66255887cfe6732d3973661f27e21613"

      val privateKey = PrivateKey(pk)

      val wif = "L1UCPwc8nVDJmr7UsqgKz8oNstusJRS2HqQxz3sPsvgjXYra9nQi"

      assert(privateKey.wif() == wif)

    }

    // https://bitcointalk.org/index.php?topic=1716322.0
    "create address" in {
      val key = 1
      val addr = "1BgGZ9tcN4rm9KBzDn7KprQz87SZ26SAMH"
      val addrUncompressed = "1EHNa6Q4Jz2uvNExL497mE43ikXhwF6kZm"
      val pkHex =
        "0000000000000000000000000000000000000000000000000000000000000001"

      val pkBase64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE="
      val pubKey =
        "0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798".toLowerCase
      val pubKeyUn =
        "0479BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8".toLowerCase

      val wif = "5HpHagT65TZzG1PH3CSu63k8DbpvD8s5ip4nEB3kEsreAnchuDf"

      val pk = PrivateKey(key)
      assert(pk.secret.toHex == pkHex)
      assert(pk.point.address() == addr)
      assert(pk.point.sec().toHex == pubKey)
      assert(pk.point.sec(false).toHex == pubKeyUn)

      assert(pk.point.address(compressed = false) == addrUncompressed)
    }

    "resolve wif" in {
      val list = List(
        (
          BigInt(1), //0000000000000000000000000000000000000000000000000000000000000001
          "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn",
          (true, false)
        ),
        (
          BigInt(
            "115792089237316194620101962879192770082288938495059262778356087116516711989248"
          ), // ffffffffffffff80000000000000000000000000000000000000000000000000
          "L5oLkpV3aqBJ4BgssVAsax1iRa77G5CVYnv9adQ6Z87te7TyUdSC",
          (true, false)
        ),
        (
          BigInt(
            "115792089237316192209694896490707356769345799983315358995051596442327459037184"
          ), // fffffffffffffe00000000000000000000000000000000000000000000000000",
          "93XfLeifX7Jx7n7ELGMAf1SUR6f9kgQs8Xke8WStMwUtrDucMzn",
          (false, true)
        ),
        (
          BigInt(
            "0dba685b4511dbd3d368e5c4358a1277de9486447af7b3604a69b8d9d8b7889d",
            16
          ),
          "5HvLFPDVgFZRK9cd4C5jcWki5Skz6fmKqi1GQJf5ZoMofid2Dty",
          (false, false)
        ),
        (
          BigInt(
            "1cca23de92fd1862fb5b76e5f4f50eb082165e5191e116c18ed1a6b24be6a53f",
            16
          ),
          "cNYfWuhDpbNM1JWc3c6JTrtrFVxU4AGhUKgw5f93NP2QaBqmxKkg",
          (true, true)
        )
      )

      for ((num, expected, (compressed, testnet)) <- list) {

        val p = PrivateKey(num)
        assert(p.wif(compressed, testnet) == expected)

      }
    }

  }
}
