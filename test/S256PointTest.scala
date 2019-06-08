import java.math.BigInteger

import fr.acinq.bitcoin.Base58.Prefix
import fr.acinq.bitcoin.{Base58Check, ByteVector32}
import models.{HashHelper, PrivateKey, S256Point, Signature}
import org.scalatest.{FlatSpec, FunSuite, WordSpec}
import scodec.bits._
import models.secp256kk1._

class S256PointTest extends FlatSpec {

  // https://www.palkeo.com/en/blog/stealing-bitcoin.html
  // https://en.bitcoin.it/wiki/Private_key

//  "brainwallet" should "make wallet" in {
//
//    val wallet = PrivateKey("my secret")
//
//    val x = "028d003eab2e428d11983f3e97c3fa0addf3b42740df0d211795ffb3be2f6c52"
//
//    val y = "0ae987b9ec6ea159c78cb2a937ed89096fb218d9e7594f02b547526d8cd309e2"
//
//    assert(true)
//    //
//
//  }

  "bitcoin examples" should "return addr" in {

    val hexKeys = List(
      (
        hex"0B4D879488BCA8998D60AE318D3F7DB971E75F712CF4B6989BCDB6453B247F3C",
        "1Nja7tnLuQmT1Yqim3FQocs93aqDBmw7dS"
      )
    )

    for ((key, want) <- hexKeys) {

      val priv = PrivateKey(key)

      val pub = priv.point

      val addr = pub.address(compressed = false, testnet = false)

      assert(addr == want)
    }
    val intKeys = List((0, "1EFYgihxCVizJ8MUTdCmgecPQBBtXKm8yY"))

  }

  "pubpoint" should "initialize the secp256k1 point" in {
    // write a test that tests the public point for the following

    type Secret = BigInt
    type X = BigInt
    type Y = BigInt
    val points: Seq[(Secret, X, Y)] = Seq(
      // secret, x, y
      (
        BigInt(7),
        BigInt(
          "5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc",
          16
        ),
        BigInt(
          "6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da",
          16
        )
      ),
      (
        BigInt(1485),
        BigInt(
          "c982196a7466fbbbb0e27a940b6af926c1a74d5ad07128c82824a11b5398afda",
          16
        ),
        BigInt(
          "7a91f9eae64438afb9ce6448a1c133db2d8fb9254e4546b6f001637d50901f55",
          16
        )
      ),
      (
        BigInt(2).pow(128),
        BigInt(
          "8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da",
          16
        ),
        BigInt(
          "662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82",
          16
        )
      ),
      (
        BigInt(2).pow(240) + BigInt(2).pow(31),
        BigInt(
          "9577ff57c8234558f293df502ca4f09cbc65a6572c842b39b366f21717945116",
          16
        ),
        BigInt(
          "10b49c67fa9365ad7b90dab070be339a1daf9052373ec30ffae4f72d5e66d053",
          16
        )
      )
    )
    // iterate over points
    for ((secret, x, y) <- points) {
      // initialize the secp256k1 point (S256Point)
      val point: S256Point = S256Point(x, y)
      // check that the secret*G is the same as the point
      assert(G * secret == point)

      println(point.address())
    }
  }

  // fixme: test includes more 00 than python
  "sec" should "return public key" in {

    // coefficient, uncompressed public key,
    val points: Seq[(BigInt, String, String)] = Seq(
      (
        BigInt(999).pow(3),
        "04009d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9",
        "03009d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5"
      ),
      (
        BigInt(123),
        "0400a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b",
        "0300a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5"
      ),
      (
        BigInt(42424242),
        "0400aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3",
        "0300aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e"
      )
    )

    for ((coefficient, uncompressed, compressed) <- points) {
      val point: S256Point = G * coefficient
      assert(point.sec(compressed = false).toHex == uncompressed)
      assert(point.sec(compressed = true).toHex == compressed)

    }
  }

  "address" should "be generated" in {

    val components = List(
      (
        BigInt(888).pow(3),
        "148dY81A9BmdpMhvYEVznrM45kWN32vSCN",
        "mieaqB68xDCtbUBYFoUNcmZNwk74xcBfTP",
        true,
        true
      ),
      (
        BigInt(321),
        "1S6g2xBJSED7Qr9CYZib5f4PYVhHZiVfj",
        "mfx3y63A7TfTtXKkv7Y6QzsPFY6QCBCXiP",
        false,
        false
      ),
      (
        BigInt(4242424242L),
        "1226JSptcStqn4Yq9aAmNXdwdc2ixuH9nb",
        "mgY3bVusRUL6ZB2Ss999CSrGVbdRwVpM8s",
        false,
        false
      )
    )

    for ((secret, mainnet, testnet, comp1, comp2) <- components) {
      //val pk = PrivateKey(ByteVector(secret.toByteArray).padLeft(32))
      val pk = PrivateKey(secret.toLong)
      val pubKey = pk.point
      assert(pubKey.address(compressed = comp1, testnet = false) == mainnet)
      if (testnet.length > 0)
        assert(pubKey.address(compressed = comp2, testnet = true) == testnet)
    }
  }

  "verify" should "work" in {

    val point = S256Point(
      BigInt(
        "887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c",
        16
      ),
      BigInt(
        "61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34",
        16
      )
    )
    val points = List(
      (
        "ec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60",
        "ac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395",
        "68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4"
      ),
      (
        "7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d",
        "eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c",
        "c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6"
      )
    )
    for ((z, r, s) <- points) assert(point.verify(z, Signature(r, s)))

  }

  "parse" should "work" in {

    val sec =
      hex"0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    val point = S256Point.parse(sec)
    val want =
      hex"0xa56c896489c71dfc65701ce25050f542f336893fb8cd15f4e8e5c124dbf58e47"
    assert(point.y.get.num.get.toString(16) == want.toHex)
  }
}
