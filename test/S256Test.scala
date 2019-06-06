import models.{Point, PointFE, S256Field, S256Point, Signature}
import org.scalatest.FunSuite
import scodec.bits.ByteVector

class S256Test extends FunSuite {

  import models.secp256kk1._

  test("Order") {
    val point = G * N
    assert(point.x.isEmpty)
  }
  test("pubpoint") {
    // write a test that tests the public point for the following
    val points: Seq[(BigInt, BigInt, BigInt)] = Seq(
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

    }
  }

  test("sec") {

    val c1 = BigInt(999).pow(3)
    val c2 = BigInt(123)
    val c3 = BigInt(42424242)

    val points: Seq[(BigInt, String, String)] = Seq(
      (
        c1,
        "049d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9",
        "039d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5"
      ),
      (
        c2,
        "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b",
        "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5"
      ),
      (
        c3,
        "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3",
        "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e"
      )
    )

    for ((coefficient, uncompressed, compressed) <- points) {
      val point: S256Point = G * coefficient
      val sec = point.sec(compressed = false).toArray
      val comp = point.sec(compressed = true).toArray
      assert(BigInt(sec) == BigInt(uncompressed, 16)) //  BigInt(uncompressed, 16).toByteArray
      assert(BigInt(comp) == BigInt(compressed, 16))

    }
  }

  test("address") {

    val secret = BigInt(888).pow(3)
    val mainnet_address = "148dY81A9BmdpMhvYEVznrM45kWN32vSCN"
    val testnet_address = "mieaqB68xDCtbUBYFoUNcmZNwk74xcBfTP"
    val point = G * secret
    assert(point.address(compressed = true, testnet = false) == mainnet_address)
    assert(point.address(compressed = true, testnet = true) == testnet_address)
    val secret1 = BigInt(321)
    val mainnet_address1 = "1S6g2xBJSED7Qr9CYZib5f4PYVhHZiVfj"
    val testnet_address1 = "mfx3y63A7TfTtXKkv7Y6QzsPFY6QCBCXiP"
    val point1 = G * secret1
    assert(
      point1.address(compressed = false, testnet = false) == mainnet_address1
    )
    assert(
      point1.address(compressed = false, testnet = true) == testnet_address1
    )
    val secret2 = BigInt(4242424242L)
    val mainnet_address2 = "1226JSptcStqn4Yq9aAmNXdwdc2ixuH9nb"
    val testnet_address2 = "mgY3bVusRUL6ZB2Ss999CSrGVbdRwVpM8s"
    val point2 = G * secret2
    assert(
      point2.address(compressed = false, testnet = false) == mainnet_address2
    )
    assert(
      point2.address(compressed = false, testnet = true) == testnet_address2
    )

  }

  test("verify") {

    val point = S256Point(
      "887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c",
      "61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34"
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

  test("parse") {

    val hex =
      "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    val sec = BigInt(hex, 16).toByteArray
    val point = S256Point.parse(sec)
    val want =
      "a56c896489c71dfc65701ce25050f542f336893fb8cd15f4e8e5c124dbf58e47"
    assert(point.y.get.num.get == BigInt(want, 16))
  }

  test("Reading hex string") {

    val hex =
      "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    val sec = BigInt(hex, 16).toByteArray

    val ss = ByteVector.fromValidHex(hex).toArray

    assert(BigInt(sec) == BigInt(ss))
  }

}
