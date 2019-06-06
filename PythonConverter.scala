
import java.io._

object PythonConverter extends App {

    val raw =
      """
        |  secret = 888**3
        |        mainnet_address = '148dY81A9BmdpMhvYEVznrM45kWN32vSCN'
        |        testnet_address = 'mieaqB68xDCtbUBYFoUNcmZNwk74xcBfTP'
        |        point = secret*G
        |        self.assertEqual(
        |            point.address(compressed=True, testnet=False), mainnet_address)
        |        self.assertEqual(
        |            point.address(compressed=True, testnet=True), testnet_address)
        |        secret = 321
        |        mainnet_address = '1S6g2xBJSED7Qr9CYZib5f4PYVhHZiVfj'
        |        testnet_address = 'mfx3y63A7TfTtXKkv7Y6QzsPFY6QCBCXiP'
        |        point = secret*G
        |        self.assertEqual(
        |            point.address(compressed=False, testnet=False), mainnet_address)
        |        self.assertEqual(
        |            point.address(compressed=False, testnet=True), testnet_address)
        |        secret = 4242424242
        |        mainnet_address = '1226JSptcStqn4Yq9aAmNXdwdc2ixuH9nb'
        |        testnet_address = 'mgY3bVusRUL6ZB2Ss999CSrGVbdRwVpM8s'
        |        point = secret*G
        |        self.assertEqual(
        |            point.address(compressed=False, testnet=False), mainnet_address)
        |        self.assertEqual(
        |            point.address(compressed=False, testnet=True), testnet_address)
        |
  """.stripMargin

  val raw2 =
    """
      | point = S256Point(
      |            0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c,
      |            0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34)
      |        z = 0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
      |        r = 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
      |        s = 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
      |        self.assertTrue(point.verify(z, Signature(r, s)))
      |        z = 0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d
      |        r = 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c
      |        s = 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6
      |        self.assertTrue(point.verify(z, Signature(r, s)))
    """.stripMargin

  val raw3 =
    """
      | sec = bytes.fromhex('0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a')
      |        point = S256Point.parse(sec)
      |        want = 0xa56c896489c71dfc65701ce25050f542f336893fb8cd15f4e8e5c124dbf58e47
      |        self.assertEqual(point.y.num, want)
    """.stripMargin
  val isVar = " = ".r
  val assertRegex = "self.assertEqual".r
  val spaceRegex = "var\\s{2,}"

  def convert(input: String = raw, fileName: String = "./test.scala") = {

    var lines = input.split("\n").toList
    var bar : List[String] = for (line <- lines) yield {
      val foo = if (isVar.findFirstIn(line).isDefined) "val " + line else line
      val f2 = foo.replaceAll(spaceRegex, """val """)
      val j = "'".r.replaceAllIn(f2, "\"")
      val bar = assertRegex.replaceAllIn(j, "assert")
      val bars = "True".r.replaceAllIn(bar, "true")
      val barss = "False".r.replaceAllIn(bars, "false")
      barss
    }
    val file = new File(fileName)
    val fileWriter = new FileWriter(file) // to append FileWriter(file, true)
    val buf = new BufferedWriter(fileWriter)
    buf.write(bar.mkString("\n"))
    buf.close()
    println("Done")

  }

  //convert(raw2, "verify.scala")
  //convert(raw3, "parse.scala")
  val raw5 =
    """
      |testcases = (
      |            (1, 2),
      |            (randint(0, 2**256), randint(0, 2**255)),
      |            (randint(0, 2**256), randint(0, 2**255)),
      |        )
      |        for r, s in testcases:
      |            sig = Signature(r, s)
      |            der = sig.der()
      |            sig2 = Signature.parse(der)
      |            self.assertEqual(sig2.r, r)
      |            self.assertEqual(sig2.s, s)
    """.stripMargin
  convert(raw5, "raw.scala")
}
