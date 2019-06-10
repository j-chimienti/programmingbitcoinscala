
import java.io._
import sys.process._

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

  val raw6 =
    """
      | script_pubkey_raw = bytes.fromhex('76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac')
      |        script_pubkey = Script.parse(script_pubkey_raw)
      |        self.assertEqual(script_pubkey.type(), 'p2pkh')
      |        self.assertEqual(script_pubkey.serialize(), script_pubkey_raw)
      |
      |        script_sig_raw = bytes.fromhex('483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a')
      |        script_sig = Script.parse(script_sig_raw)
      |        self.assertEqual(script_sig.type(), 'p2pkh sig')
      |        self.assertEqual(script_sig.serialize(), script_sig_raw)
      |        self.assertEqual(script_sig.signature(), bytes.fromhex('3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01'))
      |        self.assertEqual(script_sig.sec_pubkey(), bytes.fromhex('0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a'))
      |
    """.stripMargin


  //convert(raw6, "raw6.scala")

  val raw7 =
    """
      |  raw_tx = bytes.fromhex(
      |            '0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600')
      |        stream = BytesIO(raw_tx)
      |        tx = Tx.parse(stream)
      |        self.assertEqual(len(tx.tx_outs), 2)
      |        want = 32454049
      |        self.assertEqual(tx.tx_outs[0].amount, want)
      |        want = bytes.fromhex('76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac')
      |        self.assertEqual(tx.tx_outs[0].script_pubkey.serialize(), want)
      |        want = 10011545
      |        self.assertEqual(tx.tx_outs[1].amount, want)
      |        want = bytes.fromhex('76a9141c4bc762dd5423e332166702cb75f40df79fea1288ac')
      |        self.assertEqual(tx.tx_outs[1].script_pubkey.serialize(), want)
      |
    """.stripMargin



  convert(raw7, "outputs.scala")

  println("cat outputs.scala"!!)
}
