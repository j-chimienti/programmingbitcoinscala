object PythonConverter extends App {





  val isVar = " = ".r
  val assertRegex = "self.assertEqual".r
  val spaceRegex = "var\\s{2,}"

  def convert(input: String) = {

    var lines = input.split("\n").toList
    var bar : List[String] = for (line <- lines) yield {
      val foo = if (isVar.findFirstIn(line).isDefined) "val " + line else line
      val f2 = foo.replaceAll(spaceRegex, """val """)
      val j = "'".r.replaceAllIn(f2, "\"")
      val bar = assertRegex.replaceAllIn(j, "assert")
      val bars = "True".r.replaceAllIn(bar, "true")
      val barss = "False".r.replaceAllIn(bars, "false")
      val u = "BytesIO".r.replaceAllIn(barss, "new ByteArrayInputStream")
      val h = "bytes.fromhex".r.replaceAllIn(u, "")
      val uu = "Tx.".r.replaceAllIn(barss, "Transaction\\.")
      val g = "tx_in".r.replaceAllIn(uu, "txIn")
      val i = ".serialize\\(\\)".r.replaceAllIn(g, "\\.serialize")
      val jj = ", want\\)".r.replaceAllIn(i, " == want\\)")
      jj
    }

    bar
//    val file = new File(fileName)
//    val fileWriter = new FileWriter(file) // to append FileWriter(file, true)
//    val buf = new BufferedWriter(fileWriter)
//    buf.write(bar.mkString("\n"))
//    buf.close()
//    println("Done")

  }

  val raw9 =
    """
      |        private_key = PrivateKey(secret=8675309)
      |        tx_ins = []
      |        prev_tx = bytes.fromhex('0025bc3c0fa8b7eb55b9437fdbd016870d18e0df0ace7bc9864efc38414147c8')
      |        tx_ins.append(TxIn(
      |            prev_tx=prev_tx,
      |            prev_index=0,
      |            script_sig=b'',
      |            sequence=0xffffffff,
      |        ))
      |        tx_outs = []
      |        h160 = decode_base58('mzx5YhAH9kNHtcN481u6WkjeHjYtVeKVh2')
      |        tx_outs.append(TxOut(amount=int(0.99 * 100000000), script_pubkey=p2pkh_script(h160)))
      |        h160 = decode_base58('mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf')
      |        tx_outs.append(TxOut(amount=int(0.1 * 100000000), script_pubkey=p2pkh_script(h160)))
      |
      |        tx = Tx(
      |            version=1,
      |            tx_ins=tx_ins,
      |            tx_outs=tx_outs,
      |            locktime=0,
      |            testnet=True,
      |        )
      |        self.assertTrue(tx.sign_input(0, private_key, SIGHASH_ALL))
      |
    """.stripMargin

  val result = convert(raw9)

  result.foreach { println }

}
