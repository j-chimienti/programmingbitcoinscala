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
      |  tx = TxFetcher.fetch('452c629d67e41baec3ac6f04fe744b4b9617f8f859c63b3002f8684e7a4fee03')
      |        self.assertTrue(tx.verify())
      |        tx = TxFetcher.fetch('5418099cc755cb9dd3ebc6cf1a7888ad53a1a3beb5a025bce89eb1bf7f1650a2', testnet=True)
      |        self.assertTrue(tx.verify())
    """.stripMargin

  val result = convert(raw9)

  result.foreach { println }

}
