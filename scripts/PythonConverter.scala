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
      |script_pubkey = BytesIO(bytes.fromhex('6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937'))
      |        script = Script.parse(script_pubkey)
      |        want = bytes.fromhex('304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601')
      |        self.assertEqual(script.cmds[0].hex(), want.hex())
      |        want = bytes.fromhex('035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937')
      |        self.assertEqual(script.cmds[1], want)
    """.stripMargin

  val result = convert(raw9)

  result.foreach { println }

}
