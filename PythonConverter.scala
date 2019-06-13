
import java.io._
import sys.process._

object PythonConverter extends App {


  val isVar = " = ".r
  val assertRegex = "self.assertEqual".r
  val spaceRegex = "var\\s{2,}"

  def convert(input: String, fileName: String = "./test.scala") = {

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
    val file = new File(fileName)
    val fileWriter = new FileWriter(file) // to append FileWriter(file, true)
    val buf = new BufferedWriter(fileWriter)
    buf.write(bar.mkString("\n"))
    buf.close()
    println("Done")

  }
  val raw9 =
    """
      |raw_tx = bytes.fromhex(
      |            '0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600')
      |        stream = BytesIO(raw_tx)
      |        tx = Tx.parse(stream)
      |        want = '0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a'
      |        self.assertEqual(tx.tx_ins[0].sec_pubkey().hex(), want)
      |
    """.stripMargin

  convert(raw9, "outputs.scala")

  println("cat outputs.scala"!!)
}
