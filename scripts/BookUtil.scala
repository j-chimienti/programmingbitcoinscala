
import java.io.{BufferedWriter, _}

import scala.io.Source

object BookUtil extends App {


  val l = List("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14")


  def makeHTML = {

    for (i <- l) {

      val file = Source.fromFile(s"../public/ch$i.html")
      val lines = file.getLines.toList


      def updateLine(line: String) : String = {

        val regex = """<img src="images/""".r
        val replaced = """<img src="/assets/images/"""
        regex.replaceAllIn(line, replaced)
      }

      val updated = lines.map(l => updateLine(l))

      file.close()

      val s = updated.mkString("\n")

      val f = new File(s"../public/ch$i-test.html")

      val bw = new BufferedWriter(new FileWriter(f))

      bw.write(s)

      bw.close()
    }
  }


}
