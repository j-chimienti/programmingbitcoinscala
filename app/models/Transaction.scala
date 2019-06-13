package models

import scodec.bits.ByteVector
import java.io.{ByteArrayInputStream, InputStream}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  *
  * @param version Tranaction data format version
  * @param inputs Transaction inputs
  * @param outputs Transaction outputs
  * @param locktime the block number or timestamp at which this transaction is locked
  */
case class Transaction(version: Long,
                       inputs: Seq[TxIn],
                       outputs: Seq[TxOut],
                       locktime: Long) {

  def validate = {

    require(inputs.nonEmpty, "Inputs cannot be empty")
    require(outputs.nonEmpty, "Outputs cannot be empty")
    require(
      outputs.map(_.amount).sum < MaxMoney,
      "Sum of outputs amount is invalid"
    )
    val outPoints = inputs.map(_.prevTx.toHex)
    require(outPoints.size == outPoints.toSet.size, "Duplicate inputs")

  }

  override def toString: String = {
    var tx_ins = ""
    for (tx_in <- tx_ins) tx_ins += tx_in.toString + "\n"
    var tx_outs = ""
    for (tx_out <- tx_outs) tx_outs += tx_out.toString + "\n"
    s"version: $version\ntx_ins:\n$inputs\ntx_outs:\n$outputs\nlocktime: $locktime\n"
  }

  def isCoinbase = inputs.length == 1 && inputs.head.prevIdx == 0xffffffff
  //def hash = HashHelper.hash256(serialize).reverse

  def serialize = {

    val vers: Array[Byte] = HashHelper.intToLittleEndian(version.toInt, 4)
    var result = vers
    result = result ++ HashHelper.encodeVarint(inputs.length)
    for (txIn <- inputs) {
      result = result ++ txIn.serialize
    }
    result = result ++ HashHelper.encodeVarint(outputs.length)
    for (txOut <- outputs) {
      result = result ++ txOut.serialize
    }
    result ++ HashHelper.intToLittleEndian(locktime.toInt, 4)

  }

  /**
    *   Fee of the tx in satoshi
    * @param testnet
    * @return
    */
  def fee(testnet: Boolean = false): Future[Long] = {

    val inputValue: Seq[Future[Long]] =
      inputs
        .map(input => input.value(testnet))

    for {
      inputSum <- Future.sequence(inputValue).map(_.sum)
    } yield {

      val outputSum = outputs.map(_.amount).sum
      inputSum - outputSum
    }
  }

//  def coinbaseHeight =
//    HashHelper.littleEndianToInt(inputs.head.scriptPubKey)

}

object Transaction {

  // if lockTime >= LOCKTIME_THRESHOLD it is a unix timestamp otherwise it is a block height
  val LOCKTIME_THRESHOLD = 500000000L

  def parse(hexStr: String): Transaction = {

    val stream = new ByteArrayInputStream(
      ByteVector.fromValidHex(hexStr).toArray
    )
    parse(stream)
  }
  def parse(stream: InputStream): Transaction = {

    val buf = new Array[Byte](4)
    stream.read(buf)
    val version = HashHelper.littleEndianToInt(buf)
    val numInputs = HashHelper.readVarint(stream)
    val inputs: Seq[TxIn] = for (_ <- 1L to numInputs)
      yield TxIn.parse(stream)
    val numOutputs = HashHelper.readVarint(stream)
    val outputs: Seq[TxOut] = for (_ <- 1L to numOutputs)
      yield TxOut.parse(stream)
    val lt = new Array[Byte](4)
    stream.read(lt)
    val lockTime = HashHelper.littleEndianToInt(lt)
    Transaction(version.toInt, inputs, outputs, lockTime.toLong)
  }
}
