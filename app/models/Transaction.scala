package models

import scodec.bits.ByteVector
import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  OutputStream
}

import HashHelper._

import fr.acinq.bitcoin.Protocol.{writeCollection, writeUInt32}

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

  val PROTOCOL_VERSION = 70015
  def serialize(out: OutputStream): Unit = {

    writeUInt32(version.toInt, out)
    writeVarint(inputs.length, out)
    for (input <- inputs) input.serialize(out)
    writeVarint(outputs.length, out)
    for (output <- outputs) output.serialize(out)
    writeUInt32(locktime.toInt, out)

  }

  def serialize: ByteVector = {

    val out = new ByteArrayOutputStream()
    serialize(out)
    ByteVector.view(out.toByteArray)

  }

  /**
    *   Fee of the tx in satoshi
    * @param testnet
    * @return
    */
  def fee(testnet: Boolean = false): Future[Long] = {

    val list = inputs.map(input => input.value(testnet))
    for {
      inputsRaw <- Future.sequence(list)

    } yield {
      val inputsFound = inputsRaw.flatten
      if (inputs.length != inputsFound.length)
        throw new RuntimeException(s"Error fetching tx fee")

      val inputsSum = inputsFound.sum
      val outputsSum = outputs.map(_.amount).sum
      inputsSum - outputsSum
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

    val version = uint32(stream)
    val numInputs = readVarint(stream)
    val inputs: Seq[TxIn] = for (_ <- 1L to numInputs)
      yield TxIn.parse(stream)
    val numOutputs = readVarint(stream)
    val outputs: Seq[TxOut] = for (_ <- 1L to numOutputs)
      yield TxOut.parse(stream)
    val locktime = uint32(stream)
    Transaction(version.toInt, inputs, outputs, locktime)
  }
}
