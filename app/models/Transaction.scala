package models

import java.io.{ByteArrayInputStream, InputStream, OutputStream}

import fr.acinq.bitcoin.ByteVector32
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  *
  * @param version Tranaction data format version
  * @param txIn Transaction txIn
  * @param txOut Transaction txOut
  * @param locktime the block number or timestamp at which this transaction is locked
  */
case class Transaction(version: Long,
                       txIn: Seq[TxIn],
                       txOut: Seq[TxOut],
                       locktime: Long,
                       testnet: Boolean = false)
    extends BtcSerializable[Transaction] {

  def hash: HashHelper.ByteVector32 = HashHelper.hash256(serialize)
  def txId: HashHelper.ByteVector32 = hash.reverse
  def isCoinbase: Boolean = Transaction.isCoinbase(this)
  override def serializer: BtcSerializer[Transaction] = Transaction

  def serialize: ByteVector = Transaction.serialize(this)

  override def toString: String = {
    var tx_ins = ""
    for (tx_in <- tx_ins) tx_ins += tx_in.toString + "\n"
    var tx_outs = ""
    for (tx_out <- tx_outs) tx_outs += tx_out.toString + "\n"
    s"version: $version\ntx_ins:\n$txIn\ntx_outs:\n$txOut\nlocktime: $locktime\n"
  }

  val PROTOCOL_VERSION = 70015

}

object Transaction extends BtcSerializer[Transaction] {

  import HashHelper._

  // if lockTime >= LOCKTIME_THRESHOLD it is a unix timestamp otherwise it is a block height
  val LOCKTIME_THRESHOLD = 500000000L

  def parse(hexStr: String, testnet: Boolean): Transaction =
    parse(hexStr).copy(testnet = testnet)

  def parse(hexStr: String): Transaction = {
    val stream = new ByteArrayInputStream(
      ByteVector.fromValidHex(hexStr).toArray
    )
    parse(stream)
  }

  def parse(stream: InputStream): Transaction = parse(stream, false)
  def parse(stream: InputStream, testnet: Boolean = false): Transaction = {
    val version = uint32(stream)
    val txInCount = readVarint(stream)
    val txIn: Seq[TxIn] = for (_ <- 1L to txInCount)
      yield TxIn.parse(stream)
    val txOutCount = readVarint(stream)
    val txOut: Seq[TxOut] = for (_ <- 1L to txOutCount)
      yield TxOut.parse(stream)
    val locktime = uint32(stream)
    Transaction(version.toInt, txIn, txOut, locktime, testnet)
  }

  def serialize(tx: Transaction, out: OutputStream): Unit = {
    writeUInt32(tx.version.toInt, out)
    writeVarint(tx.txIn.length, out)
    for (input <- tx.txIn) TxIn.serialize(input, out)
    writeVarint(tx.txOut.length, out)
    for (output <- tx.txOut) TxOut.serialize(output, out)
    writeUInt32(tx.locktime.toInt, out)

  }

  val Zeroes =
    "0000000000000000000000000000000000000000000000000000000000000000"

  /**
    * Coinbase transactions must have exactly one input.
    *
    * The one input must have a previous transaction of 32 bytes of 00.
    *
    * The one input must have a previous index of ffffffff.
    *
    * @param tx
    * @return
    */
  def isCoinbase(tx: Transaction): Boolean =
    tx.txIn.length == 1 && tx.txIn.head.prevIdx == 0xffffffff && tx.txIn.head.prevTx.toHex == Zeroes

  override def validate(tx: Transaction): Unit = {

    require(tx.txIn.nonEmpty, "Inputs cannot be empty")
    require(tx.txOut.nonEmpty, "Outputs cannot be empty")
    require(
      tx.txOut.map(_.amount).sum < MaxMoney,
      "Sum of txOut amount is invalid"
    )
    val outPoints = tx.txIn.map(_.prevTx.toHex)
    require(outPoints.size == outPoints.toSet.size, "Duplicate txIn")

  }

}
