package models

import java.io.{InputStream, OutputStream}
import java.nio.ByteOrder

import scodec.bits.ByteVector

case class Block(version: Long,
                 prevBlock: ByteVector,
                 merkleRoot: ByteVector,
                 timestamp: Long,
                 bits: ByteVector,
                 nonce: ByteVector,
                 txHashes: List[ByteVector] = List.empty[ByteVector]) {

  def hash: HashHelper.ByteVector32 = Block.hash(this)
  def serialize: ByteVector = Block.serialize(this)

  def bip9: Boolean =
    (version >> 29) == 0x0b001

  /**
    *   Whether signaling BIP91 readiness
    *   is signalled if the 5th bit from the right is 1
    * @return
    */
  def bip91: Boolean = ((version >> 4) & 1) == 1

  /**
    * Whether this block is signaling BIP141 readiness
    * signalled from 2nd bit from the right is 1
    * @return
    */
  def bip141: Boolean = ((version >> 1) & 1) == 1

  def checkPOW: Boolean = {
    val proof = HashHelper.uint32(hash.toArray, ByteOrder.LITTLE_ENDIAN)
    proof < target
  }
  def target: Int = {

    1
  }

}

object Block extends BtcSerializer[Block] {

  import HashHelper._
  def parse(data: InputStream): Block = {
    val version = uint32(data)
    val prevBock = new Array[Byte](32)
    data.read(prevBock)
    val prevBlock = ByteVector(prevBock.reverse)
    val _merkleRoot = new Array[Byte](32)
    data.read(_merkleRoot)
    val merkleRoot = ByteVector(_merkleRoot.reverse)
    val timestamp = uint32(data)
    val bits = new Array[Byte](4) // uint32(data)
    val nonce = new Array[Byte](4)
    data.read(bits)
    data.read(nonce)

    Block(
      version,
      prevBlock,
      merkleRoot,
      timestamp,
      ByteVector(bits),
      ByteVector(nonce)
    )

  }

  def hash(block: Block): ByteVector32 = {

    val s = serialize(block)
    val h256 = hash256(s)
    h256.reverse
  }

  def serialize(block: Block, out: OutputStream): Unit = {

    writeUInt32(block.version, out)
    out.write(block.prevBlock.reverse.toArray)
    out.write(block.merkleRoot.reverse.toArray)
    writeUInt32(block.timestamp, out)
    out.write(block.bits.toArray)
    out.write(block.nonce.toArray)

  }

  def targetToBits(target: Long) = {

    val rawBytes = ByteVector.fromLong(target).padLeft(32)
    val foo: Array[Byte] = rawBytes.dropWhile(_ == 0).toArray
    val (exponent, coeff): (Long, Array[Byte]) =
      if (foo.head > 0x7f) (foo.length + 1, foo.take(2).+:(0x00.toByte))
      else (foo.length, foo.take(3))
    val result: Array[Byte] = coeff.reverse :+ exponent.toByte
    result
  }
}
