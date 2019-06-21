package models

import java.nio.charset.StandardCharsets

import org.scalatest.FlatSpec

class ScriptTest extends FlatSpec {

  behavior of "ScriptTest"

  // The entire PDF of Satoshi’s original whitepaper is encoded in this transaction in block 230009:
  //
  //54e48e5f5c656b26c3bca14a8c95aa583d07ebe84dde3b7dd4a78f4e4186e713

  // The creator of this transaction split up the whitepaper PDF into 64-byte chunks, which were then made into invalid uncompressed public keys. The whitepaper was encoded into 947 1-of-3 bare multisig outputs. These outputs are not spendable but have to be indexed in the UTXO sets of full nodes. This is a tax every full node has to pay and is in that sense abusive.

  // Biggest tx to data

  // bb41a757f405890fb0f5856228e23b715702d714d59bf2b1feb70d8b2b4e3e08

  // took over a min to validate

  it should "Decode ScriptSig for the Genesis Block’s Coinbase Transaction" in {

    val tx = "0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098"
    val ss =
      "04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73"

    val script = Script(ss)

    script.elements(2).asInstanceOf[OP_PUSHDATA].data

    val want =
      "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    val result = new String(
      script.elements(2).asInstanceOf[OP_PUSHDATA].data.toArray,
      StandardCharsets.US_ASCII
    )

    assert(result == want)

  }

  it should "parse" in {

    val script_pubkey =
      "6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"
    val script = Script.parse(script_pubkey)
    val want =
      "304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601"

    val elem = script.elements(1).asInstanceOf[OP_PUSHDATA].data
    assert(elem.toHex == want)
    val want1 =
      "035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"
    assert(script.elements.last.asInstanceOf[OP_PUSHDATA].data.toHex == want1)

  }

  it should "p2pkh" in {

    val pubKey = "76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"
    val script: Script = Script(pubKey)
    val scriptPubKey = script.serialize
    assert(script.`type` == "p2pkh")
    assert(scriptPubKey.toHex == pubKey)
    val script_sig_raw =
      "483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    val script_sig = Script(script_sig_raw)
    assert(script_sig.`type` == "p2pkh sig")
    assert(script_sig.serialize.toHex == script_sig_raw)

    val sig = script_sig.signature(0)
    assert(
      sig.toHex == "3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01"
    )
    assert(
      script_sig
        .secPubkey()
        .toHex == "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    )

  }
// The main feature of p2sh is that it’s flexible and at the same time reduces the UTXO set size by pushing the burden of storing part of the script back to the user.
  it should "p2sh" in {

    val h = "a91474d691da1574e6b3c192ecfb52cc8984ee7b6c5687"
    val script = Script(h)
    assert(script.`type` == "p2sh")
    assert(script.serialize.toHex == h)

    val h2 =
      "00483045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4ee942a8993701483045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e75402201475221022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb702103b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb7152ae"
    val script2 = Script(h2)
    assert(script2.`type` == "p2sh sig")

    assert(script2.serialize.toHex == h2)

    val sig = script2.signature(0)
    val sig2 = script2.signature(1)
    assert(
      sig.toHex == "3045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4ee942a8993701"
    )
    assert(
      sig2.toHex == "3045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e75402201"
    )

    assert(
      script2
        .secPubkey()
        .toHex == "022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb70"
    )
    assert(
      script2
        .secPubkey(1)
        .toHex == "03b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb71"
    )
  }

}
