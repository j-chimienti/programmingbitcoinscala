//package daos
//
//import javax.inject.Inject
//import models.Transaction
//import org.bson.codecs.Codec
//import org.bson.codecs.configuration.{CodecProvider, CodecRegistries}
//import org.mongodb.scala.MongoDatabase
//import org.mongodb.scala.bson.codecs.Macros._
//import org.mongodb.scala.bson.codecs.{DEFAULT_CODEC_REGISTRY, Macros}
//import org.bson.codecs.configuration.CodecRegistries.{
//  fromProviders,
//  fromRegistries
//}
//
//class TransactionDAO @Inject()(db: MongoDatabase,
//                               aCodecs: Seq[Codec[_]] = Seq.empty[Codec[_]],
//                               aProviders: Seq[CodecProvider]) {
//
//  private final val collectionName = "transactions"
//
//  private val codecRegistry = fromRegistries(
//    fromProviders(
//      Seq(Macros.createCodecProvider[Transaction]()) ++ aProviders: _*
//    ),
//    CodecRegistries.fromCodecs(aCodecs: _*),
//    DEFAULT_CODEC_REGISTRY
//  )
//
//  val collection =
//    db.getCollection[Transaction](collectionName)
//      .withCodecRegistry(codecRegistry)
//
//  def insert(tx: Transaction) =
//    collection.insertOne(tx).toFutureOption
//
//}
