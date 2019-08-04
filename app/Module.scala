import com.google.inject.{AbstractModule, Provides}
// import org.mongodb.scala.{MongoClient, MongoDatabase}
import services.TransactionService

/**
  * This class is a Guice module that tells Guice how to bind several
  * different types. This Guice module is created when the Play
  * application starts.

  * Play will automatically use any class called `Module` that is in
  * the root package. You can create modules in other locations by
  * adding `play.modules.enabled` settings to the `application.conf`
  * configuration file.
  */
class Module extends AbstractModule {

  override def configure() = {
    // Ask Guice to create an instance of TransactionService when the
    // applicat ion starts.
    bind(classOf[TransactionService]).asEagerSingleton()
  }

//  @Provides
//  def provideMongoDatabase: MongoDatabase = {
//
//    // To directly connect to the default server localhost on port 27017
//    val mongoClient: MongoClient = MongoClient()
//
//    // or provide custom MongoClientSettings
//
//    val database: MongoDatabase = mongoClient.getDatabase("programmingbitcoin")
//    database
//  }

}
