name := "programmingbitcoinscala"

version := "1.0"

lazy val `programmingbitcoinscala` =
  (project in file("."))
    .enablePlugins(PlayScala)
//.settings(PlayKeys.playDefaultPort := 6666, PlayKeys.playDefaultAddress :=)

PlayKeys.playDefaultPort := 9000
PlayKeys.playDefaultAddress := "localhost"

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

//enablePlugins(DockerPlugin)

scalaVersion := "2.11.11"

lazy val akkaHttpVersion = "10.1.1"
lazy val akkaVersion = "2.5.11"
lazy val GatlingTest = config("gatling") extend Test
// resolvers += "dl-john-ky" at "http://dl.john-ky.io/maven/releases"
// resolvers += "moma" at "https://github.com/prassee/moma/raw/master/snapshots"
libraryDependencies ++= Seq(
  jdbc,
  ehcache,
  ws,
  specs2 % Test,
  guice,
  // https://mvnrepository.com/artifact/com.madgag.spongycastle/core
  //"com.madgag.spongycastle" % "core" % "1.50.0.0",
  // https://mvnrepository.com/artifact/org.bouncycastle/bcprov-jdk15on
  "org.bouncycastle" % "bcprov-jdk15on" % "1.61",
  "fr.acinq" % "bitcoin-lib_2.11" % "0.11",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  //"org.bitcoinj" % "bitcoinj-core" % "0.14.5"
)

unmanagedResourceDirectories in Test <+= baseDirectory(
  _ / "target/web/public/test"
)
