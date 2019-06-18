package controllers

import javax.inject._
import models.{PrivateKey, Transaction}
import play.api.mvc._
import services.TransactionService

import scala.concurrent.ExecutionContext

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, assets: Assets)(
  implicit ec: ExecutionContext
) extends AbstractController(cc) {

  /**
    * Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index: Action[AnyContent] = Action {
    Ok(views.html.index("Your new application is ready.")).withNewSession
  }

  def pushTx = Action {

    Ok(views.html.pushTx())
  }
  def postTx(hex: String, testnet: Boolean = false) = Action.async {
    for {
      result <- TransactionService.post(hex, testnet)
    } yield {
      Ok(result)
    }
  }

  val chapters = List(
    "01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12",
    "13",
    "14"
  )
  def book(chapter: String) = {

    val ch = s"ch$chapter-test.html"
    Assets.at(ch)
  }

  def toc = Action {
    Ok(views.html.toc(chapters))
  }

  def parseTx(hex: String) = Action {

    val tx = Transaction.parse(hex)
    Ok(views.html.transaction(tx))
  }

  def tx(id: String) = Action.async {
    for {
      tx <- Transaction.fromId(id, false)
      fee <- tx.fee
    } yield Ok(views.html.transaction(tx, fee))

  }

}
