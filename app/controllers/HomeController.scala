package controllers

import akka.actor.ActorSystem
import controllers.Assets.Asset
import javax.inject._
import models.{PrivateKey, Transaction, UserInput}
import play.api.data.Form
import play.api.mvc._
import services.TransactionService
import scala.util.Try
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.filters.csrf.CSRF

import scala.concurrent.ExecutionContext

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents,
                               assets: Assets,
                               actorSystem: ActorSystem
                               //txService: TransactionService
)(implicit ec: ExecutionContext)
    extends AbstractController(cc) {

  def serve(file: Asset) = {

    assets.versioned(path = "/public", file)
  }

  def serveImg(file: Asset) = {

    assets.versioned(path = "/public/images", file)
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
    val ch = s"ch$chapter.html"
    Assets.at(ch)
  }

  def toc = Action {
    Ok(views.html.toc(chapters))
  }

  def parseTx(hex: String) = Action {
    val tx = Transaction.parse(hex)
    Ok(views.html.transaction(tx))
  }

  def tx(id: String) = Action.async { implicit request =>
    for {
      tx <- Transaction.fromId(id, false)
      fee <- tx.fee
    } yield Ok(views.html.transaction(tx, fee))

  }

  def privateKey(pk: String) = Action { implicit request =>
    Ok(views.html.privateKey(PrivateKey(BigInt(pk))))
  }

  val userForm: Form[UserInput] = Form(
    mapping("userInput" -> text)(UserInput.apply)(UserInput.unapply)
  )

  def explorer = Action { implicit req: RequestHeader =>
    Ok(views.html.explorer(userForm))
  }
  def userInput = Action { implicit request =>
    userForm.bindFromRequest.fold(
      formWithErrors => {
        Ok("error")
      },
      userInput =>
        Redirect(routes.HomeController.tx(userInput.input))
          .flashing("success" -> "Contact saved!")
    )

  }

  def block(block: String) = Action {

    Ok(views.html.block())
  }

}
