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

// import org.mongodb.scala._
import scala.concurrent.ExecutionContext

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(
  cc: ControllerComponents,
  assets: Assets,
  actorSystem: ActorSystem,
  transactionService: TransactionService
)(implicit ec: ExecutionContext)
    extends AbstractController(cc) {

  def index = {

    toc

  }

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
      result <- transactionService.post(hex, testnet)
    } yield {
      Ok(result)
    }
  }

  val chapters: List[Int] = (for (i <- 1 to 14) yield i).toList
  val ch: Map[Int, String] = chapters
    .map(ch => {
      val chapter = if (ch < 10) "0" + ch.toString else ch.toString
      val file = s"public/ch$chapter.html"
      val html = scala.io.Source.fromFile(file).mkString
      (ch, html)
    })
    .toMap
  def book(chapter: Int) = Action {
    val cc = ch.get(chapter)
    cc match {
      case Some(fc) => Ok(views.html.chapter(chapter, fc))
      case None     => Ok(views.html.toc(chapters))

    }
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
      tx <- transactionService.fetch(id, false)
      fee <- transactionService.fee(tx)
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
