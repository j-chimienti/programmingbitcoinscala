package filters

import akka.stream.Materializer
import javax.inject.Inject
import org.slf4j.LoggerFactory
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.{ExecutionContext, Future}

class LoggingFilter @Inject()(implicit val mat: Materializer,
                              ec: ExecutionContext)
    extends Filter {

  //private val logger = LoggerFactory.getLogger(this.getClass)

  def apply(
    nextFilter: RequestHeader => Future[Result]
  )(requestHeader: RequestHeader): Future[Result] = {

    val startTime = System.currentTimeMillis
    nextFilter(requestHeader).map { result =>
      val endtime = System.currentTimeMillis

      val time = endtime - startTime
//      logger.info(
//        s"${requestHeader.method} ${requestHeader.uri} took $time ms and returned ${result.header.status}"
//      )
      println(
        s"${requestHeader.method} ${requestHeader.uri} - $time ms - ${result.header.status}"
      )
      result.withHeaders("REQUEST-TIME" -> time.toString)
    }
  }
}
