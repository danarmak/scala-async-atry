package scala.async.atry

import scala.async.Async._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import Atry._

object AtryTest extends App {

  val fut = async {

    atry {
      throw new IllegalArgumentException
      await(Future(1 + 1))
//      await(Future(throw new IllegalArgumentException))
      1
    } acatch {
      case e: IllegalArgumentException => 2
    }
  }

  val result = Await.result(fut, Duration.Inf)
  println(result)
}