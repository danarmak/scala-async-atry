package scala.async.atry

import scala.language.experimental.macros

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.macros.blackbox.Context
import scala.async.Async._
import scala.util.{Failure, Success, Try}

class Acatchable[T](val result: Try[T]) extends AnyVal {
  def acatch[U >: T](handler: PartialFunction[Throwable, U])
                    (implicit ec: ExecutionContext): U = result match {
    case Success(t) => t
    case Failure(t) => handler.applyOrElse(t, (t: Throwable) => throw t)
  }
}

object Acatchable {
  def wrap[T](block: => T): Acatchable[T] = new Acatchable[T](
    try {
      Success(block)
    }
    catch {
      case t: Throwable => Failure(t)
    }
  )
}

class Atry(val c: Context) {
  import c.universe._

  def atryImpl[T: c.WeakTypeTag](block: c.Expr[T]): c.Expr[Acatchable[T]] = {
    val tree =
      q"""val future: scala.concurrent.Future[scala.util.Try[${weakTypeOf[T]}]] = try {
            async(scala.util.Success($block))
          } catch {
            case t: Throwable => scala.concurrent.Future.failed(t)
          }

          val recovered = future.recover {
            case t: Throwable => scala.util.Failure(t)
          }

          new scala.async.atry.Acatchable(await(recovered))
        """
    c.Expr[Acatchable[T]](tree)
  }
}

object Atry {
  def atry[T](block: T): Acatchable[T] = macro Atry.atryImpl[T]
}
