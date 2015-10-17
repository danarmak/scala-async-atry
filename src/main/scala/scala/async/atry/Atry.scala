package scala.async.atry

import scala.language.experimental.macros

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.macros.blackbox.Context
import scala.async.Async._
import scala.util.{Failure, Success, Try}

class Acatchable[T](val result: Try[T]) extends AnyVal {
  def acatch[U >: T](handler: Throwable => U): U = macro Acatchable.acatchImpl[T, U]
}

object Acatchable {
  def acatchImpl[T: c.WeakTypeTag, U >: T : c.WeakTypeTag](c: Context)(handler: c.Expr[Throwable => U]): c.Expr[U] = {
    import c.universe._

    val q"(..$params) => $body" = handler.tree
    val q"$param match { case ..$cases }" = body

    val tried = q"${c.prefix.tree}.result"
    
    val tree =
      q"""$tried match {
         case scala.util.Success(t) => t
         case scala.util.Failure(e) => e match { case ..$cases }
       }"""
    c.Expr[U](tree)
  }
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
