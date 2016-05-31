package interview

import language.higherKinds
import cats.Monad, cats.data.{ Xor, XorT }, cats.std.all._
import concurrent._, duration._
import ExecutionContext.Implicits.global
import org.scalatest._

class ExhaustiveMatchSpec extends WordSpec with Matchers {

  def retry[M[_], A, B](maxTimes: Int, when: A ⇒ Boolean, what: ⇒ XorT[M, A, B])(implicit M: Monad[M]): XorT[M, A, B] =
    XorT(M.flatMap(what.value) {
      case Xor.Left(a) if maxTimes > 1 && when(a) ⇒ retry(maxTimes - 1, when, what).value
      case r @ Xor.Right(_)                       ⇒ M.pure(r)
    })

  "ExhaustiveMatch" should {
    "be exhaustive⸮…" in {
      type X = XorT[Future, Int, String]
      val x1: X = XorT.left(Future.successful(5))
      val x2: X = retry(15, (_: Int) > 3, x1)
      info(Await.result(x2.value, Duration.Inf).toString)
    }
    // "be exhaustive — this one doesn’t compile" in {
    //   (??? : Xor[Int, String]) match {
    //     case Xor.Left(_) if false ⇒
    //     case Xor.Right(_) ⇒
    //   }
    // }
  }
}
