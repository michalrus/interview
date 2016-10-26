package interview

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import java.util.UUID

object Environment {
  def props = Props[Environment]

  final case class Init(broker: ActorRef, parties: Broker.Parties)
  final case class Tell(whom: ActorRef, what: Any) // an ActorRef is used only as an identifier of a party/broker, to keep the code less complicated, see README.md
  final case class VerificationResult(sessionId: UUID, result: Boolean)
}

import Environment._

final class Environment extends Actor with ActorLogging {

  def receive = state(0)

  def state(verificationsReceived: Int): Receive = logTraffic andThen {
    case Init(broker, parties) ⇒
      broker ! parties

    case Tell(whom, what) ⇒
      whom ! what

    case VerificationResult(_, _) ⇒
      val vr = verificationsReceived + 1
      context.become(state(vr))
      if (vr >= 2) {
        val _ = context.system.terminate()
      }

  }

  def logTraffic: PartialFunction[Any, Any] = {
    case msg ⇒
      log.info("{} sent {}.", sender(), msg)
      msg
  }

}
