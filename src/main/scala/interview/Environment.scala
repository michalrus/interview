package interview

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object Environment {
  def props = Props[Environment]

  final case class Init(broker: ActorRef, parties: Broker.Parties)
  final case class Tell(whom: ActorRef, what: Any) // an ActorRef is used only as an identifier of a party/broker, to keep the code less complicated, see README.md
}

import Environment._

final class Environment extends Actor with ActorLogging {

  def receive = logTraffic andThen {
    case Init(broker, parties) ⇒
      broker ! parties

    case Tell(whom, what) ⇒
      whom ! what
  }

  def logTraffic: PartialFunction[Any, Any] = {
    case msg ⇒
      log.info("{} sent {}.", sender(), msg)
      msg
  }

}
